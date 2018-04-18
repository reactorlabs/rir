#include "CodeEditor.h"

#include "R/RList.h"

#include "BC.h"
#include "CodeStream.h"
#include "analysis/dataflow.h"

#include <iomanip>
#include <iostream>

namespace rir {

std::unordered_set<CodeEditor::Iterator> CodeEditor::next(CodeEditor::Iterator ins) {
    std::unordered_set<CodeEditor::Iterator> result;
    if (isExitPoint(ins))
        return result;
    // add jump target
    if (isJmp(ins)) {
        result.insert(target(ins));
    }
    // add next instruction
    if (!isUncondJmp(ins)) {
        result.insert(ins + 1);
    }
    return result;
}

CodeEditor::CodeEditor(SEXP in) {
    SEXP bc = in;
    assert(TYPEOF(in) == EXTERNALSXP || TYPEOF(in) == CLOSXP);
    if (TYPEOF(in) == CLOSXP) {
        ::Function* f = isValidClosureSEXP(in);
        assert(f != nullptr);
        formals_ = FORMALS(in);
        DispatchTable* dispatchTable = DispatchTable::unpack(BODY(in));
        bc = dispatchTable->first()->container();
    } else {
        assert(isValidFunctionObject(in));
    }
    Function* f = Function::unpack(bc);
    Code* ch = f->body();
    ast = src_pool_at(globalContext(), ch->src);
    localsCnt = ch->localsCount;
    loadCode(f, ch, true);
}

CodeEditor::CodeEditor(Code* code) {
    ast = src_pool_at(globalContext(), code->src);
    localsCnt = code->localsCount;
    loadCode(code->function(), code, false);
}

CodeEditor::CodeEditor(Code* code, SEXP formals) {
    formals_ = formals;
    ast = src_pool_at(globalContext(), code->src);
    localsCnt = code->localsCount;
    loadCode(code->function(), code, true);
}

void CodeEditor::loadCode(Function* function, Code* code,
                          bool loadCompiledDefaultArgs) {
    std::unordered_map<Opcode*, LabelT> bcLabels;

    // Add promises that are default values of formal arguments
    if (loadCompiledDefaultArgs) {
        unsigned idx = 0;
        for (auto c : *function) {
            if (c->isDefaultArgument) {
                CodeEditor* p = new CodeEditor(c);
                if (promises.size() <= idx)
                    promises.resize(idx + 1, nullptr);
                promises[idx] = p;
                defaultArguments.push_back(idx);
            }
            ++idx;
        }
    }

    {
        Opcode* pc = code->code();
        Opcode* end = code->endCode();
        while (pc != end) {
            BC bc = BC::decode(pc);
            if (bc.isJmp()) {
                Opcode* target = BC::jmpTarget(pc);
                if (!bcLabels.count(target))
                    bcLabels[target] = mkLabel();
            }
            BC::advance(&pc);
        }
    }

    labels_.resize(labels_.size() + bcLabels.size());

    {
        BytecodeList* pos = & front;

        Opcode* pc = code->code();
        Opcode* end = code->endCode();

        while (pc != end) {
            pos->next = new BytecodeList(pc);
            auto prev = pos;
            pos = pos->next;
            pos->prev = prev;

            if (bcLabels.count(pc)) {
                LabelT label = bcLabels[pc];
                pos->bc = BC::label(label);
                labels_[label] = pos;

                pos->next = new BytecodeList();
                auto prev = pos;
                pos = pos->next;
                pos->prev = prev;
            }

            pos->srcIdx = code->getSrcIdxAt(pc, true);

            BC bc = BC::advance(&pc);
            if (bc.isJmp()) {
                Opcode* target = (Opcode*)((uintptr_t)pc + bc.immediate.offset);
                LabelT label = bcLabels[target];
                bc.immediate.offset = label;
            }

            // If this is a call, we copy the callsite information locally
            if (bc.isCallsite()) {
                auto oldCs = bc.callSite(code);
                unsigned needed = oldCs->size();
                pos->callSite = (CallSite*)new char[needed];
                memcpy(pos->callSite, oldCs, needed);
            }
            if (bc.hasPromargs()) {
                if (bc.bc == Opcode::promise_ || bc.bc == Opcode::push_code_) {
                    Code* code = function->codeAt(bc.immediate.fun);

                    unsigned idx = function->indexOf(code);
                    bc.immediate.fun = idx;

                    CodeEditor* p = new CodeEditor(code);

                    if (promises.size() <= idx)
                        promises.resize(idx + 1, nullptr);

                    promises[idx] = p;
                } else {
                    auto oldCs = bc.callSite(code);
                    auto nargs = oldCs->nargs;

                    CallSite* cs = pos->callSite;
                    assert(cs->nargs == oldCs->nargs);

                    // Load all code objects of the callsite and update
                    // the indices (in the CodeEditor they are not offsets
                    // into the code object, but index into promises vector).
                    for (unsigned i = 0; i < nargs; ++i) {
                        auto arg = oldCs->args()[i];
                        if (arg <= MAX_ARG_IDX) {
                            Code* code = function->codeAt(arg);
                            arg = function->indexOf(code);
                            CodeEditor* p = new CodeEditor(code);
                            if (promises.size() <= arg)
                                promises.resize(arg + 1, nullptr);
                            promises[arg] = p;
                            cs->args()[i] = arg;
                        }
                    }
                }
            }

            pos->bc = bc;
        }

        last.prev = pos;
        pos->next = & last;
    }
}

CodeEditor::~CodeEditor() {
    for (auto p : promises)
        delete p;

    BytecodeList* pos = front.next;
    while (pos != & last) {
        BytecodeList* patch = pos->patch;
        while (patch) {
            BytecodeList* old = patch;
            patch = patch->next;
            delete old;
        }
        BytecodeList* old = pos;
        pos = pos->next;
        delete old;
    }
}

void CodeEditor::print(bool verbose) {

    DataflowAnalysis<Type::Conservative> analysis;
    DataflowAnalysis<Type::Optimistic> specAnalysis;
    if (verbose) {
        analysis.analyze(*this);
        specAnalysis.analyze(*this);
    }

    for (auto cur = getCursor(); !cur.atEnd(); cur.advance()) {
        if (cur.src() && verbose) {
            Rprintf("     # ");
            Rf_PrintValue(cur.src());
        }
        BC bc = cur.bc();
        if (verbose) {
            // Print some analysis info
            if (bc.bc != Opcode::label && bc.bc != Opcode::return_ &&
                bc.bc != Opcode::ret_) {
                if (bc.bc == Opcode::ldvar_ || bc.bc == Opcode::ldfun_) {
                    SEXP sym = bc.immediateConst();
                    auto v = analysis[cur][sym];
                    auto sv = specAnalysis[cur][sym];
                    Rprintf("   ~~ ");
                    if (v.t == FValue::Type::Argument)
                        Rprintf("argument\n");
                    else if (v.isPresent())
                        Rprintf("local\n");
                    else if (sv.t == FValue::Type::Argument)
                        Rprintf("probably argument\n");
                    else if (sv.isPresent())
                        Rprintf("probably local\n");
                    else
                        Rprintf("??\n");
                } else if (bc.popCount() > 0) {
                    bool assumedIsBetter = false;
                    for (int i = bc.popCount() - 1; i >= 0; --i) {
                        if (analysis[cur].stack()[i] !=
                            specAnalysis[cur].stack()[i]) {
                            assumedIsBetter = true;
                            break;
                        }
                    }

                    Rprintf("   ~~ TOS : ");
                    for (int i = bc.popCount() - 1; i >= 0; --i) {
                        analysis[cur].stack()[i].print();
                        Rprintf(", ");
                    }
                    if (assumedIsBetter) {
                        Rprintf("  /  Assumed: ");
                        for (int i = bc.popCount() - 1; i >= 0; --i) {
                            specAnalysis[cur].stack()[i].print();
                            Rprintf(", ");
                        }
                    }
                    Rprintf("\n");
                }
            }
        }
        cur.print(verbose);
    }
    FunIdxT i = 0;
    for (auto p : promises) {
        ++i;
        if (!p)
            continue;

        bool isfp = false;
        for (auto fp : defaultArguments)
            if (i - 1 == fp) {
                isfp = true;
                break;
            }

        Rprintf("------------------------\n");
        if (isfp)
            Rprintf("# default argument\n");
        else
            Rprintf("@%d\n", (void*)(long)(i - 1));
        p->print(verbose);
    }
}

void CodeEditor::Cursor::print(bool verbose) {
    if (verbose && pos->callSite)
        pos->bc.print(callSite());
    else
        pos->bc.print();
}

unsigned CodeEditor::write(FunctionWriter& function, bool isDefaultArgument) {
    CodeStream cs(function, ast);
    cs.setNumLabels(labels_.size());

    // Write the promises of compiled default values of args
    for (auto arg : defaultArguments)
        if (promises[arg])
            promises[arg]->write(function, true);

    for (Cursor cur = getCursor(); !cur.atEnd(); cur.advance()) {
        BC bc = cur.bc();
        if (bc.hasPromargs()) {
            if (bc.bc == Opcode::promise_ || bc.bc == Opcode::push_code_) {
                CodeEditor* e = promises[bc.immediate.fun];
                bc.immediate.fun = e->write(function);
            } else {
                auto nargs = cur.callSite()->nargs;
                for (unsigned i = 0; i < nargs; ++i) {
                    auto arg = cur.callSite()->args()[i];
                    if (arg <= MAX_ARG_IDX) {
                        assert(arg < promises.size() && promises[arg]);
                        CodeEditor* e = promises[arg];
                        arg = e->write(function);
                    }
                    cur.callSite()->args()[i] = arg;
                }
            }
        }

        if (bc.isCallsite())
            cs.insertWithCallSite(bc.bc, cur.callSite());
        else
            cs << bc;
        if (cur.srcIdx())
            cs.addSrcIdx(cur.srcIdx());
    }

    return cs.finalize(isDefaultArgument, localsCnt);
}

Function* CodeEditor::finalize() {
    FunctionWriter fun = FunctionWriter::create();
    write(fun);
    return fun.function;
}
}

#include "interpreter/runtime.h"

C_OR_CPP void printFunctionFancy(SEXP f) { rir::CodeEditor(f).print(); }
