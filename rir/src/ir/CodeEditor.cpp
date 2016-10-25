#include "CodeEditor.h"

#include "BC.h"
#include "CodeStream.h"

#include <iomanip>
#include <iostream>

namespace rir {

CodeEditor::CodeEditor(SEXP closure) {
    ::Function * f = isValidClosureSEXP(closure);
    assert(f != nullptr);
    formals_ = FORMALS(closure);
    FunctionHandle fh(functionSEXP(f));
    CodeHandle ch = fh.entryPoint();
    ast = ch.ast();
    loadCode(fh, ch);
}

CodeEditor::CodeEditor(CodeHandle code, SEXP formals) {
    formals_ = formals;
    ast = code.ast();
    loadCode(code.function(), code);
}

void CodeEditor::loadCode(FunctionHandle function, CodeHandle code) {
    std::unordered_map<BC_t*, Label> bcLabels;

    {
        BC_t* pc = (BC_t*)code.bc();
        BC_t* end = (BC_t*)((uintptr_t)pc + code.code->codeSize);
        while (pc != end) {
            BC bc = BC::decode(pc);
            if (bc.isJmp()) {
                BC_t* target = BC::jmpTarget(pc);
                if (!bcLabels.count(target))
                    bcLabels[target] = nextLabel++;
            }
            BC::advance(&pc);
        }
    }

    labels_.resize(labels_.size() + bcLabels.size());

    {
        BytecodeList* pos = & front;

        BC_t* pc = (BC_t*)code.bc();
        BC_t* end = (BC_t*)((uintptr_t)pc + code.code->codeSize);

        while (pc != end) {
            pos->next = new BytecodeList();
            auto prev = pos;
            pos = pos->next;
            pos->prev = prev;

            if (bcLabels.count(pc)) {
                Label label = bcLabels[pc];
                pos->bc = BC::label(label);
                labels_[label] = pos;

                pos->next = new BytecodeList();
                auto prev = pos;
                pos = pos->next;
                pos->prev = prev;
            }

            pos->srcIdx = code.sourceIdx(pc);

            BC bc = BC::advance(&pc);
            if (bc.isJmp()) {
                BC_t* target = (BC_t*)((uintptr_t)pc + bc.immediate.offset);
                Label label = bcLabels[target];
                bc.immediate.offset = label;
            }

            // If this is a call, we copy the callsite information locally
            if (bc.isCallsite()) {
                auto oldCs = bc.callSite(code.code->callSites);
                auto nargs = oldCs.nargs();
                bool hasNames = oldCs.hasNames();
                unsigned needed = BC::CallSiteSize(bc.bc, nargs, hasNames);
                pos->callSite = new uint32_t[needed];
                memcpy(pos->callSite, oldCs.cs, needed * sizeof(uint32_t));
            }
            if (bc.hasPromargs()) {
                if (bc.bc == BC_t::promise_ || bc.bc == BC_t::push_code_) {
                    CodeHandle code = function.codeAtOffset(bc.immediate.fun);

                    bc.immediate.fun = code.idx();

                    CodeEditor* p = new CodeEditor(code, nullptr);

                    if (promises.size() <= code.idx())
                        promises.resize(code.idx() + 1, nullptr);

                    promises[code.idx()] = p;
                } else {
                    auto oldCs = bc.callSite(code.code->callSites);
                    auto nargs = oldCs.nargs();

                    uint32_t* cs = pos->callSite;

                    // Load all code objects of the callsite and update
                    // the indices (in the CodeEditor they are not offsets
                    // into the code object, but index into promises vector).
                    for (unsigned i = 0; i < nargs; ++i) {
                        auto arg = oldCs.arg(i);
                        if (arg <= MAX_ARG_IDX) {
                            CodeHandle code = function.codeAtOffset(arg);
                            arg = code.idx();
                            CodeEditor* p = new CodeEditor(code, nullptr);
                            if (promises.size() <= code.idx())
                                promises.resize(code.idx() + 1, nullptr);
                            promises[code.idx()] = p;
                            CallSite_args(cs)[i] = arg;
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
    for (auto p : promises) {
        if (!p)
            continue;
        delete p;
    }

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

void CodeEditor::print() {
    for (Cursor cur = getCursor(); !cur.atEnd(); cur.advance()) {
        if (cur.src()) {
            std::cout << "     # ";
            Rf_PrintValue(cur.src());
        }
        cur.print();
    }
    fun_idx_t i = 0;
    for (auto p : promises) {
        ++i;
        if (!p)
            continue;

        std::cout << "------------------------\n";
        std::cout << "@" << (void*)(long)(i-1) << "\n";
        p->print();
    }
}

void CodeEditor::Cursor::print() {
    if (pos->callSite)
        pos->bc.print(callSite());
    else
        pos->bc.print();
}

unsigned CodeEditor::write(FunctionHandle& function) {
    CodeStream cs(function, ast);
    cs.setNumLabels(nextLabel);

    for (Cursor cur = getCursor(); !cur.atEnd(); cur.advance()) {
        BC bc = cur.bc();
        if (bc.hasPromargs()) {
            if (bc.bc == BC_t::promise_ || bc.bc == BC_t::push_code_) {
                CodeEditor* e = promises[bc.immediate.fun];
                bc.immediate.fun = e->write(function);
            } else {
                auto nargs = cur.callSite().nargs();
                for (unsigned i = 0; i < nargs; ++i) {
                    auto arg = cur.callSite().arg(i);
                    if (arg <= MAX_ARG_IDX) {
                        assert(arg < promises.size() && promises[arg]);
                        CodeEditor* e = promises[arg];
                        arg = e->write(function);
                    }
                    CallSite_args(cur.callSite().cs)[i] = arg;
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

    return cs.finalize();
}

FunctionHandle CodeEditor::finalize() {
    FunctionHandle fun = FunctionHandle::create();
    write(fun);
    return fun;
}
}
