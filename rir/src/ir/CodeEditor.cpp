#include "CodeEditor.h"

#include "BC.h"
#include "CodeStream.h"

#include <iomanip>
#include <iostream>

namespace rir {

CodeEditor::CodeEditor(FunctionHandle function) : ast(function.ast()) {
    entryPoint = new BCStore;
    loadCode(function, function.entryPoint(), entryPoint);
}

CodeEditor::CodeEditor(CodeHandle code) : CodeEditor(code.function(), code.idx()) { }

CodeEditor::CodeEditor(FunctionHandle function, fun_idx_t idx) {
    entryPoint = new BCStore;
    CodeHandle code = function.codeAtIdx(idx);
    ast = code.ast();
    loadCode(function, code, entryPoint);
}

void CodeEditor::loadCode(FunctionHandle function, CodeHandle code,
                          BCStore* store) {
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

    {
        BytecodeList* pos = &store->front;

        BC_t* pc = (BC_t*)code.bc();
        BC_t* end = (BC_t*)((uintptr_t)pc + code.code->codeSize);

        unsigned idx = 0;
        while (pc != end) {
            pos->next = new BytecodeList();
            auto prev = pos;
            pos = pos->next;
            pos->prev = prev;

            if (bcLabels.count(pc)) {
                Label label = bcLabels[pc];
                pos->bc = BC::label(label);

                pos->next = new BytecodeList();
                auto prev = pos;
                pos = pos->next;
                pos->prev = prev;
            }

            SEXP ast = code.source(idx++);
            pos->src = ast;

            BC bc = BC::advance(&pc);
            if (bc.isJmp()) {
                BC_t* target = (BC_t*)((uintptr_t)pc + bc.immediate.offset);
                Label label = bcLabels[target];
                bc.immediate.offset = label;
            }

            if (bc.isCall()) {
                auto argOffset = bc.immediateCallArgs();

                for (unsigned i = 0; i < bc.immediateCallNargs(); ++i) {
                    CodeHandle code = function.codeAtOffset(argOffset[i]);
                    argOffset[i] = code.idx();

                    CodeEditor* p = new CodeEditor(function, code.idx());

                    if (promises.size() <= code.idx())
                        promises.resize(code.idx() + 1, nullptr);

                    promises[code.idx()] = p;
                }
            }

            pos->bc = bc;
        }

        store->last.prev = pos;
        pos->next = &store->last;
    }
}

CodeEditor::~CodeEditor() {
    for (auto p : promises) {
        if (!p)
            continue;
        delete p;
    }

    BytecodeList* pos = entryPoint->front.next;
    while (pos != &entryPoint->last) {
        BytecodeList* old = pos;
        pos = pos->next;
        delete old;
    }

    delete entryPoint;
}

void CodeEditor::print() {
    for (Cursor cur = getCursor(); !cur.atEnd(); ++cur) {
        if (cur.hasAst()) {
            std::cout << "     # ";
            Rf_PrintValue(cur.ast());
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

void CodeEditor::Cursor::print() { pos->bc.print(); }

void CodeEditor::normalizeReturn() {
    if (getCursor().empty())
        return;

    Cursor lastI(this, entryPoint->last.prev, entryPoint);
    if ((*lastI).bc == BC_t::ret_)
        lastI.remove();

    for (Cursor pos = getCursor(); !pos.atEnd(); ++pos) {
        // TODO replace ret with jumps to the end
        assert((*pos).bc != BC_t::ret_);
    }
}

unsigned CodeEditor::write(FunctionHandle& function) {
    CodeStream cs(function, ast);
    cs.setNumLabels(nextLabel);

    for (Cursor cur = getCursor(); !cur.atEnd(); ++cur) {
        BC bc = *cur;
        if (bc.isCall()) {
            auto arg = bc.immediateCallArgs();
            auto nargs = bc.immediateCallNargs();
            for (unsigned i = 0; i < nargs; ++i) {
                CodeEditor* e = promises[arg[i]];
                arg[i] = e->write(function);
            }
        }
        cs << *cur;
        if (cur.hasAst())
            cs.addAst(cur.ast());
    }

    return cs.finalize();
}

FunctionHandle CodeEditor::finalize() {
    FunctionHandle fun = FunctionHandle::create();
    write(fun);
    return fun;
}
}
