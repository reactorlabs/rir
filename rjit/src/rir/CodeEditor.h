#ifndef RIR_CODE_EDITOR
#define RIR_CODE_EDITOR

#include "BC_inc.h"

#include "FunctionHandle.h"

#include <set>
#include <unordered_map>
#include <cassert>

#include <iostream>

namespace rjit {
namespace rir {

class CodeEditor {
  private:
    struct BytecodeList {
        BC bc;
        SEXP src = nullptr;
        BytecodeList* next = nullptr;
        BytecodeList* prev = nullptr;
        BytecodeList(BC bc) : bc(bc) {}
        BytecodeList() {}
    };

    Label nextLabel = 0;

    struct BCStore {
        BytecodeList front;
        BytecodeList last;
    };
    BCStore* entryPoint;
    std::vector<CodeEditor*> promises;

    SEXP ast;

  public:
    class Cursor {
        CodeEditor* editor;
        BytecodeList* pos;
        BytecodeList* begin;
        BytecodeList* end;

      public:
        Cursor(CodeEditor* editor, BytecodeList* pos, BCStore* store)
            : editor(editor), pos(pos), begin(&store->front),
              end(&store->last) {}

        Label mkLabel() { return editor->nextLabel++; }

        bool atEnd() { return pos == end; }

        bool firstInstruction() { return begin->next == pos; }

        void operator++() {
            assert(!atEnd());
            pos = pos->next;
        }

        void operator--() {
            assert(!firstInstruction());
            pos = pos->prev;
        }

        BC operator*() { return pos->bc; }

        Cursor& operator<<(BC bc) {
            auto insert = new BytecodeList(bc);

            BytecodeList* prev = pos->prev;
            BytecodeList* next = pos;

            prev->next = insert;
            next->prev = insert;

            insert->prev = prev;
            insert->next = next;

            pos = next;
            return *this;
        }

        bool hasAst() { return pos->src; }

        SEXP ast() { return pos->src; }

        Cursor& operator<<(CodeEditor& other) {
            size_t labels = editor->nextLabel;
            size_t proms = editor->promises.size();

            std::unordered_map<fun_idx_t, fun_idx_t> duplicate;

            fun_idx_t j = 0;
            for (auto p : other.promises) {
                bool found = false;
                for (fun_idx_t i = 0; i < editor->promises.size(); ++i) {
                    if (p == editor->promises[i]) {
                        duplicate[j] = i;
                        editor->promises.push_back(nullptr);
                        found = true;
                        break;
                    }
                }
                if (!found)
                    editor->promises.push_back(p);
                j++;
            }

            Cursor cur = other.getCursor();
            while (!cur.atEnd()) {
                *this << *cur;

                BytecodeList* insert = pos->prev;
                pos->src = cur.pos->src;

                // Fix prom offsets
                if (insert->bc.bc == BC_t::call_) {
                    fun_idx_t* args = insert->bc.immediateCallArgs();
                    num_args_t nargs = insert->bc.immediateCallNargs();
                    for (unsigned i = 0; i < nargs; ++i) {
                        if (duplicate.count(args[i]))
                            args[i] = duplicate.at(args[i]);
                        else
                            args[i] += proms;
                    }
                }
                // Fix labels
                if (insert->bc.bc == BC_t::label)
                    insert->bc.immediate.offset += labels;
                // Adjust jmp targets
                if (insert->bc.isJmp()) {
                    insert->bc.immediate.offset += labels;
                }

                ++cur;
            }
            editor->nextLabel += other.nextLabel;
            return *this;
        }

        void addAst(SEXP ast) {
            assert(!pos->src);
            pos->src = ast;
        }

        void remove() {
            assert(!atEnd());
            assert(pos != begin);

            BytecodeList* prev = pos->prev;
            BytecodeList* next = pos->next;

            prev->next = next;
            next->prev = prev;

            delete pos;
            pos = next;
        }

        bool empty() { return begin->next == end; }

        void print();
    };
    friend Cursor;

    Cursor getCursor() {
        return Cursor(this, entryPoint->front.next, entryPoint);
    }

    CodeEditor(FunctionHandle function);
    CodeEditor(FunctionHandle function, fun_idx_t idx);

    void loadCode(FunctionHandle function, CodeHandle code, BCStore* store);

    ~CodeEditor();

    unsigned write(FunctionHandle& function);

    FunctionHandle finalize();

    void normalizeReturn();

    void print(int offset = 0);

    CodeEditor* detachPromise(fun_idx_t idx) {
        CodeEditor* e = promises[idx];
        promises[idx] = nullptr;
        return e;
    }
};
}
}

#endif
