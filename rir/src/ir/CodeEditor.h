#ifndef RIR_CODE_EDITOR
#define RIR_CODE_EDITOR

#include "BC_inc.h"

#include "utils/FunctionHandle.h"
#include "utils/CodeHandle.h"

#include <set>
#include <unordered_map>
#include <cassert>

#include <iostream>

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
            editor->changed = true;

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
            editor->changed = true;

            size_t labels = editor->nextLabel;
            size_t proms = editor->promises.size();

            std::unordered_map<fun_idx_t, fun_idx_t> duplicate;

            fun_idx_t j = 0;
            for (auto& p : other.promises) {
                bool found = false;
                for (fun_idx_t i = 0; i < editor->promises.size(); ++i) {
                    if (p == editor->promises[i]) {
                        duplicate[j] = i;
                        editor->promises.push_back(nullptr);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    // We own the promise now
                    editor->promises.push_back(p);
                    p = nullptr;
                }
                j++;
            }

            bool first = false;
            Cursor cur = other.getCursor();
            while (!cur.atEnd()) {
                *this << *cur;

                BytecodeList* insert = pos->prev;
                insert->src = cur.pos->src;
                if (first) {
                    if (!insert->src)
                        insert->src = cur.editor->ast;
                    first = false;
                }

                // Fix prom offsets
                if (insert->bc.isCall()) {
                    fun_idx_t* args = insert->bc.immediateCallArgs();
                    num_args_t nargs = insert->bc.immediateCallNargs();
                    for (unsigned i = 0; i < nargs; ++i) {
                        if (args[i] > MAX_ARG_IDX)
                            continue;

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

            // TODO: that stinks, I know
            delete &other;
            return *this;
        }

        void addAst(SEXP ast) {
            editor->changed = true;

            assert(!pos->src);
            pos->src = ast;
        }

        void remove() {
            editor->changed = true;

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

    Cursor getCursorAtEnd() {
        return Cursor(this, &entryPoint->last, entryPoint);
    }

    CodeEditor(FunctionHandle function);
    CodeEditor(FunctionHandle function, fun_idx_t idx);
    CodeEditor(CodeHandle code);

    void loadCode(FunctionHandle function, CodeHandle code, BCStore* store);

    ~CodeEditor();

    unsigned write(FunctionHandle& function);

    FunctionHandle finalize();

    void normalizeReturn();

    void print();

    CodeEditor* detachPromise(fun_idx_t idx) {
        CodeEditor* e = promises[idx];
        promises[idx] = nullptr;
        return e;
    }

    bool changed = false;
};
}

#endif
