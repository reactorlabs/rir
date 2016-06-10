#ifndef RIR_CODE_EDITOR
#define RIR_CODE_EDITOR

#include "BC_inc.h"
#include "Code.h"

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
        BytecodeList* next = nullptr;
        BytecodeList* prev = nullptr;
        BytecodeList(BC bc) : bc(bc) {}
        BytecodeList() {}
    };

    Label nextLabel = 0;

  public:
    class Cursor {
        CodeEditor* editor;
        BytecodeList* pos;

      public:
        Cursor(CodeEditor* editor, BytecodeList* pos)
            : editor(editor), pos(pos) {}

        Label mkLabel() { return editor->nextLabel++; }

        bool atEnd() { return pos == &editor->last; }

        bool firstInstruction() { return editor->front.next == pos; }

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

        bool hasAst() { return editor->astMap.count(pos); }

        SEXP ast() { return editor->astMap.at(pos); }

        Cursor& operator<<(CodeEditor& other) {
            size_t labels = editor->nextLabel;
            size_t proms = editor->children.size();
            for (auto p : other.original->children) {
                editor->children.push_back(p);
            }

            Cursor cur = other.getCursor();
            bool first = true;
            while (!cur.atEnd()) {
                *this << *cur;
                if (first) {
                    // TODO ???
                    if (!editor->astMap.count(pos))
                        addAst(other.original->ast);
                    first = false;
                }

                BytecodeList* insert = pos->prev;

                // Fix prom offsets
                if (insert->bc.bc == BC_t::call) {
                    size_t nargs = insert->bc.immediateCallNargs();
                    fun_idx_t* args = insert->bc.immediateCallArgs();
                    for (size_t i = 0; i < nargs; ++i) {
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
            editor->astMap.insert(other.astMap.begin(), other.astMap.end());
            return *this;
        }

        void addAst(SEXP ast) {
            assert(!editor->astMap.count(pos));
            editor->astMap[pos] = ast;
        }

        void remove() {
            assert(!atEnd());
            assert(pos != &editor->front);

            BytecodeList* prev = pos->prev;
            BytecodeList* next = pos->next;

            prev->next = next;
            next->prev = prev;

            delete pos;
            pos = next;
        }
    };
    friend Cursor;

    bool empty() { return front.next == &last; }

    Cursor getCursor() { return Cursor(this, front.next); }

    CodeEditor(Code* code);

    ~CodeEditor();

    Code* toCode();

    void normalizeReturn();

    void print();

  private:
    std::unordered_map<BytecodeList*, SEXP> astMap;

    BytecodeList front;
    BytecodeList last;

    Code* original;
    std::vector<Code*> children;
};
}
}

#endif
