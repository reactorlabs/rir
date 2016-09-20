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

    BytecodeList front;
    BytecodeList last;

    std::vector<CodeEditor*> promises;

    SEXP ast;

    std::vector<BytecodeList*> labels_;

    /** Closure that stores given code.

      nullptr if the code corresponds to a promise.
     */
    SEXP closure_ = nullptr;

  public:

    class Cursor {

        CodeEditor* editor;
        BytecodeList* pos;
        std::vector<BytecodeList*> deleted;

      public:
        unsigned long hash() const {
            return std::hash<unsigned long>()((unsigned long)editor) ^
                   std::hash<unsigned long>()((unsigned long)pos);
        }

        ~Cursor() {
            for (auto d : deleted)
                delete d;
        }

        Cursor():
            editor(nullptr),
            pos(nullptr) {
        }

        CodeEditor & editorX() const {
            return *editor;
        }

        Cursor(CodeEditor* editor, BytecodeList* pos)
            : editor(editor), pos(pos) {}

        Cursor(Cursor const & from):
            editor(from.editor),
            pos(from.pos) {
        }

        bool operator == (Cursor const & other) const {
            return editor == other.editor and pos == other.pos;
        }

        bool operator != (Cursor const & other) const {
            return  pos != other.pos or editor != other.editor;
        }

        Label mkLabel() { return editor->nextLabel++; }

        bool atEnd() { return *this == editor->getCursorAtEnd(); }

        bool firstInstruction() { return *this == editor->getCursor(); }

        void operator++() {
            assert(!atEnd());
            pos = pos->next;
        }

        Cursor & advance() {
            assert(!atEnd());
            pos = pos->next;
            return *this;
        }

        Cursor next() {
            assert(!atEnd());
            return Cursor(*this).advance();
        }

        void operator--() {
            assert(!firstInstruction());
            pos = pos->prev;
        }

        /** Deprecated */
        BC operator*() { return pos->bc; }

        BC peek(int i = 1) {
            BytecodeList* p = pos;
            while (i-- > 0)
                p = p->next;
            return p->bc;
        }

        /** Getter for the BC object the cursor points to.

          (this is because the iterator interface is deprecated)
         */
        BC bc() const {
            return pos->bc;
        }

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
                if (insert->bc.bc == BC_t::call_ ||
                    insert->bc.bc == BC_t::dispatch_) {
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
                } else if (insert->bc.bc == BC_t::promise_ ||
                           insert->bc.bc == BC_t::push_code_) {
                    if (duplicate.count(insert->bc.immediate.fun))
                        insert->bc.immediate.fun =
                            duplicate.at(insert->bc.immediate.fun);
                    else
                        insert->bc.immediate.fun += proms;
                } else {
                    assert(!insert->bc.hasPromargs());
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

        void erase() {
            editor->changed = true;

            assert(!atEnd());
            assert(pos != & editor->front);

            BytecodeList* prev = pos->prev;
            BytecodeList* next = pos->next;

            prev->next = next;
            next->prev = prev;

            pos->bc.bc = BC_t::invalid_;
            deleted.push_back(pos);
        }

        bool empty() { return editor->front.next == & editor->last; }

        void print();
    };
    friend class Cursor;

    Cursor getCursor() {
        return Cursor(this, front.next);
    }

    Cursor getCursorAtEnd() {
        return Cursor(this, & last);
    }

    CodeEditor(SEXP closure);

    // TODO this should be private
    CodeEditor(CodeHandle code);


    std::vector<SEXP> arguments() const {
        if (closure_ == nullptr) {
            return std::vector<SEXP>();
        } else {
            std::vector<SEXP> result;
            SEXP formals = FORMALS(closure_);
            while (formals != R_NilValue) {
                result.push_back(TAG(formals));
                formals = CDR(formals);
            }
            return result;
        }
    }

    void loadCode(FunctionHandle function, CodeHandle code);

    ~CodeEditor();

    unsigned write(FunctionHandle& function);

    FunctionHandle finalize();

    void print();

    CodeEditor* detachPromise(fun_idx_t idx) {
        CodeEditor* e = promises[idx];
        promises[idx] = nullptr;
        return e;
    }

    /** Returns cursor that points to given label.
     */
    Cursor label(size_t index) {
        assert (index < labels_.size());
        return Cursor(this, labels_[index]);
    }

    /** Returns number of labels in the code.
     */
    size_t numLabels() const {
        return labels_.size();
    }

    size_t numPromises() const {
        return promises.size();
    }

    CodeEditor & promise(size_t index) {
        assert(index < promises.size());
        return * promises[index];
    }

    bool changed = false;


};
}

namespace std {

template <>
struct hash<rir::CodeEditor::Cursor> {
    size_t operator()(rir::CodeEditor::Cursor const& x) const noexcept {
        return x.hash();
    }
};
}

#endif
