#ifndef RIR_CODE_EDITOR
#define RIR_CODE_EDITOR

#include "BC_inc.h"

#include "utils/FunctionHandle.h"
#include "utils/CodeHandle.h"
#include "interpreter/interp_context.h"

#include <set>
#include <unordered_map>
#include <cassert>
#include <stack>

#include <iostream>

namespace rir {

class CodeEditor {
  private:
    /*
     * The BytecodeList is the central datastructure of the code editor. Its a
     * doubly linked list of BC objects. Additionally every instruction can
     * have a patch-branch and be marked as deleted. In pictures:
     *
     *   BC -> BC -> BC -> BC(deleted) -> BC -> BC
     *          \                          \
     *            > BC -> BC                 > BC
     *
     * The code Editor has:
     *  1) an Iterator which will always see the *UNMODIFIED* bytecode stream
     *  2) a Cursor which sees the modified bytecode stream and allows its
     *     clients to do modifications.
     *
     * The internals of the BytecodeList are supposed to be completely
     * insulated from CodeEditor users. In particular the Cursor creates the
     * illusion of operating on regular stream.
     *
     * To make the changes available to further analysis, you have to commit()
     * them.
     */

#pragma pack(push)
#pragma pack(1)
    struct BytecodeList {
        BytecodeList() {}
        BytecodeList(BC bc, BytecodeList* prev, BytecodeList* next)
            : bc(bc), prev(prev), next(next) {}

        BC bc;
        bool deleted = false;
        unsigned srcIdx = 0;
        uint32_t* callSite = nullptr;
        BytecodeList* prev = nullptr;
        BytecodeList* next = nullptr;
        BytecodeList* patch = nullptr;

        SEXP src() const {
            return srcIdx != 0 ? src_pool_at(globalContext(), srcIdx) : 0;
        }

        ~BytecodeList() {
            if (callSite)
                delete[] callSite;
        }
    };
#pragma pack(pop)

    Label nextLabel = 0;

    BytecodeList front;
    BytecodeList last;

    std::vector<CodeEditor*> promises;

    SEXP ast;

    std::vector<BytecodeList*> labels_;

  public:
    class Cursor;

    // This is a simple iterator class. It is oblivious to any uncommited
    // changes made to the instruction stream.
    class Iterator {
      protected:
        const BytecodeList* pos;

      public:
        Iterator() : pos(nullptr) {}
        Iterator(const BytecodeList* pos) : pos(pos) {}
        Iterator(const Iterator& other) : pos(other.pos) {}
        Iterator(int fake) : pos(reinterpret_cast<const BytecodeList*>(fake)) {}

        BC operator*() const { return pos->bc; }

        void operator++() { pos = pos->next; }
        void operator--() { pos = pos->prev; }

        Iterator operator+(int num) {
            if (num < 0)
                return *this - (-num);

            Iterator n = *this;
            for (int i = 0; i < num; ++i)
                ++n;
            return n;
        }

        Iterator operator-(int num) {
            if (num < 0)
                return *this + (-num);

            Iterator n = *this;
            for (int i = 0; i < num; ++i)
                --n;
            return n;
        }

        bool operator==(const Iterator& other) const {
            return pos == other.pos;
        }

        bool operator!=(const Iterator& other) const {
            return pos != other.pos;
        }

        unsigned long hash() const {
            return std::hash<unsigned long>()((unsigned long)pos);
        }

        SEXP src() const { return pos->src(); }

        bool deleted() const { return pos->deleted; }

        Cursor asCursor(CodeEditor& editor);
    };

    // The cursor can insert changes into the codestream
    class Cursor {
        CodeEditor& editor;
        BytecodeList* pos;
        bool inPatch = false;

      public:
        Cursor(CodeEditor& editor, BytecodeList* pos)
            : editor(editor), pos(pos) {}

        Cursor(Cursor const & from):
            editor(from.editor),
            pos(from.pos),
            inPatch(from.inPatch) {
        }

        bool operator == (Cursor const & other) const {
            return &editor == &other.editor and pos == other.pos;
        }

        bool operator != (Cursor const & other) const {
            return pos != other.pos or &editor != &other.editor;
        }

        bool atEnd() const { return pos == &editor.last; }
        bool firstInstruction() const { return pos == editor.front.next; }

        Cursor& advance() {
            assert(!atEnd());
            do {
                if (pos->patch) {
                    pos = pos->patch;
                    inPatch = true;
                } else {
                    if (pos->next) {
                        pos = pos->next;
                    } else {
                        // At the end of the patch we rewind and go to the next
                        // instruction
                        while (!pos->patch)
                            pos = pos->prev;
                        pos = pos->next;
                        inPatch = false;
                    }
                }
            } while (pos->deleted && !atEnd());
            return *this;
        }

        Cursor next() const {
            assert(!atEnd());
            return Cursor(*this).advance();
        }

        Cursor& rwd() {
            do {
                assert(!firstInstruction());
                auto oldPos = pos;
                pos = pos->prev;
                if (pos->patch) {
                    // Now we need to figure out if we came backwards from the
                    // patch, or from the other instruction stream
                    if (oldPos == pos->patch) {
                        inPatch = false;
                    } else {
                        pos = pos->patch;
                        while (pos->next)
                            pos = pos->next;
                        inPatch = true;
                    }
                }
            } while (pos->deleted && !firstInstruction());
            return *this;
        }

        Cursor prev() const {
            assert(!atEnd());
            return Cursor(*this).rwd();
        }

        BC bc() const {
            return pos->bc;
        }

        SEXP src() const { return pos->src(); }
        unsigned srcIdx() const { return pos->srcIdx; }

        uint32_t* callSite() { return pos->callSite; }

        // TODO this breaks when inserting before the first instruction....
        Cursor& operator<<(BC bc) {
            editor.changed = true;

            bool nextInPatch = inPatch;
            BytecodeList* next = pos;

            Cursor p = prev();
            bool prevInPatch = p.inPatch;
            BytecodeList* prev = p.pos;

            if (prevInPatch && nextInPatch) {
                // Insert in the middle of a patch
                auto insert = new BytecodeList(bc, prev, next);
                prev->next = insert;
                next->prev = insert;
            }
            if (prevInPatch && !nextInPatch) {
                // insert at the end of a patch
                auto insert = new BytecodeList(bc, prev, nullptr);
                prev->next = insert;
            }
            if (!prevInPatch && nextInPatch) {
                // insert at the beginning of a patch
                auto oldPatch = prev->patch;
                auto insert = new BytecodeList(bc, prev, oldPatch);
                prev->patch = insert;
                oldPatch->prev = insert;
            }
            if (!prevInPatch && !nextInPatch) {
                // create a new patch
                auto insert = new BytecodeList(bc, prev, nullptr);
                prev->patch = insert;
            }

            pos = next;
            return *this;
        }

        // void insert(CodeEditor& other) {
        //    editor.changed = true;

        //    size_t labels = editor.nextLabel;
        //    size_t proms = editor.promises.size();

        //    std::unordered_map<fun_idx_t, fun_idx_t> duplicate;

        //    fun_idx_t j = 0;
        //    for (auto& p : other.promises) {
        //        bool found = false;
        //        for (fun_idx_t i = 0; i < editor.promises.size(); ++i) {
        //            if (p == editor.promises[i]) {
        //                duplicate[j] = i;
        //                editor.promises.push_back(nullptr);
        //                found = true;
        //                break;
        //            }
        //        }
        //        if (!found) {
        //            // We own the promise now
        //            editor.promises.push_back(p);
        //            p = nullptr;
        //        }
        //        j++;
        //    }

        //    bool first = true;
        //    Cursor cur = other.getCursor();
        //    while (!cur.atEnd()) {
        //        *this << cur.bc();

        //        BytecodeList* insert = prev().pos;

        //        insert->srcIdx = cur.pos->srcIdx;

        //        if (first) {
        //            if (!insert->srcIdx)
        //                insert->srcIdx =
        //                    src_pool_add(globalContext(), other.ast);
        //            first = false;
        //        }

        //        // Fix prom offsets
        //        if (insert->bc.bc == BC_t::call_ ||
        //            insert->bc.bc == BC_t::dispatch_) {
        //            fun_idx_t* args = insert->bc.immediateCallArgs();
        //            num_args_t nargs = insert->bc.immediateCallNargs();
        //            for (unsigned i = 0; i < nargs; ++i) {
        //                if (args[i] > MAX_ARG_IDX)
        //                    continue;

        //                if (duplicate.count(args[i]))
        //                    args[i] = duplicate.at(args[i]);
        //                else
        //                    args[i] += proms;
        //            }
        //        } else if (insert->bc.bc == BC_t::promise_ ||
        //                   insert->bc.bc == BC_t::push_code_) {
        //            if (duplicate.count(insert->bc.immediate.fun))
        //                insert->bc.immediate.fun =
        //                    duplicate.at(insert->bc.immediate.fun);
        //            else
        //                insert->bc.immediate.fun += proms;
        //        } else {
        //            assert(!insert->bc.hasPromargs());
        //        }
        //        // Fix labels
        //        if (insert->bc.bc == BC_t::label)
        //            insert->bc.immediate.offset += labels;
        //        // Adjust jmp targets
        //        if (insert->bc.isJmp()) {
        //            insert->bc.immediate.offset += labels;
        //        }
        //    }
        //    editor.nextLabel += other.nextLabel;

        //    // TODO: that stinks, I know
        //    delete &other;
        //}

        void remove() {
            editor.changed = true;

            assert(!atEnd());
            assert(pos != &editor.front);

            pos->deleted = true;

            advance();
        }

        bool empty() { return editor.front.next == &editor.last; }

        void print();

        unsigned long hash() const {
            return std::hash<unsigned long>()((unsigned long)&editor) ^
                   std::hash<unsigned long>()((unsigned long)pos);
        }
    };
    friend class Cursor;

    Cursor getCursor() { return Cursor(*this, front.next); }

    Iterator begin() const { return Iterator(front.next); }

    Iterator end() const { return Iterator(&last); }

    CodeEditor(SEXP closure);

    CodeEditor(CodeHandle code, SEXP formals);

    std::vector<SEXP> arguments() const {
        if (formals_ == nullptr) {
            return std::vector<SEXP>();
        } else {
            std::vector<SEXP> result;
            SEXP formals = formals_;
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
    Iterator label(size_t index) {
        assert (index < labels_.size());
        return Iterator(labels_[index]);
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

    void verify() {
        BytecodeList* pos = front.next;
        while (pos != &last) {
            if (pos->patch) {
                BytecodeList* patch = pos->patch;
                while (patch) {
                    assert(patch != &last);
                    assert(patch->patch == nullptr);
                    patch = patch->next;
                }
            }
            pos = pos->next;
        }
    }

    void commit() {
        verify();

        // Step 1: splice patches
        BytecodeList* pos = front.next;
        while (pos != &last) {
            if (pos->patch) {
                BytecodeList* next = pos->next;
                BytecodeList* patchEnd = pos->patch;
                while (patchEnd->next)
                    patchEnd = patchEnd->next;
                pos->next = pos->patch;
                pos->patch = nullptr;
                patchEnd->next = next;
                next->prev = patchEnd;
                pos = patchEnd;
            } else {
                pos = pos->next;
            }
        }

        // Step 2: remove deleted
        pos = front.next;
        while (pos != &last) {
            if (pos->deleted) {
                pos->prev->next = pos->next;
                pos->next->prev = pos->prev;
                BytecodeList* old = pos;
                pos = pos->next;
                delete old;
            } else {
                pos = pos->next;
            }
        }
    }

    bool changed = false;
    SEXP formals_;
};

inline CodeEditor::Cursor CodeEditor::Iterator::asCursor(CodeEditor& editor) {
    return CodeEditor::Cursor(editor, const_cast<BytecodeList*>(pos));
}
}

namespace std {

template <>
struct hash<rir::CodeEditor::Cursor> {
    size_t operator()(rir::CodeEditor::Cursor const& x) const noexcept {
        return x.hash();
    }
};
template <>
struct hash<rir::CodeEditor::Iterator> {
    size_t operator()(rir::CodeEditor::Iterator const& x) const noexcept {
        return x.hash();
    }
};
}

#endif
