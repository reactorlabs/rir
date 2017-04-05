#ifndef RIR_CODE_EDITOR
#define RIR_CODE_EDITOR

#include "BC_inc.h"

#include "utils/FunctionHandle.h"
#include "utils/CodeHandle.h"
#include "interpreter/interp_context.h"

#include <set>
#include <unordered_map>
#include <map>
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
        explicit BytecodeList(Opcode* pos) : origin(pos) {}
        BytecodeList(BC bc, BytecodeList* prev, BytecodeList* next)
            : bc(bc), prev(prev), next(next) {}

        bool deleted = false;
        Opcode* origin = nullptr;
        BC bc;
        unsigned srcIdx = 0;
        CallSiteStruct* callSite = nullptr;
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
        friend class CodeEditor;

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

        CallSite callSite() const { return CallSite(pos->bc, pos->callSite); }

        bool hasOrigin() { return pos->origin; }

        Opcode* origin() {
            assert(pos->origin);
            return pos->origin;
        }

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

        Iterator asItr() { return Iterator(pos); }

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
            assert(!firstInstruction());
            return Cursor(*this).rwd();
        }

        BC bc() const {
            return pos->bc;
        }

        SEXP src() const { return pos->src(); }
        unsigned srcIdx() const { return pos->srcIdx; }

        CallSite callSite() { return CallSite(pos->bc, pos->callSite); }

        // TODO this breaks when inserting before the first instruction....
        Cursor& operator<<(LabelT l) { return *this << BC::label(l); }

        Cursor& operator<<(BC bc) {
            editor.changed = true;

            bool nextInPatch = inPatch;
            BytecodeList* next = pos;

            Cursor p = prev();
            bool prevInPatch = p.inPatch;
            BytecodeList* prev = p.pos;
            BytecodeList* insert = nullptr;

            if (prevInPatch && nextInPatch) {
                // Insert in the middle of a patch
                insert = new BytecodeList(bc, prev, next);
                prev->next = insert;
                next->prev = insert;
            }
            if (prevInPatch && !nextInPatch) {
                // insert at the end of a patch
                insert = new BytecodeList(bc, prev, nullptr);
                prev->next = insert;
            }
            if (!prevInPatch && nextInPatch) {
                // insert at the beginning of a patch
                auto oldPatch = prev->patch;
                insert = new BytecodeList(bc, prev, oldPatch);
                prev->patch = insert;
                oldPatch->prev = insert;
            }
            if (!prevInPatch && !nextInPatch) {
                // create a new patch
                insert = new BytecodeList(bc, prev, nullptr);
                prev->patch = insert;
            }

            if (bc.bc == Opcode::label)
                editor.labels_[insert->bc.immediate.offset] = insert;

            pos = next;
            return *this;
        }

        void insert(CodeEditor& other) {
            editor.changed = true;

            // Merge labels
            std::unordered_map<FunIdxT, FunIdxT> labelRewrite;
            for (auto l : other.labels_)
                if (l)
                    labelRewrite[l->bc.immediate.offset] = editor.mkLabel();

            // Merge promises
            size_t proms = editor.promises.size();
            std::unordered_map<FunIdxT, FunIdxT> duplicate;
            FunIdxT j = 0;
            for (auto& p : other.promises) {
                bool found = false;
                for (FunIdxT i = 0; i < editor.promises.size(); ++i) {
                    if (p == editor.promises[i]) {
                        duplicate[j] = i;
                        editor.promises.push_back(nullptr);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    // We own the promise now
                    editor.promises.push_back(p);
                    p = nullptr;
                }
                j++;
            }

            bool first = true;
            for (auto cur = other.begin(); cur != other.end(); ++cur) {

                if ((*cur).is(Opcode::label)) {
                    *this << BC::label(labelRewrite[(*cur).immediate.offset]);
                    continue;
                }

                *this << *cur;

                BytecodeList* insert = prev().pos;

                insert->srcIdx = cur.pos->srcIdx;

                if (first) {
                    if (!insert->srcIdx)
                        insert->srcIdx =
                            src_pool_add(globalContext(), other.ast);
                    first = false;
                }

                if ((*cur).isCallsite()) {
                    auto oldCs = cur.callSite();
                    unsigned needed = CallSite_sizeOf(oldCs.cs);
                    insert->callSite = (CallSiteStruct*)new char[needed];
                    memcpy(insert->callSite, oldCs.cs, needed);
                }
                // Fix prom offsets
                if (insert->bc.bc == Opcode::call_ ||
                    insert->bc.bc == Opcode::dispatch_) {
                    auto cs = insert->callSite;
                    for (unsigned i = 0; i < cs->nargs; ++i) {
                        auto idx = CallSite_args(cs)[i];
                        if (idx > MAX_ARG_IDX)
                            continue;

                        if (duplicate.count(idx))
                            idx = duplicate.at(idx);
                        else
                            idx += proms;
                        assert(editor.promises.size() >= idx &&
                               editor.promises[idx]);
                        CallSite_args(cs)[i] = idx;
                    }
                } else if (insert->bc.bc == Opcode::promise_ ||
                           insert->bc.bc == Opcode::push_code_) {
                    auto idx = insert->bc.immediate.fun;
                    if (duplicate.count(idx))
                        idx = duplicate.at(idx);
                    else
                        idx += proms;
                    assert(editor.promises.size() >= idx &&
                           editor.promises[idx]);
                    insert->bc.immediate.fun = idx;
                } else {
                    assert(!insert->bc.hasPromargs());
                }
                // Adjust jmp targets
                if (insert->bc.isJmp()) {
                    insert->bc.immediate.offset =
                        labelRewrite.at(insert->bc.immediate.offset);
                }
            }
        }

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

    std::map<SEXP, SEXP> arguments() const {
        if (formals_ == nullptr) {
            return std::map<SEXP, SEXP>();
        } else {
            std::map<SEXP, SEXP> result;
            SEXP formals = formals_;
            while (formals != R_NilValue) {
                result[TAG(formals)] = CAR(formals);
                formals = CDR(formals);
            }
            return result;
        }
    }

    void loadCode(FunctionHandle function, CodeHandle code);

    ~CodeEditor();

    unsigned write(FunctionHandle& function);

    FunctionHandle finalize();

    void print(bool verbose = true);

    bool isPure() {
        for (auto i : *this)
            if (!i.isPure())
                return false;
        return true;
    }

    void normalizeForInline() {
        LabelT endL = mkLabel();
        end().asCursor(*this) << BC::label(endL);
        for (auto i = begin(); i != end(); ++i) {
            auto bc = *i;
            if (bc.bc == Opcode::ret_) {
                CodeEditor::Cursor e = i.asCursor(*this);
                e.remove();
                if ((i + 1) != end())
                    e << BC::br(endL);
            }
        }
        commit();
    }

    CodeEditor* detachPromise(FunIdxT idx) {
        CodeEditor* e = promises[idx];
        promises[idx] = nullptr;
        return e;
    }

    /** Returns cursor that points to given label.
     */
    Iterator target(BC bc) {
        assert(bc.isJmp());
        size_t index = bc.immediate.offset;
        assert (index < labels_.size());
        return Iterator(labels_[index]);
    }

    /** Returns number of labels in the code.
     */
    size_t numLabels() const {
        return labels_.size();
    }

    LabelT mkLabel() {
        labels_.push_back(0);
        return labels_.size() - 1;
    }

    size_t numPromises() const {
        return promises.size();
    }

    CodeEditor * promise(size_t index) {
        assert(index < promises.size());
        return promises[index];
    }

    void verify() {
        std::set<int> labels;
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
            if (pos->bc.isJmp())
                target(pos->bc);
            if (pos->bc.is(Opcode::label)) {
                assert(labels.find(pos->bc.immediate.offset) == labels.end() &&
                       "Label is used multiple times");
                assert(labels_[pos->bc.immediate.offset] && "Label is unknown");
                labels.insert(pos->bc.immediate.offset);
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

        std::set<BytecodeList*> usedLabels;

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
                if (pos->bc.isJmp()) {
                    usedLabels.insert(
                        const_cast<BytecodeList*>(target(pos->bc).pos));
                }
                pos = pos->next;
            }
        }

        for (auto l = labels_.begin(); l != labels_.end(); ++l) {
            auto oldL = *l;
            if (oldL && usedLabels.find(oldL) == usedLabels.end()) {
                oldL->prev->next = oldL->next;
                oldL->next->prev = oldL->prev;
                *l = nullptr;
                delete oldL;
            }
        }

        verify();
        changed = false;
    }

    bool changed = false;
    SEXP formals_ = nullptr;
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
