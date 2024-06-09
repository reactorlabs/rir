#ifndef RIR_DISPATCH_TABLE_H
#define RIR_DISPATCH_TABLE_H

#include "GenericDispatchTable.h"

#include "Function.h"
#include "R/Serialize.h"
#include "RirRuntimeObject.h"
#include "TypeFeedback.h"
#include "utils/random.h"
#include <ostream>

namespace rir {

#define DISPATCH_TABLE_MAGIC (unsigned)0xd7ab1e00

typedef SEXP DispatchTableEntry;

/*
 * A dispatch table (vtable) for functions.
 *
 */
#pragma pack(push)
#pragma pack(1)
struct DispatchTable
    : public RirRuntimeObject<DispatchTable, DISPATCH_TABLE_MAGIC> {

    static constexpr unsigned MaxFeedbacks = 64;
    typedef GenericDispatchTable<Context, TypeFeedback, MaxFeedbacks>
        TypeFeedbackDispatchTable;

    size_t size() const { return size_; }

    Function* get(size_t i) const {
        assert(i < size());
        auto f = Function::unpack(getEntry(i));
        assert(f->dispatchTable() == this);
        return f;
    }

    Context bestContext() const { return typeFeedbacks()->best().first; }

    Function* best() const {
        if (size() > 1)
            return get(1);
        return get(0);
    }

    Function* baseline() const {
        auto f = Function::unpack(getEntry(0));
        assert(f->signature().envCreation ==
               FunctionSignature::Environment::CallerProvided);
        assert(f->dispatchTable() == this);
        return f;
    }

    inline Function* dispatch(Context a, bool ignorePending = true) const {
        return dispatchConsideringDisabled(a, nullptr, ignorePending);
    }

    Function* dispatchConsideringDisabled(Context a, Function** disabledFunc,
                                          bool ignorePending = true) const {
        compareWithDefinedContext(a);

        Function* r2 = nullptr;
        auto outputDisabledFunc = (disabledFunc != nullptr);

        for (size_t i = 1; i < size(); ++i) {
#ifdef DEBUG_DISPATCH
            std::cout << "DISPATCH trying: " << a << " vs " << get(i)->context()
                      << "\n";
#endif
            auto e = get(i);
            if (a.smaller(e->context()) &&
                (ignorePending || !e->pendingCompilation())) {

                r2 = e;
                if (!e->disabled()) {
                    if (outputDisabledFunc)
                        *disabledFunc = r2;
                    return e;
                }
            }
        }

        auto b = baseline();

        if (outputDisabledFunc)
            *disabledFunc = (!r2 ? b : r2);

        return b;
    }

    std::pair<Context, TypeFeedback*>
    dispatchTypeFeedback(const Context& ctx) const {
        compareWithDefinedContext(ctx);
        return typeFeedbacks()->dispatch(ctx);
    }

    TypeFeedback* getTypeFeedback(const Context& ctx) const {
        compareWithDefinedContext(ctx);
        auto feedbacks = typeFeedbacks();
        auto entry = feedbacks->dispatch(ctx, [](const TypeFeedback* tf) {
            return tf && tf->recordingCount() > 0;
        });
        TypeFeedback* tf = entry.second;
        if (!tf)
            return baselineFeedback();
        return tf;
    }

    TypeFeedback* getOrCreateTypeFeedback(const Context& ctx) {
        compareWithDefinedContext(ctx);
        auto feedbacks = typeFeedbacks();
        auto entry = feedbacks->dispatch(ctx);
        TypeFeedback* tf = entry.second;
        if (entry.first != ctx || !tf) {
            assert(baselineFeedback());
            // Use closest possible feedback when type feedback table is full
            // TODO: try different approaches
            if (feedbacks->full())
                return tf ? tf : baselineFeedback();
            tf = baselineFeedback()->emptyCopy();
            PROTECT(tf->container());
            feedbacks->insert(ctx, tf);
            UNPROTECT(1);
        }
        return tf;
    }

    void insertTypeFeedback(const Context& ctx, TypeFeedback* tf) {
        auto feedbacks = typeFeedbacks();
        assert(!feedbacks->full());
        feedbacks->insert(ctx, tf);
    }

    void baseline(Function* f) {
        assert(f->signature().optimization ==
               FunctionSignature::OptimizationLevel::Baseline);
        if (size() == 0)
            size_++;
        else
            assert(baseline()->signature().optimization ==
                   FunctionSignature::OptimizationLevel::Baseline);
        setEntry(0, f->container());
        insertTypeFeedback(f->context(), f->typeFeedback());
        f->dispatchTable(this);
    }

    bool contains(const Context& assumptions) const {
        for (size_t i = 0; i < size(); ++i)
            if (get(i)->context() == assumptions)
                return !get(i)->disabled();
        return false;
    }

    void remove(Code* funCode) {
        size_t i = 1;
        for (; i < size(); ++i) {
            auto fun = get(i);
            if (fun->body() == funCode) {
                fun->dispatchTable(nullptr);
                break;
            }
        }
        if (i == size())
            return;
        for (; i < size() - 1; ++i) {
            setEntry(i, getEntry(i + 1));
        }
        get(i)->dispatchTable(nullptr);
        setEntry(i, nullptr);
        size_--;
    }

    TypeFeedback* baselineFeedback() const {
        return baseline()->typeFeedback();
    }

    // insert function ordered by increasing number of assumptions
    void insert(Function* fun) {
        // TODO: we might need to grow the DT here!
        assert(size() > 0);
        assert(fun->signature().optimization !=
               FunctionSignature::OptimizationLevel::Baseline);
        fun->dispatchTable(this);
        auto assumptions = fun->context();
        size_t i;
        for (i = size() - 1; i > 0; --i) {
            auto old = get(i);
            if (old->context() == assumptions) {
                // Remember deopt counts across recompilation to avoid
                // deopt loops
                fun->addDeoptCount(old->deoptCount());
                setEntry(i, fun->container());
                assert(get(i) == fun);
                // old->dispatchTable(nullptr);
                return;
            }
            if (!(assumptions < get(i)->context())) {
                break;
            }
        }
        i++;
        assert(!contains(fun->context()));
        if (size() == capacity()) {
#ifdef DEBUG_DISPATCH
            std::cout << "Tried to insert into a full Dispatch table. Have: \n";
            for (size_t i = 0; i < size(); ++i) {
                auto e = getEntry(i);
                std::cout << "* " << Function::unpack(e)->context() << "\n";
            }
            std::cout << "\n";
            std::cout << "Tried to insert: " << assumptions << "\n";
            Rf_error("dispatch table overflow");
#endif
            // Evict one element and retry
            auto pos = 1 + (Random::singleton()() % (size() - 1));
            get(pos)->dispatchTable(nullptr);
            size_--;
            while (pos < size()) {
                setEntry(pos, getEntry(pos + 1));
                pos++;
            }
            return insert(fun);
        }

        for (size_t j = size(); j > i; --j)
            setEntry(j, getEntry(j - 1));
        size_++;
        setEntry(i, fun->container());
        fun->dispatchTable(this);

#ifdef DEBUG_DISPATCH
        std::cout << "Added version to DT, new order is: \n";
        for (size_t i = 0; i < size(); ++i) {
            auto e = getEntry(i);
            std::cout << "* " << Function::unpack(e)->context() << "\n";
        }
        std::cout << "\n";
        for (size_t i = 1; i < size() - 1; ++i) {
            assert(get(i)->context() < get(i + 1)->context());
            assert(get(i)->context() != get(i + 1)->context());
            assert(!(get(i + 1)->context() < get(i)->context()));
        }
        assert(contains(fun->context()));
#endif
    }

    static DispatchTable* create(size_t capacity = 20) {
        size_t sz = sizeof(DispatchTable) +
                    (capacity * sizeof(DispatchTableEntry) +
                     sizeof(DispatchTableEntry)); // last DispatchTableEntry is
                                                  // used for typeFeedback table
        SEXP s = Rf_allocVector(EXTERNALSXP, sz);
        auto dp = new (INTEGER(s)) DispatchTable(capacity);
        PROTECT(dp->container());
        // create type feedback dispatch table
        // TODO: Different feedback table sizes
        auto typeFeedbackTable = TypeFeedbackDispatchTable::create();
        PROTECT(typeFeedbackTable->container());
        dp->typeFeedbacks(typeFeedbackTable);
        UNPROTECT(2);
        return dp;
    }

    size_t capacity() const { return info.gc_area_length - 1; }

    static DispatchTable* deserialize(SEXP refTable, R_inpstream_t inp) {
        DispatchTable* table = create();
        PROTECT(table->container());
        AddReadRef(refTable, table->container());
        table->size_ = InInteger(inp);
        for (size_t i = 0; i < table->size(); i++) {
            auto fun = Function::deserialize(refTable, inp);
            table->setEntry(i, fun->container());
            fun->dispatchTable(table);
            table->insertTypeFeedback(fun->context(), fun->typeFeedback());
        }
        UNPROTECT(1);
        return table;
    }

    void serialize(SEXP refTable, R_outpstream_t out) const {
        HashAdd(container(), refTable);
        OutInteger(out, 1);
        baseline()->serialize(refTable, out);
    }

    Context userDefinedContext() const { return userDefinedContext_; }
    DispatchTable* newWithUserContext(Context udc) {

        auto clone = create(this->capacity());
        PROTECT(clone->container());
        SEXP baseline = this->getEntry(0);
        clone->setEntry(0, baseline);

        auto typeFeedbacks = this->typeFeedbacks();
        auto cloneFeedbacks = clone->typeFeedbacks();
        size_t j = 0;
        for (size_t i = 0; i < typeFeedbacks->size(); ++i) {
            auto tf = TypeFeedback::unpack(typeFeedbacks->getEntry(i));
            Context ctx = typeFeedbacks->key(i);
            if (ctx.smaller(udc))
                cloneFeedbacks->insert(typeFeedbacks->key(i), tf);
        }
        j = 1;
        for (size_t i = 1; i < size(); i++) {
            if (get(i)->context().smaller(udc)) {
                auto v = get(i);
                clone->setEntry(j, getEntry(i));
                v->dispatchTable(clone);
                j++;
            }
        }

        clone->size_ = j;
        clone->userDefinedContext_ = udc;
        Function::unpack(baseline)->dispatchTable(clone);
        UNPROTECT(1);
        return clone;
    }

    Context combineContextWith(Context anotherContext) {
        return userDefinedContext_ | anotherContext;
    }

    void print(std::ostream& out, bool verbose) const {
        std::cout << "== dispatch table " << this << " ==\n";

        for (size_t entry = 0; entry < size(); ++entry) {
            Function* f = get(entry);
            std::cout << "= version " << entry << " (" << f << ") =\n";
            f->disassemble(std::cout);
        }

        if (verbose) {
            auto code = baseline()->body();
            auto pc = code->code();
            auto print_header = true;

            Opcode* prev = NULL;
            Opcode* pprev = NULL;

            while (pc < code->endCode()) {
                auto bc = BC::decode(pc, code);
                if (bc.bc == Opcode::close_) {
                    if (print_header) {
                        out << "== nested closures ==\n";
                        print_header = false;
                    }

                    // prev is the push_ of srcref
                    // pprev is the push_ of body
                    auto body = BC::decodeShallow(pprev).immediateConst();
                    auto dt = DispatchTable::unpack(body);
                    dt->print(std::cout, verbose);
                }
                pprev = prev;
                prev = pc;
                pc = bc.next(pc);
            }
        }
    }

  private:
    DispatchTable() = delete;
    explicit DispatchTable(size_t capacity)
        : RirRuntimeObject(
              // GC area starts at the end of the DispatchTable
              // GC area is the pointers in the entry array
              // and pointer to TypeFeedback dispatch table
              sizeof(DispatchTable), capacity + 1),
          typeFeedbackPos_(info.gc_area_length - 1) {}

    TypeFeedbackDispatchTable* typeFeedbacks() {
        return TypeFeedbackDispatchTable::unpack(getEntry(typeFeedbackPos_));
    }

    const TypeFeedbackDispatchTable* typeFeedbacks() const {
        return TypeFeedbackDispatchTable::unpack(getEntry(typeFeedbackPos_));
    }

    void typeFeedbacks(TypeFeedbackDispatchTable* tfdp) {
        setEntry(typeFeedbackPos_, tfdp->container());
    }

    void compareWithDefinedContext(const Context& ctx) const {
        if (!ctx.smaller(userDefinedContext_)) {
#ifdef DEBUG_DISPATCH
            std::cout << "DISPATCH trying: " << a
                      << " vs annotation: " << userDefinedContext_ << "\n";
#endif
            Rf_error("Provided context does not satisfy user defined context");
        }
    }

    size_t size_ = 0;
    Context userDefinedContext_;
    size_t typeFeedbackPos_;
};

#pragma pack(pop)
} // namespace rir

#endif
