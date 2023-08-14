#ifndef RIR_DISPATCH_TABLE_H
#define RIR_DISPATCH_TABLE_H

#include "Function.h"
#include "R/Serialize.h"
#include "RirRuntimeObject.h"
#include "runtime/log/RirObjectPrintStyle.h"
#include "serializeHash/hash/getConnected.h"
#include "serializeHash/hash/hashRoot.h"
#include "TypeFeedback.h"
#include "utils/ByteBuffer.h"
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

    size_t size() const { return size_; }

    Function* get(size_t i) const {
        assert(i < capacity());
        return Function::unpack(getEntry(i));
    }

    Function* best() const {
        if (size() > 1)
            return get(1);
        return get(0);
    }
    Function* baseline() const {
        auto f = Function::unpack(getEntry(0));
        assert(f->signature().envCreation ==
               FunctionSignature::Environment::CallerProvided);
        return f;
    }

    inline Function* dispatch(Context a, bool ignorePending = true) const {
        return dispatchConsideringDisabled(a, nullptr, ignorePending);
    }

    Function* dispatchConsideringDisabled(Context a, Function** disabledFunc,
                                          bool ignorePending = true) const {
        if (!a.smaller(userDefinedContext_)) {
#ifdef DEBUG_DISPATCH
            std::cout << "DISPATCH trying: " << a
                      << " vs annotation: " << userDefinedContext_ << "\n";
#endif
            Rf_error("Provided context does not satisfy user defined context");
        }

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

    void baseline(Function* f) {
        assert(f->signature().optimization ==
               FunctionSignature::OptimizationLevel::Baseline);
        if (size() == 0)
            size_++;
        else
            assert(baseline()->signature().optimization ==
                   FunctionSignature::OptimizationLevel::Baseline);
        setEntry(0, f->container());
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
        setEntry(i, nullptr);
        size_--;
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
                if (i != 0) {
                    // Remember deopt counts across recompilation to avoid
                    // deopt loops
                    fun->addDeoptCount(old->deoptCount());
                    setEntry(i, fun->container());
                    assert(get(i) == fun);
                }
                old->dispatchTable(nullptr);
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
        size_t sz =
            sizeof(DispatchTable) + (capacity * sizeof(DispatchTableEntry));
        SEXP s = Rf_allocVector(EXTERNALSXP, sz);
        return new (INTEGER(s)) DispatchTable(capacity);
    }

  private:
    /// Create a DispatchTable with just 1 version, the baseline, and a limited
    /// alternate capacity.
    static DispatchTable* onlyBaseline(Function* baseline,
                                       const Context& userDefinedContext,
                                       size_t capacity);
  public:
    /// Create a CLOSXP which has a DispatchTable with just 1 version, the
    /// baseline
    static SEXP onlyBaselineClosure(Function* baseline,
                                    const Context& userDefinedContext,
                                    size_t capacity);

    size_t capacity() const { return info.gc_area_length; }

    static DispatchTable* deserializeR(SEXP refTable, R_inpstream_t inp);
    void serializeR(SEXP refTable, R_outpstream_t out) const;
    static DispatchTable* deserialize(AbstractDeserializer& deserializer);
    void serialize(AbstractSerializer& deserializer) const;
    void hash(Hasher& hasher) const;
    void addConnected(ConnectedCollector& collector) const;
    void print(std::ostream&, bool isDetailed = false) const;
    void printPrettyGraphContent(const PrettyGraphInnerPrinter& print) const;
    /// Check if 2 dispatch tables are the same, for validation and sanity check
    /// (before we do operations which will cause weird errors otherwise). If
    /// not, will add each difference to differences.
    static void debugCompare(const DispatchTable* dt1, const DispatchTable* dt2,
                             std::stringstream& differences,
                             bool compareFeedbackAndExtraPoolRBytecodes = true);


    Context userDefinedContext() const { return userDefinedContext_; }
    DispatchTable* newWithUserContext(Context udc) {

        auto clone = create(this->capacity());
        clone->setEntry(0, this->getEntry(0));

        auto j = 1;
        for (size_t i = 1; i < size(); i++) {
            if (get(i)->context().smaller(udc)) {
                clone->setEntry(j, getEntry(i));
                j++;
            }
        }

        clone->size_ = j;
        clone->userDefinedContext_ = udc;
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
              sizeof(DispatchTable),
              // GC area is just the pointers in the entry array
              capacity) {}

    size_t size_ = 0;
    Context userDefinedContext_;
};

#pragma pack(pop)
} // namespace rir

#endif
