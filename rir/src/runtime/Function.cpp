#include "Function.h"
#include "R/Protect.h"
#include "R/Serialize.h"
#include "Rinternals.h"
#include "compiler/compiler.h"
#include "runtime/log/printPrettyGraph.h"
#include "serializeHash/hash/UUIDPool.h"
#include "serializeHash/serialize/serialize.h"
#include "runtime/TypeFeedback.h"

namespace rir {

void Function::setFlag(rir::Function::Flag f) {
    // UUIDPool::reintern(container());
    flags_.set(f);
}

void Function::resetFlag(rir::Function::Flag f) {
    // UUIDPool::reintern(container());
    flags_.reset(f);
}

Function* Function::deserialize(SEXP refTable, R_inpstream_t inp) {
    Protect p;
    size_t functionSize = InInteger(inp);
    const FunctionSignature sig = FunctionSignature::deserialize(refTable, inp);
    const Context as = Context::deserialize(refTable, inp);
    SEXP store = p(Rf_allocVector(EXTERNALSXP, functionSize));
    AddReadRef(refTable, store);
    useRetrieveHashIfSet(inp, store);
    void* payload = DATAPTR(store);
    Function* fun =
        new (payload) Function(functionSize, nullptr, {}, sig, as, nullptr);
    fun->numArgs_ = InInteger(inp);
    fun->info.gc_area_length += fun->numArgs_;
    // What this loop does is that it sets the function owned (yet not
    // deserialized) SEXPs to something reasonable so it will not confuse the GC
    // which might run while they are deserialized.
    // TODO: wouldn't it be better to change the serialization order?
    for (unsigned i = 0; i < fun->numArgs_ + NUM_PTRS; i++) {
        fun->setEntry(i, R_NilValue);
    }
    auto feedback = p(UUIDPool::readItem(refTable, inp));
    fun->typeFeedback(TypeFeedback::unpack(feedback));
    auto body = p(UUIDPool::readItem(refTable, inp));
    fun->body(body);
    for (unsigned i = 0; i < fun->numArgs_; i++) {
        if ((bool)InInteger(inp)) {
            SEXP arg = p(UUIDPool::readItem(refTable, inp));
            assert(Code::check(arg));
            fun->setEntry(Function::NUM_PTRS + i, arg);
        } else {
            fun->setEntry(Function::NUM_PTRS + i, nullptr);
        }
    }
    fun->flags_ = EnumSet<Flag>(InInteger(inp));
    return fun;
}

void Function::serialize(SEXP refTable, R_outpstream_t out) const {
    HashAdd(container(), refTable);
    OutInteger(out, size);
    signature().serialize(refTable, out);
    context_.serialize(refTable, out);
    OutInteger(out, numArgs_);

    UUIDPool::writeItem(typeFeedback()->container(), refTable, noHashOut);
    UUIDPool::writeItem(getEntry(0), refTable, out);

    for (unsigned i = 0; i < numArgs_; i++) {
        CodeSEXP arg = defaultArg_[i];
        OutInteger(out, (int)(arg != nullptr));
        if (arg) {
            assert(Code::check(arg));
            // arg->serialize(false, refTable, out);
            UUIDPool::writeItem(arg, refTable, out);
        }
    }
    OutInteger(out, (int)flags_.to_i());
}

void Function::hash(Hasher& hasher) const {
    hasher.hashBytesOf(signature());
    hasher.hashBytesOf(context_);
    hasher.hashBytesOf(numArgs_);
    // TODO: why are body and args not set sometimes when we hash
    //  deserialized value to check hash consistency? It probably has
    //  something to do with cyclic references in serialization, but why?
    //  (This is one of the reasons we use SEXP instead of unpacking Code
    //  for body and default args, also because we are going to serialize
    //  the SEXP anyways to properly handle cyclic references)
    hasher.hash(getEntry(0));

    for (unsigned i = 0; i < numArgs_; i++) {
        CodeSEXP arg = defaultArg_[i];
        hasher.hashNullable(arg);
    }

    // Don't hash flags because they change
}

void Function::addConnected(ConnectedCollector& collector) const {
    collector.add(getEntry(0));

    for (unsigned i = 0; i < numArgs_; i++) {
        CodeSEXP arg = defaultArg_[i];
        collector.addNullable(arg);
    }
}

void Function::disassemble(std::ostream& out) const {
    print(out);
}

void Function::print(std::ostream& out, bool isDetailed) const {
    if (isDetailed) {
        out << "[size]" << size << "\n[numArgs] " << numArgs_ << "\n";
    }
    out << "[signature] ";
    signature().print(out);
    if (!context_.empty())
        out << "| context: [" << context_ << "]";
    out << "\n";
    out << "[flags]    ";
#define V(F)                                                                   \
if (flags_.includes(F))                                                    \
    out << #F << " ";
    RIR_FUNCTION_FLAGS(V)
#undef V
    out << "\n";
    out << "[stats]    ";
    out << "invoked: " << invocationCount()
        << ", time: " << ((double)invocationTime() / 1e6)
        << "ms, deopt: " << deoptCount();
    out << "\n";
    if (isDetailed) {
        body()->print(out, isDetailed);
        for (unsigned i = 0; i < numArgs_; i++) {
            CodeSEXP arg = defaultArg_[i];
            if (arg) {
                out << "[default arg " << i << "]\n";
                Code::unpack(arg)->print(out, isDetailed);
            }
        }
    } else {
        body()->disassemble(out);
    }
}

void Function::printPrettyGraphContent(const PrettyGraphInnerPrinter& print) const {
    print.addName([&](std::ostream& s) {
        auto ast = src_pool_at(body()->src);
        auto headAst = TYPEOF(ast) == LANGSXP ? CAR(ast) : R_NilValue;
        if (TYPEOF(headAst) == SYMSXP) {
            s << CHAR(PRINTNAME(headAst));
        } else {
            s << "<anon size=" << size << " numArgs=" << numArgs_ << ">";
        }
    });
    print.addBody([&](std::ostream& s) {
        s << "<p class=\"function-signature\">(";
        signature().print(s);
        s << ")</p>";
        if (!context_.empty()) {
            s << "<p class=\"function-context\">[" << context_
              << "]</p>";
        }
        if (!flags_.empty()) {
            s << "<p class=\"function-flags\">{";
        }
#define V(F)                                                                   \
        if (flags_.includes(F))                                        \
            s << #F << " ";
            RIR_FUNCTION_FLAGS(V)
#undef V
        if (!flags_.empty()) {
            s << "}</p>";
        }
        s << "<p class=\"function-stats\">"
          << "invoked: " << invocationCount()
          << ", time: " << ((double)invocationTime() / 1e6)
          << "ms, deopt: " << deoptCount()
          << "</p>";
    });
    print.addEdgeTo(body()->container(), true, "body");
    for (unsigned i = 0; i < numArgs_; i++) {
        CodeSEXP arg = defaultArg_[i];
        if (arg) {
            print.addEdgeTo(arg, true, "default-arg", [&](std::ostream& s) {
                s << "arg " << i << " default";
            });
        }
    }
}

static int GLOBAL_SPECIALIZATION_LEVEL =
    getenv("PIR_GLOBAL_SPECIALIZATION_LEVEL")
        ? atoi(getenv("PIR_GLOBAL_SPECIALIZATION_LEVEL"))
        : 100;
void Function::clearDisabledAssumptions(Context& given) const {
    if (flags_.contains(Function::DisableArgumentTypeSpecialization))
        given.clearTypeFlags();
    if (flags_.contains(Function::DisableNumArgumentsSpezialization))
        given.clearNargs();
    if (flags_.contains(Function::DisableAllSpecialization))
        given.clearExcept(pir::Compiler::minimalContext);

    if (GLOBAL_SPECIALIZATION_LEVEL < 100)
        given.setSpecializationLevel(GLOBAL_SPECIALIZATION_LEVEL);
}

} // namespace rir
