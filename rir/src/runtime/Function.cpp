#include "Function.h"
#include "R/Protect.h"
#include "Rinternals.h"
#include "compiler/compiler.h"
#include "interpreter/instance.h"
#include "runtime/TypeFeedback.h"
#include "runtime/log/printPrettyGraph.h"

namespace rir {

void Function::setFlag(rir::Function::Flag f) {
    // UUIDPool::reintern(container());
    flags_.set(f);
}

void Function::resetFlag(rir::Function::Flag f) {
    // UUIDPool::reintern(container());
    flags_.reset(f);
}

Function* Function::deserialize(AbstractDeserializer& deserializer) {
    Protect p;
    auto funSize = deserializer.readBytesOf<R_xlen_t>(SerialFlags::FunMiscBytes);
    auto sig = FunctionSignature::deserialize(deserializer);
    auto ctx = Context(deserializer.readBytesOf<unsigned long>(SerialFlags::FunMiscBytes));
    auto flags = EnumSet<Flag>(deserializer.readBytesOf<unsigned long>(SerialFlags::FunMiscBytes));
    auto invocationCount_ = deserializer.readBytesOf<unsigned>(SerialFlags::FunStats);
    auto deoptCount_ = deserializer.readBytesOf<unsigned>(SerialFlags::FunStats);
    auto deadCallReached_ = deserializer.readBytesOf<unsigned>(SerialFlags::FunStats);
    auto invoked = deserializer.readBytesOf<unsigned long>(SerialFlags::FunStats);
    auto execTime = deserializer.readBytesOf<unsigned long>(SerialFlags::FunStats);
    SEXP store = p(Rf_allocVector(EXTERNALSXP, funSize));
    deserializer.addRef(store);

    auto feedback = p(deserializer.read(SerialFlags::FunFeedback));
    auto body = p(deserializer.read(SerialFlags::FunBody));
    std::vector<SEXP> defaultArgs(sig.numArguments, nullptr);
    for (unsigned i = 0; i < sig.numArguments; i++) {
        if (deserializer.readBytesOf<bool>(SerialFlags::FunDefaultArg)) {
            defaultArgs[i] = p(deserializer.read(SerialFlags::FunDefaultArg));
        }
    }

    auto fun = new (DATAPTR(store))
        Function(funSize, body, defaultArgs, sig, ctx,
                 TypeFeedback::unpack(feedback));
    fun->flags_ = flags;
    fun->invocationCount_ = invocationCount_;
    fun->deoptCount_ = deoptCount_;
    fun->deadCallReached_ = deadCallReached_;
    fun->invoked = invoked;
    fun->execTime = execTime;
    return fun;
}

void Function::serialize(AbstractSerializer& serializer) const {
    serializer.writeBytesOf<R_xlen_t>((R_xlen_t)size, SerialFlags::FunMiscBytes);
    signature().serialize(serializer);
    serializer.writeBytesOf<unsigned long>(context_.toI(), SerialFlags::FunMiscBytes);
    serializer.writeBytesOf<unsigned long>(flags_.to_i(), SerialFlags::FunMiscBytes);
    serializer.writeBytesOf<unsigned>(invocationCount_, SerialFlags::FunStats);
    serializer.writeBytesOf<unsigned>(deoptCount_, SerialFlags::FunStats);
    serializer.writeBytesOf<unsigned>(deadCallReached_, SerialFlags::FunStats);
    serializer.writeBytesOf<unsigned long>(invoked, SerialFlags::FunStats);
    serializer.writeBytesOf<unsigned long>(execTime, SerialFlags::FunStats);

    serializer.write(typeFeedback()->container(), SerialFlags::FunFeedback);
    serializer.write(body()->container(), SerialFlags::FunBody);
    for (unsigned i = 0; i < numArgs_; i++) {
        serializer.writeBytesOf(defaultArg_[i] != nullptr, SerialFlags::FunDefaultArg);
        if (defaultArg_[i]) {
            serializer.write(defaultArg_[i], SerialFlags::FunDefaultArg);
        }
    }
}

void Function::hash(HasherOld& hasher) const {
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

void Function::addConnected(ConnectedCollectorOld& collector) const {
    collector.add(getEntry(0), false);

    for (unsigned i = 0; i < numArgs_; i++) {
        CodeSEXP arg = defaultArg_[i];
        collector.addNullable(arg, false);
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
        if (flags_.includes(F))                                                \
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

void Function::debugCompare(const Function* f1, const Function* f2,
                            std::stringstream& differences,
                            bool compareFeedbackAndExtraPoolRBytecodes) {
    FunctionSignature::debugCompare(f1->signature(), f2->signature(), differences);
    if (f1->context() != f2->context()) {
        differences << "context: " << f1->context() << " != " << f2->context()
                    << "\n";
    }
    if (f1->flags() != f2->flags()) {
        differences << "flags: ";
#define V(F)                                                                   \
        if (f1->flags_.includes(F))                                                \
            differences << #F << " ";
        RIR_FUNCTION_FLAGS(V)
#undef V
        differences << " != ";
#define V(F)                                                                   \
        if (f2->flags_.includes(F))                                                \
            differences << #F << " ";
        RIR_FUNCTION_FLAGS(V)
#undef V
        differences << "\n";
    }
    if (f1->size != f2->size) {
        differences << "size: " << f1->size << " != " << f2->size << "\n";
    }
    if (f1->numArgs_ != f2->numArgs_) {
        differences << "numArgs: " << f1->numArgs_ << " != " << f2->numArgs_
                    << "(note: signature also has numArgs)\n";
    }
    if (compareFeedbackAndExtraPoolRBytecodes) {
        if (f1->invocationCount_ != f2->invocationCount_) {
            differences << "invocationCount: " << f1->invocationCount_
                        << " != " << f2->invocationCount_ << "\n";
        }
        if (f1->deoptCount_ != f2->deoptCount_) {
            differences << "deoptCount: " << f1->deoptCount_
                        << " != " << f2->deoptCount_ << "\n";
        }
        if (f1->deadCallReached_ != f2->deadCallReached_) {
            differences << "deadCallReached: " << f1->deadCallReached_
                        << " != " << f2->deadCallReached_ << "\n";
        }
        if (f1->invoked != f2->invoked) {
            differences << "invoked: " << f1->invoked
                        << " != " << f2->invoked << "\n";
        }
        if (f1->execTime != f2->execTime) {
            differences << "invocationTime: " << f1->execTime
                        << " != " << f2->execTime << "\n";
        }
    }
    Code::debugCompare(f1->body(), f2->body(), "body", differences,
                       compareFeedbackAndExtraPoolRBytecodes);
    for (unsigned i = 0; i < std::min(f1->numArgs_, f2->numArgs_); i++) {
        auto arg1 = f1->defaultArg_[i];
        auto arg2 = f2->defaultArg_[i];
        auto hasArg1 = (arg1 != nullptr);
        auto hasArg2 = (arg2 != nullptr);
        if (hasArg1 != hasArg2) {
            differences << "defaultArg[" << i << "] != nullptr: " << hasArg1
                        << " != " << hasArg2 << "\n";
        }
        if (hasArg1 && hasArg2) {
            char prefix[100];
            sprintf(prefix, "defaultArg[%u]", i);
            Code::debugCompare(Code::unpack(arg1), Code::unpack(arg2),
                               prefix, differences, compareFeedbackAndExtraPoolRBytecodes);
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
