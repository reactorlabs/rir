#include "Code.h"
#include "Function.h"
#include "R/Printing.h"
#include "bc/BC.h"
#include "bc/BC_inc.h"
#include "compiler/native/pir_jit_llvm.h"
#include "compiler/parameter.h"
#include "rirObjectMagic.h"
#include "runtime/log/printPrettyGraph.h"
#include "runtime/TypeFeedback.h"
#include "serializeHash/hash/UUIDPool.h"
#include "serializeHash/hash/hashAst.h"
#include "serializeHash/serialize/serialize.h"
#include "utils/HTMLBuilder/escapeHtml.h"
#include "utils/Pool.h"
#include "utils/measuring.h"

#include <llvm/ExecutionEngine/JITSymbol.h>

#include <iomanip>
#include <sstream>

namespace rir {

static const unsigned PRETTY_GRAPH_CODE_NAME_MAX_LENGTH = 25;

// cppcheck-suppress uninitMemberVar; symbol=data
Code::Code(Kind kind, FunctionSEXP fun, SEXP src, unsigned srcIdx, unsigned cs,
           unsigned sourceLength, size_t localsCnt, size_t bindingsCnt)
    : RirRuntimeObject(
          // GC area starts just after the header
          (intptr_t)&locals_ - (intptr_t)this,
          NumLocals),
      kind(kind), nativeCode_(nullptr), src(srcIdx), trivialExpr(nullptr),
      stackLength(0), localsCount(localsCnt), bindingCacheSize(bindingsCnt),
      codeSize(cs), srcLength(sourceLength), extraPoolSize(0) {
    setEntry(0, R_NilValue);
    if (src && TYPEOF(src) == SYMSXP)
        trivialExpr = src;
    assert(!fun || rir::Function::check(fun));
    if (fun)
        setEntry(3, fun);
}

Code* Code::New(Kind kind, Immediate ast, size_t codeSize, size_t sources,
                size_t locals, size_t bindingCache) {
    unsigned totalSize = Code::size(codeSize, sources);
    SEXP store = Rf_allocVector(EXTERNALSXP, totalSize);
    void* payload = DATAPTR(store);
    return new (payload) Code(kind, nullptr, src_pool_at(ast), ast, codeSize,
                              sources, locals, bindingCache);
}

Code* Code::NewBytecode(Immediate ast, size_t codeSize, size_t sources,
                        size_t locals, size_t bindingCache) {
    return New(Kind::Bytecode, ast, codeSize, sources, locals, bindingCache);
}

Code* Code::NewNative(Immediate ast) {
    return New(Kind::Native, ast, 0, 0, 0, 0);
}

void Code::setLazyCodeModuleFinalizer() {
    makeFinalizer(Code::finalizeLazyCodeModuleFromContainer, false);
}

void Code::finalizeLazyCodeModuleFromContainer(SEXP sexp) {
    Code::unpack(sexp)->finalizeLazyCodeModule();
}

void Code::finalizeLazyCodeModule() {
    assert(lazyCodeModule);
    // Causes this to free the shared reference
    lazyCodeModule = nullptr;
}

void Code::lazyCode(const std::string& handle, const SerialModuleRef& module) {
    assert(!handle.empty());
    assert(handle.size() < MAX_CODE_HANDLE_LENGTH);
    assert(kind == Kind::Native);
    assert(lazyCodeHandle[0] == '\0' && !lazyCodeModule);
    strncpy(lazyCodeHandle, handle.c_str(), MAX_CODE_HANDLE_LENGTH - 1);
    lazyCodeModule = module;
    UUIDPool::reintern(container());
    if (module) {
        setLazyCodeModuleFinalizer();
    }
}

void Code::function(Function* fun) { setEntry(3, fun->container()); }

rir::Function* Code::functionOpt() const {
    auto f = getEntry(3);
    if (!f && kind == Kind::Deserializing) {
        return nullptr;
    }
    assert(f && "no function, but code is not being deserialized");
    return rir::Function::check(f);
}

rir::Function* Code::function() const {
    auto f = getEntry(3);
    if (!f && kind == Kind::Deserializing) {
        assert(false && "can't access function of code while it's being deserialized");
    }
    assert(f);
    return rir::Function::unpack(f);
}

unsigned Code::getSrcIdxAt(const Opcode* pc, bool allowMissing) const {
    if (srcLength == 0) {
        assert(allowMissing);
        return 0;
    }

    SrclistEntry* sl = srclist();
    Opcode* start = code();
    auto pcOffset = pc - start;

    if (srcLength == 1) {
        auto sidx = sl[0].pcOffset == pcOffset ? sl[0].srcIdx : 0;
        SLOWASSERT(allowMissing || sidx);
        return sidx;
    }

    // Binary search through src list
    int lower = 0;
    int upper = srcLength - 1;
    int finger = upper / 2;
    unsigned sidx = 0;

    while (lower <= upper) {
        if (sl[finger].pcOffset == pcOffset) {
            sidx = sl[finger].srcIdx;
            break;
        }
        if (sl[finger].pcOffset < pcOffset)
            lower = finger + 1;
        else
            upper = finger - 1;
        finger = lower + (upper - lower) / 2;
    }
    SLOWASSERT(sidx == 0 || sl[finger].pcOffset == pcOffset);
    SLOWASSERT(allowMissing || sidx);

    return sidx;
}

Code* Code::deserialize(AbstractDeserializer& deserializer) {
    Protect p;
    auto size = deserializer.readBytesOf<R_xlen_t>(SerialFlags::CodeMisc);
    auto store = p(Rf_allocVector(EXTERNALSXP, size));
    auto code = new (DATAPTR(store)) Code;
    // Magic is already set
    deserializer.addRef(store);

    // Header
    DESERIALIZE(code->src, readSrc, SerialFlags::CodeAst);
    DESERIALIZE(code->trivialExpr, readNullable, SerialFlags::CodeAst);
    DESERIALIZE(code->stackLength, readBytesOf<unsigned>, SerialFlags::CodeMisc);
    DESERIALIZE(*const_cast<unsigned*>(&code->localsCount), readBytesOf<unsigned>, SerialFlags::CodeMisc);
    DESERIALIZE(*const_cast<unsigned*>(&code->bindingCacheSize), readBytesOf<unsigned>, SerialFlags::CodeMisc);
    DESERIALIZE(code->codeSize, readBytesOf<unsigned>, SerialFlags::CodeMisc);
    DESERIALIZE(code->srcLength, readBytesOf<unsigned>, SerialFlags::CodeMisc);
    DESERIALIZE(code->extraPoolSize, readBytesOf<unsigned>, SerialFlags::CodeMisc);
    auto argReorder = deserializer.readNullable(SerialFlags::CodeArglistOrder);
    auto outer = p.nullable(deserializer.read(SerialFlags::CodeOuterFun));
    // Can't check magic because it may not be assigned yet
    assert((!outer || TYPEOF(outer) == EXTERNALSXP) &&
           "sanity check failed: code's outer is not a Function");

    // Bytecode
    BC::deserialize(deserializer, code->code(), code->codeSize, code);

    // Extra pool
    SEXP extraPool = Rf_allocVector(VECSXP, code->extraPoolSize);
    for (unsigned i = 0; i < code->extraPoolSize; ++i) {
        auto extraPoolFlag = SerialFlags::ById[deserializer.readBytesOf<unsigned>(SerialFlags::CodeMisc)];
        SET_VECTOR_ELT(extraPool, i, deserializer.read(extraPoolFlag));
    }

    // Srclist
    if (deserializer.willRead(SerialFlags::CodeMisc)) {
        for (unsigned i = 0; i < code->srcLength; i++) {
            code->srclist()[i].pcOffset = deserializer.readBytesOf<uint32_t>(SerialFlags::CodeMisc);
            code->srclist()[i].srcIdx = deserializer.readSrc(SerialFlags::CodeAst);
        }
    }
    code->info = {// GC area starts just after the header
                  (uint32_t)((intptr_t)&code->locals_ - (intptr_t)code),
                  NumLocals, CODE_MAGIC};
    code->setEntry(0, extraPool);
    if (outer) {
        code->setEntry(3, outer);
    }
    if (argReorder) {
        code->setEntry(2, argReorder);
    }

    // Native code
    if (deserializer.willRead(SerialFlags::CodeNative)) {
        code->kind = deserializer.readBytesOf<Kind>(SerialFlags::CodeNative);
        if (code->kind == Kind::Native) {
            auto lazyCodeHandleLen =
                deserializer.readBytesOf<unsigned>(SerialFlags::CodeNative);
            deserializer.readBytes(code->lazyCodeHandle, lazyCodeHandleLen,
                                   SerialFlags::CodeNative);
            code->lazyCodeHandle[lazyCodeHandleLen] = '\0';
            if (deserializer.readBytesOf<bool>(SerialFlags::CodeNative)) {
                code->lazyCodeModule =
                    pir::PirJitLLVM::deserializeModule(deserializer, code);
                code->setLazyCodeModuleFinalizer();
            }
        }
    }
    // Native code is always null here because it's lazy
    code->nativeCode_ = nullptr;

    return code;
}

void Code::serialize(AbstractSerializer& serializer) const {
    serializer.writeBytesOf((R_xlen_t)size(), SerialFlags::CodeMisc);

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serialize source", container(), [&]{
        serializer.writeSrc(src, SerialFlags::CodeAst);
        serializer.writeNullable(trivialExpr, SerialFlags::CodeAst);
    });
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serialize numbers", container(), [&]{
        serializer.writeBytesOf((unsigned)stackLength, SerialFlags::CodeMisc);
        serializer.writeBytesOf((unsigned)localsCount, SerialFlags::CodeMisc);
        serializer.writeBytesOf((unsigned)bindingCacheSize, SerialFlags::CodeMisc);
        serializer.writeBytesOf((unsigned)codeSize, SerialFlags::CodeMisc);
        serializer.writeBytesOf((unsigned)srcLength, SerialFlags::CodeMisc);
        serializer.writeBytesOf((unsigned)extraPoolSize, SerialFlags::CodeMisc);
    });

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serialize call argument reordering metadata", container(), [&]{
        serializer.writeNullable(getEntry(2), SerialFlags::CodeArglistOrder);
    });
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serialize outer function", container(), [&]{
        serializer.write(getEntry(3), SerialFlags::CodeOuterFun);
    });

    std::vector<SerialFlags> extraPoolFlags(extraPoolSize, SerialFlags::CodePoolUnknown);
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serialize bytecode", container(), [&]{
        // One might think we can skip serializing entries which are just
        // recorded calls, but it breaks semantics and causes a test failure
        BC::serialize(serializer, extraPoolFlags, code(), codeSize, this);
    });

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serialize extra pool", container(), [&]{
        for (unsigned i = 0; i < extraPoolSize; ++i) {
            serializer.writeBytesOf(extraPoolFlags[i].id(), SerialFlags::CodeMisc);
            serializer.write(getExtraPoolEntry(i), extraPoolFlags[i]);
        }
    });

    if (serializer.willWrite(SerialFlags::CodeMisc)) {
        Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serialize srclist", container(), [&]{
            for (unsigned i = 0; i < srcLength; i++) {
                serializer.writeBytesOf(srclist()[i].pcOffset, SerialFlags::CodeMisc);
                serializer.writeSrc(srclist()[i].srcIdx, SerialFlags::CodeAst);
            }
        });
    }

    if (serializer.willWrite(SerialFlags::CodeNative)) {
        Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: serialize native", container(), [&]{
            serializer.writeBytesOf(kind, SerialFlags::CodeNative);
            assert((kind != Kind::Native || lazyCodeHandle[0] != '\0') &&
                   "Code in bad pending state");
            if (kind == Kind::Native && lazyCodeHandle[0] != '\0') {
                assert(lazyCodeHandle[0] != '\0');
                auto lazyCodeHandleLen = (unsigned)strlen(lazyCodeHandle);
                serializer.writeBytesOf(lazyCodeHandleLen, SerialFlags::CodeNative);
                serializer.writeBytes(lazyCodeHandle, lazyCodeHandleLen, SerialFlags::CodeNative);
                serializer.writeBytesOf(lazyCodeModule != nullptr, SerialFlags::CodeNative);
                if (lazyCodeModule) {
                    lazyCodeModule->serialize(serializer);
                }
            }
        });
    }
}

void Code::hash(HasherOld& hasher) const {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: hash source", container(), [&]{
        hasher.hashSrc(src);
    });
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: hash numbers", container(), [&]{
        hasher.hashBytesOf<unsigned>(stackLength);
        hasher.hashBytesOf<unsigned>(localsCount);
        hasher.hashBytesOf<unsigned>(bindingCacheSize);
        hasher.hashBytesOf<unsigned>(codeSize);
        hasher.hashBytesOf<unsigned>(srcLength);
        hasher.hashBytesOf<unsigned>(extraPoolSize);
    });
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: hash call argument reordering metadata", container(), [&]{
        hasher.hashNullable(getEntry(2));
    });
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: hash outer function", container(), [&]{
        hasher.hash(function()->container());
    });

    std::vector<bool> extraPoolIgnored;
    extraPoolIgnored.resize(extraPoolSize);
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: hash bytecode", container(), [&]{
        BC::hash(hasher, extraPoolIgnored, code(), codeSize, this);
    });

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: hash extra pool", container(), [&]{
        hasher.hashBytesOf(extraPoolSize);
        for (unsigned i = 0; i < extraPoolSize; ++i) {
            if (!extraPoolIgnored[i]) {
                hasher.hash(getExtraPoolEntry(i));
            }
        }
    });

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: hash srclist", container(), [&]{
        for (unsigned i = 0; i < srcLength; i++) {
            hasher.hashBytesOf<unsigned>(srclist()[i].pcOffset);
            hasher.hashSrc(srclist()[i].srcIdx);
        }
    });

    // Don't hash native code
}

void Code::addConnected(ConnectedCollectorOld& collector) const {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: add connected in source", container(), [&]{
        collector.addSrc(src);
    });
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: add connected in call argument reordering metadata", container(), [&]{
        collector.addNullable(getEntry(2), false);
    });
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: add connected in outer function", container(), [&]{
        collector.add(function()->container(), false);
    });

    std::vector<bool> extraPoolChildren;
    extraPoolChildren.resize(extraPoolSize);
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: add connected in bytecode", container(), [&]{
        BC::addConnected(extraPoolChildren, collector, code(), codeSize, this);
    });

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: add connected in extra pool", container(), [&]{
        for (unsigned i = 0; i < extraPoolSize; ++i) {
            collector.add(getExtraPoolEntry(i), extraPoolChildren[i]);
        }
    });

    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "Code.cpp: add connected in srclist", container(), [&]{
        for (unsigned i = 0; i < srcLength; i++) {
            collector.addSrc(srclist()[i].srcIdx);
        }
    });

    // No connected in SEXPs native code
}

void Code::disassemble(std::ostream& out, const std::string& prefix) const {
    if (kind == Kind::Deserializing) {
        out << "(code is being deserialized)\n";
        return;
    }

    if (auto map = pirTypeFeedback()) {
        map->forEachSlot(
            [&](size_t i, const PirTypeFeedback::MDEntry& mdEntry) {
                auto feedback = mdEntry.feedback;
                out << " - slot #" << i << ": " << mdEntry.rirIdx << " : [";
                feedback.print(out);
                out << "] (" << mdEntry.sampleCount << " records - "
                    << (mdEntry.readyForReopt ? "ready" : "not ready")
                    << ") prev: " << mdEntry.previousType << "\n";
            });
    }

    switch (kind) {
    case Kind::Bytecode: {
        auto fun = functionOpt();
        auto typeFeedback = fun && !fun->isDeserializing() ? fun->typeFeedback() : nullptr;
        Opcode* pc = code();
        size_t label = 0;
        std::map<Opcode*, size_t> targets;
        targets[pc] = label++;
        while (pc < endCode()) {
            if (BC::decodeShallow(pc).isJmp()) {
                auto t = BC::jmpTarget(pc);
                if (!targets.count(t))
                    targets[t] = label++;
            }
            pc = BC::next(pc);
        }
        // sort labels ascending
        label = 0;
        for (auto& t : targets)
            t.second = label++;

        auto formatLabel = [&](size_t label) { out << label; };

        pc = code();
        std::vector<BC::FunIdx> promises;

        while (pc < endCode()) {

            if (targets.count(pc)) {
                formatLabel(targets[pc]);
                out << ":\n";
            }

            BC bc = BC::decode(pc, this);
            bc.addMyPromArgsTo(promises);

            const size_t OFFSET_WIDTH = 7;
            out << std::right << std::setw(OFFSET_WIDTH)
                << ((uintptr_t)pc - (uintptr_t)code()) << std::left;

            unsigned s = getSrcIdxAt(pc, true);
            if (s != 0)
                out << "   ; " << Print::dumpSexp(src_pool_at(s)) << "\n"
                    << std::setw(OFFSET_WIDTH) << "";

            // Print call ast
            switch (bc.bc) {
            case Opcode::call_:
            case Opcode::named_call_:
                out << "   ; "
                    << Print::dumpSexp(
                           Pool::get(bc.immediate.callFixedArgs.ast))
                    << "\n"
                    << std::setw(OFFSET_WIDTH) << "";
                break;
            default: {
            }
            }

            if (bc.isJmp()) {
                out << "   ";
                bc.printOpcode(out);
                formatLabel(targets[BC::jmpTarget(pc)]);
                out << "\n";
            } else if (bc.isRecord()) {
                out << "   "
                    << "[ ";
                if (bc.bc == Opcode::record_call_) {
                    if (typeFeedback) {
                        typeFeedback->callees(bc.immediate.i).print(out, fun);
                    } else {
                        out << "<no outer fun!>";
                    }
                    out << " ] Call#";
                } else if (bc.bc == Opcode::record_test_) {
                    if (typeFeedback) {
                        typeFeedback->test(bc.immediate.i).print(out);
                    } else {
                        out << "<no outer fun!>";
                    }
                    out << " ] Test#";
                } else {
                    if (typeFeedback) {
                        typeFeedback->types(bc.immediate.i).print(out);
                    } else {
                        out << "<no outer fun!>";
                    }
                    out << " ] Type#";
                }
                out << bc.immediate.i << "\n";
            } else {
                bc.print(out);
            }
            pc = BC::next(pc);
        }

        for (auto i : promises) {
            auto c = getPromise(i);
            out << "\n[Prom (index " << prefix << i << ")]\n";
            std::stringstream ss;
            ss << prefix << i << ".";
            c->disassemble(out, ss.str());
        }
        break;
    }
    case Kind::Native: {
        if (nativeCode_) {
            out << "nativeCode " << nativeCode_ << ", module:";
            if (lazyCodeModule) {
                out << "\n" << lazyCodeModule;
            } else {
                out << " (elided)";
            }
            out << "\n";
        } else {
            out << "nativeCode (compilation pending)\n";
        }
        break;
    }
    default:
        assert(false);
    }

    if (auto a = arglistOrder()) {
        out << "arglistOrder:\n";
        for (size_t i = 0; i < a->nCalls; i++) {
            out << "  id " << i << ": ";
            for (size_t j = 0; j < a->originalArglistLength(i); j++) {
                out << ArglistOrder::decodeArg(a->index(i, j))
                    << (ArglistOrder::isArgNamed(a->index(i, j)) ? "n " : " ");
            }
            out << "\n";
        }
    }
}

void Code::print(std::ostream& out, bool isDetailed) const {
    out << "Code object\n";
    out << std::left << std::setw(20) << "   Source: " << src
        << " (index into src pool)\n";
    out << std::left << std::setw(20) << "   Magic: " << std::hex
        << info.magic << std::dec << " (hex)\n";
    out << std::left << std::setw(20) << "   Stack (o): " << stackLength
        << "\n";
    out << std::left << std::setw(20) << "   Code size: " << codeSize
        << "[B]\n";
    if (isDetailed) {
        out << std::left << std::setw(20) << "   Size: " << size()
            << "[B]\n";
    }

    if (info.magic != CODE_MAGIC) {
        out << "Wrong magic number -- corrupted IR bytecode";
        Rf_error("Wrong magic number -- corrupted IR bytecode");
    }

    out << "\n";
    disassemble(out);

    if (isDetailed) {
        out << "extra pool = \n"
            << Print::dumpSexp(getEntry(0), SIZE_MAX) << "\n";
        out << "src = \n"
            << Print::dumpSexp(src_pool_at(src), SIZE_MAX)
            << ", hash = " << hashAst(src_pool_at(src)) << "\n";
        for (unsigned i = 0; i < srcLength; i++) {
            out << "src[" << i << "] @ " << srclist()[i].pcOffset
                << " = \n";
            out << Print::dumpSexp(src_pool_at(i), SIZE_MAX)
                << ", hash = " << hashAst(src_pool_at(i)) << "\n";
        }
    }
}


static bool isEqualOrInExtraPool(const Code* outer, const Code* inner) { // NOLINT(*-no-recursion)
    if (outer == inner) {
        return true;
    }
    for (unsigned i = 0; i < outer->extraPoolSize; ++i) {
        auto codeEntry = Code::check(outer->getExtraPoolEntry(i));
        if (codeEntry && isEqualOrInExtraPool(codeEntry, inner)) {
            return true;
        }
    }
    return false;
}

static bool isInFunction(const Function* outer, const Code* inner) {
    if (isEqualOrInExtraPool(outer->body(), inner)) {
        return true;
    }
    for (unsigned i = 0; i < outer->nargs(); ++i) {
        auto arg = outer->defaultArg(i);
        if (arg && isEqualOrInExtraPool(arg, inner)) {
            return true;
        }
    }
    return false;
}

void Code::printPrettyGraphContent(const PrettyGraphInnerPrinter& print) const {
    auto srcPrint = Print::dumpSexp(src_pool_at(src), SIZE_MAX);
    print.addName([&](std::ostream& s) {
        if (srcPrint.length() < PRETTY_GRAPH_CODE_NAME_MAX_LENGTH) {
            s << srcPrint;
        } else {
            s << srcPrint.substr(0, PRETTY_GRAPH_CODE_NAME_MAX_LENGTH)
              << "...";
        }
    });
    print.addBody([&](std::ostream& s) {
        if (srcPrint.length() >= PRETTY_GRAPH_CODE_NAME_MAX_LENGTH) {
            s << "<pre>" << escapeHtml(srcPrint) << "</pre>\n";
        }
        std::stringstream str;
        disassemble(str);
        s << "<pre>" << escapeHtml(str.str()) << "</pre>";
    });
    auto addSourceEdge = [&](SEXP sexp, const char* type, size_t index = SIZE_MAX){
        if (sexp && sexp != R_NilValue && TYPEOF(sexp) != SYMSXP &&
            TYPEOF(sexp) != LANGSXP && TYPEOF(sexp) != INTSXP &&
            TYPEOF(sexp) != LGLSXP && TYPEOF(sexp) != REALSXP &&
            TYPEOF(sexp) != CPLXSXP && TYPEOF(sexp) != CHARSXP &&
            TYPEOF(sexp) != STRSXP) {
            print.addEdgeTo(sexp, false, "unexpected-ast", [&](std::ostream& s){
                s << type;
                if (index != SIZE_MAX) {
                    s << " " << index;
                }
                s << " isn't a source type!";
            });
        }
    };
    addSourceEdge(src_pool_at(src), "source");
    addSourceEdge(trivialExpr, "trivial-expr");
    for (unsigned i = 0; i < srcLength; i++) {
        addSourceEdge(src_pool_at(i), "src-pool entry", i);
    }
    if (arglistOrderContainer()) {
        print.addEdgeTo(arglistOrderContainer(), true, "arglist-order", [&](std::ostream& s) {
            s << "arglist order";
        });
    }
    auto fun = functionOpt();
    if (fun && !isInFunction(fun, this)) {
        print.addEdgeTo(fun->container(), false, "unexpected", [&](std::ostream& s) {
            s << "function, its not this code's parent!";
        });
    }
    std::vector<bool> addedExtraPoolEntries;
    addedExtraPoolEntries.resize(extraPoolSize);
    BC::addToPrettyGraph(print, addedExtraPoolEntries, code(), codeSize, this);
    for (unsigned i = 0; i < extraPoolSize; i++) {
        if (!addedExtraPoolEntries[i]) {
            print.addEdgeTo(getExtraPoolEntry(i), false, "unknown-extra-pool", [&](std::ostream& s) {
                s << "pool " << i;
            });
        }
    }
}

static void compareAsts(SEXP ast1, SEXP ast2,
                        const char* prefix, const char* srcPrefix,
                        std::stringstream& differences) {
    // Asts can be compared via printing
    auto print1 = ast1 ? Print::dumpSexp(ast1, SIZE_MAX) : "(null)";
    auto print2 = ast1 ? Print::dumpSexp(ast2, SIZE_MAX) : "(null)";
    if (print1 != print2) {
        differences << prefix << " " << srcPrefix << " asts differ:\n";
        differences << prefix << "  " << srcPrefix << "1: " << print1 << "\n";
        differences << prefix << "  " << srcPrefix << "2: " << print2 << "\n";
    }
}

// Can probably be compared for equivalency by comparing the debug prints (no
// pointers in debug prints). This is used for debugging so doesn't have to be
// 100% accurate
static bool isProbablyDirectlyComparable[] = {
    /* NILSXP */      true,
    /* SYMSXP */      true,
    /* LISTSXP */     true,
    /* CLOSXP */      false,
    /* ENVSXP */      false,
    /* PROMSXP */     false,
    /* LANGSXP */     true,
    /* SPECIALSXP */  true,
    /* BUILTINSXP */  true,
    /* CHARSXP */     true,
    /* LGLSXP */      true,
    /* unused */      false,
    /* unused */      false,
    /* INTSXP */      true,
    /* REALSXP */     true,
    /* CPLXSXP */     true,
    /* STRSXP */      true,
    /* DOTSXP */      true,
    /* ANYSXP */      false,
    /* VECSXP */      true,
    /* EXPRSXP */     true,
    /* BCODESXP */    false,
    /* EXTPTRSXP */   false,
    /* WEAKREFSXP */  false,
    /* RAWSXP */      false,
    /* S4SXP */       false,
    /* EXTERNALSXP */ false
};

static void compareSexps(SEXP sexp1, SEXP sexp2,
                         const char* prefix, const char* srcPrefix,
                         std::stringstream& differences,
                         bool compareExtraPoolRBytecodes) {
    if (TYPEOF(sexp1) != TYPEOF(sexp2)) {
        if (compareExtraPoolRBytecodes ||
            TYPEOF(sexp1) != BCODESXP || TYPEOF(sexp2) != NILSXP) {
            differences << prefix << " " << srcPrefix
                        << " types differ: " << Rf_type2char(TYPEOF(sexp1))
                        << " vs " << Rf_type2char(TYPEOF(sexp2)) << "\n";
        }
        return;
    }
    if (TYPEOF(sexp1) == EXTERNALSXP &&
        rirObjectMagic(sexp1) != rirObjectMagic(sexp2)) {
        differences << prefix << " " << srcPrefix << " rir types differ: "
                    << rirObjectMagic(sexp1) << " vs "
                    << rirObjectMagic(sexp2) << "\n";
        return;
    }

    if (Code::check(sexp1)) {
        auto poolPrefix = std::string(prefix) + " " + srcPrefix;

        Code::debugCompare(
            Code::unpack(sexp1),
            Code::unpack(sexp2),
            poolPrefix.c_str(),
            differences,
            compareExtraPoolRBytecodes
        );
    } else if (TYPEOF(sexp1) == RAWSXP) {
        auto raw1 = RAW(sexp1);
        auto raw2 = RAW(sexp2);
        auto len1 = XLENGTH(sexp1);
        auto len2 = XLENGTH(sexp2);
        if (len1 != len2) {
            differences << prefix << " " << srcPrefix << " raw lengths differ: "
                        << len1 << " vs " << len2 << "\n";
        }
        if (memcmp(raw1, raw2, len1) != 0) {
            differences << prefix << " " << srcPrefix << " raws differ\n";
        }
    } else if (isProbablyDirectlyComparable[TYPEOF(sexp1)]) {
        compareAsts(sexp1, sexp2, prefix, srcPrefix, differences);
    }
}

static void compareSrcs(unsigned src1, unsigned src2,
                        const char* prefix, const char* srcPrefix,
                        std::stringstream& differences) {
    compareAsts(src_pool_at(src1), src_pool_at(src2), prefix,
                srcPrefix, differences);
}

void Code::debugCompare(const Code* c1, const Code* c2, const char* prefix,
                        std::stringstream& differences, bool compareExtraPoolRBytecodes) {
    compareSrcs(c1->src, c2->src, prefix, "src", differences);
    compareAsts(c1->trivialExpr, c2->trivialExpr, prefix, "trivialExpr", differences);
    if (c1->srcLength != c2->srcLength) {
        differences << prefix << " srcLengths differ: " << c1->srcLength
                    << " vs " << c2->srcLength << "\n";
    }
    if (c1->codeSize != c2->codeSize) {
        differences << prefix << " codeSizes differ: " << c1->codeSize << " vs "
                    << c2->codeSize << "\n";
    }
    if (c1->stackLength != c2->stackLength) {
        differences << prefix << " stackLengths differ: " << c1->stackLength
                    << " vs " << c2->stackLength << "\n";
    }
    // c1 may have extra pool R-bytecodes than c2,
    // if it was from a closure with them and c2 was from an AST-only closure
    if (compareExtraPoolRBytecodes ?
        c1->extraPoolSize != c2->extraPoolSize :
        c1->extraPoolSize < c2->extraPoolSize) {
        differences << prefix << " extraPoolSizes differ: " << c1->extraPoolSize
                    << " vs " << c2->extraPoolSize << "\n";
    }
    if (c1->bindingCacheSize != c2->bindingCacheSize) {
        differences << prefix << " bindingCacheSizes differ: "
                    << c1->bindingCacheSize << " vs " << c2->bindingCacheSize
                    << "\n";
    }
    for (unsigned i = 0; i < std::min(c1->srcLength, c2->srcLength); i++) {
        auto src1 = c1->srclist()[i];
        auto src2 = c2->srclist()[i];
        if (src1.pcOffset != src2.pcOffset) {
            differences << prefix << " src " << i << " pcOffsets differ: "
                        << src1.pcOffset << " vs " << src2.pcOffset << "\n";
        }
        char srcPrefix[100];
        sprintf(srcPrefix, "src %u", i);
        compareSrcs(src1.srcIdx, src2.srcIdx, prefix,
                    srcPrefix, differences);
    }
    BC::debugCompare(c1->code(), c2->code(), c1->codeSize, c2->codeSize, c1, c2,
                     prefix, differences);
    for (unsigned i = 0; i < std::min(c1->extraPoolSize, c2->extraPoolSize); i++) {
        auto pool1 = c1->getExtraPoolEntry(i);
        auto pool2 = c2->getExtraPoolEntry(i);

        char poolPrefix[100];
        sprintf(poolPrefix, "entry %u", i);
        compareSexps(pool1, pool2, prefix, poolPrefix, differences, compareExtraPoolRBytecodes);
    }
}

unsigned Code::addExtraPoolEntry(SEXP v) {
    SEXP cur = getEntry(0);
    unsigned curLen = cur == R_NilValue ? 0 : (unsigned)LENGTH(cur);
    if (curLen == extraPoolSize) {
        unsigned newCapacity = curLen ? curLen * 2 : 2;
        PROTECT(v);
        SEXP newPool = PROTECT(Rf_allocVector(VECSXP, newCapacity));
        for (unsigned i = 0; i < curLen; ++i) {
            SET_VECTOR_ELT(newPool, i, VECTOR_ELT(cur, i));
        }
        setEntry(0, newPool);
        UNPROTECT(2);
        cur = newPool;
    }
    SET_VECTOR_ELT(cur, extraPoolSize, v);
    return extraPoolSize++;
}

llvm::ExitOnError ExitOnErr;

NativeCode Code::lazyCompile() {
    assert(kind == Kind::Native);
    assert(*lazyCodeHandle != '\0');
    auto symbol = ExitOnErr(pir::PirJitLLVM::JIT->lookup(lazyCodeHandle));
    nativeCode_ = (NativeCode)symbol.getAddress();
    return nativeCode_;
}

} // namespace rir
