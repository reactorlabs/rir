#include "Code.h"
#include "Function.h"
#include "R/Printing.h"
#include "R/Serialize.h"
#include "bc/BC.h"
#include "compiler/native/pir_jit_llvm.h"
#include "utils/Pool.h"
#include "utils/UUIDPool.h"

#include <llvm/ExecutionEngine/JITSymbol.h>
#include <llvm/Support/Errno.h>

#include <iomanip>
#include <sstream>

namespace rir {

// cppcheck-suppress uninitMemberVar; symbol=data
Code::Code(Kind kind, FunctionSEXP fun, SEXP src, unsigned srcIdx, unsigned cs,
           unsigned sourceLength, size_t localsCnt, size_t bindingsCnt)
    : RirRuntimeObject(
          // GC area starts just after the header
          (intptr_t)&locals_ - (intptr_t)this,
          // GC area has only 1 pointer
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

Code::~Code() {
    // TODO: Not sure if this is actually called
    // Otherwise the pointer will leak a few bytes
}

void Code::function(Function* fun) { setEntry(3, fun->container()); }

rir::Function* Code::function() const {
    auto f = getEntry(3);
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

Code* Code::deserialize(Function* rirFunction, SEXP refTable, R_inpstream_t inp) {
    Protect p;
    size_t size = InInteger(inp);
    SEXP store = p(Rf_allocVector(EXTERNALSXP, size));
    Code* code = new (DATAPTR(store)) Code;
    AddReadRef(refTable, store);
    code->nativeCode_ = nullptr; // not serialized for now
    code->src = InInteger(inp);
    bool hasTr = InInteger(inp);
    if (hasTr)
        code->trivialExpr = UUIDPool::readItem(refTable, inp);
    code->stackLength = InInteger(inp);
    *const_cast<unsigned*>(&code->localsCount) = InInteger(inp);
    *const_cast<unsigned*>(&code->bindingCacheSize) = InInteger(inp);
    code->codeSize = InInteger(inp);
    code->srcLength = InInteger(inp);
    code->extraPoolSize = InInteger(inp);
    SEXP extraPool = p(UUIDPool::readItem(refTable, inp));
    auto hasArgReorder = InInteger(inp);
    SEXP argReorder = nullptr;
    if (hasArgReorder) {
        argReorder = p(UUIDPool::readItem(refTable, inp));
    }
    if (!rirFunction) {
        // Have to readItem so we read a cyclic reference if necessary
        rirFunction = Function::unpack(UUIDPool::readItem(refTable, inp));
        p(rirFunction->container());
    }

    // Bytecode
    BC::deserialize(refTable, inp, code->code(), code->codeSize, code);

    // Srclist
    for (unsigned i = 0; i < code->srcLength; i++) {
        code->srclist()[i].pcOffset = InInteger(inp);
        // TODO: Intern
        code->srclist()[i].srcIdx = src_pool_read_item(refTable, inp);
    }
    code->info = {// GC area starts just after the header
                  (uint32_t)((intptr_t)&code->locals_ - (intptr_t)code),
                  // GC area has only 1 pointer
                  NumLocals, CODE_MAGIC};
    code->setEntry(0, extraPool);
    code->function(rirFunction);
    if (hasArgReorder) {
        code->setEntry(2, argReorder);
    }

    return code;
}

void Code::serialize(bool includeFunction, SEXP refTable, R_outpstream_t out) const {
    OutInteger(out, size());
    HashAdd(container(), refTable);
    // Header
    OutInteger(out, src);
    OutInteger(out, trivialExpr != nullptr);
    if (trivialExpr)
        UUIDPool::writeItem(trivialExpr, refTable, out);
    OutInteger(out, stackLength);
    OutInteger(out, localsCount);
    OutInteger(out, bindingCacheSize);
    OutInteger(out, codeSize);
    OutInteger(out, srcLength);
    OutInteger(out, extraPoolSize);
    UUIDPool::writeItem(getEntry(0), refTable, out);
    OutInteger(out, getEntry(2) != nullptr);
    if (getEntry(2))
        UUIDPool::writeItem(getEntry(2), refTable, out);
    if (includeFunction) {
        // Have to writeItem so we write a reference if necessary
        UUIDPool::writeItem(function()->container(), refTable, out);
    }

    // Bytecode
    BC::serialize(refTable, out, code(), codeSize, this);

    // Srclist
    for (unsigned i = 0; i < srcLength; i++) {
        OutInteger(out, srclist()[i].pcOffset);
        src_pool_write_item(srclist()[i].srcIdx, refTable, out);
    }
}

void Code::disassemble(std::ostream& out, const std::string& prefix) const {
    if (auto map = pirTypeFeedback()) {
        map->forEachSlot(
            [&](size_t i, const PirTypeFeedback::MDEntry& mdEntry) {
                auto feedback = mdEntry.feedback;
                out << " - slot #" << i << ": " << mdEntry.offset << " : [";
                feedback.print(out);
                out << "] (" << mdEntry.sampleCount << " records - "
                    << (mdEntry.readyForReopt ? "ready" : "not ready")
                    << ") prev: " << mdEntry.previousType << "\n";
            });
    }

    switch (kind) {
    case Kind::Bytecode: {
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
            out << "nativeCode " << (void*)nativeCode_ << "\n";
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

void Code::print(std::ostream& out) const {
    out << "Code object\n";
    out << std::left << std::setw(20) << "   Source: " << src
        << " (index into src pool)\n";
    out << std::left << std::setw(20) << "   Magic: " << std::hex << info.magic
        << std::dec << " (hex)\n";
    out << std::left << std::setw(20) << "   Stack (o): " << stackLength
        << "\n";
    out << std::left << std::setw(20) << "   Code size: " << codeSize
        << "[B]\n";

    if (info.magic != CODE_MAGIC) {
        out << "Wrong magic number -- corrupted IR bytecode";
        Rf_error("Wrong magic number -- corrupted IR bytecode");
    }

    out << "\n";
    disassemble(out);
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
    assert(*lazyCodeHandle_ != '\0');
    auto symbol = ExitOnErr(pir::PirJitLLVM::JIT->lookup(lazyCodeHandle_));
    nativeCode_ = (NativeCode)symbol.getAddress();
    return nativeCode_;
}

} // namespace rir
