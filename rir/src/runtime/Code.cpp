#include "Code.h"
#include "Function.h"
#include "R/Printing.h"
#include "R/Serialize.h"
#include "compiler/native/pir_jit_llvm.h"
#include "ir/BC.h"
#include "utils/Pool.h"

#include <llvm/ExecutionEngine/JITSymbol.h>
#include <llvm/Support/Errno.h>

#include <iomanip>
#include <sstream>

#include "utils/UMap.h"
#include "runtime/DispatchTable.h"

namespace rir {

// cppcheck-suppress uninitMemberVar; symbol=data
Code::Code(FunctionSEXP fun, SEXP src, unsigned srcIdx, unsigned cs,
           unsigned sourceLength, size_t localsCnt, size_t bindingsCnt)
    : RirRuntimeObject(
          // GC area starts just after the header
          (intptr_t)&locals_ - (intptr_t)this,
          // GC area has only 1 pointer
          NumLocals),
      nativeCode_(nullptr), src(srcIdx), trivialExpr(nullptr), stackLength(0),
      localsCount(localsCnt), bindingCacheSize(bindingsCnt), codeSize(cs),
      srcLength(sourceLength), extraPoolSize(0) {
    setEntry(0, R_NilValue);
    if (src && TYPEOF(src) == SYMSXP)
        trivialExpr = src;
    assert(!fun || rir::Function::check(fun));
    if (fun)
        setEntry(3, fun);
}

Code* Code::New(SEXP ast, size_t codeSize, size_t sources, size_t locals,
                size_t bindingCache) {
    auto src = src_pool_add(globalContext(), ast);
    return New(src, codeSize, sources, locals, bindingCache);
}

Code* Code::New(Immediate ast, size_t codeSize, size_t sources, size_t locals,
                size_t bindingCache) {
    unsigned totalSize = Code::size(codeSize, sources);
    SEXP store = Rf_allocVector(EXTERNALSXP, totalSize);
    void* payload = DATAPTR(store);
    return new (payload) Code(nullptr, src_pool_at(globalContext(), ast), ast,
                              codeSize, sources, locals, bindingCache);
}

Code* Code::New(Immediate ast) { return New(ast, 0, 0, 0, 0); }

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

Code* Code::deserialize(SEXP refTable, R_inpstream_t inp) {
    size_t size = InInteger(inp);
    SEXP store = Rf_allocVector(EXTERNALSXP, size);
    PROTECT(store);
    Code* code = new (DATAPTR(store)) Code;
    code->nativeCode_ = nullptr; // not serialized for now
    code->src = InInteger(inp);
    bool hasTr = InInteger(inp);
    if (hasTr)
        code->trivialExpr = ReadItem(refTable, inp);
    code->stackLength = InInteger(inp);
    *const_cast<unsigned*>(&code->localsCount) = InInteger(inp);
    *const_cast<unsigned*>(&code->bindingCacheSize) = InInteger(inp);
    code->codeSize = InInteger(inp);
    code->srcLength = InInteger(inp);
    code->extraPoolSize = InInteger(inp);
    SEXP extraPool = ReadItem(refTable, inp);
    PROTECT(extraPool);
    auto hasArgReorder = InInteger(inp);
    SEXP argReorder = nullptr;
    if (hasArgReorder) {
        argReorder = ReadItem(refTable, inp);
        PROTECT(argReorder);
    }
    SEXP rirFunction = ReadItem(refTable, inp);
    PROTECT(rirFunction);

    // Bytecode
    BC::deserialize(refTable, inp, code->code(), code->codeSize, code);

    // Srclist
    for (unsigned i = 0; i < code->srcLength; i++) {
        code->srclist()[i].pcOffset = InInteger(inp);
        code->srclist()[i].srcIdx =
            src_pool_add(globalContext(), ReadItem(refTable, inp));
    }
    code->info = {// GC area starts just after the header
                  (uint32_t)((intptr_t)&code->locals_ - (intptr_t)code),
                  // GC area has only 1 pointer
                  NumLocals, CODE_MAGIC};
    code->setEntry(0, extraPool);
    code->setEntry(3, rirFunction);
    if (hasArgReorder) {
        code->setEntry(2, argReorder);
        UNPROTECT(1);
    }
    UNPROTECT(3);

    return code;
}

void Code::serialize(SEXP refTable, R_outpstream_t out) const {
    OutInteger(out, size());
    // Header
    OutInteger(out, src);
    OutInteger(out, trivialExpr != nullptr);
    if (trivialExpr)
        WriteItem(trivialExpr, refTable, out);
    OutInteger(out, stackLength);
    OutInteger(out, localsCount);
    OutInteger(out, bindingCacheSize);
    OutInteger(out, codeSize);
    OutInteger(out, srcLength);
    OutInteger(out, extraPoolSize);
    WriteItem(getEntry(0), refTable, out);
    OutInteger(out, getEntry(2) != nullptr);
    if (getEntry(2))
        WriteItem(getEntry(2), refTable, out);
    WriteItem(getEntry(3), refTable, out);

    // Bytecode
    BC::serialize(refTable, out, code(), codeSize, this);

    // Srclist
    for (unsigned i = 0; i < srcLength; i++) {
        OutInteger(out, srclist()[i].pcOffset);
        WriteItem(src_pool_at(globalContext(), srclist()[i].srcIdx), refTable,
                  out);
    }
}

Code * Code::getSrcAtOffset(bool mainSrc, int & index, int reqOffset) {

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

    pc = code();
    std::vector<BC::FunIdx> promises;

    Protect p;
    index++;


    if (index == reqOffset) return this;

    while (pc < endCode()) {
        BC bc = BC::decode(pc, this);
        bc.addMyPromArgsTo(promises);

        if (bc.bc == Opcode::push_ && TYPEOF(bc.immediateConst()) == EXTERNALSXP) {
            SEXP iConst = bc.immediateConst();
            if (DispatchTable::check(iConst)) {
                Code * res = DispatchTable::unpack(iConst)->baseline()->body()->getSrcAtOffset(false, index, reqOffset);
                if (res != nullptr) return res;
            }
        }
        pc = BC::next(pc);
    }


    for (auto i : promises) {
        auto c = getPromise(i);
        Code * res = c->getSrcAtOffset(false, index, reqOffset);
        if (res != nullptr) return res;
    }


    if (mainSrc) {
        rir::Function* func = function();
        if (func) {
            auto nargs = func->nargs();
            for (unsigned i = 0; i < nargs; i++) {
                auto code = func->defaultArg(i);
                if (code != nullptr) {
                    Code * res = code->getSrcAtOffset(false, index, reqOffset);
                    if (res != nullptr) return res;
                }
            }
        }
    }
    return nullptr;
}

unsigned Code::getSrcIdxAtOffset(bool mainSrc, int & index, int reqOffset) {

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

    pc = code();
    std::vector<BC::FunIdx> promises;

    Protect p;
    index++;


    if (index == reqOffset) return this->src;

    while (pc < endCode()) {
        BC bc = BC::decode(pc, this);
        bc.addMyPromArgsTo(promises);

        if (bc.bc == Opcode::push_ && TYPEOF(bc.immediateConst()) == EXTERNALSXP) {
            SEXP iConst = bc.immediateConst();
            if (DispatchTable::check(iConst)) {
                unsigned res = DispatchTable::unpack(iConst)->baseline()->body()->getSrcIdxAtOffset(false, index, reqOffset);
                if (res != 0) return res;
            }
        }
        pc = BC::next(pc);
    }


    for (auto i : promises) {
        auto c = getPromise(i);
        unsigned res = c->getSrcIdxAtOffset(false, index, reqOffset);
        if (res != 0) return res;
    }


    if (mainSrc) {
        rir::Function* func = function();
        if (func) {
            auto nargs = func->nargs();
            for (unsigned i = 0; i < nargs; i++) {
                auto code = func->defaultArg(i);
                if (code != nullptr) {
                    unsigned res = code->getSrcIdxAtOffset(false, index, reqOffset);
                    if (res != 0) return res;
                }
            }
        }
    }
    return 0;
}

SEXP Code::getTabAtOffset(bool mainSrc, int & index, int reqOffset) {

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

    pc = code();
    std::vector<BC::FunIdx> promises;

    Protect p;
    index++;


    if (index == reqOffset) return R_TrueValue;

    while (pc < endCode()) {
        BC bc = BC::decode(pc, this);
        bc.addMyPromArgsTo(promises);

        if (bc.bc == Opcode::push_ && TYPEOF(bc.immediateConst()) == EXTERNALSXP) {
            SEXP iConst = bc.immediateConst();
            if (DispatchTable::check(iConst)) {
                SEXP res = DispatchTable::unpack(iConst)->baseline()->body()->getTabAtOffset(false, index, reqOffset);
                if (res == R_TrueValue) {
                    return iConst;
                } else if (res) {
                    return res;
                }
            }
        }

        pc = BC::next(pc);
    }


    for (auto i : promises) {
        auto c = getPromise(i);
        SEXP res = c->getTabAtOffset(false, index, reqOffset);
        if (res != nullptr) return res;
    }


    if (mainSrc) {
        rir::Function* func = function();
        if (func) {
            auto nargs = func->nargs();
            for (unsigned i = 0; i < nargs; i++) {
                auto code = func->defaultArg(i);
                if (code != nullptr) {
                    SEXP res = code->getTabAtOffset(false, index, reqOffset);
                    if (res != nullptr) return res;
                }
            }
        }
    }
    return nullptr;
}


void Code::printSource(bool mainSrc, int & index) {
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

    pc = code();
    std::vector<BC::FunIdx> promises;

    Protect p;
    index++;

    std::cout << "(" << index << "," << src << "), ";

    while (pc < endCode()) {
        BC bc = BC::decode(pc, this);
        bc.addMyPromArgsTo(promises);

        if (bc.bc == Opcode::push_ && TYPEOF(bc.immediateConst()) == EXTERNALSXP) {
            SEXP iConst = bc.immediateConst();
            if (DispatchTable::check(iConst)) {
                DispatchTable::unpack(iConst)->baseline()->body()->printSource(false, index);
            }
        }

        pc = BC::next(pc);
    }


    for (auto i : promises) {
        auto c = getPromise(i);
        c->printSource(false, index);
    }

    if (mainSrc) {
        rir::Function* func = function();
        if (func) {
            auto nargs = func->nargs();
            for (unsigned i = 0; i < nargs; i++) {
                auto code = func->defaultArg(i);
                if (code != nullptr) {
                    code->printSource(false, index);
                }
            }
        }
        std::cout << std::endl;
    }
}

void Code::populateSrcIdxData() {

    if (hast && hast != R_NilValue) {
        // std::cout << "updating src: " << container() << ", " << CHAR(PRINTNAME(hast)) << ", " << offsetIndex << std::endl;
        SEXP lMap = Pool::get(HAST_VTAB_MAP);
        auto resolvedContainer = Rf_findVarInFrame(lMap, hast);
        if (!DispatchTable::check(resolvedContainer)) {
            Rf_error("Deserializer: Invalid vtable container while updating src idx");
        }
        DispatchTable * vv = DispatchTable::unpack(resolvedContainer);

        int idx = 0;
        unsigned calc = vv->baseline()->body()->getSrcIdxAtOffset(true, idx, offsetIndex);

        src = calc;
        SEXP astAtSrc = src_pool_at(globalContext(), src);
        if (astAtSrc && TYPEOF(astAtSrc) == SYMSXP) {
            trivialExpr = astAtSrc;
        }
        hast = R_NilValue;
        offsetIndex = 0;
    }

    for (unsigned int i = 0; i < extraPoolSize; i++) {
        SEXP ele = VECTOR_ELT(getEntry(0), i);
        if (TYPEOF(ele) == EXTERNALSXP && Code::check(ele)) {
            Code::unpack(ele)->populateSrcIdxData();
        }
    }
}

void Code::populateSrcData(SEXP parentHast, SEXP map, bool mainSrc, int & index) {
    // Opcode* pc = code();
    // size_t label = 0;
    // std::map<Opcode*, size_t> targets;
    // targets[pc] = label++;
    // while (pc < endCode()) {
    //     if (BC::decodeShallow(pc).isJmp()) {
    //         auto t = BC::jmpTarget(pc);
    //         if (!targets.count(t))
    //             targets[t] = label++;
    //     }
    //     pc = BC::next(pc);
    // }

    // // sort labels ascending
    // label = 0;
    // for (auto& t : targets)
    //     t.second = label++;

    // pc = code();
    // std::vector<BC::FunIdx> promises;

    // Protect p;
    // index++;

    // if (mainSrc) {
    //     #if PRINT_SRC_HAST_MAP_UPDATES == 1
    //     std::cout << "hast(" << CHAR(PRINTNAME(parentHast)) << ", " << src << "): [ ";
    //     #endif
    // } else {
    //     #if PRINT_SRC_HAST_MAP_UPDATES == 1
    //     std::cout << "(" << index << ", " << src << ") ";
    //     #endif
    // }

    // SEXP srcSym = Rf_install(std::to_string(src).c_str());
    // SEXP hastSym = parentHast;
    // SEXP indexSym = Rf_install(std::to_string(index).c_str());
    // SEXP resVec;
    // p(resVec = Rf_allocVector(VECSXP, 2));
    // SET_VECTOR_ELT(resVec, 0, hastSym);
    // SET_VECTOR_ELT(resVec, 1, indexSym);
    // Rf_defineVar(srcSym, resVec, map);

    // while (pc < endCode()) {
    //     BC bc = BC::decode(pc, this);
    //     bc.addMyPromArgsTo(promises);

    //     if (bc.bc == Opcode::push_ && TYPEOF(bc.immediateConst()) == EXTERNALSXP) {
    //         SEXP iConst = bc.immediateConst();
    //         if (DispatchTable::check(iConst)) {
    //             DispatchTable::unpack(iConst)->baseline()->body()->populateSrcData(parentHast, map, false, index);
    //         }
    //     }

    //     pc = BC::next(pc);
    // }


    // for (auto i : promises) {
    //     auto c = getPromise(i);
    //     c->populateSrcData(parentHast, map, false, index);
    // }
    // if (mainSrc) {
    //     rir::Function* func = function();
    //     if (func) {
    //         auto nargs = func->nargs();
    //         #if PRINT_SRC_HAST_MAP_UPDATES == 1
    //         std::cout << "(" << nargs << "): { ";
    //         #endif
    //         for (unsigned i = 0; i < nargs; i++) {
    //             auto code = func->defaultArg(i);
    //             if (code != nullptr) {
    //                 code->populateSrcData(parentHast, map, false, index);
    //             }
    //         }
    //         #if PRINT_SRC_HAST_MAP_UPDATES == 1
    //         std::cout << "} ";
    //         #endif
    //     }
    // }

    // #if PRINT_SRC_HAST_MAP_UPDATES == 1
    // if (mainSrc) {
    //     std::cout << "]" << std::endl;
    // }
    // #endif
}

void Code::disassemble(std::ostream& out, const std::string& prefix) const {
    if (auto map = pirTypeFeedback()) {
        map->forEachSlot([&](size_t i,
                             const PirTypeFeedback::MDEntry& mdEntry) {
            auto feedback = mdEntry.feedback;
            out << " - slot #" << i << ": " << mdEntry.offset << " : [";
            feedback.print(out);
            out << "] (" << mdEntry.sampleCount << " records - "
                << (mdEntry.readyForReopt ? "ready" : "not ready")
                << ") prev: " << mdEntry.previousType << "\n";
        });
    }

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
            out << "   ; " << Print::dumpSexp(src_pool_at(globalContext(), s))
                << "\n"
                << std::setw(OFFSET_WIDTH) << "";

        // Print call ast
        switch (bc.bc) {
        case Opcode::call_:
        case Opcode::named_call_:
            out << "   ; "
                << Print::dumpSexp(Pool::get(bc.immediate.callFixedArgs.ast))
                << "\n"
                << std::setw(OFFSET_WIDTH) << "";
            break;
        default: {}
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

    if (nativeCode_) {
        out << "nativeCode " << (void*)nativeCode_ << "\n";
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

    for (auto i : promises) {
        auto c = getPromise(i);
        out << "\n[Prom (index " << prefix << i << ")]\n";
        std::stringstream ss;
        ss << prefix << i << ".";
        c->disassemble(out, ss.str());
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
    auto symbol = ExitOnErr(pir::PirJitLLVM::JIT->lookup(lazyCodeHandle_));
    nativeCode_ = (NativeCode)symbol.getAddress();
    return nativeCode_;
}

} // namespace rir
