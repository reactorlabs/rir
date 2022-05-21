#include "BitcodeLinkUtility.h"

#include "R/Preserve.h"
#include "R/Protect.h"
#include "R/r.h"
#include "runtime/DispatchTable.h"
#include "utils/FunctionWriter.h"
#include "utils/Pool.h"
#include "utils/UMap.h"

#include <unordered_map>
#include <iostream>
#include <functional>
#include <cassert>

#include "runtimePatches.h"
#include "R/Printing.h"
#include "api.h"

#include "compiler/pir/module.h"
#include "compiler/log/stream_logger.h"
#include "compiler/compiler.h"
#include "compiler/backend.h"

#include <chrono>
using namespace std::chrono;


#define GROWTHRATE 5
#define PRINT_LINKING_STATUS 0
#define PRINT_WORKLIST_ENTRIES 0
#define DEBUG_BLACKLIST 0
#define PRINT_HAST_SRC_ENTRIES 0


namespace rir {

SEXP BitcodeLinkUtil::getHast(SEXP body, SEXP env) {
    std::stringstream qHast;
    SEXP x = env;
    if (x == R_GlobalEnv) {
        qHast << "GE:";
    } else if (x == R_BaseEnv) {
        qHast << "BE:";
    } else if (x == R_EmptyEnv) {
        qHast << "EE:";
    } else if (R_IsPackageEnv(x)) {
        qHast << "PE:" << Rf_translateChar(STRING_ELT(R_PackageEnvName(x), 0)) << ":";
    } else if (R_IsNamespaceEnv(x)) {
        qHast << "NS:" << Rf_translateChar(STRING_ELT(R_NamespaceEnvSpec(x), 0)) << ":";
    } else {
        return R_NilValue;
        qHast << "AE:";
    }
    size_t hast = 0;
    hash_ast(body, hast);
    qHast << hast;
    SEXP calcHast = Rf_install(qHast.str().c_str());
    return calcHast;
}


struct TraversalResult {
    Code * code;
    DispatchTable * vtable;
    unsigned srcIdx;
};

static inline TraversalResult getResultAtOffset(DispatchTable * vtab, const unsigned & requiredOffset) {
    bool done = false;
    unsigned indexOffset = 0;

    DispatchTable * currVtab = vtab;
    Code * currCode = nullptr;
    unsigned poolIdx = currVtab->baseline()->body()->src;

    std::function<void(Code *, Function *)> iterateOverCodeObjs = [&] (Code * c, Function * funn) {
        if (done) return;
        if (indexOffset == requiredOffset) {
            // required thing was a code obj or a dispatch table obj
            poolIdx = c->src;
            currCode = c;
            done = true;
            return;
        }
        indexOffset++;

        // Default args
        if (funn) {
            auto nargs = funn->nargs();
            for (unsigned i = 0; i < nargs; i++) {
                auto code = funn->defaultArg(i);
                if (code != nullptr) {
                    iterateOverCodeObjs(code, nullptr);
                    if (done) return;
                }
            }
        }

        Opcode* pc = c->code();
        std::vector<BC::FunIdx> promises;
        Protect p;
        while (pc < c->endCode()) {
            if (done) return;
            BC bc = BC::decode(pc, c);
            bc.addMyPromArgsTo(promises);

            // src code language objects
            unsigned s = c->getSrcIdxAt(pc, true);
            if (s != 0) {
                if (indexOffset == requiredOffset) {
                    // required thing was src pool index
                    poolIdx = s;
                    done = true;
                    return;
                }
                indexOffset++;
            }

            // call sites
            switch (bc.bc) {
                case Opcode::call_:
                case Opcode::named_call_:
                    if (indexOffset == requiredOffset) {
                        // required thing was constant pool index
                        poolIdx = bc.immediate.callFixedArgs.ast;
                        done = true;
                        return;
                    }
                    indexOffset++;
                    break;
                case Opcode::call_dots_:
                    if (indexOffset == requiredOffset) {
                        // required thing was constant pool index
                        poolIdx = bc.immediate.callFixedArgs.ast;
                        done = true;
                        return;
                    }
                    indexOffset++;
                    break;
                case Opcode::call_builtin_:
                    if (indexOffset == requiredOffset) {
                        // required thing was constant pool index
                        poolIdx = bc.immediate.callBuiltinFixedArgs.ast;
                        done = true;
                        return;
                    }
                    indexOffset++;
                    break;
                default: {}
            }

            // inner functions
            if (bc.bc == Opcode::push_ && TYPEOF(bc.immediateConst()) == EXTERNALSXP) {
                SEXP iConst = bc.immediateConst();
                if (DispatchTable::check(iConst)) {
                    currVtab = DispatchTable::unpack(iConst);
                    auto c = currVtab->baseline()->body();
                    auto f = c->function();
                    iterateOverCodeObjs(c, f);
                    if (done) return;
                }
            }

            pc = BC::next(pc);
        }

        // Iterate over promises code objects recursively
        for (auto i : promises) {
            auto prom = c->getPromise(i);
            iterateOverCodeObjs(prom, nullptr);
            if (done) return;
        }
    };

    Code * genesisCodeObj = currVtab->baseline()->body();
    Function * genesisFunObj = genesisCodeObj->function();

    iterateOverCodeObjs(genesisCodeObj, genesisFunObj);

    TraversalResult r;
    r.code = currCode;
    r.vtable = currVtab;
    r.srcIdx = poolIdx;
    return r;
}

Code * BitcodeLinkUtil::getCodeObjectAtOffset(SEXP hastSym, int requiredOffset) {
    REnvHandler vtabMap(HAST_VTAB_MAP);
    SEXP vtabContainer = vtabMap.get(hastSym);
    if (!vtabContainer) {
        Rf_error("getCodeObjectAtOffset failed!");
    }
    if (!DispatchTable::check(vtabContainer)) {
        Rf_error("getCodeObjectAtOffset vtable corrupted");
    }
    auto vtab = DispatchTable::unpack(vtabContainer);
    auto r = getResultAtOffset(vtab, requiredOffset);

    return r.code;
}

unsigned BitcodeLinkUtil::getSrcPoolIndexAtOffset(SEXP hastSym, int requiredOffset) {
    REnvHandler vtabMap(HAST_VTAB_MAP);
    SEXP vtabContainer = vtabMap.get(hastSym);
    if (!vtabContainer) {
        Rf_error("getSrcPoolIndexAtOffset failed!");
    }
    if (!DispatchTable::check(vtabContainer)) {
        Rf_error("getSrcPoolIndexAtOffset vtable corrupted");
    }
    auto vtab = DispatchTable::unpack(vtabContainer);
    auto r = getResultAtOffset(vtab, requiredOffset);

    return r.srcIdx;
}

SEXP BitcodeLinkUtil::getVtableContainerAtOffset(SEXP hastSym, int requiredOffset) {
    REnvHandler vtabMap(HAST_VTAB_MAP);
    SEXP vtabContainer = vtabMap.get(hastSym);
    if (!vtabContainer) {
        Rf_error("getVtableContainerAtOffset failed!");
    }
    if (!DispatchTable::check(vtabContainer)) {
        Rf_error("getVtableContainerAtOffset vtable corrupted");
    }
    auto vtab = DispatchTable::unpack(vtabContainer);
    auto r = getResultAtOffset(vtab, requiredOffset);
    return r.vtable->container();
}

DispatchTable * BitcodeLinkUtil::getVtableAtOffset(DispatchTable * vtab, int requiredOffset) {
    auto res = getResultAtOffset(vtab, requiredOffset);
    return res.vtable;
}

void BitcodeLinkUtil::populateHastSrcData(DispatchTable* vtable, SEXP parentHast) {
    REnvHandler srcHastMap(SRC_HAST_MAP);
    DispatchTable * currVtab = vtable;
    unsigned indexOffset = 0;

    auto addSrcToMap = [&] (const unsigned & src, bool sourcePool = true) {
        // create a mapping for each src [representing code object to its hast and offset index]
        SEXP srcSym;
        if (sourcePool) {
            srcSym = Rf_install(std::to_string(src).c_str());
        } else {
            srcSym = Rf_install((std::to_string(src) + "_cp").c_str());
        }
        SEXP indexSym = Rf_install(std::to_string(indexOffset).c_str());

        if (!srcHastMap.get(srcSym)) {
            SEXP resVec;
            PROTECT(resVec = Rf_allocVector(VECSXP, 2));
            SET_VECTOR_ELT(resVec, 0, parentHast);
            SET_VECTOR_ELT(resVec, 1, indexSym);
            srcHastMap.set(srcSym, resVec);
            UNPROTECT(1);
        }
        // else {
        //     std::cout << "More than one entry for the same src" << std::endl;
        // }

    };

    std::function<void(Code *, Function *)> iterateOverCodeObjs = [&] (Code * c, Function * funn) {
        #if PRINT_HAST_SRC_ENTRIES == 1
        std::cout << "src_hast_entry[C] at indexOffset: " << indexOffset << "," << c->src << std::endl;
        #endif

        addSrcToMap(c->src);
        indexOffset++;
        if (funn) {
            auto nargs = funn->nargs();
            for (unsigned i = 0; i < nargs; i++) {
                auto code = funn->defaultArg(i);
                if (code != nullptr) {
                    iterateOverCodeObjs(code, nullptr);
                }
            }
        }

        Opcode* pc = c->code();
        std::vector<BC::FunIdx> promises;
        Protect p;
        while (pc < c->endCode()) {
            BC bc = BC::decode(pc, c);
            bc.addMyPromArgsTo(promises);

            // src code language objects
            unsigned s = c->getSrcIdxAt(pc, true);
            if (s != 0) {
                addSrcToMap(s);
                #if PRINT_HAST_SRC_ENTRIES == 1
                std::cout << "src_hast_entry[source_pool] at indexOffset: " << indexOffset << "," << s << std::endl;
                #endif
                indexOffset++;
            }

            // call sites
            switch (bc.bc) {
                case Opcode::call_:
                case Opcode::named_call_:
                    #if PRINT_HAST_SRC_ENTRIES == 1
                    std::cout << "src_hast_entry[constant_pool] at indexOffset: " << indexOffset << "," << bc.immediate.callFixedArgs.ast << std::endl;
                    #endif
                    addSrcToMap(bc.immediate.callFixedArgs.ast, false);
                    indexOffset++;
                    break;
                case Opcode::call_dots_:
                    #if PRINT_HAST_SRC_ENTRIES == 1
                    std::cout << "src_hast_entry[constant_pool] at indexOffset: " << indexOffset << "," << bc.immediate.callFixedArgs.ast << std::endl;
                    #endif
                    addSrcToMap(bc.immediate.callFixedArgs.ast, false);
                    indexOffset++;
                    break;
                case Opcode::call_builtin_:
                    #if PRINT_HAST_SRC_ENTRIES == 1
                    std::cout << "src_hast_entry[constant_pool] at indexOffset: " << indexOffset << "," << bc.immediate.callBuiltinFixedArgs.ast << std::endl;
                    #endif
                    addSrcToMap(bc.immediate.callBuiltinFixedArgs.ast, false);
                    indexOffset++;
                    break;
                default: {}
            }

            // inner functions
            if (bc.bc == Opcode::push_ && TYPEOF(bc.immediateConst()) == EXTERNALSXP) {
                SEXP iConst = bc.immediateConst();
                if (DispatchTable::check(iConst)) {
                    currVtab = DispatchTable::unpack(iConst);
                    auto c = currVtab->baseline()->body();
                    auto f = c->function();
                    iterateOverCodeObjs(c, f);
                }
            }

            pc = BC::next(pc);
        }

        // Iterate over promises code objects recursively
        for (auto i : promises) {
            auto prom = c->getPromise(i);
            iterateOverCodeObjs(prom, nullptr);
        }
    };

    Code * genesisCodeObj = currVtab->baseline()->body();
    Function * genesisFunObj = currVtab->baseline()->body()->function();
    iterateOverCodeObjs(genesisCodeObj, genesisFunObj);
}

void BitcodeLinkUtil::printSources(DispatchTable* vtable, SEXP parentHast) {
    // TODO
}

void BitcodeLinkUtil::insertVTable(DispatchTable* vtable, SEXP hastSym) {
    REnvHandler vtabMap(HAST_VTAB_MAP);
    vtabMap.set(hastSym, vtable->container());
}

void BitcodeLinkUtil::insertClosObj(SEXP clos, SEXP hastSym) {
    REnvHandler closMap(HAST_CLOS_MAP);
    closMap.set(hastSym, clos);
}

void BitcodeLinkUtil::insertToBlacklist(SEXP hastSym) {
    REnvHandler blacklistMap(BL_MAP);
    if (!blacklistMap.get(hastSym)) {
        blacklistMap.set(hastSym, R_TrueValue);
        #if DEBUG_BLACKLIST == 1
        std::cout << "(R) Blacklisting: " << hast << " (" << hastSym << ")" << std::endl;
        #endif
        serializerCleanup();
    }
}

bool BitcodeLinkUtil::readyForSerialization(SEXP clos, DispatchTable* vtable, SEXP hastSym) {
    // if the hast already corresponds to other src address and is a different closureObj then
    // there was a collision and we cannot use the function and all functions that depend on it
    REnvHandler vtabMap(HAST_VTAB_MAP);
    if (vtabMap.get(hastSym)) {
        insertToBlacklist(hastSym);
        return false;
    }
    return true;
}

SEXP BitcodeLinkUtil::getOptUnlockMap() {
    SEXP tabMap = Pool::get(OPT_UNLOCK_MAP);
    if (tabMap == R_NilValue) {
        SEXP tmp;
        PROTECT(tmp = R_NewEnv(R_EmptyEnv,0,0));
        Pool::patch(OPT_UNLOCK_MAP, tmp);
        UNPROTECT(1);
        tabMap = Pool::get(OPT_UNLOCK_MAP);
    }
    return tabMap;
}

void BitcodeLinkUtil::addToWorklistTwo(SEXP hastOfReq, unsigned long & con, int & nargs, SEXP unlockMetaSym) {
    std::stringstream ss;
    ss << CHAR(PRINTNAME(hastOfReq)) << "_" << con;
    SEXP optMapKey = Rf_install(ss.str().c_str());
    #if PRINT_LINKING_STATUS == 1
    std::cout << "Add to worklist two: " << ss.str() << std::endl;
    #endif
    REnvHandler optUnlockMap(OPT_UNLOCK_MAP);

    SEXP worklistElement;
    PROTECT(worklistElement = Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(worklistElement, 0, Rf_ScalarInteger(nargs));
    SET_VECTOR_ELT(worklistElement, 1, unlockMetaSym);

    if (SEXP existingWorklist = optUnlockMap.get(optMapKey)) {
        int freeSpaceAt = -1;
        for (int i = 0; i < Rf_length(existingWorklist); i++) {
            auto ele = VECTOR_ELT(existingWorklist, i);
            if (ele == R_NilValue) {
                freeSpaceAt = i;
                break;
            }
        }

        if (freeSpaceAt != -1) {
            SET_VECTOR_ELT(existingWorklist, freeSpaceAt, worklistElement);
        } else {
            // grow vector
            SEXP oldVec = existingWorklist;
            auto oldSize = Rf_length(oldVec);

            SEXP newWorkVec;
            PROTECT(newWorkVec = Rf_allocVector(VECSXP, oldSize + GROWTHRATE));
            memcpy(DATAPTR(newWorkVec), DATAPTR(oldVec), oldSize * sizeof(SEXP));
            SET_VECTOR_ELT(newWorkVec, oldSize, worklistElement);
            optUnlockMap.set(optMapKey, newWorkVec);
            UNPROTECT(1);
        }

    } else {
        SEXP worklist;
        PROTECT(worklist = Rf_allocVector(VECSXP, GROWTHRATE));
        SET_VECTOR_ELT(worklist, 0, worklistElement);
        optUnlockMap.set(optMapKey, worklist);
        UNPROTECT(1);
    }

    UNPROTECT(1);
}

// This worklist get called when a hast is initially compiled.
// If the counter becomes zero, we also link the dependent bitcodes.
void BitcodeLinkUtil::addToWorklistOne(SEXP hastOfReq, SEXP unlockMeta) {
    REnvHandler hastUnlockMap(HAST_UNLOCK_MAP);

    // SEXP existingEntry = Rf_findVarInFrame(unlockMap, hastOfReq);
    if (SEXP worklist = hastUnlockMap.get(hastOfReq)) {
        // worklist already exists for this hast, all to it

        int freeSpaceAt = -1;
        for (int i = 0; i < Rf_length(worklist); i++) {
            auto ele = VECTOR_ELT(worklist, i);
            if (ele == R_NilValue) {
                freeSpaceAt = i;
                break;
            }
        }

        if (freeSpaceAt != -1) {
            SET_VECTOR_ELT(worklist, freeSpaceAt, unlockMeta);
        } else {
            // grow vector
            SEXP oldVec = worklist;
            auto oldSize = Rf_length(oldVec);

            SEXP newWorkVec;
            PROTECT(newWorkVec = Rf_allocVector(VECSXP, oldSize + GROWTHRATE));
            memcpy(DATAPTR(newWorkVec), DATAPTR(oldVec), oldSize * sizeof(SEXP));
            SET_VECTOR_ELT(newWorkVec, oldSize, unlockMeta); // set the first free index to unlock metadata
            hastUnlockMap.set(hastOfReq, newWorkVec); // update the worklist vector
            UNPROTECT(1);
        }

    } else {
        SEXP worklistContainerVector;
        PROTECT(worklistContainerVector = Rf_allocVector(VECSXP, GROWTHRATE)); // create a vector of initial size GROWTHRATE
        SET_VECTOR_ELT(worklistContainerVector, 0, unlockMeta); // set the first free index to unlock metadata
        hastUnlockMap.set(hastOfReq, worklistContainerVector); // update the worklist vector
        UNPROTECT(1);
    }
}

void BitcodeLinkUtil::linkBitcode(SEXP cData, SEXP hSym, SEXP offsetSymbol, DispatchTable * vtab) {
    auto start = high_resolution_clock::now();
    contextData c(cData);
    // if this context was already added due to unexpected compilation at runtime, skip addition, this can result in duplicate LLVM symbols
    for (size_t i = 0; i < vtab->size(); i++) {
        auto entry = vtab->get(i);
        if (entry->context().toI() == c.getContext()) {
            #if PRINT_LINKING_STATUS == 1
            std::cout << "duplicate linkage: " << CHAR(PRINTNAME(hSym)) << "_" << CHAR(PRINTNAME(offsetSymbol)) << "_" << c.getContext() << std::endl;
            #endif
            return;
        }
    }

    pir::Module* m = new pir::Module;
    pir::StreamLogger logger(pir::DebugOptions::DefaultDebugOptions);
    std::stringstream ss;
    ss << CHAR(PRINTNAME(hSym)) << "_" << CHAR(PRINTNAME(offsetSymbol)) << "_" << c.getContext();
    std::string name = ss.str();
    logger.title("Deserializing " + name);
    pir::Compiler cmp(m, logger);
    pir::Backend backend(m, logger, name);
    backend.deserializeAndPopulateBitcode(cData, hSym, offsetSymbol, vtab);
    delete m;
    auto stop = high_resolution_clock::now();
    auto duration = duration_cast<milliseconds>(stop - start);
    linkTime += duration.count();
    logger.title("Deserialized " + name);
}

static inline void doLinking(SEXP unlockMeta, SEXP linkageSymbol) {
    REnvHandler linkageMap(LINKAGE_MAP);
    // Entry 0: counter, when this becomes zero all dependencies are satisfied
    SEXP counterContainer = VECTOR_ELT(unlockMeta, 0);
    int* counter = (int *) DATAPTR(counterContainer);

    // Entry 1: linking  Metadata
    SEXP cData = VECTOR_ELT(unlockMeta, 1);
    contextData c(cData);

    // Entry 2: hast we are unlocking
    SEXP hSym = VECTOR_ELT(unlockMeta, 2);

    // Entry 3: offset at which we are unlocking
    SEXP offsetSym = VECTOR_ELT(unlockMeta, 3);

    // Entry 4: vtable which we are unlocking
    SEXP vtabContainer = VECTOR_ELT(unlockMeta, 4);

    *counter = *counter - 1;

    // all dependencies are satisfied
    if (*counter == 0) {
        if (!DispatchTable::check(vtabContainer)) {
            Rf_error("bitcode linking, dispatch table corrupted!");
        }
        #if PRINT_LINKING_STATUS == 1
        std::cout << "  linking  (" << *counter << "): " << CHAR(PRINTNAME(hSym)) << "_" << CHAR(PRINTNAME(offsetSym)) << "_" << c.getContext() << std::endl;
        #endif
        // Remove the linkageSymbol to prevent infinite linking recursion
        linkageMap.remove(linkageSymbol);

        // link the unlocked bitcode
        BitcodeLinkUtil::linkBitcode(cData, hSym, offsetSym, DispatchTable::unpack(vtabContainer));

        // DispatchTable::unpack(vtabContainer)->disableFurtherSpecialization = false;
    } else {
        #if PRINT_LINKING_STATUS == 1
        std::cout << "  counting (" << *counter << "): " << CHAR(PRINTNAME(hSym)) << "_" << CHAR(PRINTNAME(offsetSym)) << "_" << c.getContext() << std::endl;
        #endif
        // if (!DispatchTable::check(vtabContainer)) {
        //     Rf_error("bitcode linking, dispatch table corrupted! (was still counting)");
        // }
        // DispatchTable::unpack(vtabContainer)->disableFurtherSpecialization = true;
    }
    #if PRINT_LINKING_STATUS == 1
    std::cout << "doLinking end" << std::endl;
    #endif
}

static inline void doWorklistOpt(SEXP worklist, const int & nargs) {
    REnvHandler linkageMap(LINKAGE_MAP);
    for (int i = 0; i < Rf_length(worklist); i++) {
        auto ele = VECTOR_ELT(worklist, i);
        if (ele == R_NilValue) continue;

        int * nargsExpected = INTEGER(VECTOR_ELT(ele, 0));
        if (nargs < *nargsExpected) {
            #if PRINT_LINKING_STATUS == 1
            std::cout << "  optimistic linking failure, nargs less than expected" << std::endl;
            #endif
            // Free slot from worklist
            SET_VECTOR_ELT(worklist, i, R_NilValue);
            continue;
        }

        SEXP linkageSymbol = VECTOR_ELT(ele, 1);

        if (SEXP unlockMeta = linkageMap.get(linkageSymbol)) {
            doLinking(unlockMeta, linkageSymbol);
        } else {
            #if PRINT_LINKING_STATUS == 1
            std::cout << "  stale: " << CHAR(PRINTNAME(linkageSymbol)) << std::endl;
            #endif
        }

        // Free slot from worklist
        SET_VECTOR_ELT(worklist, i, R_NilValue);

    }
    #if PRINT_LINKING_STATUS == 1
    std::cout << "doWorklistOpt end" << std::endl;
    #endif
}

static inline void doWorklist(SEXP worklist) {
    REnvHandler linkageMap(LINKAGE_MAP);
    for (int i = 0; i < Rf_length(worklist); i++) {
        auto linkageSymbol = VECTOR_ELT(worklist, i);
        if (linkageSymbol == R_NilValue) continue;

        if (SEXP unlockMeta = linkageMap.get(linkageSymbol)) {
            doLinking(unlockMeta, linkageSymbol);
        } else {
            #if PRINT_LINKING_STATUS == 1
            std::cout << "  stale: " << CHAR(PRINTNAME(linkageSymbol)) << std::endl;
            #endif
        }

        // Free slot from worklist
        SET_VECTOR_ELT(worklist, i, R_NilValue);

    }
    #if PRINT_LINKING_STATUS == 1
    std::cout << "doWorklist end" << std::endl;
    #endif
}

void BitcodeLinkUtil::tryUnlocking(SEXP currHastSym) {
    REnvHandler hastUnlockMap(HAST_UNLOCK_MAP);

    if (SEXP worklist = hastUnlockMap.get(currHastSym)) {
        #if PRINT_LINKING_STATUS == 1
        std::cout << "[worklist: " << CHAR(PRINTNAME(currHastSym)) << "]" << std::endl;
        #endif
        doWorklist(worklist);
        // remove entry from the worklist
        hastUnlockMap.remove(currHastSym);
    }

    #if PRINT_LINKING_STATUS == 1
    std::cout << "tryUnlocking end" << std::endl;
    #endif
}

void BitcodeLinkUtil::tryUnlockingOpt(SEXP currHastSym, const unsigned long & con, const int & nargs) {
    std::stringstream ss;
    ss << CHAR(PRINTNAME(currHastSym)) << "_" << con;
    SEXP optMapKey = Rf_install(ss.str().c_str());

    REnvHandler optUnlockMap(OPT_UNLOCK_MAP);

    if (SEXP worklist = optUnlockMap.get(optMapKey)) {
        #if PRINT_LINKING_STATUS == 1
        std::cout << "[opt worklist: " << CHAR(PRINTNAME(optMapKey)) << "]" << std::endl;
        #endif
        doWorklistOpt(worklist, nargs);
        // remove entry from the worklist
        optUnlockMap.remove(currHastSym);
    }
}

void BitcodeLinkUtil::markStale(SEXP currHastSym, const unsigned long & con) {
    std::stringstream ss;
    ss << CHAR(PRINTNAME(currHastSym)) << "_" << con;
    SEXP optMapKey = Rf_install(ss.str().c_str());

    REnvHandler linkageMap(LINKAGE_MAP);

    if (linkageMap.get(optMapKey)) {
        #if PRINT_LINKING_STATUS == 1
        std::cout << "  MARKED STALE: " << ss.str() << std::endl;
        #endif
        linkageMap.remove(optMapKey);
    }
}

void BitcodeLinkUtil::applyMask(DispatchTable * vtab, SEXP hSym) {
    REnvHandler hastDepMap(HAST_DEPENDENCY_MAP);
    if (hastDepMap.isEmpty()) return;

    SEXP hastEnv = hastDepMap.get(hSym);
    SEXP maskSym = Rf_install("mask");

    if (hastEnv) {
        REnvHandler hastVtabMap(HAST_VTAB_MAP);
        REnvHandler hastEnvMap(hastEnv);
        hastEnvMap.iterate([&] (SEXP offsetSym, SEXP offsetEnv) {
            int reqOffset = std::stoi(CHAR(PRINTNAME(offsetSym)));
            DispatchTable * requiredVtab = getVtableAtOffset(vtab,reqOffset);
            REnvHandler offsetMap(offsetEnv);

            offsetMap.iterate([&] (SEXP contextSym, SEXP cData) {
                if (maskSym == contextSym) {
                    // Add mask to dispatch table
                    unsigned long* res = (unsigned long *) DATAPTR(cData);
                    // std::cout << "found mask: " << Context(*res) << std::endl;
                    requiredVtab->mask = Context(*res);
                }
            });
        });

        // remove the metadata after processing it
        hastDepMap.remove(hSym);
    }
}

void BitcodeLinkUtil::tryLinking(DispatchTable * vtab, SEXP hSym) {
    REnvHandler hastDepMap(HAST_DEPENDENCY_MAP);
    if (hastDepMap.isEmpty()) return;

    SEXP hastEnv = hastDepMap.get(hSym);
    SEXP maskSym = Rf_install("mask");

    if (hastEnv) {
        #if PRINT_LINKING_STATUS == 1
        std::cout << "[linking    : " << CHAR(PRINTNAME(hSym)) << "]" << std::endl;
        #endif
        REnvHandler hastVtabMap(HAST_VTAB_MAP);
        REnvHandler hastEnvMap(hastEnv);
        hastEnvMap.iterate([&] (SEXP offsetSym, SEXP offsetEnv) {
            #if PRINT_LINKING_STATUS == 1
            std::cout << "  offset[" << CHAR(PRINTNAME(offsetSym)) << "]" << std::endl;
            #endif
            int reqOffset = std::stoi(CHAR(PRINTNAME(offsetSym)));
            DispatchTable * requiredVtab = getVtableAtOffset(vtab,reqOffset);
            REnvHandler offsetMap(offsetEnv);

            offsetMap.iterate([&] (SEXP contextSym, SEXP cData) {
                if (maskSym == contextSym) {
                    // Add mask to dispatch table
                    unsigned long* res = (unsigned long *) DATAPTR(cData);
                    // std::cout << "found mask: " << Context(*res) << std::endl;
                    requiredVtab->mask = Context(*res);
                } else {
                    contextData c(cData);
                    SEXP rMap = c.getReqMapAsVector();
                    int rMapSize = Rf_length(rMap);

                    #if PRINT_LINKING_STATUS == 1
                    std::cout << "    context[" << CHAR(PRINTNAME(contextSym)) << "]" << std::endl;
                    #endif

                    if (rMapSize == 0) {
                        #if PRINT_LINKING_STATUS == 1
                        std::cout << "      (*) [Early linking]" << std::endl;
                        #endif
                        // early linking possible, no dependencies
                        linkBitcode(cData, hSym, offsetSym, requiredVtab);
                        // remove context entry from offsetEnv upon successful linking
                        offsetMap.remove(contextSym);
                    } else {
                        std::stringstream ss;
                        ss << CHAR(PRINTNAME(hSym)) << "_" << CHAR(PRINTNAME(offsetSym)) << "_" << CHAR(PRINTNAME(contextSym));
                        SEXP linkageMapSym = Rf_install(ss.str().c_str());
                        int unsatisfiedDependencies = 0;
                        bool allDepsSatisfied = true;
                        #if PRINT_WORKLIST_ENTRIES == 1
                        std::vector<SEXP> currWorklist;
                        #endif

                        for (int i = 0; i < rMapSize; i++) {

                            SEXP ele = VECTOR_ELT(rMap, i);

                            SEXP hastOfReq;
                            unsigned long con;
                            int numArgs;

                            bool optimisticCase = false;
                            auto n = std::string(CHAR(PRINTNAME(ele)));

                            auto firstDel = n.find('_');
                            if (firstDel != std::string::npos) {
                                // optimistic dispatch case
                                auto secondDel = n.find('_', firstDel + 1);
                                auto hast = n.substr(0, firstDel);
                                auto context = n.substr(firstDel + 1, secondDel - firstDel - 1);
                                auto nargs = n.substr(secondDel + 1);

                                con = std::stoul(context);
                                numArgs = std::stoi(nargs);
                                hastOfReq = Rf_install(hast.c_str());
                                optimisticCase = true;
                            } else {
                                hastOfReq = ele;
                            }

                            // check if the dependency is already satisfied
                            if (!optimisticCase) {
                                // check in hast is satisfied already
                                if (!hastVtabMap.get(hastOfReq)) {
                                    unsatisfiedDependencies++;
                                    #if PRINT_WORKLIST_ENTRIES == 1
                                    currWorklist.push_back(ele);
                                    #endif

                                    // hast is not satisfied yet, add to worklist
                                    addToWorklistOne(hastOfReq, linkageMapSym);
                                    allDepsSatisfied = false;
                                }

                            } else {
                                // optimistic case

                                // check if optimistic site already exists
                                if (SEXP vtabContainer = hastVtabMap.get(hastOfReq)) {
                                    if (!DispatchTable::check(vtabContainer)) {
                                        Rf_error("linking error, corrupted vtable");
                                    }

                                    bool optimisticSiteExists = false;
                                    DispatchTable * requiredVtab = DispatchTable::unpack(vtabContainer);
                                    for (size_t i = 0; i < requiredVtab->size(); i++) {
                                        auto entry = requiredVtab->get(i);
                                        if (entry->context().toI() == con &&
                                            entry->signature().numArguments >= (unsigned)numArgs) {
                                                optimisticSiteExists = true;
                                                break;
                                        }
                                    }

                                    if (!optimisticSiteExists) {
                                        unsatisfiedDependencies++;
                                        #if PRINT_WORKLIST_ENTRIES == 1
                                        currWorklist.push_back(ele);
                                        #endif

                                        // hast is not satisfied yet, add to worklist
                                        addToWorklistTwo(hastOfReq, con, numArgs, linkageMapSym);
                                        allDepsSatisfied = false;
                                    }

                                } else {
                                    unsatisfiedDependencies++;
                                    #if PRINT_WORKLIST_ENTRIES == 1
                                    currWorklist.push_back(ele);
                                    #endif

                                    // hast is not satisfied yet, add to worklist
                                    addToWorklistTwo(hastOfReq, con, numArgs, linkageMapSym);
                                    allDepsSatisfied = false;

                                }
                            }
                        }
                        if (allDepsSatisfied) {
                            #if PRINT_LINKING_STATUS == 1
                            std::cout << "      (*) [Early linking, all dependencies already satisfied]" << std::endl;
                            #endif
                            // early linking possible, no dependencies
                            linkBitcode(cData, hSym, offsetSym, requiredVtab);
                            // remove context entry from offsetEnv upon successful linking
                            offsetMap.remove(contextSym);
                        } else {
                            // update linkage map
                            SEXP unlockMeta;
                            PROTECT(unlockMeta = Rf_allocVector(VECSXP, 5));
                            SEXP counter;
                            PROTECT(counter = Rf_allocVector(RAWSXP, sizeof(int)));
                            int * tmp = (int *) DATAPTR(counter);
                            *tmp = unsatisfiedDependencies;
                            SET_VECTOR_ELT(unlockMeta, 0, counter);
                            UNPROTECT(1);
                            SET_VECTOR_ELT(unlockMeta, 1, cData);
                            SET_VECTOR_ELT(unlockMeta, 2, hSym);
                            SET_VECTOR_ELT(unlockMeta, 3, offsetSym);
                            SET_VECTOR_ELT(unlockMeta, 4, requiredVtab->container());

                            REnvHandler linkageMap(LINKAGE_MAP);
                            linkageMap.set(linkageMapSym, unlockMeta);

                            UNPROTECT(1);
                            #if PRINT_LINKING_STATUS == 1
                            std::cout << "      (*) [Not linked yet: " << CHAR(PRINTNAME(hSym)) << "_" << CHAR(PRINTNAME(offsetSym)) << "_" << c.getContext() << "]" << std::endl;
                            std::cout << "      (*) (waiting for " << *tmp << " dependencies)" << std::endl;
                            #endif

                            #if PRINT_WORKLIST_ENTRIES == 1
                            std::cout << "      [";
                            for (auto & ele : currWorklist) {
                                std::cout << CHAR(PRINTNAME(ele)) << " ";
                            }
                            std::cout << "]" << std::endl;
                            #endif
                        }
                    }
                }
            });
        });

        // remove the metadata after processing it
        hastDepMap.remove(hSym);
    }
}

size_t BitcodeLinkUtil::linkTime = 0;
}
