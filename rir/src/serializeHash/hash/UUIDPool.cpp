//
// Created by Jakob Hain on 6/1/23.
//

#include "UUIDPool.h"
#include "R/Printing.h"
#include "R/Protect.h"
#include "R/Serialize.h"
#include "R/disableGc.h"
#include "compiler/parameter.h"
#include "compilerClientServer/CompilerClient.h"
#include "compilerClientServer/CompilerServer.h"
#include "runtime/PoolStub.h"
#include "runtime/log/printPrettyGraphFromEnv.h"
#include "runtime/log/printRirObject.h"
#include "runtime/rirObjectMagic.h"
#include "serializeHash/hash/getConnected.h"
#include "serializeHash/hash/hashRoot.h"
#include "serializeHash/serialize/serialize.h"
#include "utils/measuring.h"

// Can change this to log interned and uninterned hashes and pointers
#define LOG(stmt) if (pir::Parameter::PIR_LOG_INTERNING) stmt
#define LOG_WARN(stmt) if (pir::Parameter::PIR_LOG_INTERNING || pir::Parameter::PIR_WARN_INTERNING) stmt

namespace rir {

bool pir::Parameter::PIR_LOG_INTERNING =
    getenv("PIR_LOG_INTERNING") != nullptr &&
    strcmp(getenv("PIR_LOG_INTERNING"), "") != 0 &&
    strcmp(getenv("PIR_LOG_INTERNING"), "0") != 0 &&
    strcmp(getenv("PIR_LOG_INTERNING"), "false") != 0;

bool pir::Parameter::PIR_WARN_INTERNING =
    getenv("PIR_WARN_INTERNING") != nullptr &&
    strcmp(getenv("PIR_WARN_INTERNING"), "") != 0 &&
    strcmp(getenv("PIR_WARN_INTERNING"), "0") != 0 &&
    strcmp(getenv("PIR_WARN_INTERNING"), "false") != 0;

bool pir::Parameter::PIR_MEASURE_INTERNING =
    getenv("PIR_MEASURE_INTERNING") != nullptr &&
    strtol(getenv("PIR_MEASURE_INTERNING"), nullptr, 10);


bool UUIDPool::isInitialized = false;
std::unordered_map<UUID, SEXP> UUIDPool::interned;
std::unordered_map<SEXP, UUID> UUIDPool::hashes;
std::unordered_map<SEXP, SEXP> UUIDPool::nextToIntern;
std::unordered_map<SEXP, SEXP> UUIDPool::prevToIntern;
std::unordered_set<SEXP> UUIDPool::preserved;

#ifdef DEBUG_DISASSEMBLY
static std::unordered_map<UUID, std::string> disassembly;
#endif

bool UUIDPool::internable(SEXP sexp) {
    // TypeFeedback and ArglistOrder aren't interned, they're serialized inline
    // like non-RIR SEXPs because we never need to refer to them alone, identity
    // doesn't matter (only equivalence), and they usually aren't big. Plus,
    // TypeFeedback changes frequently, so it would need to be re-interned
    // frequently
    return TYPEOF(sexp) == EXTERNALSXP &&
           !TypeFeedback::check(sexp) &&
           !ArglistOrder::check(sexp) &&
           !PoolStub::check(sexp);
}

#ifdef DO_INTERN
static void registerFinalizerIfPossible(SEXP e, R_CFinalizer_t finalizer) {
    switch (TYPEOF(e)) {
    case NILSXP:
    case ENVSXP:
    case EXTPTRSXP:
    case BCODESXP:
    case EXTERNALSXP:
        R_RegisterCFinalizerEx(e, finalizer, (Rboolean) true);
        break;
    default:
        // can't register finalizer, assume these don't get gcd
        break;
    }
}

void UUIDPool::initialize() {
    assert(!isInitialized);
    isInitialized = true;
}

void UUIDPool::unintern(SEXP e, bool isGettingGcd) {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_INTERNING, "UUIDPool.cpp: unintern", e, !isGettingGcd, [&] {
        Protect p(e);
        assert(hashes.count(e) && "SEXP not interned");

        // Remove hash
        auto hash = hashes.at(e);
        hashes.erase(e);
        if (!interned.count(hash)) {
            LOG_WARN(std::cerr << "WARNING: SEXP was interned, but the "
                                  "corresponding UUID is empty:\n"
                               << Print::dumpSexp(e) << "\n");
            // Don't return
        }

        // Remove from the intern list for this UUID. If this is the first entry,
        // update the interned UUID to point to the next SEXP. If there is no next,
        // erase the interned UUID since there are no live SEXPs with that hash
        // anymore.
        if (prevToIntern.count(e)) {
            // This isn't the first entry in the list with this UUID

            // Linked list intermediate removal algorithm
            auto prev = prevToIntern.at(e);
            prevToIntern.erase(e);
            assert(nextToIntern.count(prev) && nextToIntern.at(prev) == e);
            if (nextToIntern.count(e)) {
                auto next = nextToIntern.at(e);
                nextToIntern.erase(e);
                assert(prevToIntern.count(next) && prevToIntern.at(next) == e);
                nextToIntern.at(prev) = next;
                prevToIntern.at(next) = prev;
            } else {
                nextToIntern.erase(prev);
            }
            LOG(std::cout << "GC intern: " << hash << " -> " << e << "\n");
        } else if (nextToIntern.count(e)) {
            // This is the first entry in the list with this UUID, and there is
            // another entry

            // Linked list head removal algorithm
            auto next = nextToIntern.at(e);
            nextToIntern.erase(e);
            assert(prevToIntern.count(next) && prevToIntern.at(next) == e);
            prevToIntern.erase(next);

            // Replace interned at UUID with the next SEXP
            interned.at(hash) = next;
            LOG(std::cout << "Switch intern: " << hash << " -> was " << e << " now " << next << "\n");
        } else {
            // This is the first and only entry in the list with this UUID

            // Erase interned at UUID
            interned.erase(hash);
            LOG(std::cout << "Remove intern: " << hash << " -> " << e << "\n");
        }
    });
}

void UUIDPool::uninternGcd(SEXP e) {
    // There seems to be a bug somewhere where R is calls finalizer on the wrong
    // object, or calls it twice. Or maybe it's in our code...
    if (preserved.count(e)) {
        LOG_WARN(std::cerr << "WARNING: preserved SEXP is supposedly getting gcd"
                           << std::endl);
        return;
    }
    if (!hashes.count(e)) {
        // Can happen if we manually unintern, since we can't remove the finalizer
        return;
    }

    unintern(e, true);
}
#endif

SEXP UUIDPool::intern(SEXP e, const UUID& hash, bool preserve, bool isSexpComplete) {
    return Measuring::timeEventIf2(pir::Parameter::PIR_MEASURE_INTERNING, "UUIDPool.cpp: intern specific", e, isSexpComplete, [&] {
        Protect p(e);
        assert(internable(e));
        (void)isSexpComplete;

#ifdef DO_INTERN
        if (interned.count(hash)) {
            // Reuse interned SEXP
            auto existing = interned.at(hash);
            assert(TYPEOF(e) == TYPEOF(existing) && "obvious hash collision (different types)");
            assert((TYPEOF(e) != EXTERNALSXP || rirObjectMagic(e) == rirObjectMagic(existing) || !isSexpComplete) &&
                   "obvious hash collision (different RIR types)");
            if (!hashes.count(e)) {
                // This SEXP is structurally-equivalent to the interned SEXP but not
                // the same (different pointers), so we must still record it
                LOG(std::cout << "Reuse intern: " << hash << " -> " << e << (isSexpComplete ? "\n" : " (recursive)\n"));
                hashes[e] = hash;

                // Add to intern list for this UUID
                auto oldLast = existing;
                while (nextToIntern.count(oldLast)) {
                    oldLast = nextToIntern.at(oldLast);
                }
                nextToIntern[oldLast] = e;
                prevToIntern[e] = oldLast;

                // And register finalizer
                if (!preserve) {
                    registerFinalizerIfPossible(e, uninternGcd);
                }
            }
            // If preserve = true, we want to preserve both this SEXP and the
            // interned one, because we could later fetch either. In the future,
            // we can probably switch e to be existing so we don't need to
            // preserve redundant SEXPs like this.
            if (preserve && !preserved.count(e)) {
                // Preserve this SEXP
                R_PreserveObject(e);
                preserved.insert(e);
            }
            e = existing;
            if (preserve && !preserved.count(e)) {
                // Preserve the interned SEXP
                R_PreserveObject(e);
                preserved.insert(e);
            }
            return e;
        }

        // Intern new SEXP
#ifdef DEBUG_DISASSEMBLY
        disassembly[hash] =
            isSexpComplete
                ? printRirObject(e, RirObjectPrintStyle::Detailed)
            : "(couldn't be computed at the time it was interned)";
#endif

        // Sanity check in case the UUID changed
        if (hashes.count(e)) {
            LOG_WARN(std::cerr << "SEXP UUID changed from " << hashes.at(e)
                               << " to " << hash << ": " << e << "\n"
                               << Print::dumpSexp(e) << "\n");

#ifdef DEBUG_DISASSEMBLY
            auto oldDisassembly = disassembly[hashes.at(e)];
            auto newDisassembly = disassembly[hash];
            if (oldDisassembly != newDisassembly) {
                LOG_WARN(std::cerr << "note: disassembly changed from:\n"
                                   << oldDisassembly << "\nto:\n"
                                   << newDisassembly << "\n");
            } else {
                LOG_WARN(std::cerr << "note: disassembly:\n" << oldDisassembly
                                   << "\n");
            }
#endif

            // assert(false);
            LOG_WARN(std::cerr << "WARNING: SEXP UUID changed. Unsound, and "
                                  "semantic errors may occur if we rely on "
                                  "outdated behavior\n");
            // DON'T unintern because we or the compiler peer may request it
            // from the old hash.
        }

        // Do intern
        LOG(std::cout << "New intern: " << hash << " -> " << e << "\n");
#ifdef DEBUG_DISASSEMBLY
        LOG(std::cout << "Disassembly:\n" << disassembly[hash] << "\n");
#endif
        if (isSexpComplete) {
            printPrettyGraphOfInternedIfNecessary(e, hash);
        }
        interned[hash] = e;
        hashes[e] = hash;

        // Preserve or register finalizer
        if (preserve) {
            R_PreserveObject(e);
            preserved.insert(e);
        } else {
            registerFinalizerIfPossible(e, uninternGcd);
        }
#endif

        return e;
    });
}

SEXP UUIDPool::intern(SEXP e, bool recursive, bool preserve) {
#ifdef DO_INTERN
    return disableGc2([&]{
        return Measuring::timeEventIf2(pir::Parameter::PIR_MEASURE_INTERNING, recursive ? "UUIDPool.cpp: intern recursive" : "UUIDPool.cpp: intern", e, [&] {
            if (hashes.count(e) && !recursive) {
                // Already interned, don't compute hash
                if (preserve && !preserved.count(e)) {
                    R_PreserveObject(e);
                    preserved.insert(e);
                }
                return e;
            }
            auto ret = internable(e) ? intern(e, hashRoot(e), preserve) : e;
            if (recursive) {
                ConnectedSet connected = getConnected(e);
                for (auto sexp : connected) {
                    if (hashes.count(sexp) || !internable(sexp)) {
                        continue;
                    }

                    intern(sexp, hashRoot(sexp), preserve);
                }
            }
            return ret;
        });
    });
#else
    return e;
#endif
}

SEXP UUIDPool::reintern(SEXP e) {
#ifdef DO_INTERN
    // This is called before everything is initialized, so we need to ensure
    // that isInitialized is set before we check hashes or we will crash
    if (isInitialized && hashes.count(e)) {
        unintern(e);
        return Measuring::timeEventIf2(pir::Parameter::PIR_MEASURE_INTERNING, "UUIDPool.cpp: reintern", e, [&] {
            return intern(e, false, false);
        });
    }
#endif
    return e;
}

SEXP UUIDPool::get(const UUID& hash) {
#ifdef DO_INTERN
    if (interned.count(hash)) {
        return interned.at(hash);
    }
#endif
    return nullptr;
}

const UUID& UUIDPool::getHash(SEXP sexp) {
#ifdef DO_INTERN
    if (hashes.count(sexp)) {
        return hashes.at(sexp);
    }
#endif
    static UUID empty;
    return empty;
}

SEXP UUIDPool::retrieve(const UUID& hash) {
    if (interned.count(hash)) {
        LOG(std::cout << "Retrieved by hash locally: " << hash << " -> "
                      << interned.at(hash) << "\n");
        return interned.at(hash);
    }
    if (CompilerClient::isRunning()) {
        LOG(std::cout << "Retrieving by hash from server: " << hash << "\n");
        auto sexp = CompilerClient::retrieve(hash);
        LOG(std::cout << "Retrieved by hash from server: " << hash << " -> "
                      << sexp << "\n");
        if (sexp) {
#ifdef DEBUG_DISASSEMBLY
            disassembly[hash] = printRirObject(sexp, RirObjectPrintStyle::Detailed);
            LOG(std::cout << "Disassembly:\n" << disassembly[hash] << "\n");
#endif
            intern(sexp, hash, false, true);
            return sexp;
        }
        Rf_error("SEXP deserialized from hash which we don't have, and server also doesn't have it");
    } else if (CompilerServer::isRunning()) {
        LOG(std::cout << "Retrieving by hash from client: " << hash << "\n");
        auto sexp = CompilerServer::retrieve(hash);
        LOG(std::cout << "Retrieved by hash from client: " << hash << " -> "
                      << sexp << "\n");
        if (sexp) {
#ifdef DEBUG_DISASSEMBLY
            disassembly[hash] = printRirObject(sexp, RirObjectPrintStyle::Detailed);
            LOG(std::cout << "Disassembly:\n" << disassembly[hash] << "\n");
#endif
            intern(sexp, hash, true, true);
            return sexp;
        }
        LOG(std::cout << "SEXP deserialized from hash which we don't have, and client also doesn't have it");
        // TODO: Should we be returning this, or returning an explicit "not
        //  found" token, or still erroring and instead handling explicitly
        //  via a separate method, maybe renaming the old tryReadHash to
        //  readHashIfNecessary and the new one to tryReadHashIfNecessary?
        return R_NilValue;
    }
    Rf_error("SEXP deserialized from hash which we don't have, and no server");
}

SEXP UUIDPool::tryReadHash(R_inpstream_t inp) {
    auto readHashInstead = InBool(inp);
    if (readHashInstead) {
        // Read hash instead of regular data,
        // then retrieve by hash from interned or peer
        UUID hash;
        InBytes(inp, &hash, sizeof(hash));
        return retrieve(hash);
    }
    return nullptr;
}

bool UUIDPool::tryWriteHash(SEXP sexp, R_outpstream_t out) {
    auto writeHash = internable(sexp);
    // Write whether we are serializing hash
    OutBool(out, writeHash);
    if (writeHash) {
        // Write hash instead of regular data
        if (!hashes.count(sexp)) {
            LOG(std::cout << "Interning new SEXP at write: " << sexp << "\n");
            intern(sexp, hashRoot(sexp), false);
        }
        // cppcheck is wrong, this is read
        // cppcheck-suppress unreadVariable
        auto hash = hashes.at(sexp);
        OutBytes(out, &hash, sizeof(hash));
    }
    return writeHash;
}

SEXP UUIDPool::tryReadHash(const ByteBuffer& buf) {
    auto readHashInstead = buf.getBool();
    if (readHashInstead) {
        // Read hash instead of regular data,
        // then retrieve by hash from interned or peer
        UUID hash;
        buf.getBytes((uint8_t*)&hash, sizeof(hash));
        return retrieve(hash);
    }
    return nullptr;
}

bool UUIDPool::tryWriteHash(SEXP sexp, ByteBuffer& buf) {
    auto writeHash = internable(sexp);
    // Write whether we are serializing hash
    buf.putBool(writeHash);
    if (writeHash) {
        // Write hash instead of regular data
        if (!hashes.count(sexp)) {
            LOG(std::cout << "Interning new SEXP at write: " << sexp << "\n");
            intern(sexp, hashRoot(sexp), false);
        }
        // cppcheck is wrong, this is read
        // cppcheck-suppress unreadVariable
        auto hash = hashes.at(sexp);
        buf.putBytes((uint8_t*)&hash, sizeof(hash));
    }
    return writeHash;
}

SEXP UUIDPool::readItem(const ByteBuffer& buf, bool useHashes) {
    if (useHashes) {
        if (auto result = tryReadHash(buf)) {
            return result;
        }
    }

    // Read regular data
    return deserialize(buf, SerialOptions{useHashes, useHashes, false, false, SerialOptions::SourcePools()});
}

void UUIDPool::writeItem(SEXP sexp, __attribute__((unused)) bool isChild,
                         ByteBuffer& buf, bool useHashes) {
    if (useHashes) {
        if (tryWriteHash(sexp, buf)) {
            return;
        }
    }

    // Write regular data
    serialize(sexp, buf, SerialOptions{useHashes, useHashes, false, false, SerialOptions::SourcePools()});
}

} // namespace rir