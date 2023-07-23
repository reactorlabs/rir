//
// Created by Jakob Hain on 6/1/23.
//

#include "UUIDPool.h"
#include "CompilerClient.h"
#include "CompilerServer.h"
#include "R/Protect.h"
#include "R/Serialize.h"
#include "api.h"
#include "compiler/parameter.h"
#include "interpreter/serialize.h"
#include "runtime/DispatchTable.h"
#include "utils/measuring.h"
#include <queue>

#define DEBUG_DISASSEMBLY

// Can change this to log interned and uninterned hashes and pointers
#define LOG(stmt) if (CompilerClient::isRunning() || CompilerServer::isRunning()) stmt

namespace rir {

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

static bool internable(SEXP e) {
    return TYPEOF(e) == EXTERNALSXP;
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

void UUIDPool::unintern(SEXP e) {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_INTERNING, "UUIDPool.cpp: unintern", e, [&] {
        Protect p(e);
        assert(hashes.count(e) && "SEXP not interned");

        // Remove hash
        auto hash = hashes.at(e);
        hashes.erase(e);
        if (!interned.count(hash)) {
            Rf_warning("SEXP was interned, but the corresponding UUID is empty");
            Rf_PrintValue(e);
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
    Protect p(e);

    // There seems to be a bug somewhere where R is calls finalizer on the wrong
    // object, or calls it twice...
    if (preserved.count(e)) {
        Rf_warning("Preserved SEXP is supposedly getting gcd");
        Rf_PrintValue(e);
        return;
    }
    if (!hashes.count(e)) {
        Rf_warning("SEXP getting gcd is supposedly never interned");
        Rf_PrintValue(e);
        return;
    }

    unintern(e);
}
#endif

SEXP UUIDPool::intern(SEXP e, const UUID& hash, bool preserve, bool expectHashToBeTheSame) {
    return Measuring::timeEventIf<SEXP>(pir::Parameter::PIR_MEASURE_INTERNING, "UUIDPool.cpp: intern specific", e, expectHashToBeTheSame, [&] {
        Protect p(e);
        assert(internable(e));
        (void)expectHashToBeTheSame;

#ifdef DO_INTERN
        SLOWASSERT((!expectHashToBeTheSame || hashRoot(e) == hash) &&
                   "SEXP hash isn't deterministic or `hash` in `UUIDPool::intern(e, hash)` is wrong");
        if (interned.count(hash)) {
            // Reuse interned SEXP
            auto existing = interned.at(hash);
            if (!hashes.count(e)) {
                // This SEXP is structurally-equivalent to the interned SEXP but not
                // the same (different pointers), so we must still record it
                LOG(std::cout << "Reuse intern: " << hash << " -> " << e << "\n");
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
            e = existing;
            if (preserve && !preserved.count(e)) {
                // Hashing with preserve and this interned SEXP wasn't yet preserved
                R_PreserveObject(e);
                preserved.insert(e);
            }
            return e;
        }

        // Intern new SEXP
#ifdef DEBUG_DISASSEMBLY
        if (expectHashToBeTheSame) {
            if (DispatchTable::check(e)) {
                auto dt = DispatchTable::unpack(e);
                std::stringstream s;
                dt->print(s, true);
                disassembly[hash] = s.str();
            } else if (Function::check(e)) {
                auto fun = Function::unpack(e);
                if (!Code::check(EXTERNALSXP_ENTRY(fun->container(), 0))) {
                    std::cerr
                        << "Tried to serialize function during its construction: "
                        << e << "\n";
                    Rf_PrintValue(e);
                    assert(false);
                }
                std::stringstream s;
                fun->print(s, true);
                disassembly[hash] = s.str();
            } else if (Code::check(e)) {
                auto code = Code::unpack(e);
                std::stringstream s;
                code->print(s, true);
                disassembly[hash] = s.str();
            }
        } else {
            disassembly[hash] = "(recursively interned, can't debug this way)";
        }
#endif

        // Sanity check in case the UUID changed
        if (hashes.count(e)) {
            std::cerr << "SEXP UUID changed from " << hashes.at(e) << " to "
                      << hash << ": " << e << "\n";
            Rf_PrintValue(e);

#ifdef DEBUG_DISASSEMBLY
            auto oldDisassembly = disassembly[hashes.at(e)];
            auto newDisassembly = disassembly[hash];
            if (oldDisassembly != newDisassembly) {
                std::cerr << "note: disassembly changed from:\n" << oldDisassembly
                          << "\nto:\n" << newDisassembly << "\n";
            } else {
                std::cerr << "note: disassembly:\n" << oldDisassembly << "\n";
            }
#endif

            // assert(false);
            Rf_warning("SEXP UUID changed. Uninterning, but unless we're"
                       "testing, semantic deviations have probably occurred and"
                       "we'll probably crash soon");
            unintern(e);
        }

        // Do intern
        LOG(std::cout << "New intern: " << hash << " -> " << e << "\n");
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
    return Measuring::timeEventIf<SEXP>(pir::Parameter::PIR_MEASURE_INTERNING, recursive ? "UUIDPool.cpp: intern recursive" : "UUIDPool.cpp: intern", e, [&] {
        Protect p(e);
        if (hashes.count(e) && !recursive) {
            // Already interned, don't compute hash
            if (preserve && !preserved.count(e)) {
                R_PreserveObject(e);
                preserved.insert(e);
            }
            return e;
        }
        if (recursive) {
            ConnectedWorklist connected;
            // Compute hash, whether internable or not, to add connected objects
            // which are internable to connected
            // cppcheck-suppress unreadVariable
            auto hash = hashRoot(e, connected);
            auto ret = internable(e) ? intern(e, hash, preserve) : e;
            while ((e = connected.pop())) {
                if (hashes.count(e) || !internable(e)) {
                    continue;
                }

                intern(e, hashRoot(e), preserve);
            }
            return ret;
        } else {
            return internable(e) ? intern(e, hashRoot(e), preserve) : e;
        }
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
        return Measuring::timeEventIf<SEXP>(pir::Parameter::PIR_MEASURE_INTERNING, "UUIDPool.cpp: reintern", e, [&] {
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

SEXP UUIDPool::readItem(SEXP ref_table, R_inpstream_t in) {
    if (useHashes(in)) {
        // Read whether we are serializing hash
        auto isInternable = InBool(in);
        if (isInternable) {
            // Read hash instead of regular data,
            // then retrieve by hash from interned or server
            UUID hash;
            InBytes(in, &hash, sizeof(hash));
            if (interned.count(hash)) {
                LOG(std::cout << "Retrieved by hash locally: " << hash << "\n");
                return interned.at(hash);
            }
            if (CompilerClient::isRunning()) {
                LOG(std::cout << "Retrieving by hash from server: " << hash << "\n");
                auto sexp = CompilerClient::retrieve(hash);
                if (sexp) {
                    return sexp;
                }
                Rf_error("SEXP deserialized from hash which we don't have, and server also doesn't have it");
            }
            Rf_error("SEXP deserialized from hash which we don't have, and no server");
        }
    }

    // Read regular data
    return ReadItem(ref_table, in);
}

SEXP UUIDPool::readItem(ByteBuffer& buf, bool useHashes) {
    if (useHashes) {
        // Read whether we are serializing hash
        auto isInternable = buf.getBool();
        if (isInternable) {
            // Read hash instead of regular data,
            // then retrieve by hash from interned or server
            UUID hash;
            buf.getBytes((uint8_t*)&hash, sizeof(hash));
            if (interned.count(hash)) {
                LOG(std::cout << "Retrieved by hash locally: " << hash << "\n");
                return interned.at(hash);
            }
            if (CompilerClient::isRunning()) {
                LOG(std::cout << "Retrieving by hash from server: " << hash << "\n");
                auto sexp = CompilerClient::retrieve(hash);
                if (sexp) {
                    return sexp;
                }
                Rf_error("SEXP deserialized from hash which we don't have, and server also doesn't have it");
            }
            Rf_error("SEXP deserialized from hash which we don't have, and no server");
        }
    }

    // Read regular data
    return deserialize(buf, useHashes);
}

void UUIDPool::writeItem(SEXP sexp, SEXP ref_table, R_outpstream_t out) {
    if (useHashes(out)) {
        auto isInternable = internable(sexp);
        // Write whether we are serializing hash
        OutBool(out, isInternable);
        if (isInternable) {
            // Write hash instead of regular data
            assert(hashes.count(sexp) && "SEXP not interned");
            // Why does cppcheck think this is unused?
            // cppcheck-suppress unreadVariable
            auto hash = hashes.at(sexp);
            OutBytes(out, &hash, sizeof(hash));
            return;
        }
    }

    // Write regular data
    WriteItem(sexp, ref_table, out);
}

void UUIDPool::writeItem(SEXP sexp, ByteBuffer& buf, bool useHashes) {
    if (useHashes) {
        auto isInternable = internable(sexp);
        // Write whether we are serializing hash
        buf.putBool(isInternable);
        if (isInternable) {
            // Write hash instead of regular data
            assert(hashes.count(sexp) && "SEXP not interned");
            // Why does cppcheck think this is unused?
            // cppcheck-suppress unreadVariable
            auto hash = hashes.at(sexp);
            buf.putBytes((uint8_t*)&hash, sizeof(hash));
            return;
        }
    }

    // Write regular data
    serialize(sexp, buf, useHashes);
}

} // namespace rir