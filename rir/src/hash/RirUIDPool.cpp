//
// Created by Jakob Hain on 6/1/23.
//

#include "RirUIDPool.h"
#include "CompilerClient.h"
#include "CompilerServer.h"
#include "R/SerialAst.h"
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

std::unordered_map<UUID, SmallSet<SEXP>> RirUIDPool::interned;
std::unordered_map<SEXP, UUID> RirUIDPool::hashes;
std::unordered_set<SEXP> RirUIDPool::preserved;

#ifdef DEBUG_DISASSEMBLY
static std::unordered_map<UUID, std::string> disassembly;
#endif

static bool internable(SEXP e) {
    return TYPEOF(e) == EXTERNALSXP;
}

static auto smallHashEq(const UUID& small) {
    return [&](SEXP e) { return smallHashSexp(e) == small; };
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

void RirUIDPool::uninternGcd(SEXP e) {
    // There seems to be a bug somewhere where R is calls finalizer on the wrong
    // object, or calls it twice...
    if (preserved.count(e)) {
        Rf_warning("WARNING: preserved SEXP is supposedly getting gcd");
        Rf_PrintValue(e);
        return;
    }
    if (!hashes.count(e)) {
        Rf_warning("WARNING: SEXP getting gcd is supposedly never interned");
        Rf_PrintValue(e);
        return;
    }

    // Remove hash
    auto hash = hashes.at(e);
    hashes.erase(e);

    auto& similar = interned[hash];
    assert(similar.count(e) && "SEXP was interned because it has a SEXP->UUID entry, but the corresponding UUID->SEXP entry is missing");
    similar.erase(e);

    LOG(std::cout << "Remove intern: " << hash << " -> " << e << "\n");
}
#endif

SEXP RirUIDPool::intern(SEXP e, const RirUID& hash, bool preserve, bool expectHashToBeTheSame) {
    return Measuring::timeEventIf<SEXP>(pir::Parameter::PIR_MEASURE_INTERNING, "specific intern", e, [&] {
        assert(internable(e));
        (void)expectHashToBeTheSame;

#ifdef DO_INTERN
        PROTECT(e);
        SLOWASSERT((!expectHashToBeTheSame || hashSexp(e) == hash) &&
                   "SEXP hash isn't deterministic or `hash` in `RirUIDPool::intern(e, hash)` is wrong");
        UNPROTECT(1);
        auto& similar = interned[hash.big];
        auto existing = std::find_if(similar.begin(), similar.end(),
                                     smallHashEq(hash.small));
        if (existing != similar.end()) {
            // Reuse interned SEXP
            if (!hashes.count(e)) {
                // This SEXP is structurally-equivalent to the interned SEXP but not the same (different pointers), so we must still record it. Since we are using SmallSet, we can insert it after and then it will only be used if the previous SEXP changes its RirUID or gets gcd and uninterned.
                LOG(std::cout << "Reuse intern: " << hash << " -> " << e
                              << "\n");
                similar.insert(e);
                hashes[e] = hash.big;
                if (!preserve) {
                    registerFinalizerIfPossible(e, uninternGcd);
                }
            }
            e = *existing;
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
                disassembly[hash.big] = s.str();
            } else if (Function::check(e)) {
                auto fun = Function::unpack(e);
                if (!Code::check(EXTERNALSXP_ENTRY(fun->container(), 0))) {
                    std::cerr << "Tried to serialize function during its construction: "
                              << e << "\n";
                    Rf_PrintValue(e);
                    assert(false);
                }
                std::stringstream s;
                fun->print(s, true);
                disassembly[hash.big] = s.str();
            } else if (Code::check(e)) {
                auto code = Code::unpack(e);
                std::stringstream s;
                code->print(s, true);
                disassembly[hash.big] = s.str();
            }
        } else {
            disassembly[hash.big] =
                "(recursively interned, can't debug this way)";
        }
#endif

        // Sanity check in case the big UUID changed
        if (hashes.count(e) && hashes.at(e) != hash.big) {
            std::cerr << "SEXP UUID changed from " << hashes.at(e) << " to "
                      << hash.big << ": " << e << "\n";
            Rf_PrintValue(e);

#ifdef DEBUG_DISASSEMBLY
            auto oldDisassembly = disassembly[hashes.at(e)];
            auto newDisassembly = disassembly[hash.big];
            if (oldDisassembly != newDisassembly) {
                std::cerr << "note: disassembly changed from:\n"
                          << oldDisassembly << "\nto:\n"
                          << newDisassembly << "\n";
            } else {
                std::cerr << "note: disassembly:\n" << oldDisassembly << "\n";
            }
#endif

            assert(false);
        }

        // Do intern
        LOG(std::cout << "New intern: " << hash << " -> " << e << "\n");
        similar.insert(e);
        hashes[e] = hash.big;

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

SEXP RirUIDPool::intern(SEXP e, bool recursive, bool preserve) {
#ifdef DO_INTERN
    return Measuring::timeEventIf<SEXP>(pir::Parameter::PIR_MEASURE_INTERNING, "intern", e, [&] {
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
            auto hash = hashSexp(e, connected);
            auto ret = internable(e) ? intern(e, hash, preserve) : e;
            while ((e = connected.pop())) {
                assert(internable(e));
                if (hashes.count(e)) {
                    continue;
                }

                intern(e, hashSexp(e), preserve);
            }
            return ret;
        } else {
            return internable(e) ? intern(e, hashSexp(e), preserve) : e;
        }
    });
#else
    return e;
#endif
}

SEXP RirUIDPool::get(const RirUID& hash) {
#ifdef DO_INTERN
    auto& similar = interned[hash.big];
    auto existing = std::find_if(similar.begin(), similar.end(), smallHashEq(hash.small));
    if (existing != similar.end()) {
        return *existing;
    }
#endif
    return nullptr;
}

SEXP RirUIDPool::getAny(const UUID& bigHash) {
#ifdef DO_INTERN
    auto& similar = interned[bigHash];
    if (!similar.empty()) {
        return *similar.begin();
    }
#endif
    return nullptr;
}

RirUID RirUIDPool::getHash(SEXP sexp) {
#ifdef DO_INTERN
    if (hashes.count(sexp)) {
        return {hashes.at(sexp), smallHashSexp(sexp)};
    }
#endif
    return {};
}

SEXP RirUIDPool::readItem(SEXP ref_table, R_inpstream_t in) {
    if (useHashes(in)) {
        // Read whether we are serializing hash
        auto isInternable = InBool(in);
        if (isInternable) {
            // Read hash instead of regular data,
            // then retrieve by hash from interned or server
            RirUID hash;
            InBytes(in, &hash, sizeof(hash));
            auto& similar = interned[hash.big];
            auto existing = std::find_if(similar.begin(), similar.end(), smallHashEq(hash.small));
            if (existing != similar.end()) {
                LOG(std::cout << "Retrieved by hash locally: " << hash << "\n");
                return *existing;
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

SEXP RirUIDPool::readItem(ByteBuffer& buf, bool useHashes) {
    if (useHashes) {
        // Read whether we are serializing hash
        auto isInternable = buf.getBool();
        if (isInternable) {
            // Read hash instead of regular data,
            // then retrieve by hash from interned or server
            RirUID hash;
            buf.getBytes((uint8_t*)&hash, sizeof(hash));
            auto& similar = interned[hash.big];
            auto existing = std::find_if(similar.begin(), similar.end(), smallHashEq(hash.small));
            if (existing != similar.end()) {
                LOG(std::cout << "Retrieved by hash locally: " << hash << "\n");
                return *existing;
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

void RirUIDPool::writeItem(SEXP sexp, SEXP ref_table, R_outpstream_t out) {
    assert(!connected(out) || !useHashes(out));
    auto wl = connected(out);
    if (wl && internable(sexp) && !hashes.count(sexp)) {
        wl->insert(sexp);
    }
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

void RirUIDPool::writeItem(SEXP sexp, ByteBuffer& buf, bool useHashes) {
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

void RirUIDPool::writeAst(SEXP src, SEXP refTable, R_outpstream_t out) {
    if (isHashing(out)) {
        auto uuid = hashAst(src);
        OutBytes(out, (const char*)&uuid, sizeof(uuid));
    } else {
        writeItem(src, refTable, out);
    }
}

} // namespace rir