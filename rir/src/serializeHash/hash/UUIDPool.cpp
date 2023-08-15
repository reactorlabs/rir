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
#include "getConnected.h"
#include "runtime/log/printRirObject.h"
#include "runtime/rirObjectMagic.h"
#include "serializeHash/serialize/serialize.h"
#include "serializeHash/serialize/serializeR.h"
#include "utils/measuring.h"
#include <sys/stat.h>
#include <unistd.h>

// Can change this to log interned and uninterned hashes and pointers
#define LOG(stmt) if (pir::Parameter::PIR_LOG_INTERNING) stmt

namespace rir {

bool pir::Parameter::PIR_LOG_INTERNING =
    getenv("PIR_LOG_INTERNING") != nullptr &&
    strcmp(getenv("PIR_LOG_INTERNING"), "") != 0 &&
    strcmp(getenv("PIR_LOG_INTERNING"), "0") != 0 &&
    strcmp(getenv("PIR_LOG_INTERNING"), "false") != 0;

bool pir::Parameter::PIR_MEASURE_INTERNING =
    getenv("PIR_MEASURE_INTERNING") != nullptr &&
    strtol(getenv("PIR_MEASURE_INTERNING"), nullptr, 10);


bool pir::Parameter::PIR_PRINT_INTERNED_RIR_OBJECTS =
    getenv("PIR_PRINT_INTERNED_RIR_OBJECTS") != nullptr &&
    strcmp(getenv("PIR_PRINT_INTERNED_RIR_OBJECTS"), "") != 0 &&
    strcmp(getenv("PIR_PRINT_INTERNED_RIR_OBJECTS"), "0") != 0 &&
    strcmp(getenv("PIR_PRINT_INTERNED_RIR_OBJECTS"), "false") != 0;
const char* pir::Parameter::PIR_PRINT_INTERNED_RIR_OBJECTS_PATH =
    getenv("PIR_PRINT_INTERNED_RIR_OBJECTS") != nullptr &&
    strcmp(getenv("PIR_PRINT_INTERNED_RIR_OBJECTS"), "") != 0 &&
    strcmp(getenv("PIR_PRINT_INTERNED_RIR_OBJECTS"), "0") != 0 &&
    strcmp(getenv("PIR_PRINT_INTERNED_RIR_OBJECTS"), "false") != 0 &&
    strcmp(getenv("PIR_PRINT_INTERNED_RIR_OBJECTS"), "1") != 0 &&
    strcmp(getenv("PIR_PRINT_INTERNED_RIR_OBJECTS"), "true") != 0 ?
        getenv("PIR_PRINT_INTERNED_RIR_OBJECTS") : nullptr;
unsigned pir::Parameter::PIR_PRINT_INTERNED_RIR_OBJECTS_FREQUENCY =
    getenv("PIR_PRINT_INTERNED_RIR_OBJECTS_FREQUENCY") != nullptr
        ? strtol(getenv("PIR_PRINT_INTERNED_RIR_OBJECTS_FREQUENCY"), nullptr, 10)
        : 10;


bool UUIDPool::isInitialized = false;
std::unordered_map<UUID, SEXP> UUIDPool::interned;
std::unordered_map<SEXP, UUID> UUIDPool::hashes;
std::unordered_map<SEXP, SEXP> UUIDPool::nextToIntern;
std::unordered_map<SEXP, SEXP> UUIDPool::prevToIntern;
std::unordered_set<SEXP> UUIDPool::preserved;
static unsigned prettyPrintCount = 0;

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
    if (pir::Parameter::PIR_PRINT_INTERNED_RIR_OBJECTS_PATH) {
        // Create folder (not recursively) if it doesn't exist
        auto code = mkdir(pir::Parameter::PIR_PRINT_INTERNED_RIR_OBJECTS_PATH, 0777);
        if (code != 0 && errno != EEXIST) {
            std::cerr << "Could not create folder for PIR_PRINT_INTERNED_RIR_OBJECTS: "
                      << strerror(errno) << std::endl;
            std::abort();
        }
        // Also softlink rirPrettyGraph (HTML dependency) in the folder.
        // We do this even if the folder already exists, because the user may
        // have corrupted it.
        auto linkSource = getenv("PIR_PRETTY_GRAPH_DEPENDENCY_LOCATION");
        assert(linkSource && "PIR_PRETTY_GRAPH_DEPENDENCY_LOCATION should be set by the R executable, we need it to softlink rirPrettyGraph for the HTML prints");
        std::stringstream linkTarget;
        linkTarget << pir::Parameter::PIR_PRINT_INTERNED_RIR_OBJECTS_PATH << "/rirPrettyGraph";
        code = symlink(linkSource, linkTarget.str().c_str());
        if (code != 0 && errno != EEXIST) {
            std::cerr << "Could not symlink associated common styles/scripts for PIR_PRINT_INTERNED_RIR_OBJECTS: "
                      << strerror(errno) << std::endl;
            std::abort();
        }
    }
}

static void printInterned(SEXP sexp, const UUID& hash) {
    if (pir::Parameter::PIR_PRINT_INTERNED_RIR_OBJECTS_PATH) {
        // Create new file which is denoted by the current date and hash
        std::stringstream filePath;
        filePath << pir::Parameter::PIR_PRINT_INTERNED_RIR_OBJECTS_PATH << "/" << time(nullptr) << "-" << hash.str() << ".html";
        std::ofstream file(filePath.str());
        if (!file.is_open()) {
            std::cerr << "Could not open file for PIR_PRINT_INTERNED_RIR_OBJECTS: "
                      << strerror(errno) << std::endl;
            std::abort();
        }
        // Print HTML pretty graph to file
        printRirObject(sexp, file, RirObjectPrintStyle::PrettyGraph);
        // File closes automatically (RAII)
    } else {
        // Just print HTML pretty graph to stdout
        printRirObject(sexp, std::cout, RirObjectPrintStyle::PrettyGraph);
    }
}

void UUIDPool::printInternedIfNecessary(SEXP sexp, const UUID& hash) {
    if (pir::Parameter::PIR_PRINT_INTERNED_RIR_OBJECTS) {
        prettyPrintCount++;
        if (prettyPrintCount == pir::Parameter::PIR_PRINT_INTERNED_RIR_OBJECTS_FREQUENCY) {
            printInterned(sexp, hash);
            prettyPrintCount = 0;
        }
    }
}

void UUIDPool::unintern(SEXP e, bool isGettingGcd) {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_INTERNING, "UUIDPool.cpp: unintern", e, !isGettingGcd, [&] {
        Protect p(e);
        assert(hashes.count(e) && "SEXP not interned");

        // Remove hash
        auto hash = hashes.at(e);
        hashes.erase(e);
        if (!interned.count(hash)) {
            std::cerr << "WARNING: SEXP was interned, but the corresponding UUID is empty:\n"
                      << Print::dumpSexp(e) << "\n";
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
        std::cerr << "WARNING: preserved SEXP is supposedly getting gcd" << std::endl;
        return;
    }
    if (!hashes.count(e)) {
        // Can happen if we manually unintern, since we can't remove the finalizer
        return;
    }

    unintern(e, true);
}
#endif

SEXP UUIDPool::intern(SEXP e, const UUID& hash, bool preserve, bool expectHashToBeTheSame) {
    return Measuring::timeEventIf2(pir::Parameter::PIR_MEASURE_INTERNING, "UUIDPool.cpp: intern specific", e, expectHashToBeTheSame, [&] {
        Protect p(e);
        assert(internable(e));
        (void)expectHashToBeTheSame;

#ifdef DO_INTERN
        SLOWASSERT((!expectHashToBeTheSame || hashRoot(e) == hash) &&
                   "SEXP hash isn't deterministic or `hash` in `UUIDPool::intern(e, hash)` is wrong");
        if (interned.count(hash)) {
            // Reuse interned SEXP
            auto existing = interned.at(hash);
            assert(TYPEOF(e) == TYPEOF(existing) && "obvious hash collision (different types)");
            assert((TYPEOF(e) != EXTERNALSXP || rirObjectMagic(e) == rirObjectMagic(existing) || !expectHashToBeTheSame) &&
                   "obvious hash collision (different RIR types)");
            if (!hashes.count(e)) {
                // This SEXP is structurally-equivalent to the interned SEXP but not
                // the same (different pointers), so we must still record it
                LOG(std::cout << "Reuse intern: " << hash << " -> " << e << (expectHashToBeTheSame ? "\n" : " (recursive)\n"));
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
        disassembly[hash] = expectHashToBeTheSame
            ? printRirObject(e, RirObjectPrintStyle::Detailed)
            : "(couldn't be computed at the time it was interned)";
#endif

        // Sanity check in case the UUID changed
        if (hashes.count(e)) {
            std::cerr << "SEXP UUID changed from " << hashes.at(e) << " to "
                      << hash << ": " << e << "\n" << Print::dumpSexp(e)
                      << "\n";

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
            std::cerr << "WARNING: SEXP UUID changed. Unsound, and semantic "
                         "errors may occur if we rely on outdated behavior\n";
            // DON'T unintern because we or the compiler peer may request it
            // from the old hash.
        }

        // Do intern
        LOG(std::cout << "New intern: " << hash << " -> " << e << "\n");
#ifdef DEBUG_DISASSEMBLY
        LOG(std::cout << "Disassembly:\n" << disassembly[hash] << "\n");
#endif
        if (expectHashToBeTheSame) {
            printInternedIfNecessary(e, hash);
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

static bool isRecursivelySerializable(SEXP sexp) {
    if (auto c = Code::check(sexp)) {
        // Native code may be pending compilation, and if so, it can't yet be
        // serialized. Even if it's not pending, we need hashes to be consistent
        if (c->kind == Code::Kind::Native) {
            return false;
        }
    }
    return true;
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
                for (auto& s : connected) {
                    if (hashes.count(s.sexp) || !internable(s.sexp) ||
                        (s.isChild && isRecursivelySerializable(s.sexp))) {
                        continue;
                    }

                    intern(s.sexp, hashRoot(s.sexp), preserve);
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

SEXP UUIDPool::readItem(SEXP ref_table, R_inpstream_t in) {
    if (useHashes(in)) {
        // Read whether we are serializing hash
        auto readHashInstead = InBool(in);
        if (readHashInstead) {
            // Read hash instead of regular data,
            // then retrieve by hash from interned or peer
            UUID hash;
            InBytes(in, &hash, sizeof(hash));
            if (interned.count(hash)) {
                LOG(std::cout << "Retrieved by hash locally: " << hash << " -> "
                              << interned.at(hash) << "\n");
                return interned.at(hash);
            }
            if (CompilerClient::isRunning()) {
                LOG(std::cout << "Retrieving by hash from server: " << hash
                              << "\n");
                auto sexp = CompilerClient::retrieve(hash);
                if (sexp) {
                    intern(sexp, hash, false, false);
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
        if (auto result = tryReadHash(buf)) {
            return result;
        }
    }

    // Read regular data
    return deserialize(buf, SerialOptions{useHashes, false, false, false});
}

void UUIDPool::writeItem(SEXP sexp, __attribute__((unused)) bool isChild,
                         SEXP ref_table, R_outpstream_t out) {
    if (useHashes(out)) {
        auto writeHashInstead = internable(sexp);
        // Write whether we are serializing hash
        OutBool(out, writeHashInstead);
        if (writeHashInstead) {
            // Write hash instead of regular data
            assert(hashes.count(sexp) && "SEXP not interned");
            // Why does cppcheck think this is unused?
            // cppcheck-suppress unreadVariable
            auto hash = hashes.at(sexp);
            // Not necessarily true: sexp == interned[hash]. But the following are true...
            assert(interned.count(hash) &&
                   "SEXP interned with hash but the there's no \"main\" SEXP with that hash");
            assert((sexp == interned[hash] ||
                    TYPEOF(sexp) == TYPEOF(interned[hash])) &&
                   "sanity check failed: SEXP -> hash -> SEXP returned an obviously different SEXP (different SEXP types)");
            assert(
                (sexp == interned[hash] || TYPEOF(sexp) != EXTERNALSXP ||
                 rirObjectMagic(sexp) == rirObjectMagic(interned[hash])) &&
                "sanity check failed: SEXP -> hash -> SEXP returned an obviously different SEXP (different RIR types)");
            assert(hashes[interned[hash]] == hash &&
                   "sanity check failed: SEXP -> hash -> SEXP -> hash returned a different hash");
            assert(interned[hashes[interned[hash]]] == interned[hash] &&
                   "sanity check failed: SEXP -> hash -> SEXP -> hash -> SEXP returned a different SEXP");
            OutBytes(out, &hash, sizeof(hash));
            return;
        }
    }

    // Write regular data
    WriteItem(sexp, ref_table, out);
}

void UUIDPool::writeItem(SEXP sexp, __attribute__((unused)) bool isChild,
                         ByteBuffer& buf, bool useHashes) {
    if (useHashes) {
        if (tryWriteHash(sexp, buf)) {
            return;
        }
    }

    // Write regular data
    serialize(sexp, buf, SerialOptions{useHashes, false, false, false});
}

void UUIDPool::writeNullableItem(SEXP sexp, bool isChild, SEXP ref_table, R_outpstream_t out) {
    OutBool(out, sexp != nullptr);
    if (sexp) {
        writeItem(sexp, isChild, ref_table, out);
    }
}

SEXP UUIDPool::readNullableItem(SEXP ref_table, R_inpstream_t in) {
    auto isNotNull = InBool(in);
    if (isNotNull) {
        return readItem(ref_table, in);
    } else {
        return nullptr;
    }
}

// TODO: Some refactoring (see TODO in serialize.cpp as well), lots of duplicate
//  code and we probably shouldn't just return nullptr iff we're on server, but
//  instead use a separate function.
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
        auto hash = hashes.at(sexp);
        buf.putBytes((uint8_t*)&hash, sizeof(hash));
    }
    return writeHash;
}

SEXP UUIDPool::tryReadHash(ByteBuffer& buf) {
    auto readHashInstead = buf.getBool();
    if (readHashInstead) {
        // Read hash instead of regular data,
        // then retrieve by hash from interned or peer
        UUID hash;
        buf.getBytes((uint8_t*)&hash, sizeof(hash));
        if (interned.count(hash)) {
            LOG(std::cout << "Retrieved by hash locally: " << hash << " -> "
                          << interned.at(hash) << "\n");
            return interned.at(hash);
        }
        if (CompilerClient::isRunning()) {
            LOG(std::cout << "Retrieving by hash from server: " << hash
                          << "\n");
            auto sexp = CompilerClient::retrieve(hash);
            if (sexp) {
                intern(sexp, hash, false, false);
                return sexp;
            }
            Rf_error("SEXP deserialized from hash which we don't have, and server also doesn't have it");
        } else if (CompilerServer::isRunning()) {
            LOG(std::cout << "Retrieving by hash from client: " << hash << "\n");
            auto sexp = CompilerServer::retrieve(hash);
            if (sexp) {
                intern(sexp, hash, true, false);
                return sexp;
            }
            LOG(std::cout << "SEXP deserialized from hash which we don't have, and client also doesn't have it");
            return R_NilValue;
        }
        Rf_error("SEXP deserialized from hash which we don't have, and no server");
    }
    return nullptr;
}

} // namespace rir