#include "hashAst.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "compiler/parameter.h"
#include "utils/measuring.h"
#include <stack>
#include <unordered_map>
#include <vector>

namespace rir {

// Assumes all symbols are never freed (currently yes because they're in a pool,
// and it makes sense since they're all AST nodes that they're persistent)
static std::unordered_map<SEXP, UUID> hashCache;

inline static void
serializeAstVector(SEXP s, const std::function<void(size_t)>& serializeElem) {
    // These haven't caused problems yet, but maybe need to be handled?
    // assert(ATTRIB(s) == R_NilValue && "unexpected attributes in AST");
    // assert(!OBJECT(s) && "unexpected object in AST");
    // assert(!IS_S4_OBJECT(s) && "unexpected S4 object in AST");
    // assert(!ALTREP(s) && "unexpected altrep in AST");
    size_t length = LENGTH(s);
    for (size_t i = 0; i < length; ++i) {
        serializeElem(i);
    }
}

/// Manual tagged union simulating a stack frame of a function which takes an
/// SEXP and creates a UUID from hashing.
struct Frame {
    bool started = false;
    Frame* parent;
    unsigned parentIdx;
    SEXP sexp;
    UUID::Hasher hasher;
    std::vector<UUID> children;

    explicit Frame(Frame* parent, SEXP sexp)
        : started(false), parent(parent),
          parentIdx(parent ? parent->children.size() : 0), sexp(sexp), hasher(),
          children() {}

    UUID finalize() {
        for (auto child : children) {
            hasher.hashBytesOf(child);
        }
        return hasher.finalize();
    }
};
using Stack = std::stack<Frame>;

static void hashNewAst(SEXP s, UUID::Hasher& hasher,
                       std::function<void(SEXP)> recurse) {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashAst.cpp: hashNewAst", s, [&]{
        // 2 fastcases below mean that every SEXP on the stack is not yet hashed,
        // unless the symbol is a shared global, in which case it's trivial. So we
        // don't bother checking if it's in hashCache

        hasher.hashBytesOf<int>(TYPEOF(s));
        switch (TYPEOF(s)) {
        case NILSXP: {
            break;
        }

        case SYMSXP: {
            if (s == R_UnboundValue) {
                hasher.hashBytesOf<int>(0);
            } else if (s == R_MissingArg) {
                hasher.hashBytesOf<int>(1);
            } else if (s == R_RestartToken) {
                hasher.hashBytesOf<int>(2);
            } else if (s == symbol::expandDotsTrigger) {
                hasher.hashBytesOf<int>(3);
            } else {
                hasher.hashBytesOf<int>(4);
                const char* name = CHAR(PRINTNAME(s));
                hasher.hashBytesOf<size_t>(strlen(name));
                hasher.hashBytes((const void*)name, strlen(name));
            }
            break;
        }

        case LISTSXP: {
            hasher.hashBytesOf<int>(Rf_length(s));
            for (SEXP cur = s; cur != R_NilValue; cur = CDR(cur)) {
                recurse(CAR(cur));
            }
            break;
        }

        case CLOSXP: {
            assert(false && "unexpected CLOSXP in AST");
        }

        case ENVSXP: {
            assert(false && "unexpected ENVSXP in AST");
        }

        case PROMSXP: {
            assert(false && "unexpected PROMSXP in AST");
        }

        case LANGSXP: {
            hasher.hashBytesOf<int>(Rf_length(s));
            for (SEXP cur = s; cur != R_NilValue; cur = CDR(cur)) {
                recurse(CAR(cur));
            }
            break;
        }

        case SPECIALSXP:
        case BUILTINSXP: {
            hasher.hashBytesOf<int>(getBuiltinNr(s));
            break;
        }

        case CHARSXP: {
            if (s == NA_STRING) {
                hasher.hashBytesOf<int>(0);
            } else {
                hasher.hashBytesOf<int>(1);
                const char* chr = CHAR(s);
                hasher.hashBytesOf<size_t>(strlen(chr));
                hasher.hashBytes((const void*)chr, strlen(chr));
            }
            break;
        }

        case LGLSXP: {
            serializeAstVector(s, [&](int i) {
                hasher.hashBytesOf<int>(LOGICAL(s)[i]);
            });
            break;
        }

        case INTSXP: {
            serializeAstVector(s, [&](int i) {
                hasher.hashBytesOf<int>(INTEGER(s)[i]);
            });
            break;
        }

        case REALSXP: {
            serializeAstVector(s, [&](int i) {
                hasher.hashBytesOf<double>(REAL(s)[i]);
            });
            break;
        }

        case CPLXSXP: {
            serializeAstVector(s, [&](int i) {
                hasher.hashBytesOf<Rcomplex>(COMPLEX(s)[i]);
            });
            break;
        }

        case STRSXP: {
            serializeAstVector(s, [&](int i) {
                const char* chr = CHAR(STRING_ELT(s, i));
                hasher.hashBytesOf<size_t>(strlen(chr));
                hasher.hashBytes((const void*)chr, strlen(chr));
            });
            break;
        }

        case VECSXP: {
            serializeAstVector(s, [&](int i) {
                recurse(VECTOR_ELT(s, i));
            });
            break;
        }

        case RAWSXP: {
            serializeAstVector(s, [&](int i) {
                hasher.hashBytesOf<Rbyte>(RAW(s)[i]);
            });
            break;
        }

        case EXTERNALSXP: {
            assert(false && "unexpected RIR object in AST");
        }

        case DOTSXP:
        case ANYSXP:
        case EXPRSXP:
        case BCODESXP:
        case EXTPTRSXP:
        case WEAKREFSXP:
        case S4SXP:
        case NEWSXP:
        case FREESXP:
        default: {
            assert(false && "unexpected type in AST");
        }
        }
    });
}

UUID hashAst(SEXP root) {
    return Measuring::timeEventIf<UUID>(pir::Parameter::PIR_MEASURE_SERIALIZATION, "hashAst", root, [&]{
        // Fastcase
        if (hashCache.count(root)) {
            return hashCache.at(root);
        }

        // Simulate a recursive call chain. Is this better or even as good letting
        // the compiler do it? (There are a few differences in semantics from
        // regular recursion which don't affect hash quality, like putting all SEXPs
        // at the end)
        Stack stack;
        stack.emplace(nullptr, root);
        while (true) {
            auto& top = stack.top();
            // Hash this SEXP, changing the hasher and pushing not-started recursive
            // calls onto the stack
            top.started = true;
            hashNewAst(top.sexp, top.hasher, [&](SEXP next){
                if (hashCache.count(next)) {
                    // Fastcase
                    top.children.push_back(hashCache.at(next));
                } else {
                    stack.emplace(&top, next);
                    // Push null UUID to be filled in later. Need to push after
                    // emplace because the emplaced Frame uses the vector's size
                    // as its parent index
                    top.children.emplace_back();
                }
            });

            // If this SEXP pushed not-started recursive calls we have to
            // process them. If not, we can finish this call, and then finish
            // outer calls which also have no more not-started recursive calls.
            while (stack.top().started) {
                auto parent = stack.top().parent;
                auto parentIdx = stack.top().parentIdx;
                auto sexp = stack.top().sexp;
                auto hash = stack.top().finalize();
                hashCache[sexp] = hash;
                stack.pop();
                if (parent) {
                    // The SEXP's hash is part of the parent's hash.
                    parent->children[parentIdx] = hash;
                } else {
                    // Done
                    assert(parentIdx == 0);
                    return hash;
                }
            }
        }
    });
}

} // namespace rir
