#include "arg_match.h"
#include "R/Protect.h"
#include "compiler/pir/pir_impl.h"
#include "utils/Pool.h"

namespace rir {
namespace pir {

// cppcheck-suppress constParameter
bool ArgumentMatcher::reorder(MaybeDots maybeDots, SEXP formals,
                              GivenArglistAccessor given,
                              std::vector<Value*>& matchedArgs,
                              ArglistOrder::CallArglistOrder& argOrderOrig) {
    // Build up the list of supplied args. We store the argument, its name, its
    // index and a used flag.
    std::vector<Arg> supplied;
    for (size_t i = 0; i < given.size(); i++) {
        supplied.push_back({given.getName(i), given.getArg(i), i, 0});
    }

    // The following code is a rewrite of Rf_machArgs from main/match.c,
    // where errors have been replaced by 'return false'.

    RList formals_(formals);
    std::vector<ActualArg> actuals(formals_.length());
    std::vector<int8_t> fargused(actuals.size(), 0);

    // First pass: exact matches by tag
    // Grab matched arguments and check for multiple exact matches.

    size_t fai = 0;
    for (auto f = formals_.begin(); f != RList::end(); ++f, ++fai) {
        if (f.hasTag() && f.tag() != R_DotsSymbol) {
            auto ftag = CHAR(PRINTNAME(f.tag()));
            for (auto& s : supplied) {
                if (s.name != R_NilValue) {
                    auto stag = CHAR(PRINTNAME(s.name));
                    if (!strcmp(ftag, stag)) {
                        if (fargused[fai] == 2 || s.used == 2)
                            return false;
                        actuals[fai] = ActualArg(s);
                        fargused[fai] = s.used = 2;
                    }
                }
            }
        }
    }

    // Second pass: partial matches based on tags
    // An exact match is required after first ...
    // The location of the first ... is saved in "dotsi"

    size_t dotsi = 0;
    bool havedots = false;
    fai = 0;
    for (auto f = formals_.begin(); f != RList::end(); ++f, ++fai) {
        if (fargused[fai] == 0) {
            if (f.tag() == R_DotsSymbol && !havedots) {
                // Record where ... value goes
                dotsi = fai;
                havedots = true;
            } else {
                for (auto& s : supplied) {
                    if (s.used != 2 && s.name != R_NilValue &&
                        pmatch(f.tag(), s.name, havedots ? TRUE : FALSE))
                        return false;
                }
            }
        }
    }

    // Third pass: matches based on order
    // All args specified in tag=value form have now been matched. If we find
    // ... we gobble up all the remaining args. Otherwise we bind untagged
    // values in order to any unmatched formals.

    fai = 0;
    auto f = formals_.begin();
    auto s = supplied.begin();
    while (f != RList::end() && s != supplied.end()) {
        if (f.tag() == R_DotsSymbol) {
            // Skip ... matching until all tags done
            break;
        } else if (actuals[fai].kind != ActualArg::Missing) {
            // Already matched by tag, skip to next formal
            ++f;
            ++fai;
        } else if (s->used || s->name != R_NilValue) {
            // This value used or tagged, skip to next value
            // The second test above is needed because we shouldn't consider
            // tagged values for positional matches.
            // The formal being considered remains the same
            ++s;
        } else {
            // We have a positional match
            actuals[fai] = ActualArg(*s);
            s->used = 1;
            ++s;
            ++f;
            ++fai;
        }
    }

    std::vector<ActualArg> dots;
    if (havedots) {
        // Gobble up all unused actuals
        for (auto& s : supplied)
            if (!s.used)
                dots.push_back(ActualArg(s));
        if (!dots.empty())
            actuals[dotsi] = ActualArg::Dots();
    } else {
        // Check that all arguments are used
        for (auto& s : supplied)
            if (!s.used)
                return false;
    }

    // Passing ... is impossible to statically match, we first need to figure
    // out what's in the ... list.
    for (auto a : actuals) {
        if (a.kind == ActualArg::Index) {
            if (ExpandDots::Cast(a.arg.value))
                return false;
        } else if (a.kind == ActualArg::Dotslist) {
            for (auto d : dots) {
                if (ExpandDots::Cast(d.arg.value))
                    return false;
            }
        }
    }

    matchedArgs.clear();
    argOrderOrig.resize(given.size());
    size_t pos = 0;
    f = formals_.begin();
    constexpr bool DEBUG = true;
    if (DEBUG)
        std::cout << "MATCHED : ";
    for (auto a : actuals) {
        if (DEBUG)
            std::cout << CHAR(PRINTNAME(f.tag())) << "=";
        if (a.kind == ActualArg::Index) {
            argOrderOrig[a.arg.index] =
                ArglistOrder::encodeArg(pos++, a.arg.name != R_NilValue);
            matchedArgs.push_back(a.arg.value);
            if (DEBUG)
                a.arg.value->printRef(std::cout);
        } else if (a.kind == ActualArg::Missing) {
            pos++;
            matchedArgs.push_back(MissingArg::instance());
            if (DEBUG)
                std::cout << "miss";
        } else if (a.kind == ActualArg::Dotslist) {
            // We pass individual arguments, but the callee receives them as
            // `...` list. Therefore we gobble all arguments up into a dotslist
            // and pass them as a single arg.
            auto conv = new DotsList;
            if (DEBUG)
                std::cout << "(";
            for (auto d = dots.begin(); d != dots.end(); ++d) {
                assert(d->kind == ActualArg::Index);
                argOrderOrig[d->arg.index] =
                    ArglistOrder::encodeArg(pos++, d->arg.name != R_NilValue);
                conv->addInput(d->arg.name, d->arg.value);
                if (DEBUG) {
                    if (d->arg.name != R_NilValue)
                        std::cout << CHAR(PRINTNAME(d->arg.name)) << "=";
                    d->arg.value->printRef(std::cout);
                    if (d + 1 != dots.end())
                        std::cout << ", ";
                }
            }
            if (DEBUG)
                std::cout << ")";
            matchedArgs.push_back(conv);
            maybeDots(conv);
        } else {
            assert(false);
        }
        ++f;
        if (DEBUG && f != RList::end())
            std::cout << ", ";
    }
    if (DEBUG) {
        std::cout << " { ";
        for (auto a : argOrderOrig)
            std::cout << ArglistOrder::decodeArg(a)
                      << (ArglistOrder::isArgNamed(a) ? "n " : " ");
        std::cout << "}\n";
    }

    while (!matchedArgs.empty() && matchedArgs.back() == MissingArg::instance())
        matchedArgs.pop_back();

    return true;
}

} // namespace pir
} // namespace rir
