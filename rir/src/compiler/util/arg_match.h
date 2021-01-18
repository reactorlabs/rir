#ifndef PIR_ARGUMENT_MATCHER_H
#define PIR_ARGUMENT_MATCHER_H

#include "../pir/pir.h"
#include "runtime/ArglistOrder.h"

namespace rir {
namespace pir {

class DotsList;
struct ArgumentMatcher {
    struct Arg {
        SEXP name;
        Value* value;
        size_t index;
        int8_t used;
    };
    // cppcheck-suppress uninitMemberVar
    struct ActualArg {
        enum { Missing, Index, Dotslist } kind;
        Arg arg;
        ActualArg() : kind(Missing) {}
        explicit ActualArg(const Arg& a) : kind(Index), arg(a) {}
        static ActualArg Dots() {
            ActualArg a;
            a.kind = Dotslist;
            return a;
        }
    };
    struct GivenArglistAccessor {
        std::function<size_t()> size;
        std::function<Value*(size_t)> getArg;
        std::function<SEXP(size_t)> getName;
    };
    using MaybeDots = std::function<void(DotsList*)>;
    static bool reorder(MaybeDots maybeDots, SEXP formals,
                        GivenArglistAccessor given,
                        std::vector<Value*>& matchedArgs,
                        ArglistOrder::CallArglistOrder& argOrderOrig);
};

} // namespace pir
} // namespace rir

#endif
