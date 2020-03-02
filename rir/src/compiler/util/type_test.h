#ifndef PIR_TYPE_TEST
#define PIR_TYPE_TEST

#include "../pir/instruction.h"

namespace rir {
namespace pir {

class TypeTest {
  public:
    struct Info {
        PirType result;
        Instruction* test;
        bool expectation;
        rir::Code* srcCode;
        Opcode* origin;
    };
    static void Create(Value* i, const Instruction::TypeFeedback& feedback,
                       const PirType& suggested, const PirType& required,
                       const std::function<void(Info)>& action,
                       const std::function<void()>& failed) {
        auto expected = i->type & feedback.type;

        if (i->type.isA(expected))
            return;

        if (expected.isVoid() || expected.maybeLazy()) {
            if (i->type.isA(required))
                return;
            else
                return failed();
        }

        assert(feedback.origin);
        // First try to refine the type
        if (!expected.maybeObj() && // TODO: Is this right?
            (expected.noAttribs().isA(RType::integer) ||
             expected.noAttribs().isA(RType::real) ||
             expected.noAttribs().isA(RType::logical))) {
            // Do not check for non-NaN unless the value is also scalar.
            // Checking for non-NaN in vectors takes too long (requires
            // iterating every element) and rarely helps us
            if (!expected.isScalar() && !expected.maybeNan()) {
                expected = expected.orNan();
            }

            return action({expected, new IsType(expected, i), true,
                           feedback.srcCode, feedback.origin});
        }

        // Second try to test for object-ness, or attribute-ness.
        // Let's only do that if required, to avoid testing a non-object for
        // non-attribute. ie. convert a val' to a val'', which is technically a
        // refinement, but hardly ever useful.
        if (i->type.isA(suggested))
            return;

        auto checkFor = i->type.notLazy().noAttribs();
        if (expected.isA(checkFor)) {
            assert(!expected.maybeObj());
            assert(!expected.maybeHasAttrs());
            return action({checkFor, new IsType(checkFor, i), true,
                           feedback.srcCode, feedback.origin});
        }

        checkFor = i->type.notLazy().notObject();
        if (expected.isA(checkFor)) {
            assert(!expected.maybeObj());
            return action({checkFor, new IsType(checkFor, i), true,
                           feedback.srcCode, feedback.origin});
        }

        if (i->type.isA(required))
            return;
        failed();
    }
};

} // namespace pir
} // namespace rir

#endif
