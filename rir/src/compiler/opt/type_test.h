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
        FeedbackOrigin feedbackOrigin;
    };
    static void Create(Value* i, const TypeFeedback& feedback,
                       const PirType& suggested, const PirType& required,
                       const std::function<void(Info)>& action,
                       const std::function<void()>& failed,
                       bool debug = false) {

        auto expected = i->type & feedback.type;

        // NA checks are only possible on scalars
        if (i->type.maybeNAOrNaN() && !expected.maybeNAOrNaN() &&
            !expected.isSimpleScalar())
            expected = expected.orNAOrNaN();

        if (i->type.isA(expected) && i->type.isA(required)) {
            if (debug)
                std::cerr
                    << "SPEC not needed - type is expected and required \n";
            assert(false);
            return;
        }

        if (expected.isVoid() || expected.maybeLazy()) {
            if (i->type.isA(required)) {
                if (debug)
                    std::cerr << "SPEC not needed - intersection is void or "
                                 "lazy but required fulfilled\n";
                return;
            } else {
                if (debug)
                    std::cerr << "SPEC FAILED - interesection is void or lazy "
                                 "and required not fulfilled\n";
                return failed();
            }
        }

        if (!feedback.feedbackOrigin.hasSlot())
            return failed();

        assert(feedback.feedbackOrigin.hasSlot());
        // First try to refine the type
        if (!expected.maybeObj() && // TODO: Is this right?
            (expected.noAttribsOrObject().isA(RType::integer) ||
             expected.noAttribsOrObject().isA(RType::real) ||
             expected.noAttribsOrObject().isA(RType::logical))) {
            if (debug)
                std::cerr << "SPEC SUCCEDED - main\n";
            return action({expected, new IsType(expected, i), true,
                           feedback.feedbackOrigin});
        }

        if (debug)
            std::cerr << "SPEC - first try failed\n";

        // Second try to test for object-ness, or attribute-ness.
        // Let's only do that if required, to avoid testing a non-object for
        // non-attribute. ie. convert a val' to a val'', which is technically a
        // refinement, but hardly ever useful.
        if (i->type.isA(suggested)) {
            if (debug)
                std::cerr << "SPEC not needed - suggested fulfilled\n";
            return;
        }

        auto checkFor = i->type.notLazy().noAttribsOrObject();
        if (expected.isA(checkFor)) {
            assert(!expected.maybeObj(false));
            assert(!expected.maybeHasAttrs());

            if (debug)
                std::cerr << "SPEC SUCCEDED - objectness 1\n";

            return action({checkFor, new IsType(checkFor, i), true,
                           feedback.feedbackOrigin});
        }

        if (debug)
            std::cerr << "SPEC - second try objectness1 failed\n";

        checkFor = i->type.notLazy().notObject();
        if (expected.isA(checkFor)) {
            assert(!expected.maybeObj(false));

            if (debug)
                std::cerr << "SPEC SUCCEDED - objectness 2\n";

            return action({checkFor, new IsType(checkFor, i), true,
                           feedback.feedbackOrigin});
        }

        if (debug)
            std::cerr << "SPEC - second try objectness2 failed\n";

        if (i->type.isA(required)) {

            if (debug)
                std::cerr
                    << "SPEC not needed - end. required fulfilled anyways\n";
            return;
        }

        if (debug) {
            std::cerr << "SPEC FAILED - end\n";
            std::cerr << "type: " << i->type << " - expected: " << expected
                      << " -  required: " << required << "\n";
        }

        failed();
    }
};

} // namespace pir
} // namespace rir

#endif
