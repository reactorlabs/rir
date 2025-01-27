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

        bool defaultFeedback;
        bool typeFeedbackNarrowedWithStaticType;
        bool exactMatch;
    };
    static void Create(Value* i, const TypeFeedback& feedback,
                       const PirType& suggested, const PirType& required,
                       const std::function<void(Info)>& action,
                       const std::function<void()>& failed) {
        auto expected = i->type & feedback.type;

        // NA checks are only possible on scalars
        if (i->type.maybeNAOrNaN() && !expected.maybeNAOrNaN() &&
            !expected.isSimpleScalar())
            expected = expected.orNAOrNaN();

        if (i->type.isA(expected) && i->type.isA(required))
            return;

        if (expected.isVoid() || expected.maybeLazy()) {
            if (i->type.isA(required))
                return;
            else
                return failed();
        }

        if (!feedback.feedbackOrigin.hasSlot())
            return failed();

        assert(feedback.feedbackOrigin.hasSlot());

        auto typeFeedbackNarrowedWithStaticType = expected != feedback.type;

        // First try to refine the type
        if (!expected.maybeObj() && // TODO: Is this right?
            (expected.noAttribsOrObject().isA(RType::integer) ||
             expected.noAttribsOrObject().isA(RType::real) ||
             expected.noAttribsOrObject().isA(RType::logical))) {

            return action({expected, new IsType(expected, i), true,
                           feedback.feedbackOrigin, feedback.defaultFeedback,
                           typeFeedbackNarrowedWithStaticType, true});
        }

        // Second try to test for object-ness, or attribute-ness.
        // Let's only do that if required, to avoid testing a non-object for
        // non-attribute. ie. convert a val' to a val'', which is technically a
        // refinement, but hardly ever useful.
        if (i->type.isA(suggested))
            return;

        auto checkFor = i->type.notLazy().noAttribsOrObject();
        assert(i->type != checkFor);
        if (expected.isA(checkFor)) {
            assert(!expected.maybeObj());
            assert(!expected.maybeHasAttrs());

            if (!feedback.defaultFeedback) {
                std::cerr << "WIDENED notLazy notAttribOrObj "
                             "*************************************************"
                             "**************"
                          << "\n";
                std::cerr << "type: " << i->type << " - expected: " << expected
                          << "\n"
                          << "checkFor: " << checkFor << "\n\n\n";
            }

            return action({checkFor, new IsType(checkFor, i), true,
                           feedback.feedbackOrigin, feedback.defaultFeedback,
                           typeFeedbackNarrowedWithStaticType, false

            });
        }

        checkFor = i->type.notLazy().notObject();
        assert(i->type != checkFor);
        if (expected.isA(checkFor)) {
            assert(!expected.maybeObj());

            if (!feedback.defaultFeedback) {
                std::cerr << "WIDENED notLazy notObj "
                             "*************************************************"
                             "**************"
                          << "\n";
                std::cerr << "type: " << i->type << " - expected: " << expected
                          << "\n"
                          << "checkFor: " << checkFor << "\n\n\n";
            }

            return action({checkFor, new IsType(checkFor, i), true,
                           feedback.feedbackOrigin, feedback.defaultFeedback,
                           typeFeedbackNarrowedWithStaticType, false});
        }

        if (i->type.isA(required))
            return;
        failed();
    }
};

} // namespace pir
} // namespace rir

#endif
