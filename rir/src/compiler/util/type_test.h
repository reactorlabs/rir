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
                       const std::function<void(Info)>& action,
                       const std::function<void()>& failed) {
        auto possible = i->type & feedback.type;

        if (i->type.isA(possible))
            return;

        if (possible.isVoid())
            return failed();

        assert(feedback.origin);
        if (possible.isA(PirType(RType::integer).orPromiseWrapped()) ||
            possible.isA(PirType(RType::real).orPromiseWrapped()) ||
            possible.isA(PirType(RType::logical).orPromiseWrapped())) {
            return action({possible, new IsType(possible, i), true,
                           feedback.srcCode, feedback.origin});
        }

        if (possible.maybeLazy())
            return failed();

        if (i->type.maybeHasAttrs() && !possible.maybeHasAttrs()) {
            auto expect = i->type.notLazy().noAttribs();
            assert(!possible.maybeObj());
            assert(!possible.maybeHasAttrs());
            return action({expect, new IsType(expect, i), true,
                           feedback.srcCode, feedback.origin});
        }

        if (i->type.maybeObj() && !possible.maybeObj()) {
            auto expect = i->type.notLazy().notObject();
            assert(!possible.maybeObj());
            return action({expect, new IsType(expect, i), true,
                           feedback.srcCode, feedback.origin});
        }

        failed();
    }
};

} // namespace pir
} // namespace rir

#endif
