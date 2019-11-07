#include "insert_cast.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"

namespace rir {
namespace pir {

pir::Instruction* InsertCast::cast(pir::Value* v, PirType t, Value* env) {
    if (v->type.maybePromiseWrapped() && !t.maybePromiseWrapped()) {
        return new pir::Force(v, env);
    }
    if (v->type.isRType() && !v->type.maybeLazy() && t == NativeType::test) {
        return new pir::AsTest(v);
    }
    if (!v->type.isA(RType::closure) && t == RType::closure) {
        return new pir::ChkClosure(v);
    }

    std::cerr << "Cannot cast " << v->type << " to " << t;
    std::cerr << " at ";
    v->printRef(std::cerr);
    std::cerr << "\n";
    return nullptr;
}

void InsertCast::operator()() {
    AvailableCheckpoints checkpoint(
        nullptr, code, StreamLogger(DebugOptions(0)).begin(nullptr));
    Visitor::run(code->entry, [&](BB* bb) { apply(bb, checkpoint); });
}

void InsertCast::apply(BB* bb, AvailableCheckpoints& cp) {
    auto ip = bb->begin();
    while (ip != bb->end()) {
        Instruction* instr = *ip;
        Phi* p = nullptr;
        if ((p = Phi::Cast(instr))) {
            p->updateTypeAndEffects();
        }
        instr->eachArg([&](InstrArg& arg) {
            while (!arg.type().isSuper(arg.val()->type)) {
                auto c = cast(arg.val(), arg.type(), env);
                if (!c) {
                    bb->print(std::cerr, true);
                    assert(false);
                }
                auto argument = Instruction::Cast(arg.val());
                if (argument && !instr->bb()->isDeopt() && Force::Cast(c) &&
                    !cp.next(instr) && cp.next(argument)) {
                    auto iterator = argument->bb()->insert(
                        argument->bb()->atPosition(argument) + 1, c);
                    if (argument->bb() == bb)
                        ip = iterator;
                } else {
                    ip = bb->insert(ip, c) + 1;
                }
                arg.val() = c;
            }
        });
        ip++;
    }
}

} // namespace pir
} // namespace rir
