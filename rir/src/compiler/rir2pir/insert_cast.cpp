#include "insert_cast.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"

namespace rir {
namespace pir {

Instruction* InsertCast::cast(Value* v, PirType t, Value* env) {
    if (v->type.maybePromiseWrapped() && !t.maybePromiseWrapped()) {
        return new Force(v, env, Tombstone::framestate());
    }
    if (!v->type.isA(PirType::function()) && t.isA(PirType::function())) {
        return new ChkFunction(v);
    }

    std::cerr << "Cannot cast " << v->type << " to " << t;
    std::cerr << " at ";
    v->printRef(std::cerr);
    std::cerr << "\n";
    return nullptr;
}

void InsertCast::operator()() {
    SimpleLogStream stdOut;
    AvailableCheckpoints checkpoint(nullptr, code, stdOut);
    Visitor::run(code->entry, [&](BB* bb) { apply(bb, checkpoint); });
}

void InsertCast::apply(BB* bb, AvailableCheckpoints& cp) {
    auto ip = bb->begin();
    while (ip != bb->end()) {
        Instruction* instr = *ip;
        if (auto p = Phi::Cast(instr))
            p->updateTypeAndEffects();
        if (auto f = Force::Cast(instr))
            f->updateTypeAndEffects();
        instr->eachArg([&](InstrArg& arg) {
            while (!arg.type().isSuper(arg.val()->type)) {
                auto c = cast(arg.val(), arg.type(), env);
                if (!c) {
                    bb->print(std::cerr, true);
                    assert(false);
                }
                ip = bb->insert(ip, c) + 1;
                arg.val() = c;
            }
        });
        ip++;
    }
}

} // namespace pir
} // namespace rir
