#include "insert_cast.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"

namespace rir {
namespace pir {

pir::Instruction* InsertCast::cast(pir::Value* v, PirType t, Value* env) {
    if (v->type.maybePromiseWrapped() && !t.maybePromiseWrapped()) {
        return new pir::Force(v, env);
    }
    if (v->type.maybeMissing() && !t.maybeMissing()) {
        return new pir::ChkMissing(v);
    }
    if (v->type == RType::logical && t == NativeType::test) {
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
    Visitor::run(start, [&](BB* bb) { apply(bb); });
}

void InsertCast::apply(BB* bb) {
    auto ip = bb->begin();
    while (ip != bb->end()) {
        Instruction* instr = *ip;
        Phi* p = nullptr;
        if ((p = Phi::Cast(instr))) {
            p->updateType();
        }
        instr->eachArg([&](InstrArg& arg) {
            while (!arg.type().isSuper(arg.val()->type)) {
                auto c = cast(arg.val(), arg.type(), env);
                if (!c) {
                    bb->print(std::cerr);
                    assert(false);
                }
                c->bb_ = bb;
                arg.val() = c;
                ip = bb->insert(ip, c) + 1;
            }
        });
        ip++;
    }
}

} // namespace pir
} // namespace rir
