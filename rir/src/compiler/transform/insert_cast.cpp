#include "insert_cast.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"

namespace rir {
namespace pir {

pir::Instruction* InsertCast::cast(pir::Value* v, PirType t) {
    if (v->type.maybeLazy() && !t.maybeLazy()) {
        return new pir::Force(v);
    }
    if (v->type.maybeMissing() && !t.maybeMissing()) {
        return new pir::ChkMissing(v);
    }
    if (v->type == RType::logical && t == NativeType::test) {
        return new pir::AsTest(v);
    }

    std::cerr << "Cannot cast ";
    v->printRef(std::cerr);
    std::cerr << " (type " << v->type << ") to " << t << "\n";
    assert(false);
    return nullptr;
}

void InsertCast::operator()() {
    Visitor::run(start, [&](BB* bb) { apply(bb); });
}

void InsertCast::apply(BB* bb) {
    auto ip = bb->begin();
    while (ip != bb->end()) {
        Instruction* instr = *ip;
        auto next = ip + 1;

        Phi* p = nullptr;
        if ((p = Phi::Cast(instr))) {
            p->updateType();
        }
        instr->eachArg([&](InstrArg& arg) {
            size_t added = 0;
            while (!arg.type().isSuper(arg.val()->type)) {
                auto c = cast(arg.val(), arg.type());
                c->bb_ = bb;
                arg.val() = c;
                next = bb->insert((ip + added), c);
                added++;
            }
        });
        ip = next;
    }
}
}
}
