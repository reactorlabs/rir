#include "verifier.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"

namespace rir {
namespace pir {

void Verifier::operator()() {
    bool ok = Visitor::check(f->entry, [&](BB* bb) { return verify(bb); });
    if (!ok) {
        std::cerr << "Verification of function " << *f << " failed\n";
        f->print(std::cerr);
        assert(false);
    }

    for (auto p : f->promise) {
        if (!verify(p)) {
            std::cerr << "Verification of promise failed\n";
            p->print(std::cerr);
            assert(false);
        }
    }
    for (auto p : f->default_arg) {
        if (!verify(p)) {
            std::cerr << "Verification of argument failed\n";
            p->print(std::cerr);
            assert(false);
        }
    }
}

bool Verifier::verify(BB* bb) {
    for (auto i : *bb)
        if (!verify(i, bb)) {
            assert(false);
            return false;
        }
    if (bb->empty()) {
        if (!bb->next0 && !bb->next1) {
            assert(false);
            return false;
        }
    } else {
        Instruction* last = bb->last();
        if ((Branch::Cast(last))) {
            if (!bb->next0 || !bb->next1) {
                assert(false);
                return false;
            }
        } else if ((Deopt::Cast(last)) || (Return::Cast(last))) {
            if (bb->next0 || bb->next1) {
                assert(false);
                return false;
            }
        } else {
            if (!bb->next0 || bb->next1) {
                assert(false);
                return false;
            }
        }
    }
    return true;
}

bool Verifier::verify(Promise* p) {
    return Visitor::check(p->entry, [&](BB* bb) { return verify(bb); });
}

bool Verifier::verify(Instruction* i, BB* bb) {
    bool success = true;
    i->each_arg([&](Value* v, PirType t) -> void {
        if (!(t >= v->type)) {
            std::cerr << "Error at instruction '";
            i->print(std::cerr);
            std::cerr << "': Value ";
            v->printRef(std::cerr);
            std::cerr << " has type " << v->type
                      << " which is not a subtype of " << t << "\n";
            success = false;
        }
        if (!Phi::Cast(i)) {
            // v->bb([&](BB* valueBB) {
            // if (valueBB != bb) {
            //     std::cerr << "Error at instruction '";
            //     i->print(std::cerr);
            //     std::cerr << "': Value ";
            //     v->printRef(std::cerr);
            //     std::cerr << " does not come from the same BB\n";
            //     success = false;
            // }
            // });
        }
    });
    return success;
}
}
}
