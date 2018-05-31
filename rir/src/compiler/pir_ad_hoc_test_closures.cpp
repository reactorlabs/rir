#include "pir/pir_impl.h"
#include "util/builder.h"
#include "R/Protect.h"
#include "util/visitor.h"

namespace rir {
namespace pir {

void sameBBSimpleRedundancy(pir::Closure* function) {
    Protect p;
    Builder builder(function, Env::notClosed());
    SEXP value = p(Rf_ScalarInteger(1));
    builder(new LdConst(value));
    builder(new LdConst(value));
}

void sameBBComplexRedundancy(pir::Closure* function) {
    Protect p;
    Builder builder(function, Env::notClosed());
    SEXP value = p(Rf_ScalarInteger(1));
    SEXP value = p(Rf_ScalarInteger(2));
    builder(new LdConst(value));
    builder(new LdConst(value));
}

void controlFlowRedundancy(pir::Closure* function) {
    Protect p;
    Builder builder(function, Env::notClosed());
    SEXP value = p(Rf_ScalarInteger(1));
    BB* bbIfTrue = builder.createBB();
    builder.bb->next0 = bbIfTrue;
    BB* bbIfFalse = builder.createBB();
    builder.bb->next1 = bbIfFalse;
    builder.bb = bbIfTrue;
    Instruction* varIfTrue = new LdConst(value);
    builder(varIfTrue);
    builder.bb = bbIfFalse;
    Instruction* varIfFalse = new LdConst(value);
    builder(varIfFalse);
    BB* join = builder.createBB();
    bbIfTrue->next0 = join;
    bbIfFalse->next0 = join;
    builder.bb = join;
    Phi* phi = new Phi();
    phi->addInput(bbIfTrue, varIfTrue);
    phi->addInput(bbIfFalse, varIfFalse);
    builder(phi);
    builder(new LdConst(value));
}

bool hasOneConstantLoad(BB* bb){
    std::vector<SEXP> values;
    return !Visitor::check (
        bb, [&](Instruction* i) { 
            if (LdConst::Cast(i)) {
                if(std::find(values.begin(), values.end(), LdConst::Cast(i)->c) != values.end())
                    return false;
                else
                    values.push_back(LdConst::Cast(i)->c);
            }
            return true;
        });
}

bool hasTwoConstantLoads(BB* bb){
    std::vector<SEXP> values;
    return !Visitor::check (
        bb, [&](Instruction* i) { 
            if (LdConst::Cast(i)) {
                if(std::find(values.begin(), values.end(), LdConst::Cast(i)->c) != values.end())
                    return false;
                else
                    values.push_back(LdConst::Cast(i)->c);
            }
            return true;
        });
}

BB* firstBB(pir::Closure* closure) {
    return closure->entry;
}

BB* mergeBB(pir::Closure* closure) {
    return closure->entry->next0->next0;
}

}
}
