#include "global_redundancies.h"

namespace rir {
namespace pir {
  
void GlobalRedundanciesAnalysis::apply(GRPartition& partition, Instruction* instruction) const {
    // No operator
    LdArg* loadArg = LdArg::Cast(instruction);
    LdFun* loadFun = LdFun::Cast(instruction);
    Instruction* loadVar = LdVar::Cast(instruction);
    LdVarSuper* superLoadVar = LdVarSuper::Cast(instruction);
    LdConst* loadConst = LdConst::Cast(instruction);

    // Operator and 1 operand
    Inc* inc = Inc::Cast(instruction);
    IsObject* isObject = IsObject::Cast(instruction);

    // Operator and 2 operands
    Add* add = Add::Cast(instruction);
    Gte* gte = Gte::Cast(instruction);
    Lte* lte = Lte::Cast(instruction);
    Mul* mul = Mul::Cast(instruction);
    Div* division = Div::Cast(instruction);
    IDiv* idiv = IDiv::Cast(instruction);
    Mod* mod = Mod::Cast(instruction);
    Colon* colon = Colon::Cast(instruction);
    Pow* power = Pow::Cast(instruction);
    Sub* sub = Sub::Cast(instruction);
    Gt* gt = Gt::Cast(instruction);
    Lt* lt = Lt::Cast(instruction);
    Neq* neq = Neq::Cast(instruction);
    Eq* eq = Eq::Cast(instruction);
    LAnd* land = LAnd::Cast(instruction);
    LOr* lor = LOr::Cast(instruction);

    Phi* phi = Phi::Cast(instruction);
    if (phi) {
        /* It is a phi node. Create a phi eq-class if it is the first time
         * Note that the original algorithm does not process phis but creates
         * the phi equivalance classes at join points. For that it requires to
         * be in CSSA. We workaround this by creating the phi equivalaence here.
         * Check if we mantain semantics. 
        */
        if (!partition.count(instruction)) {
            GREquivalenceClass klass;
            klass.phis = new ValuePhiFunction(phi->arg(0).val(), phi->arg(1).val());
            partition.insert({instruction, klass});
        }
        return;
    }

    partition.removeVariableFromClasses(instruction);

    PIRAssignment assignment;
    if (loadConst) {
        assignment.setConstant(&loadConst->c);
    } else if (loadArg) {
        assignment.setIndex(loadArg->id);
    } else {
        ValueExpression::Expression expression;
        if (loadFun || loadVar || superLoadVar) {
            expression.operand1 = instruction->arg(0).first;
        } else if (inc || isObject) {
            expression.operand1 = instruction->arg(0).first;
            expression.operation = &instruction->tag;
        } else if (add || gte || lte || mul || division || idiv || mod || colon || 
                power || sub || gt || lt || neq || eq || land || lor) {
            expression.operand1 = instruction->arg(0).first;
            expression.operand2 = instruction->arg(1).first;
            expression.operation = &instruction->tag;            
        }
        assignment.setExpression(&expression);
    }
    Value* eqClass = partition.detectClassContaining(assignment);
    if (eqClass) {
        // There is already a value for that expression
        // so add this new pir variable to that class
        partition.at(eqClass).variables.push_back(instruction);
    } else {
        // No existing value class for the expression. 
        ValuePhiFunction* valuePhiFunc = this->createValuePhiFunction(partition, assignment);
        Value* eqClass = partition.detectClassContaining(*valuePhiFunc);
        if (eqClass) {
            // There is already a value for the expression's phi equivalence class 
            partition.at(eqClass).variables.push_back(instruction);
            partition.at(eqClass).expressions.push_back(assignment);
        } else {
            auto it = partition.find(instruction);
            if (it != partition.end()){
                GREquivalenceClass& klass = it->second;
                klass.expressions.push_back(assignment);
                klass.phis = valuePhiFunc;
            } else {
                GREquivalenceClass klass;
                klass.expressions.push_back(assignment);
                klass.phis = valuePhiFunc;
                partition.insert({instruction, klass});
            }
        }
    
        /*if (add || gte || lte || mul || division || idiv || mod || colon || 
                power || sub || gt || lt || neq || eq || land || lor)*/
    }
}

std::vector<Instruction*> GlobalRedundanciesAnalysis::redundantInstructions() {
    std::vector<Instruction*> redundant;
    this->foreach<PositioningStyle::AfterInstruction>(
    [&](const GRPartition& partition, Instruction* i) {
        if (!partition.at(i).variables.empty() || partition.at(i).phis)
            redundant.push_back(i);
    });
    return redundant;
}

ValuePhiFunction* GlobalRedundanciesAnalysis::createValuePhiFunction(GRPartition&, PIRAssignment&) const {
    // This is the only remaining function to implement
    return nullptr;
}

}
}
