#ifndef COMPILER_PIR_GR_H
#define COMPILER_PIR_GR_H

#include "../pir/pir.h"
#include "generic_static_analysis.h"
#include <algorithm>

namespace rir {
namespace pir {

/*
 * Polynomial Time Global Value Numbering analysis based on:
 * https://www.sciencedirect.com/science/article/pii/S1477842415300622
 */

/*
struct GBNValue {
    Value* value;
    SEXP* constant;
    GBNValue() : value(nullptr), constant(nullptr) {};
    GBNValue(Value* aValue) : value(aValue), constant(nullptr) {};
    GBNValue(SEXP* aConstant) : constant(aConstant), value(nullptr) {};
    
    bool isValid() const { return value != nullptr || constant != nullptr; } 
    bool operator==(const GBNValue& anotherGV) {
        if (value!=nullptr && anotherGV.value!=nullptr) 
            return value == anotherGV.value;
        if (constant!=nullptr && anotherGV.constant!=nullptr)
            return *constant == *anotherGV.constant; 
        return false;
    }
    
};*/

/*
 * An abstraction of a set of equivalent expressions.
 * For more information see page 170. 
*/
struct ValueExpression {
    typedef ValueExpression Expression;
    ValueExpression() : operand1(), operand2(), operation(nullptr) {};
    ValueExpression(Value* value) : operand1(value), operand2(), operation(nullptr) {};
    ValueExpression(Value* value, Value* anotherValue, Tag* operation) : 
        operand1(value), operand2(anotherValue), operation(operation) {};
    Value* operand1;
    Value* operand2;
    Tag* operation;

    bool contains(Expression& expression) {
        return (operand1 == expression.operand1) &&
                (operand2 == expression.operand2) &&
                (operation == expression.operation); 
    }
};

/*
 * An abstraction of a set of equivalent phi-functions.
 * For more information see page 170. 
*/
struct ValuePhiFunction {
    ValuePhiFunction(Value* left, Value* right) : leftPath(left), rightPath(right) {};
    Value* leftPath;
    Value* rightPath;
    //BB* joinPoint;
    bool operator==(const ValuePhiFunction& anotherPF) {
        return leftPath == anotherPF.leftPath &&
                rightPath == anotherPF.rightPath;
    }        
};

union GRPIRAssignment {
    SEXP* constant;
    size_t argIndex;
    ValueExpression::Expression* expression;
};

struct PIRAssignment {
    GRPIRAssignment argument;
    unsigned char type;
    
    void setConstant(SEXP* constant) {
        argument.constant = constant;
        type = 1;        
    }

    void setIndex(size_t index) {
        argument.argIndex = index;
        type = 2;        
    }

    void setExpression(ValueExpression::Expression* expession) {
        argument.expression = expession;
        type = 3;        
    }

    ValueExpression::Expression* getExpression() {
        assert(type == 3);
        return argument.expression;
    }

    bool isConstant() { return type == 1; }
    bool isArgIndex() { return type == 2; }
    bool isExp() { return type == 3; }

    bool operator==(const PIRAssignment& anotherPA) {
        if (type != anotherPA.type) return false;
        if (this->isConstant()) return *argument.constant == *anotherPA.argument.constant;
        else if (this->isArgIndex()) return argument.argIndex == anotherPA.argument.argIndex;
        else return argument.expression == anotherPA.argument.expression;
    }
};

// This is the abstract domain.
class GREquivalenceClass {
  public:
    //GBNEquivalenceClass();
    
    Value* number;
    std::vector<Value*> variables;
    std::vector<PIRAssignment> expressions;
    ValuePhiFunction* phis;

    bool merge(const GREquivalenceClass& otherClass){
        bool changed = false;
        assert(number == otherClass.number);
        for (auto var : otherClass.variables){
            size_t size = variables.size();
            variables.push_back(var);
            changed = changed || (variables.size() > size);
        }
        for (auto exp : otherClass.expressions){
            // TODO: Verify after merge that there are no more that one argument and constants
            size_t size = expressions.size();
            expressions.push_back(exp);
            changed = changed || (expressions.size() > size);
        }
        // Is it possible to have more than one phi eq class
        if (phis == nullptr && otherClass.phis!=nullptr){
            this->phis = otherClass.phis;
            changed = true;
        }
        return changed;
    };
};

/*
 * A partition at a point in the program represents equivalences 
 * that hold at that point along with available expressions. 
 * A partition is a set of equivalence classes. 
*/
class GRPartition : public std::unordered_map<Value*, GREquivalenceClass> {
  public:
    Value* detectClassContaining(PIRAssignment& assignment){
        for (auto it : *this) {
            for (PIRAssignment otherAssigment : it.second.expressions) {
                if (assignment == otherAssigment) return it.first;
            }
        } 
        return nullptr;
    }

    Value* detectClassContaining(ValuePhiFunction& phi){
        /*for (auto it : *this) {
            if (*it.second.phis == phi) return it.first;
        }*/
        return nullptr;             
    }

    bool merge(const GRPartition& otherState) {
        bool changed;
        for (auto it : otherState) {
            if (this->count(it.first)) {
                changed = changed || (this->at(it.first).merge(it.second));
            } else {
                this->insert({it.first, it.second});
                changed = true;
            }
        }
        return changed;
    }

    void removeVariableFromClasses(Value* variable) {
        for (auto it : *this) {
            std::vector<Value*> variables = it.second.variables;
            variables.erase(std::remove_if(variables.begin(), variables.end(), 
                [&](const Value* var) { return var == variable; }), 
                variables.end());
        }
    }    

    Value* variableDefiningEquivalentExpression(Value* variable) const{
        for (auto it : *this) {
            for (Value* var : it.second.variables) {
                if (var == variable) return var;
            }
        }    
        return nullptr;
    }
};         

class GlobalRedundanciesAnalysis : public StaticAnalysis<GRPartition> {
  public:
    typedef StaticAnalysis<GRPartition> Super;
    GlobalRedundanciesAnalysis(Closure* closure) : Super(closure) {}
    
    void apply(GRPartition&, Instruction*) const override;
    std::vector<Instruction*> redundantInstructions();
  private:
    ValuePhiFunction* createValuePhiFunction(GRPartition&, PIRAssignment&) const;
};
}
}

#endif
