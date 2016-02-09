/** State. All templated.


  State must allow merge





  */


#include "llvm.h"
#include "RIntlns.h"




class Value {




};



class Shape {
public:
    enum class Type {
        scalarDouble,
        scalarInteger,
        scalarLogical,
        other,
    };
};



/** This is a simple state that cares about registers and variables.
 */
template<typename VALUE>
class State {
public:



protected:
    std::map<llvm::Instruction *, VALUE> registers_;
    std::map<SEXP, VALUE> variables_;



};
