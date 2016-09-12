#pragma once

#include "ir/BC.h"
#include "ir/CodeEditor.h"

namespace rir {

typedef CodeEditor Code;
typedef CodeEditor::Cursor Cursor;


/** Dispatcher prototype.

  Dispatchers are used to determine the current situation and execute the appropriate code. Each dispatcher must override the doDispatch() method. If the dispatch fails, the dispatcher is supposed to call the fail() method, so that the dispatch() can return proper result.

  Note that failing the dispatch does not necessarily mean an error, it merely signifies that the dispatcher did not recognize the situation, or that the code dispatched to wanted to override the failure.
  */

class Dispatcher {
public:
    ~Dispatcher() {
    }

    /** Dispatches on the given cursor, and returns true if the dispatch was successful, false if not.
     */
    bool dispatch(Cursor & ins) {
        success_ = true;
        doDispatch(ins);
        return success_;
    }
protected:

    Dispatcher() = default;

    /** Called by actual dispatchers when they want to notify the dispatching that it has failed.

      When this method is called from a dispatched routine, the dispatch() method will then return false.
     */
    void fail() {
        success_ = false;
    }

private:

    /** Actual dispatch code.

      Must be implemented in children.
     */
    virtual void doDispatch(Cursor & ins) = 0;

    bool success_;
};



/** TODO Frankly, I think the driver is a useless abstraction and only makes the implementation more complex.
 */
class Driver {

private:
    virtual void doRun(Code & code, Dispatcher & dispatcher) = 0;



};






}



