#pragma once

#include "framework.h"

namespace rir {

/** A linear driver that just executes the given dispatcher over the entire code in the code editor starting from the first instruction to the last one.

 */
class LinearDriver : public Driver {
protected:
    virtual void run(CodeEditor & code, Dispatcher & dispatcher) {
        // no need to iterate in the loop - the dispatcher should advance accordingly
        for (CodeEditor::Cursor i = code.getCursor(), e = code.getCursorAtEnd(); i != e; i.advance()) {
            dispatcher.dispatch(i);
        }
    }
};


}
