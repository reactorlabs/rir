#pragma once

#include "LinearDriver.h"
#include "InstructionVisitor.h"

namespace rir {

/** A simple demonstration of the dispatching, a printer.

  As long as we need only single dispatcher, single driver and single receiver, they can all be parents of the class as they are in this simple example.

*/

class Printer : public InstructionVisitor::Receiver, public LinearDriver {
public:
    Printer():
        dispatcher_(*this) {
    }

    void run(CodeEditor & code) {
        pc_ = 0;
        LinearDriver::run(code, dispatcher_);
        for (size_t i = 0, e = code.numPromises(); i != e; ++i) {
            Rprintf("\n");
            printOffset();
            Rprintf("promise %i:\n", i);
            offset_ += 4;
            run(code.promise(i));
            offset_ -= 4;
        }
    }

    /** Some silly printer stuff.
     */
    void any(Cursor & ins) {
        if (ins.hasAst()) {
            printOffset();
            Rprintf("          # ");
            Rf_PrintValue(ins.ast());
        }
        printOffset();
        Rprintf(" %5x ", pc_);
        BC bc = *ins;
        bc.print();
        pc_ += bc.size();
    }

    void label(Cursor & ins) {
        printOffset();
        Rprintf("Label %i:\n", ins.bc().immediate.offset);
    }

private:

    // TODO slow & ugly
    void printOffset() {
        for (size_t i = 0; i != offset_; ++i)
            Rprintf(" ");
    }

    InstructionVisitor dispatcher_;
    size_t pc_ = 0;
    size_t offset_ = 0;
};

}
