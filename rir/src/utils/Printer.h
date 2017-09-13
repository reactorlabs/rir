#pragma once

#include "analysis_framework/dispatchers.h"

namespace rir {

/** A simple demonstration of the dispatching, a printer.

  As long as we need only single dispatcher, single driver and single receiver,
  they can all be parents of the class as they are in this simple example.

*/

class Printer : public InstructionDispatcher::Receiver {
  public:
    Printer() : dispatcher_(*this) {}

    void run(CodeEditor& code) {
        pc_ = 0;

        for (auto i = code.begin(); i != code.end(); ++i)
            dispatcher_.dispatch(i);

        for (size_t i = 0, e = code.numPromises(); i != e; ++i) {
            Rprintf("\n");
            printOffset();
            Rprintf("promise %i:\n", i);
            offset_ += 4;
            run(*code.promise(i));
            offset_ -= 4;
        }
    }

    /** Some silly printer stuff.
     */
    void any(CodeEditor::Iterator ins) override {
        if (ins.src()) {
            printOffset();
            Rprintf("          # ");
            Rf_PrintValue(ins.src());
        }
        printOffset();
        Rprintf(" %5x ", pc_);
        BC bc = *ins;
        // TODO(o)
        //        bc.print();
        pc_ += bc.size();
    }

    void label(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        printOffset();
        Rprintf("Label %i:\n", bc.immediate.offset);
    }

  private:
    // TODO slow & ugly
    void printOffset() {
        for (size_t i = 0; i != offset_; ++i)
            Rprintf(" ");
    }

    InstructionDispatcher dispatcher_;
    size_t pc_ = 0;
    size_t offset_ = 0;
};
}
