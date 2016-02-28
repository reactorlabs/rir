#ifndef PASS_DRIVER_H
#define PASS_DRIVER_H

#include "llvm/Pass.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/BasicBlock.h"
#include "ir/Pass.h"
#include "llvm.h"
#include "ir/State.h"

namespace rjit {
namespace ir {

template <typename aPass>
class PassDriver : public FunctionPass {
  public:
    static char ID;

    template <typename T>
    struct RegisterMe {
        RegisterMe() : X(aPass::getPassName(), "", false, false) {
            std::cout << aPass::getPassName() << "Registered\n";
        }
        RegisterPass<T> X;
    };
    static RegisterMe<PassDriver<aPass>> Registered;

    PassDriver() : FunctionPass(ID) {}

    void getAnalysisUsage(AnalysisUsage& AU) const override {}

};

template <typename aPass>
char PassDriver<aPass>::ID = 0;

template <typename aPass>
PassDriver<aPass>::RegisterMe<PassDriver<aPass>> PassDriver<aPass>::Registered;

template <typename Pass>
class LinearDriver : public PassDriver<Pass> {
  public:
    Pass pass;

    bool runOnFunction(Function& f) override {
        if (f.isDeclaration() || f.empty())
            return false;

        pass.setFunction(&f);
        return runOnFunction_(f);
    }

protected:

    virtual bool runOnFunction_(Function& f) { return dispatch_(f); }

    virtual bool dispatch_(Function& f) {
        for (auto& b : f) {
            BasicBlock::iterator i = b.begin();
            while (i != b.end()) {
                if (!pass.dispatch(i))
                    i++;
            }
        }

        return false;
    }
};

/** Forward driver for passes.

 */
template <typename PASS>
class ForwardDriver : public PassDriver<PASS> {
    typedef std::pair<llvm::BasicBlock *, typename PASS::State> QueueItem;
public:
    bool runOnFunction(llvm::Function & f) override {
        if (f.isDeclaration() or f.empty())
            return false;
        pass_.setFunction(&f);
        q_.push_back(QueueItem(f.begin(), pass_.initialState(&f)));
        while (not q_.empty()) {
            auto & i = q_.front();
            runOnBlock(i.first, std::move(i.second));
            q_.pop_front();
        }
        // forward driver for state passes is only for analyses
        return false;
    }

    PASS * pass() {
        return &pass_;
    }

protected:

    /** Executes the analysis pass on given block, checking whether a fixpoint has been reached, updating the current state and enqueuing its successors.
     */
    void runOnBlock(llvm::BasicBlock * block, typename PASS::State && incomming) {
        if (not pass_.setState(block, std::move(incomming)))
            return;
        // iterate over all instructions in the block
        BasicBlock::iterator i = block->begin();
        while (i != block->end())
            pass_.dispatch(i);
        // enqueue next block(s)
        llvm::TerminatorInst * t = block->getTerminator();
        for (unsigned ii = 1, end = t->getNumSuccessors(); ii < end; ++ii)
            q_.push_back(std::pair<llvm::BasicBlock *, typename PASS::State>(t->getSuccessor(ii), pass_.state));
        // enque first successor with move semantics
        if (t->getNumSuccessors() > 0)
            q_.push_back(std::pair<llvm::BasicBlock *, typename PASS::State>(t->getSuccessor(0), std::move(pass_.state)));
    }

    PASS pass_;
private:
    std::deque<QueueItem> q_;
};



/** A driver for optimizations.

  Is templated by the optimization's pass class and by the types of analyses that are required by the pass.

  Linearly passes over the basic blocks in the function and advances all required analyses as well.

  TODO this only works for forward analyses, we might beed a backward optimization pass too, or a backward analysis advancement that will work with fw passes too.

 */
template<typename PASS, typename... ANALYSES>
class OptimizationDriver : public ir::PassDriver<PASS> {
public:
    void getAnalysisUsage(llvm::AnalysisUsage& au) const override {
        require<ANALYSES...>(au);
    }

    /** LLVM's runOnFunction interface.

      The function will only be analysed if it is not a declaration and is not empty.
     */
    virtual bool runOnFunction(llvm::Function & f) override {
        if (f.isDeclaration() || f.empty())
            return false;
        return optimize(&f);
    }


protected:
    PASS pass;

    virtual bool optimize(llvm::Function * f) {
        setFunction(f);
        for (auto & b : *f) {
            // at the beginning of each basic block we must set the proper state to all analyses we require
            if (setState<ANALYSES...>(&b)) {
                llvm::BasicBlock::iterator i = b.begin();
                while (i != b.end()) {
                    // each analysis must advance its state past the instruction
                    // TODO the fact we do this before the optimization is fine because the state before cannot be different for the inputs of currently used instructions & patterns
                    advanceAnalysis<ANALYSES...>(i);
                    // do the opimization pass
                    if (!pass.dispatch(i))
                        i++;
                }
            }
        }
        return pass.hasChanged();
    }


    virtual void setFunction(llvm::Function * f) {
        pass.setFunction(f);
    }

private:

    template<typename T>
    void require(llvm::AnalysisUsage & au) const {
        au.addRequired<T>();
    }

    template<typename T1, typename T2>
    void require(llvm::AnalysisUsage & au) const {
        au.addRequired<T1>();
        au.addRequired<T2>();
    }

    template<typename T1, typename T2, typename... A>
    void require(llvm::AnalysisUsage & au) const {
        require<T1, T2>(au);
        require<A...>(au);
    }

    template<typename T>
    bool setState(llvm::BasicBlock * bb) {
        return this->template getAnalysis<T>().pass()->setState(bb);
    }

    template<typename T1, typename T2>
    bool setState(llvm::BasicBlock * bb) {
        return this->template getAnalysis<T1>().pass()->setState(bb) and this->template getAnalysis<T2>().pass()->setState(bb);
    }

    template<typename T1, typename T2, typename... A>
    bool setState(llvm::BasicBlock * bb) {
        return setState<T1, T2>(bb) and setState<A...>(bb);
    }

    template<typename T>
    void advanceAnalysis(llvm::BasicBlock::iterator i) {
        this->template getAnalysis<T>().pass()->dispatch(i);
    }

    template<typename T1, typename T2>
    void advanceAnalysis(llvm::BasicBlock::iterator i) {
        this->template getAnalysis<T1>().pass()->dispatch(i);
        this->template getAnalysis<T2>().pass()->dispatch(i);
    }

    template<typename T1, typename T2, typename... A>
    void advanceAnalysis(llvm::BasicBlock::iterator i) {
        advanceAnalysis<T1, T2>(i);
        advanceAnalysis<A...>(i);
    }


};




} // namespace ir
} // namespace rjit

#endif
