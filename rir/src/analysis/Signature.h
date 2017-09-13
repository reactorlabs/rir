#pragma once

#include "analysis_framework/analysis.h"
#include "analysis_framework/dispatchers.h"

namespace rir {

enum class Bool3 {
    no,
    yes,
    maybe,
};

inline std::ostream& operator<<(std::ostream& stream, Bool3 val) {
    switch (val) {
    case Bool3::no:
        stream << "no";
        break;
    case Bool3::yes:
        stream << "yes";
        break;
    case Bool3::maybe:
        stream << "maybe";
        break;
    }
    return stream;
}

/** Argument evaluation record.


 */
class ArgumentEvaluation {
  public:
    /** Determines whether the promise associated with the argument has been
     * evaluated.
     */
    Bool3 forced;

    /** Determines whether currently, the variable still contains the original
     * value.
     */
    Bool3 contains;

    /** Merges two argument evaluation records, returns true if the source
      changed.

      forced n m y
         n   n m m
         m   m m m
         y   m m y

      contains n m y
         n     n m m
         m     m m m
         y     m m y

     */
    bool mergeWith(ArgumentEvaluation const& other) {
        bool result = false;
        if (forced != Bool3::maybe and forced != other.forced) {
            result = true;
            forced = Bool3::maybe;
        }
        if (contains != Bool3::maybe and contains != other.contains) {
            result = true;
            contains = Bool3::maybe;
        }
        return result;
    }

    ArgumentEvaluation() : forced(Bool3::no), contains(Bool3::yes) {}

  private:
};

class Signature : public State {
  public:
    bool isLeaf() const { return leaf_; }

    Bool3 isArgumentEvaluated(SEXP name) {
        auto i = argumentEvaluation_.find(name);
        assert(i != argumentEvaluation_.end() and "Not an argument");
        return i->second.forced;
    }

    SEXP exportToR() const {
        SEXP result = PROTECT(allocVector(STRSXP, argumentEvaluation_.size()));
        SEXP names = PROTECT(allocVector(STRSXP, argumentEvaluation_.size()));
        size_t i = 0;
        for (auto x : argumentEvaluation_) {
            SET_STRING_ELT(names, i, PRINTNAME(x.first));
            switch (x.second.forced) {
            case Bool3::no:
                SET_STRING_ELT(result, i, mkChar("no"));
                break;
            case Bool3::maybe:
                SET_STRING_ELT(result, i, mkChar("maybe"));
                break;
            case Bool3::yes:
                SET_STRING_ELT(result, i, mkChar("yes"));
                break;
            }
            ++i;
        }
        SEXP x = PROTECT(allocVector(VECSXP, 2));
        SET_VECTOR_ELT(x, 0, result);
        SET_VECTOR_ELT(x, 1, names);
        UNPROTECT(3);
        return x;
    }

    Signature* clone() const override { return new Signature(*this); }

    bool mergeWith(State const* other) override {
        Signature const& sig = *dynamic_cast<Signature const*>(other);
        bool result = false;
        if (leaf_ and not sig.leaf_) {
            leaf_ = false;
            result = true;
        }
        for (auto& i : argumentEvaluation_) {
            auto const& e = *(sig.argumentEvaluation_.find(i.first));
            result = i.second.mergeWith(e.second) or result;
        }
        return result;
    }

    Signature() : leaf_(true) {}

    Signature(CodeEditor const& code) : leaf_(true) {
        for (auto a : code.arguments())
            argumentEvaluation_[a.first] = ArgumentEvaluation();
    }

    Signature(Signature const&) = default;

    void print() const {
        Rprintf("Leaf function:       ");
        if (isLeaf())
            Rprintf("yes\n");
        else
            Rprintf("no\n");
        Rprintf("Argument evaluation: \n");
        for (auto i : argumentEvaluation_) {
            Rprintf("    ");
            Rprintf(CHAR(PRINTNAME(i.first)));
            switch (i.second.forced) {
            case Bool3::no:
                Rprintf(" no\n");
                break;
            case Bool3::maybe:
                Rprintf(" maybe\n");
                break;
            case Bool3::yes:
                Rprintf(" yes\n");
                break;
            }
        }
    }

  protected:
    friend class SignatureAnalysis;

    void setAsNotLeaf() { leaf_ = false; }

    void forceArgument(SEXP name) {
        auto i = argumentEvaluation_.find(name);
        if (i == argumentEvaluation_.end())
            return;
        if (i->second.contains == Bool3::no)
            return;
        if (i->second.contains == Bool3::maybe and
            i->second.forced == Bool3::no)
            i->second.forced = Bool3::maybe;
        else if (i->second.contains == Bool3::yes)
            i->second.forced = Bool3::yes;
    }

    void storeArgument(SEXP name) {
        auto i = argumentEvaluation_.find(name);
        if (i == argumentEvaluation_.end())
            return;
        i->second.contains = Bool3::no;
    }

  private:
    bool leaf_;

    std::map<SEXP, ArgumentEvaluation> argumentEvaluation_;
};

class SignatureAnalysis : public ForwardAnalysisFinal<Signature>,
                          InstructionDispatcher::Receiver {
  public:
    void print() override { finalState().print(); }

    SignatureAnalysis() : dispatcher_(InstructionDispatcher(*this)) {}

  protected:
    void ldfun_(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        current().forceArgument(bc.immediateConst());
    }

    void ldddvar_(CodeEditor::Iterator ins) override {}

    void ldarg_(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        current().forceArgument(bc.immediateConst());
    }

    void ldvar_(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        current().forceArgument(bc.immediateConst());
    }

    void call_(CodeEditor::Iterator ins) override { current().setAsNotLeaf(); }

    void dispatch_(CodeEditor::Iterator ins) override {}

    void dispatch_stack_(CodeEditor::Iterator ins) override {}

    void call_stack_(CodeEditor::Iterator ins) override {}

    void stvar_(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        current().storeArgument(bc.immediateConst());
    }

    Dispatcher& dispatcher() override { return dispatcher_; }

  protected:
    Signature* initialState() override { return new Signature(*code_); }

  private:
    InstructionDispatcher dispatcher_;
};
}
