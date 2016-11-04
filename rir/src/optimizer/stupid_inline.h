#ifndef RIR_STUPID_INLINE_H
#define RIR_STUPID_INLINE_H

#include "ir/CodeEditor.h"
#include "code/analysis.h"
#include "code/dispatchers.h"
#include "interpreter/interp_context.h"
#include "ir/Compiler.h"
#include "R/RList.h"

#include <unordered_set>

namespace rir {

class StupidInliner {
  public:
    CodeEditor& code_;

    StupidInliner(CodeEditor& code) : code_(code) {}

    bool isSafeTarget(SEXP target) {
        if (TYPEOF(target) != BUILTINSXP)
            return false;
        int i = ((sexprec_rjit*)target)->u.i;
        switch (i) {
        case 107:
            return true;
        }
        printf("Warn: cannot inline due to unsafe builtin %s (%u)\n",
               R_FunTab[i].name, i);
        return false;
    }

    bool canInline(Code* c) {
        if (c->codeSize > 250)
            return false;

        BC_t* pc = (BC_t*)code(c);
        BC_t* end = pc + c->codeSize;
        while (pc != end) {
            BC bc = BC::advance(&pc);
            if (bc.isCallsite()) {
                CallSite cs = bc.callSite(c);
                if (!cs.hasTarget())
                    return false;
                if (!isSafeTarget(cs.target())) {
                    return false;
                }
            } else if (bc.bc == BC_t::ldvar_) {
                return false;
            }
        }
        return true;
    }

    void doInline(CodeEditor::Cursor& pos, SEXP t, CodeEditor* arg = nullptr) {
        CodeEditor edit(t);
        edit.normalizeForInline();

        bool loadedDefaultArg = false;

        for (auto i = edit.begin(); i != edit.end(); ++i) {
            if ((*i).bc == BC_t::ldarg_) {
                if (!arg) {
                    RList formals(FORMALS(t));
                    SEXP arg1 = formals[0];
                    Compiler comp(arg1);
                    auto res = comp.finalize();
                    arg = new CodeEditor(res.bc);
                }

                arg->normalizeForInline();
#ifdef ENABLE_SLOWASSERT
                for (auto i : *arg) {
                    assert(i.bc != BC_t::ldarg_);
                }
#endif
                CodeEditor::Cursor e = i.asCursor(edit);
                e.remove();
                e.insert(*arg);
            }
        }

        if (loadedDefaultArg)
            delete arg;
        edit.commit();
        pos.insert(edit);
    }

    void run() {
        for (auto i = code_.begin(); i != code_.end(); ++i) {
            BC bc = *i;
            if (bc.bc != BC_t::call_)
                continue;

            if (bc.immediate.call_args.nargs > 1)
                continue;

            CallSite cs = i.callSite();

            if (!cs.hasProfile())
                continue;

            CallSiteProfile* p = cs.profile();

            if (p->taken < 200) {
                continue;
            }

            if (p->numTargets != 1)
                continue;

            SEXP t = p->targets[0];
            if (TYPEOF(t) != CLOSXP)
                continue;

            SEXP fun = BODY(t);

            if (!isValidFunctionSEXP(fun))
                continue;

            Function* f = (Function*)INTEGER(fun);
            Code* c = functionCode(f);

            // TODO: This is a bit of a hack to find out if the function
            // has just one code object.
            if (begin(f) != c)
                continue;

            if (!canInline(c))
                continue;

            CodeEditor::Cursor cur = i.asCursor(code_).prev();

            if (cur.bc().bc != BC_t::ldfun_) {
                printf("cannot inline, did not find ldfun\n");
                continue;
            }
            SEXP name = cur.bc().immediateConst();
            cur.remove();
            cur.remove();

            cur << BC::checkName(name, t);

            if (cs.nargs() == 1) {
                auto arg = code_.detachPromise(cs.args()[0]);
                doInline(cur, t, arg);
                delete arg;
            } else {
                doInline(cur, t);
            }
        }
    }
};
}
#endif
