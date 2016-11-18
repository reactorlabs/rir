#ifndef RIR_STUPID_INLINE_H
#define RIR_STUPID_INLINE_H

#include "ir/CodeEditor.h"
#include "code/analysis.h"
#include "code/dispatchers.h"
#include "code/dataflow.h"
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
        if (TYPEOF(target) != BUILTINSXP && TYPEOF(target) != SPECIALSXP)
            return false;
        if (isSafeBuiltin(target->u.primsxp.offset))
            return true;
        // printf("Warn: cannot inline due to unsafe builtin %s (%u)\n",
        //        R_FunTab[target->u.primsxp.offset].name,
        //        target->u.primsxp.offset);
        return false;
    }

    // For simplicity, for now we only inline functions which do not have local
    // variables and do not leak the environment.
    bool canInline(Code* c) {
        if (c->codeSize > 800)
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
            } else if (bc.is(BC_t::guard_arg_) || bc.is(BC_t::guard_local_)) {
                // we want to get rid of the environment, so this checks are
                // not possible
                return false;
            } else if (bc.is(BC_t::ldarg_)) {
                // ldarg is fine, we'll inline the promise here
                continue;
            } else if (!bc.isPure()) {
                return false;
            }
        }
        return true;
    }

    void doInline(CodeEditor::Cursor& pos, SEXP t,
                  std::unordered_map<SEXP, CodeEditor*>& args) {
        CodeEditor edit(t);
        edit.normalizeForInline();

        for (auto i = edit.begin(); i != edit.end(); ++i) {
            BC bc = *i;
            if (bc.bc == BC_t::ldarg_) {
                CodeEditor* arg = args[(*i).immediateConst()];
                arg->normalizeForInline();
                CodeEditor::Cursor e = i.asCursor(edit);
                e.remove();
                e.insert(*arg);
            }

            assert(bc.bc != BC_t::guard_arg_);
        }

        edit.commit();
        pos.insert(edit);
    }

    void run() {
        for (auto i = code_.begin(); i != code_.end(); ++i) {
            BC bc = *i;
            if (bc.bc != BC_t::call_)
                continue;

            CallSite cs = i.callSite();

            if (cs.hasNames())
                continue;

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

            CodeEditor::Cursor cur = i.asCursor(code_).prev();
            SEXP name = cur.bc().immediateConst();
            if (!canInline(c)) {
                continue;
            }

            if (cur.bc().bc != BC_t::ldfun_) {
                printf("cannot inline, did not find ldfun\n");
                continue;
            }

            std::unordered_map<SEXP, CodeEditor*> args;
            RList formals(FORMALS(t));
            if (formals.length() < cs.nargs())
                continue;

            size_t idx = 0;
            for (auto f = formals.begin(); f != formals.end(); ++f) {
                CodeEditor* arg;
                if (idx < cs.nargs()) {
                    arg = &code_.promise(cs.args()[idx]);
                } else {
                    arg = new CodeEditor(Compiler::compileExpression(*f).bc);
                }
                args[f.tag()] = arg;
                ++idx;
            }

            cur.remove();
            cur.remove();

            cur << BC::guardName(name, t);

            doInline(cur, t, args);

            idx = 0;
            for (auto f = formals.begin(); f != formals.end(); ++f) {
                auto arg = args[f.tag()];
                if (idx < cs.nargs()) {
                    arg = code_.detachPromise(cs.args()[idx]);
                }
                delete arg;
                ++idx;
            }
        }
    }
};
}
#endif
