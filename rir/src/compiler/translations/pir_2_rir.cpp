#include "pir_2_rir.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "interpreter/runtime.h"
#include "ir/CodeEditor.h"
#include "ir/CodeStream.h"
#include "ir/CodeVerifier.h"
#include "ir/Optimizer.h"
#include "utils/FunctionWriter.h"

// #define DEBUGGING
#define ALLOC_DEBUG 1
#define PHI_REMOVE_DEBUG 1

#ifdef DEBUGGING
#define DEBUGCODE(flag, code)                                                  \
    if (flag)                                                                  \
    code
#else
#define DEBUGCODE(flag, code) /* nothing */
#endif

namespace rir {
namespace pir {

namespace {

class Alloc {
  public:
    typedef size_t LocalSlotIdx;

    Alloc(Code* code) {
        BreadthFirstVisitor::run(code->entry, [&](Instruction* instr) {
            Phi* phi = Phi::Cast(instr);
            if (phi) {
                auto slot = allocateLocal(phi);
                phi->eachArg([&](Value* arg) {
                    // here the arg must not yet be allocated
                    if (shouldAllocate(arg))
                        allocateLocal(arg, slot);
                });
            }
        });
        BreadthFirstVisitor::run(code->entry, [&](Instruction* instr) {
            if (shouldAllocate(instr) && alloc.count(instr) == 0) {
                allocateLocal(instr);
                instr->eachArg([&](Value* arg) {
                    if (shouldAllocate(arg) && alloc.count(arg) == 0)
                        allocateLocal(arg);
                });
            }
        });
    }

    size_t slots() { return maxLocalIdx; }

    LocalSlotIdx operator[](Value* val) {
        assert(alloc.count(val) && "getting unallocated val...");
        return alloc[val];
    }

  private:
    bool shouldAllocate(Value* val) const {
        return val->type != PirType::voyd() &&
               val->type != PirType::missing() &&
               val->type != PirType::bottom() && !Env::isStaticEnv(val);
    }

    LocalSlotIdx allocateLocal(Value* val) {
        assert(alloc.count(val) == 0);
        assert(val->type != PirType::voyd());
        assert(!Env::isStaticEnv(val));
        alloc[val] = maxLocalIdx;
        DEBUGCODE(ALLOC_DEBUG, {
            val->printRef(std::cout);
            std::cout << "\t" << maxLocalIdx << "\n";
        });
        return maxLocalIdx++;
    }

    void allocateLocal(Value* val, LocalSlotIdx i) {
        assert(i < maxLocalIdx);
        assert(alloc.count(val) == 0);
        assert(val->type != PirType::voyd());
        assert(!Env::isStaticEnv(val));
        alloc[val] = i;
        DEBUGCODE(ALLOC_DEBUG, {
            val->printRef(std::cout);
            std::cout << "\t" << i << "\n";
        });
    }

    LocalSlotIdx maxLocalIdx = 0;
    std::unordered_map<Value*, LocalSlotIdx> alloc;
};

class Context {
  public:
    std::stack<CodeStream*> css;
    FunctionWriter& fun;

    Context(FunctionWriter& fun) : fun(fun) {}
    ~Context() { assert(css.empty()); }

    CodeStream& cs() { return *css.top(); }

    void pushDefaultArg(SEXP ast) {
        defaultArg.push(true);
        push(ast);
    }
    void pushPromise(SEXP ast) {
        defaultArg.push(false);
        push(ast);
    }
    void pushBody(SEXP ast) {
        defaultArg.push(false);
        push(ast);
    }

    FunIdxT pop(size_t localsCnt) {
        auto da = defaultArg.top();
        defaultArg.pop();
        auto idx = cs().finalize(da, localsCnt);
        delete css.top();
        css.pop();
        return idx;
    }

  private:
    std::stack<bool> defaultArg;
    void push(SEXP ast) { css.push(new CodeStream(fun, ast)); }
};

class Pir2Rir {
  public:
    Closure* cls;

    Pir2Rir(Closure* cls) : cls(cls) {}
    size_t compile(Context& ctx, Code* code);
    void removePhis(Code* code);
    rir::Function* finalize();

  private:
    std::unordered_map<Promise*, FunIdxT> promises;
    std::unordered_map<Promise*, SEXP> argNames;
};

size_t Pir2Rir::compile(Context& ctx, Code* code) {

    removePhis(code);
    Alloc alloc(code);

    // create labels for all bbs
    std::unordered_map<BB*, LabelT> bbLabels;
    BreadthFirstVisitor::run(code->entry, [&](BB* bb) {
        if (!bb->isEmpty())
            bbLabels[bb] = ctx.cs().mkLabel();
    });

    BreadthFirstVisitor::run(code->entry, [&](BB* bb) {
        if (bb->isEmpty())
            return;

        CodeStream& cs = ctx.cs();
        cs << bbLabels[bb];

        /*
            this attempts to eliminate redundant store-load pairs, ie.
            if there is an instruction that has only one use, and the use is the
            next instruction, and it is its only input, and PirCopy is not
            involved
            TODO: is it always the case that the store and load use the same
            local slot?
        */
        auto store = [&](BB::Instrs::iterator it, Value* what) {
            if (it + 1 != bb->end()) {
                auto next = it + 1;
                if (*next == (*it)->hasSingleUse() && (*next)->nargs() == 1)
                    return; // no store...
            }
            cs << BC::stloc(alloc[what]);
        };
        auto load = [&](BB::Instrs::iterator it, Value* what) {
            if (it != bb->begin()) {
                auto prev = it - 1;
                if ((*prev)->hasSingleUse() == *it && (*it)->nargs() == 1)
                    return; // no load...
            }
            cs << BC::ldloc(alloc[what]);
        };

        auto loadEnv = [&](BB::Instrs::iterator it, Value* val) {
            assert(Env::isAnyEnv(val));
            if (Env::isStaticEnv(val))
                cs << BC::push(Env::Cast(val)->rho);
            else
                load(it, val);
        };

        for (auto it = bb->begin(); it != bb->end(); ++it) {
            auto instr = *it;
            switch (instr->tag) {
            case Tag::LdConst: {
                cs << BC::push(LdConst::Cast(instr)->c);
                store(it, instr);
                break;
            }
            case Tag::LdFun: {
                auto ldfun = LdFun::Cast(instr);
                loadEnv(it, ldfun->env());
                cs << BC::setEnv() << BC::ldfun(ldfun->varName);
                store(it, ldfun);
                break;
            }
            case Tag::LdVar: {
                auto ldvar = LdVar::Cast(instr);
                loadEnv(it, ldvar->env());
                cs << BC::setEnv() << BC::getvar(ldvar->varName);
                store(it, ldvar);
                break;
            }
            case Tag::ForSeqSize: {
                // make sure that the seq is at TOS? the instr doesn't push it!
                cs << BC::forSeqSize();
                store(it, instr);
                break;
            }
            case Tag::LdArg: {
                cs << BC::ldarg(LdArg::Cast(instr)->id);
                store(it, instr);
                break;
            }
            case Tag::ChkMissing: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::ChkClosure: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::StVarSuper: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::LdVarSuper: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::StVar: {
                auto stvar = StVar::Cast(instr);
                loadEnv(it, stvar->env());
                cs << BC::setEnv() << BC::stvar(stvar->varName);
                break;
            }
            case Tag::Branch: {
                auto br = Branch::Cast(instr);
                load(it, br->arg(0).val());

                // jump through empty blocks
                auto next0 = bb->next0;
                while (next0->isEmpty())
                    next0 = next0->next0;
                auto next1 = bb->next1;
                while (next1->isEmpty())
                    next1 = next1->next0;

                bool useBrFalse = true;
                if (next0->id == bb->id + 1)
                    useBrFalse = false;

                if (useBrFalse)
                    cs << BC::brfalse(bbLabels[next0])
                       << BC::br(bbLabels[next1]);
                else
                    cs << BC::brtrue(bbLabels[next1])
                       << BC::br(bbLabels[next0]);

                // this is the end of this BB
                return;
            }
            case Tag::Return: {
                Return* ret = Return::Cast(instr);
                load(it, ret->arg(0).val());
                cs << BC::ret();

                // this is the end of this BB
                return;
            }
            case Tag::MkArg: {
                break;
            }
            case Tag::Seq: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::MkCls: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::MkFunCls: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::CastType: {
                auto cast = CastType::Cast(instr);
                load(it, cast->arg(0).val());
                store(it, cast);
                break;
            }
            case Tag::Subassign1_1D: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::Subassign2_1D: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::Extract1_1D: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::Extract2_1D: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::Extract1_2D: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::Extract2_2D: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::IsObject: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::LdFunctionEnv: {
                // TODO: what should happen? For now get the current env (should
                // be the promise environment that the evaluator was called
                // with) and store it into local and leave it set as current
                cs << BC::getEnv();
                store(it, instr);
                break;
            }
            case Tag::PirCopy: {
                auto cpy = PirCopy::Cast(instr);
                load(it, cpy->arg(0).val());
                store(it, cpy);
                break;
            }

#define SIMPLE_INSTR(Name, Factory)                                            \
    case Tag::Name: {                                                          \
        auto simple = Name::Cast(instr);                                       \
        load(it, simple->arg(0).val());                                        \
        cs << BC::Factory();                                                   \
        cs.addSrcIdx(simple->srcIdx);                                          \
        store(it, simple);                                                     \
        break;                                                                 \
    }
                SIMPLE_INSTR(Inc, inc);
                SIMPLE_INSTR(Force, force);
                SIMPLE_INSTR(AsLogical, asLogical);
                SIMPLE_INSTR(AsTest, asbool);
                // unary operators are simple, too
                SIMPLE_INSTR(Plus, uplus);
                SIMPLE_INSTR(Minus, uminus);
                SIMPLE_INSTR(Not, Not);
                SIMPLE_INSTR(Length, length);

#undef SIMPLE_INSTR

#define BINOP(Name, Factory)                                                   \
    case Tag::Name: {                                                          \
        auto binop = Name::Cast(instr);                                        \
        load(it, binop->arg(0).val());                                         \
        load(it, binop->arg(1).val());                                         \
        cs << BC::Factory();                                                   \
        cs.addSrcIdx(binop->srcIdx);                                           \
        store(it, binop);                                                      \
        break;                                                                 \
    }
                BINOP(Add, add);
                BINOP(Sub, sub);
                BINOP(Mul, mul);
                BINOP(Div, div);
                BINOP(IDiv, idiv);
                BINOP(Mod, mod);
                BINOP(Pow, pow);
                BINOP(Lt, lt);
                BINOP(Gt, gt);
                BINOP(Lte, ge);
                BINOP(Gte, le);
                BINOP(Eq, eq);
                BINOP(Neq, ne);
                BINOP(LOr, lglOr);
                BINOP(LAnd, lglAnd);
                BINOP(Colon, colon);
#undef BINOP

            case Tag::Is: {
                assert(false &&
                       "is_ takes an immediate, but in PIR it is unop");
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::Call: {
                auto call = Call::Cast(instr);
                auto cls = call->cls();
                loadEnv(it, call->env());
                cs << BC::setEnv()
                   << BC::ldloc(alloc[cls]); // TODO: can use load(it, cls) for
                                             // the closure?

                std::vector<FunIdxT> callArgs;
                call->eachCallArg([&](Value* arg) {
                    auto mkarg = MkArg::Cast(arg);
                    callArgs.push_back(promises[mkarg->prom]);
                });

                cs.insertCall(Opcode::call_, callArgs, {},
                              Pool::get(call->srcIdx));
                store(it, call);
                break;
            }
            case Tag::StaticCall: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::StaticEagerCall: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::CallBuiltin: {
                auto blt = CallBuiltin::Cast(instr);
                // TODO: is it needed to set the environment?
                loadEnv(it, blt->env());
                cs << BC::setEnv();
                blt->eachCallArg([&](Value* arg) { load(it, arg); });
                cs.insertStackCall(Opcode::static_call_stack_, blt->nCallArgs(),
                                   {}, Pool::get(blt->srcIdx), blt->blt);
                store(it, blt);
                break;
            }
            case Tag::CallSafeBuiltin: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::MkEnv: {
                auto mkenv = MkEnv::Cast(instr);
                loadEnv(it, mkenv->parent());
                cs << BC::makeEnv() << BC::dup() << BC::setEnv();
                // bind all args
                mkenv->eachLocalVar([&](SEXP name, Value* val) {
                    load(it, val);
                    cs << BC::stvar(name);
                });
                store(it, mkenv);
                break;
            }
            case Tag::Phi: {
                auto phi = Phi::Cast(instr);
                load(it, phi->arg(0).val());
                store(it, phi);
                break;
            }
            case Tag::Deopt: {
                assert(false && "not yet implemented.");
                break;
            }
            // values, not instructions
            case Tag::Missing:
            case Tag::Env:
            case Tag::Nil:
                break;
            // dummy sentinel enum item
            case Tag::_UNUSED_:
                break;
            // no default to get compiler warnings for missing...
            }
        }

        // this BB has exactly one successor, next0
        // jump through empty blocks
        auto next = bb->next0;
        while (next->isEmpty())
            next = next->next0;
        cs << BC::br(bbLabels[next]);
    });

    return alloc.slots();
}

void Pir2Rir::removePhis(Code* code) {

    // For each Phi, insert copies
    BreadthFirstVisitor::run(code->entry, [&](BB* bb) {
        // TODO: move all phi's to the beginning, then insert the copies not
        // after each phi but after all phi's
        for (auto it = bb->begin(); it != bb->end(); ++it) {
            auto instr = *it;
            Phi* phi = Phi::Cast(instr);
            if (phi) {
                for (size_t i = 0; i < phi->nargs(); ++i) {
                    BB* pred = phi->input[i];
                    // pred is either jump (insert copy at end) or branch
                    // (insert copy before the branch instr)
                    auto it = pred->isJmp() ? pred->end() : pred->end() - 1;
                    Instruction* iav = Instruction::Cast(phi->arg(i).val());
                    auto copy = pred->insert(it, new PirCopy(iav));
                    phi->arg(i).val() = *copy;
                }
                auto phiCopy = new PirCopy(phi);
                phi->replaceUsesWith(phiCopy);
                it = bb->insert(it + 1, phiCopy);
            }
        }
    });

    DEBUGCODE(PHI_REMOVE_DEBUG, {
        std::cout << "--- phi copies inserted ---\n";
        code->print(std::cout);
    });
}

rir::Function* Pir2Rir::finalize() {
    // TODO: asts are NIL for now, how to deal with that? after inlining
    // functions and asts don't correspond anymore

    FunctionWriter function = FunctionWriter::create();
    Context ctx(function);

    size_t i = 0;
    for (auto arg : cls->defaultArgs) {
        if (!arg)
            continue;
        ctx.pushDefaultArg(R_NilValue);
        size_t localsCnt = compile(ctx, arg);
        promises[arg] = ctx.pop(localsCnt);
        argNames[arg] = cls->argNames[i++];
    }
    for (auto prom : cls->promises) {
        if (!prom)
            continue;
        ctx.pushPromise(R_NilValue);
        size_t localsCnt = compile(ctx, prom);
        promises[prom] = ctx.pop(localsCnt);
    }
    ctx.pushBody(R_NilValue);
    size_t localsCnt = compile(ctx, cls);
    ctx.pop(localsCnt);

    CodeEditor code(function.function->body());

    for (size_t i = 0; i < code.numPromises(); ++i)
        if (code.promise(i))
            Optimizer::optimize(*code.promise(i));
    Optimizer::optimize(code);

    auto opt = code.finalize();
    opt->isPirCompiled = true;

#ifdef ENABLE_SLOWASSERT
    CodeVerifier::verifyFunctionLayout(opt->container(), globalContext());
#endif

    return opt;
}

} // namespace

rir::Function* Pir2RirCompiler::operator()(Module* m, rir::Function* orig) {
    Closure* cls = m->get(orig);
    Pir2Rir cmp(cls);
    return cmp.finalize();
}

} // namespace pir
} // namespace rir
