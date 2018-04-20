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

    LocalSlotIdx maxLocalIdx = 0;
    std::unordered_map<Value*, LocalSlotIdx> alloc;

    Alloc(Code* code) {
        BreadthFirstVisitor::run(code->entry, [&](Instruction* instr) {
            Phi* phi = Phi::Cast(instr);
            if (phi) {
                auto slot = allocateLocal(phi);
                phi->eachArg([&](Value* arg) { allocateLocal(arg, slot); });
            }
        });
        BreadthFirstVisitor::run(code->entry, [&](Instruction* instr) {
            if (instr->type != PirType::voyd() && alloc.count(instr) == 0)
                allocateLocal(instr);
        });
    }

    LocalSlotIdx allocateLocal(Value* val) {
        assert(alloc.count(val) == 0);
        alloc[val] = maxLocalIdx;
        DEBUGCODE(ALLOC_DEBUG, {
            val->printRef(std::cout);
            std::cout << "\t" << maxLocalIdx << "\n";
        });
        return maxLocalIdx++;
    }

    void allocateLocal(Value* val, LocalSlotIdx i) {
        assert(alloc.count(val) == 0);
        assert(i < maxLocalIdx);
        alloc[val] = i;
        DEBUGCODE(ALLOC_DEBUG, {
            val->printRef(std::cout);
            std::cout << "\t" << i << "\n";
        });
    }

    size_t slots() { return maxLocalIdx; }
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
};

size_t Pir2Rir::compile(Context& ctx, Code* code) {

    removePhis(code);
    Alloc a(code);

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
        auto store = [&](BB::Instrs::iterator it, Alloc::LocalSlotIdx where) {
            if (it + 1 != bb->end()) {
                auto next = it + 1;
                if (*next == (*it)->hasSingleUse() && (*next)->nargs() == 1)
                    return; // no store...
            }
            cs << BC::stloc(where);
        };
        auto load = [&](BB::Instrs::iterator it, Alloc::LocalSlotIdx where) {
            if (it != bb->begin()) {
                auto prev = it - 1;
                if ((*prev)->hasSingleUse() == *it && (*it)->nargs() == 1)
                    return; // no load...
            }
            cs << BC::ldloc(where);
        };

        for (auto it = bb->begin(); it != bb->end(); ++it) {
            auto instr = *it;
            switch (instr->tag) {
            case Tag::LdConst: {
                auto res = a.alloc[instr];
                cs << BC::push(LdConst::Cast(instr)->c);
                store(it, res);
                break;
            }
            case Tag::LdFun: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::LdVar: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::ForSeqSize: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::LdArg: {
                auto res = a.alloc[instr];
                cs << BC::ldarg(LdArg::Cast(instr)->id);
                store(it, res);
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
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::Branch: {
                auto br = Branch::Cast(instr);
                auto argslot = a.alloc[br->arg(0).val()];
                load(it, argslot);

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
                auto argslot = a.alloc[ret->arg(0).val()];
                load(it, argslot);
                cs << BC::ret();

                // this is the end of this BB
                return;
            }
            case Tag::MkArg: {
                assert(false && "not yet implemented.");
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
            case Tag::Force: {
                auto force = Force::Cast(instr);
                auto res = a.alloc[force];
                auto argslot = a.alloc[force->arg(0).val()];
                load(it, argslot);
                cs << BC::force();
                store(it, res);
                break;
            }
            case Tag::CastType: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::AsLogical: {
                auto aslogical = AsLogical::Cast(instr);
                auto res = a.alloc[aslogical];
                auto argslot = a.alloc[aslogical->arg(0).val()];
                load(it, argslot);
                cs << BC::asLogical();
                store(it, res);
                break;
            }
            case Tag::AsTest: {
                auto test = AsTest::Cast(instr);
                auto res = a.alloc[test];
                auto argslot = a.alloc[test->arg(0).val()];
                load(it, argslot);
                cs << BC::asbool();
                store(it, res);
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
            case Tag::Inc: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::IsObject: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::LdFunctionEnv: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::PirCopy: {
                PirCopy* copy = PirCopy::Cast(instr);
                auto target = a.alloc[copy];
                auto source = a.alloc[copy->arg(0).val()];
                load(it, source);
                store(it, target);
                break;
            }

#define BINOP(Name, Factory)                                                   \
    case Tag::Name: {                                                          \
        auto binop = Name::Cast(instr);                                        \
        auto lhs = a.alloc[binop->arg(0).val()];                               \
        auto rhs = a.alloc[binop->arg(1).val()];                               \
        auto res = a.alloc[binop];                                             \
        load(it, lhs);                                                         \
        load(it, rhs);                                                         \
        cs << BC::Factory();                                                   \
        cs.addSrcIdx(binop->srcIdx);                                           \
        store(it, res);                                                        \
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

#define UNOP(Name, Factory)                                                    \
    case Tag::Name: {                                                          \
        auto binop = Name::Cast(instr);                                        \
        auto lhs = a.alloc[binop->arg(0).val()];                               \
        auto rhs = a.alloc[binop->arg(1).val()];                               \
        auto res = a.alloc[binop];                                             \
        load(it, lhs);                                                         \
        load(it, rhs);                                                         \
        cs << BC::Factory();                                                   \
        cs.addSrcIdx(binop->srcIdx);                                           \
        store(it, res);                                                        \
        break;                                                                 \
    }
                UNOP(Plus, uplus);
                UNOP(Minus, uminus);
                UNOP(Not, Not);
                UNOP(Length, length);
#undef UNOP

            case Tag::Is: {
                assert(false &&
                       "is_ takes an immediate, but in PIR it is unop");
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::Call: {
                assert(false && "not yet implemented.");
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
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::CallSafeBuiltin: {
                assert(false && "not yet implemented.");
                break;
            }
            case Tag::MkEnv: {
                auto mkenv = MkEnv::Cast(instr);
                // create env, parent?
                auto parent = Env::Cast(mkenv->parent());
                assert(parent->rho);
                cs << BC::push(parent->rho) << BC::makeEnv() << BC::setEnv();
                // bind all args
                mkenv->eachLocalVar([&](SEXP name, Value* val) {
                    auto slot = a.alloc[val];
                    cs << BC::ldloc(slot) << BC::stvar(name);
                });
                break;
            }
            case Tag::Phi: {
                auto phi = Phi::Cast(instr);
                auto src = a.alloc[phi->arg(0).val()];
                auto res = a.alloc[phi];
                load(it, src);
                store(it, res);
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

    return a.slots();
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

    std::unordered_map<Promise*, FunIdxT> promises;
    std::unordered_map<Promise*, SEXP> argNames;
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
