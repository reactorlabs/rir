#include "pir_compiler.h"
#include "R/Funtab.h"
#include "R/RList.h"
#include "analysis/query.h"
#include "analysis/verifier.h"
#include "ir/BC.h"
#include "opt/cleanup.h"
#include "opt/delay_env.h"
#include "opt/delay_instr.h"
#include "opt/elide_env.h"
#include "opt/force_dominance.h"
#include "opt/inline.h"
#include "opt/scope_resolution.h"
#include "pir/pir_impl.h"
#include "transform/insert_cast.h"
#include "util/builder.h"
#include "util/cfg.h"
#include "util/visitor.h"

#include <deque>
#include <vector>

namespace {

using namespace rir::pir;
typedef rir::pir::Function Function;
typedef rir::Opcode Opcode;
typedef rir::BC BC;
typedef rir::RList RList;

class TheCompiler;

class CodeCompiler {
  public:
    Builder& b;
    rir::Function* src;
    rir::Code* srcCode;
    TheCompiler* cmp;

    CodeCompiler(Builder& b, rir::Function* src, rir::Code* srcCode,
                 TheCompiler* cmp)
        : b(b), src(src), srcCode(srcCode), cmp(cmp) {}

    struct MachineState {
        std::deque<Value*> stack;
        Opcode* pc;
        BB* entry = nullptr;

        size_t stack_size() { return stack.size(); }

        void clear() {
            stack.clear();
            entry = nullptr;
            pc = nullptr;
        }
    };

    typedef std::pair<BB*, Value*> ReturnSite;
    std::vector<ReturnSite> results;

    void push(Value* v) { m.stack.push_back(v); }
    Value* top() { return m.stack.back(); }
    Value* at(size_t n) {
        assert(n < m.stack_size());
        return m.stack[m.stack_size() - n - 1];
    }
    void set(size_t n, Value* v) {
        assert(n < m.stack_size());
        m.stack[m.stack_size() - n - 1] = v;
    }
    Value* pop() {
        assert(m.stack_size() > 0);
        auto v = m.stack.back();
        m.stack.pop_back();
        return v;
    }
    bool empty() { return m.stack.empty(); }

    MachineState m;
    std::deque<MachineState> worklist;
    std::unordered_map<Opcode*, MachineState> mergepoint;

    template <size_t SIZE>
    struct Matcher {
        const std::array<Opcode, SIZE> seq;

        typedef std::function<void(Opcode*)> MatcherMaybe;

        bool operator()(Opcode* pc, Opcode* end, MatcherMaybe m) const {
            for (size_t i = 0; i < SIZE; ++i) {
                if (*pc != seq[i])
                    return false;
                BC::advance(&pc);
                if (pc == end)
                    return false;
            }
            m(pc);
            return true;
        }
    };

    Value* operator()(bool addReturn);

    void run(BC bc) {
        Value* v;
        Value* x;
        Value* y;
        switch (bc.bc) {
        case Opcode::push_:
            push(b(new LdConst(bc.immediateConst())));
            break;
        case Opcode::ldvar_:
            v = b(new LdVar(bc.immediateConst(), b.env));
            push(b(new Force(v)));
            break;
        case Opcode::stvar_:
            v = pop();
            b(new StVar(bc.immediateConst(), v, b.env));
            break;
        case Opcode::ldvar2_:
            b(new LdVarSuper(bc.immediateConst(), b.env));
            break;
        case Opcode::stvar2_:
            v = pop();
            b(new StVarSuper(bc.immediateConst(), v, b.env));
            break;
        case Opcode::ret_:
            results.push_back(ReturnSite(b.bb, pop()));
            assert(empty());
            break;
        case Opcode::asbool_:
        case Opcode::aslogical_:
            push(b(new AsLogical(pop())));
            break;
        case Opcode::ldfun_:
            push(b(new LdFun(bc.immediateConst(), b.env)));
            break;
        case Opcode::guard_fun_:
            std::cout << "warn: guard ignored "
                      << CHAR(PRINTNAME(
                             rir::Pool::get(bc.immediate.guard_fun_args.name)))
                      << "\n";
            break;
        case Opcode::swap_:
            x = pop();
            y = pop();
            push(x);
            push(y);
            break;
        case Opcode::dup_:
            push(top());
            break;
        case Opcode::dup2_:
            x = at(0);
            y = at(1);
            push(y);
            push(x);
            break;
        case Opcode::pull_: {
            size_t i = bc.immediate.i;
            push(at(i));
            break;
        }
        case Opcode::close_: {
            Value* x = pop();
            Value* y = pop();
            Value* z = pop();
            push(b(new MkCls(x, y, z, b.env)));
            break;
        }
        case Opcode::nop_:
            break;
        case Opcode::pop_:
            pop();
            break;
        case Opcode::call_: {
            unsigned n = bc.immediate.call_args.nargs;
            rir::CallSite* cs = bc.callSite(srcCode);

            std::vector<Value*> args;
            for (size_t i = 0; i < n; ++i) {
                unsigned argi = cs->args()[i];
                if (argi == DOTS_ARG_IDX) {
                    assert(false);
                } else if (argi == MISSING_ARG_IDX) {
                    assert(false);
                }
                rir::Code* code = src->codeAt(argi);
                Promise* prom = b.function->createProm();
                {
                    Builder pb(b.function, prom);
                    CodeCompiler c(pb, src, code, cmp);
                    c(true);
                }
                Value* val = Missing::instance();
                if (Query::pure(prom)) {
                    CodeCompiler c(b, src, code, cmp);
                    val = c(false);
                }
                args.push_back(b(new MkArg(prom, val, b.env)));
            }

            push(b(new Call(b.env, pop(), args)));
            break;
        }
        case Opcode::promise_: {
            unsigned promi = bc.immediate.i;
            rir::Code* code = src->codeAt(promi);
            Promise* prom = b.function->createProm();
            {
                Builder pb(b.function, prom);
                CodeCompiler c(pb, src, code, cmp);
                c(true);
            }
            Value* val = Missing::instance();
            if (Query::pure(prom)) {
                CodeCompiler c(b, src, code, cmp);
                val = c(false);
            }
            push(b(new MkArg(prom, val, b.env)));
            break;
        }
        case Opcode::static_call_stack_: {
            unsigned n = bc.immediate.call_args.nargs;
            rir::CallSite* cs = bc.callSite(srcCode);
            SEXP target = rir::Pool::get(*cs->target());

            std::vector<Value*> args(n);
            for (size_t i = 0; i < n; ++i)
                args[n - i - 1] = pop();

            // TODO: compile a list of safe builtins
            static int vector = findBuiltin("vector");

            if (getBuiltinNr(target) == vector)
                push(b(new CallSafeBuiltin(target, args)));
            else
                push(b(new CallBuiltin(b.env, target, args)));
            break;
        }
        case Opcode::seq_: {
            auto x = pop();
            auto y = pop();
            auto z = pop();
            push(b(new Seq(x, y, z)));
            break;
        }
        case Opcode::for_seq_size_:
            push(b(new ForSeqSize(top())));
            break;

        case Opcode::extract1_1_: {
            Value* vec = pop();
            Value* idx = pop();
            push(b(new Extract1_1D(vec, idx)));
            break;
        }

        case Opcode::extract2_1_: {
            Value* vec = pop();
            Value* idx = pop();
            push(b(new Extract2_1D(vec, idx)));
            break;
        }

        case Opcode::extract1_2_: {
            Value* vec = pop();
            Value* idx1 = pop();
            Value* idx2 = pop();
            push(b(new Extract1_2D(vec, idx1, idx2)));
            break;
        }

        case Opcode::extract2_2_: {
            Value* vec = pop();
            Value* idx1 = pop();
            Value* idx2 = pop();
            push(b(new Extract2_2D(vec, idx1, idx2)));
            break;
        }

        case Opcode::subassign1_: {
            Value* vec = pop();
            Value* idx = pop();
            Value* val = pop();
            push(b(new Subassign1_1D(vec, idx, val)));
            break;
        }

        case Opcode::subassign2_: {
            Value* vec = pop();
            Value* idx = pop();
            Value* val = pop();
            push(b(new Subassign2_1D(vec, idx, val)));
            break;
        }

#define BINOP(Name, Op)                                                        \
    case Opcode::Op:                                                           \
        x = pop();                                                             \
        y = pop();                                                             \
        push(b(new Name(x, y)));                                               \
        break
            BINOP(LOr, lgl_or_);
            BINOP(LAnd, lgl_and_);
            BINOP(Lt, lt_);
            BINOP(Gt, gt_);
            BINOP(Gte, le_);
            BINOP(Lte, ge_);
            BINOP(Mod, mod_);
            BINOP(Div, div_);
            BINOP(IDiv, idiv_);
            BINOP(Add, add_);
            BINOP(Mul, mul_);
            BINOP(Colon, colon_);
            BINOP(Pow, pow_);
            BINOP(Sub, sub_);
            BINOP(Eq, eq_);
            BINOP(Neq, ne_);

#undef BINOP
#define UNOP(Name, Op)                                                         \
    case Opcode::Op:                                                           \
        v = pop();                                                             \
        push(b(new Name(v)));                                                  \
        break
            UNOP(Plus, uplus_);
            UNOP(Minus, uminus_);
            UNOP(Inc, inc_);
            UNOP(Not, not_);
            UNOP(Is, is_);
            UNOP(Length, length_);
#undef UNOP

        case Opcode::pick_:
            push(at(bc.immediate.i));
            break;

        case Opcode::put_:
            x = top();
            for (size_t i = 0; i < bc.immediate.i - 1; ++i)
                set(i, at(i + 1));
            set(bc.immediate.i, x);
            break;

        // TODO implement!
        // (silently ignored)
        case Opcode::set_shared_:
        case Opcode::invisible_:
        case Opcode::visible_:
        case Opcode::isfun_:
        case Opcode::make_unique_:
        case Opcode::brobj_:
            break;

        // Currently unused opcodes:
        case Opcode::ldarg_:
        case Opcode::alloc_:
        case Opcode::push_code_:
        case Opcode::set_names_:
        case Opcode::names_:
        case Opcode::force_:

        // Invalid opcodes:
        case Opcode::label:
        case Opcode::invalid_:
        case Opcode::num_of:

        // Opcodes handled elsewhere
        case Opcode::brtrue_:
        case Opcode::brfalse_:
        case Opcode::br_:
            assert(false);

        // Unsupported opcodes:
        case Opcode::make_env_:
        case Opcode::get_env_:
        case Opcode::set_env_:
        case Opcode::ldloc_:
        case Opcode::stloc_:
        case Opcode::ldlval_:
        case Opcode::asast_:
        case Opcode::missing_:
        case Opcode::dispatch_stack_:
        case Opcode::dispatch_:
        case Opcode::guard_env_:
        case Opcode::call_stack_:
        case Opcode::return_:
        case Opcode::beginloop_:
        case Opcode::endcontext_:
        case Opcode::ldddvar_:
        case Opcode::int3_:
            std::cerr << "Cannot compile Function. Unsupported bc\n";
            bc.print();
            assert(false);
            break;
        }
    }

    void recoverCfg() {
        std::unordered_map<Opcode*, std::vector<Opcode*>> incom;
        // Mark incoming jmps
        for (auto pc = srcCode->code(); pc != srcCode->endCode();) {
            BC bc = BC::decode(pc);
            if (bc.isJmp()) {
                incom[bc.jmpTarget(pc)].push_back(pc);
            }
            BC::advance(&pc);
        }
        // Mark falltrough to label
        for (auto pc = srcCode->code(); pc != srcCode->endCode();) {
            BC bc = BC::decode(pc);
            if (!bc.isUncondJmp()) {
                Opcode* next = BC::next(pc);
                if (incom.count(next))
                    incom[next].push_back(pc);
            }
            BC::advance(&pc);
        }
        // Create mergepoints
        for (auto m : incom) {
            if (std::get<1>(m).size() > 1) {
                mergepoint[std::get<0>(m)] = MachineState();
            }
        }
    }

    bool doMerge(Opcode* trg) {
        MachineState& s = mergepoint.at(trg);
        if (s.entry == nullptr) {
            s.entry = b.createBB();
            s.pc = trg;
            for (size_t i = 0; i < m.stack_size(); ++i) {
                auto v = m.stack.at(i);
                auto p = new Phi;
                s.entry->append(p);
                p->addInput(b.bb, v);
                s.stack.push_back(p);
            }

            return true;
        }

        assert(m.stack_size() == s.stack_size());

        for (size_t i = 0; i < m.stack_size(); ++i) {
            Phi* p = Phi::Cast(s.stack.at(i));
            assert(p);
            Value* incom = m.stack.at(i);
            if (incom != p) {
                p->addInput(b.bb, incom);
            }
        }
        return false;
    }

    void pop_worklist() {
        assert(!worklist.empty());
        m = worklist.back();
        worklist.pop_back();
        b.bb = m.entry;
    }
};

class FunctionCompiler {
  public:
    rir::Function* src;
    Env* enclos;
    std::vector<SEXP> args;
    TheCompiler* cmp;

    FunctionCompiler(rir::Function* src, Env* enclos, std::vector<SEXP> args,
                     TheCompiler* cmp)
        : src(src), enclos(enclos), args(args), cmp(cmp) {}
    Function* operator()();
};

class TheCompiler {
  public:
    Module* m;
    TheCompiler() : m(new Module) {}

    Function* declare(rir::Function* src, const std::vector<SEXP>& a) {
        Function* fun = new Function(a);
        // TODO: catch duplicates
        m->functions.push_back(fun);
        return fun;
    }

    void operator()(SEXP in) {
        assert(isValidClosureSEXP(in));
        DispatchTable* tbl = DispatchTable::unpack(BODY(in));
        rir::Function* fun = tbl->first();
        auto formals = RList(FORMALS(in));

        std::vector<SEXP> fml;
        for (auto it = formals.begin(); it != formals.end(); ++it) {
            fml.push_back(it.tag());
        }

        FunctionCompiler f(fun, Env::theContext(), fml, this);
        f();
    }
};

Function* FunctionCompiler::operator()() {
    Function* f = cmp->declare(src, args);
    Builder b(f, enclos);
    CodeCompiler c(b, src, src->body(), cmp);
    c(true);

    if (Verify::apply(f))
        return f;
    return nullptr;
}

Value* CodeCompiler::operator()(bool addReturn) {
    recoverCfg();

    m.pc = srcCode->code();
    Opcode* end = srcCode->endCode();

    while (m.pc != end || !worklist.empty()) {
        if (m.pc == end)
            pop_worklist();

        BC bc = BC::decode(m.pc);

        if (mergepoint.count(m.pc) > 0) {
            bool todo = doMerge(m.pc);
            m = mergepoint.at(m.pc);
            b.next(m.entry);
            if (!todo) {
                if (worklist.empty()) {
                    m.clear();
                    break;
                }
                pop_worklist();
                continue;
            }
        }

        if (bc.isJmp()) {
            auto trg = bc.jmpTarget(m.pc);
            Opcode* fallpc = BC::next(m.pc);
            if (bc.isUncondJmp()) {
                m.pc = trg;
                continue;
            }

            // Conditional jump
            switch (bc.bc) {
            case Opcode::brtrue_:
            case Opcode::brfalse_: {
                Value* v = pop();
                b(new Branch(v));
                break;
            }
            case Opcode::brobj_: {
                Value* v = b(new IsObject(top()));
                b(new Branch(v));
                break;
            }
            default:
                assert(false);
            }

            BB* branch = b.createBB();
            BB* fall = b.createBB();

            switch (bc.bc) {
            case Opcode::brtrue_:
                b.bb->next0 = fall;
                b.bb->next1 = branch;
                break;
            case Opcode::brfalse_:
            case Opcode::brobj_:
                b.bb->next0 = branch;
                b.bb->next1 = fall;
                break;
            default:
                assert(false);
            }

            switch (bc.bc) {
            case Opcode::brtrue_:
            case Opcode::brfalse_: {
                m.pc = trg;
                m.entry = branch;
                worklist.push_back(m);
                break;
            }
            case Opcode::brobj_: {
                m.pc = trg;
                m.entry = branch;
                b.bb = branch;
                b(new Deopt(b.env, m.pc, m.stack_size(), &m.stack.front()));
                break;
            }
            default:
                assert(false);
            }

            m.pc = fallpc;
            m.entry = fall;
            b.bb = fall;
            continue;
        }

        const static Matcher<4> ifFunctionLiteral(
            {{{Opcode::push_, Opcode::push_, Opcode::push_, Opcode::close_}}});

        bool matched = false;

        ifFunctionLiteral(m.pc, end, [&](Opcode* next) {
            BC ldfmls = BC::advance(&m.pc);
            BC ldcode = BC::advance(&m.pc);
            /* BC ldsrc = */ BC::advance(&m.pc);
            BC::advance(&m.pc); // close

            auto fmlsl = RList(ldfmls.immediateConst());
            SEXP code = ldcode.immediateConst();
            // SEXP src = ldsrc.immediateConst();

            std::vector<SEXP> fmls;
            for (auto it = fmlsl.begin(); it != fmlsl.end(); ++it) {
                fmls.push_back(it.tag());
            }

            DispatchTable* dt = DispatchTable::unpack(code);
            rir::Function* fun = dt->first();

            FunctionCompiler f(fun, Env::theContext(), fmls, cmp);
            Function* innerF = f();

            push(b(new MkFunCls(innerF, b.env)));

            matched = true;
            m.pc = next;
        });

        if (!matched) {
            int s = m.stack_size();
            run(bc);
            assert(m.stack_size() == s - bc.popCount() + bc.pushCount());
            BC::advance(&m.pc);
        }
    }
    assert(empty());

    Value* res;
    assert(results.size() > 0);
    if (results.size() == 1) {
        b.bb = std::get<0>(results.back());
        res = std::get<1>(results.back());
    } else {
        BB* merge = b.createBB();
        Phi* phi = b(new Phi());
        for (auto r : results) {
            r.first->next0 = merge;
            phi->addInput(r.first, r.second);
        }
        phi->updateType();
        res = phi;
    }

    results.clear();
    if (addReturn)
        b(new Return(res));

    CFG cfg(b.code->entry);

    // Remove excessive Phis
    Visitor::run(b.code->entry, [&](BB* bb) {
        auto it = bb->begin();
        while (it != bb->end()) {
            Phi* p = Phi::Cast(*it);
            if (!p) {
                it++;
                continue;
            }
            if (p->nargs() == 1) {
                if (p == res)
                    res = p->arg(0).val();
                p->replaceUsesWith(p->arg(0).val());
                it = bb->remove(it);
                continue;
            }
            p->updateType();
            it++;
        }
    });

    InsertCast c(b.code->entry);
    c();

    return res;
}
}

namespace rir {

Module* PirCompiler::compileFunction(SEXP f, bool verbose) {
    TheCompiler cmp;
    cmp(f);

    size_t passnr = 0;
    auto print = [&](const std::string& pass, pir::Function* f) {
        std::cout << "============== " << pass << " == " << passnr++
                  << " ======================\n";
        f->print(std::cout);
    };

    auto apply = [&](pir::Function* f, bool verb) {
        ForceDominance::apply(f);
        if (verb)
            print("force", f);
        ScopeResolution::apply(f);
        if (verb)
            print("scope", f);
        Cleanup::apply(f);
        if (verb)
            print("cleanup", f);
        DelayInstr::apply(f);
        if (verb)
            print("delay instr", f);
        ElideEnv::apply(f);
        if (verb)
            print("elide env", f);
        DelayEnv::apply(f);
        if (verb)
            print("delay env", f);
    };

    if (verbose)
        cmp.m->print(std::cout);

    for (auto f : cmp.m->functions) {
        apply(f, false);
        apply(f, false);
    }

    if (verbose) {
        std::cout << "============== whole module passes ======================\n";
        cmp.m->print(std::cout);
    }

    auto fun = *cmp.m->functions.begin();
    for (size_t iter = 0; iter < 5; ++iter) {
        Inline::apply(fun);
        if (verbose)
            print("inline", fun);

        apply(fun, verbose);
    }

    return cmp.m;
}
}

extern "C" {
// For debugging
using namespace rir::pir;
void dumpf(rir::pir::Function* x) {
    x->print(std::cerr);
    std::cerr << "\n";
    std::flush(std::cerr);
}
void dumpp(rir::pir::Promise* x) {
    x->print(std::cerr);
    std::cerr << "\n";
    std::flush(std::cerr);
}
void dumpi(rir::pir::Instruction* x) {
    x->print(std::cerr);
    std::cerr << "\n";
    std::flush(std::cerr);
}
void dumpv(rir::pir::Value* x) {
    x->printRef(std::cerr);
    std::cerr << "\n";
    std::flush(std::cerr);
}
void dumpb(rir::pir::BB* x) {
    x->print(std::cerr);
    std::cerr << "\n";
    std::flush(std::cerr);
}
};
