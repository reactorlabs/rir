#include "pir_2_rir.h"
#include "../../analysis/last_env.h"
#include "../../pir/pir_impl.h"
#include "../../pir/value_list.h"
#include "../../transform/bb.h"
#include "../../util/cfg.h"
#include "../../util/visitor.h"
#include "R/BuiltinIds.h"
#include "allocators.h"
#include "compiler/analysis/reference_count.h"
#include "compiler/analysis/verifier.h"
#include "compiler/native/lower_llvm.h"
#include "compiler/parameter.h"
#include "event_counters.h"
#include "interpreter/instance.h"
#include "ir/CodeStream.h"
#include "ir/CodeVerifier.h"
#include "runtime/DispatchTable.h"
#include "simple_instruction_list.h"
#include "utils/FunctionWriter.h"

#include "../../debugging/PerfCounter.h"

#include <algorithm>
#include <chrono>
#include <iomanip>
#include <list>
#include <sstream>

namespace rir {
namespace pir {

namespace {

class Context {
  public:
    std::stack<CodeStream*> css;
    FunctionWriter& fun;

    explicit Context(FunctionWriter& fun) : fun(fun) {}
    ~Context() { assert(css.empty()); }

    CodeStream& cs() { return *css.top(); }

    rir::Code* finalizeCode(size_t localsCnt, size_t cacheBindingsCount) {
        auto res = cs().finalize(localsCnt, cacheBindingsCount);
        delete css.top();
        css.pop();
        return res;
    }

    void push(SEXP ast) { css.push(new CodeStream(fun, ast)); }
};

class Pir2Rir {
  public:
    Pir2Rir(Pir2RirCompiler& cmp, ClosureVersion* cls, bool dryRun,
            ClosureStreamLogger& log)
        : compiler(cmp), cls(cls), dryRun(dryRun), log(log) {}
    rir::Code* compileCode(Context& ctx, Code* code);
    rir::Code* getPromise(Context& ctx, Promise* code);

    void lower(Code* code);
    void toCSSA(Code* code);
    rir::Function* finalize();

  private:
    // bool isInfiniteLoopHead(BB* bb, const DominanceGraph& dom);

    Pir2RirCompiler& compiler;
    ClosureVersion* cls;
    std::unordered_map<Promise*, rir::Code*> promises;
    bool dryRun;
    ClosureStreamLogger& log;

    class CodeBuffer {
      private:
        struct Src {
            enum { None, Sexp, Idx } tag;
            union {
                SEXP sexp;
                unsigned idx;
            } u;
        };
        using RIRInstruction = std::pair<BC, Src>;
        std::list<RIRInstruction> code;
        CodeStream& cs;

        void emplace_back(BC&& bc, Src src) {
            code.emplace_back(std::make_pair(std::move(bc), std::move(src)));
        }

        void peephole() {
            /*TODO: there are still more patterns that could be cleaned up:
             *   pick(2) swap() pick(2) -> swap()
             *   pick(2) pop() swap() pop() pop() -> pop() pop() pop()
             */

            auto plus = [](std::list<RIRInstruction>::iterator it, int n) {
                while (n--)
                    it++;
                return it;
            };

            Src noSource;
            noSource.tag = Src::None;
            int steam = 5;
            bool changed = false;

            do {
                changed = false;
                auto it = code.begin();
                while (it != code.end()) {
                    auto next = plus(it, 1);
                    auto& bc = it->first;

                    if (bc.is(rir::Opcode::pick_)) {
                        unsigned arg = bc.immediate.i;
                        unsigned n = 1;
                        // Have 1 pick(arg), can remove if we find arg + 1 of
                        // them
                        auto last = next;
                        for (; last != code.end(); last++) {
                            auto& bc = last->first;
                            if (bc.is(rir::Opcode::pick_) &&
                                bc.immediate.i == arg)
                                n++;
                            else
                                break;
                        }
                        if (n == arg + 1 && last != code.end()) {
                            next = code.erase(it, last);
                            changed = true;
                        } else if (arg == 1) {
                            next = code.erase(it);
                            next = code.emplace(next, BC::swap(), noSource);
                            changed = true;
                        }
                    } else if (bc.is(rir::Opcode::pull_)) {
                        if (bc.immediate.i == 0) {
                            next = code.erase(it);
                            next = code.emplace(next, BC::dup(), noSource);
                            changed = true;
                        } else if (bc.immediate.i == 1 && next != code.end() &&
                                   next->first.is(rir::Opcode::pull_) &&
                                   next->first.immediate.i == 1) {
                            next = code.erase(it, plus(next, 1));
                            next = code.emplace(next, BC::dup2(), noSource);
                            changed = true;
                        }
                    } else if (bc.is(rir::Opcode::swap_) &&
                               next != code.end() &&
                               next->first.is(rir::Opcode::swap_)) {
                        next = code.erase(it, plus(next, 1));
                        changed = true;
                    } else if (bc.is(rir::Opcode::push_) &&
                               next != code.end() &&
                               next->first.is(rir::Opcode::pop_)) {
                        next = code.erase(it, plus(next, 1));
                        changed = true;
                    } else if (bc.is(rir::Opcode::dup_) && next != code.end() &&
                               plus(next, 1) != code.end() &&
                               plus(next, 2) != code.end() &&
                               next->first.is(rir::Opcode::for_seq_size_) &&
                               plus(next, 1)->first.is(rir::Opcode::swap_) &&
                               plus(next, 2)->first.is(rir::Opcode::pop_)) {
                        next = plus(code.erase(it), 1);
                        next = code.erase(next, plus(next, 2));
                        changed = true;
                    } else if (bc.is(rir::Opcode::ldvar_noforce_) &&
                               next != code.end() &&
                               next->first.is(rir::Opcode::force_)) {
                        auto arg = Pool::get(bc.immediate.pool);
                        next = code.erase(it, plus(next, 1));
                        next = code.emplace(next, BC::ldvar(arg), noSource);
                        changed = true;
                    } else if (bc.is(rir::Opcode::ldvar_noforce_cached_) &&
                               next != code.end() &&
                               next->first.is(rir::Opcode::force_)) {
                        auto arg =
                            Pool::get(bc.immediate.poolAndCache.poolIndex);
                        auto cacheIndex = bc.immediate.poolAndCache.cacheIndex;
                        next = code.erase(it, plus(next, 1));
                        next = code.emplace(
                            next, BC::ldvarCached(arg, cacheIndex), noSource);
                        changed = true;
                    } else if (bc.is(rir::Opcode::pop_)) {
                        unsigned n = 1;
                        auto last = next;
                        for (; last != code.end(); last++) {
                            auto& bc = last->first;
                            if (bc.is(rir::Opcode::pop_))
                                n++;
                            else
                                break;
                        }
                        if (n > 1 && last != code.end()) {
                            next = code.erase(it, last);
                            next = code.emplace(next, BC::popn(n), noSource);
                            changed = true;
                        }
                    }

                    it = next;
                }
            } while (changed && steam-- > 0);
        }

      public:
        void flush() {
            peephole();
            for (auto const& instr : code) {
                cs << instr.first;
                switch (instr.second.tag) {
                case Src::Sexp:
                    cs.addSrc(instr.second.u.sexp);
                    break;
                case Src::Idx:
                    cs.addSrcIdx(instr.second.u.idx);
                    break;
                default:
                    break;
                }
            }
            code.clear();
        }

        explicit CodeBuffer(CodeStream& cs) : cs(cs) {}
        ~CodeBuffer() { assert(code.empty()); }

        void add(BC&& bc) {
            Src s;
            s.tag = Src::None;
            emplace_back(std::move(bc), s);
        }

        void add(BC&& bc, SEXP src) {
            Src s;
            s.tag = src ? Src::Sexp : Src::None;
            s.u.sexp = src;
            emplace_back(std::move(bc), s);
        }

        void add(BC&& bc, unsigned idx) {
            Src s;
            s.tag = Src::Idx;
            s.u.idx = idx;
            emplace_back(std::move(bc), s);
        }

        void add(BC::Label label) {
            flush();
            cs << label;
        }
    };
};

#ifdef ENABLE_EVENT_COUNTERS
static unsigned MkEnvEmited =
    EventCounters::instance().registerCounter("mkenv emited");
static unsigned MkEnvStubEmited =
    EventCounters::instance().registerCounter("mkenvstub emited");
static unsigned ClosuresCompiled =
    EventCounters::instance().registerCounter("closures compiled");
#endif

static int PIR_NATIVE_BACKEND =
    getenv("PIR_NATIVE_BACKEND") ? atoi(getenv("PIR_NATIVE_BACKEND")) : 0;

rir::Code* Pir2Rir::compileCode(Context& ctx, Code* code) {
#ifdef ENABLE_EVENT_COUNTERS
    if (ENABLE_EVENT_COUNTERS) {
        VisitorNoDeoptBranch::run(code->entry, [&](Instruction* i) {
            if (auto mkenv = MkEnv::Cast(i)) {
                if (mkenv->stub)
                    EventCounters::instance().count(MkEnvStubEmited);
                else
                    EventCounters::instance().count(MkEnvEmited);
            }
        });
    }
#endif

    lower(code);
    toCSSA(code);
#ifdef FULLVERIFIER
    Verify::apply(cls, "Error after lowering", true);
#else
#ifdef ENABLE_SLOWASSERT
    Verify::apply(cls, "Error after lowering");
#endif
#endif
    log.CSSA(code);

    Visitor::run(code->entry,
                 [](Instruction* i) { i->updateTypeAndEffects(); });

    LivenessIntervals live(code, code->nextBBId);
    SSAAllocator alloc(code, cls, live, true, log.out());
    log.afterAllocator(code, [&](std::ostream& o) { alloc.print(o); });
    alloc.compute();
    alloc.verify();

    // Create labels for all bbs
    std::unordered_map<BB*, BC::Label> bbLabels;
    BreadthFirstVisitor::run(
        code->entry, [&](BB* bb) { bbLabels[bb] = ctx.cs().mkLabel(); });

    LastEnv lastEnv(cls, code, log.out());
    std::unordered_map<Value*, BC::Label> pushContexts;
    std::unordered_set<BC::Label> pushContextsPopped;

    std::deque<unsigned> order;
    LoweringVisitor::run(code->entry, [&](BB* bb) { order.push_back(bb->id); });

    NeedsRefcountAdjustment refcount;
    {
        StaticReferenceCount refcountAnalysis(cls, log.out());
        refcountAnalysis();
        refcount = refcountAnalysis.getGlobalState();
    }
    std::unordered_set<Instruction*> needsLdVarForUpdate;
    {
        Visitor::run(code->entry, [&](Instruction* i) {
            switch (i->tag) {
            case Tag::Subassign1_1D:
            case Tag::Subassign2_1D:
            case Tag::Subassign1_2D:
            case Tag::Subassign2_2D:
                // Subassigns override the vector, even if the named count
                // is 1. This is only valid, if we are sure that the vector
                // is local, ie. vector and subassign operation come from
                // the same lexical scope.
                if (auto vec = Instruction::Cast(
                        i->arg(1).val()->followCastsAndForce())) {
                    if (auto ld = LdVar::Cast(vec)) {
                        if (auto su = vec->hasSingleUse()) {
                            if (auto st = StVar::Cast(su)) {
                                if (ld->env() != st->env())
                                    needsLdVarForUpdate.insert(vec);
                                break;
                            }
                        }
                        if (ld->env() != i->env())
                            needsLdVarForUpdate.insert(vec);
                    }
                }
                break;
            default: {}
            }
        });
    }

    std::unordered_map<Promise*, unsigned> promMap;

    CodeBuffer cb(ctx.cs());

    const CachePosition cache(code);
    if (cache.globalEnvsCacheSize() > 0)
        cb.add(BC::clearBindingCache(0, cache.globalEnvsCacheSize()));

    LoweringVisitor::run(code->entry, [&](BB* bb) {
        order.pop_front();
        cb.add(bbLabels[bb]);

        // Only needed for deopt branches
        for (auto it = bb->begin(); it != bb->end(); ++it) {
            auto instr = *it;

            // Prepare stack and arguments
            {

                std::vector<bool> removedStackValues(
                    alloc.sa->stackBefore(instr).size(), false);
                size_t pushedStackValues = 0;

                auto debugAddVariableName = [&](Value* v) -> SEXP {
#ifdef ENABLE_SLOWASSERT
                    std::stringstream ss;
                    v->printRef(ss);
                    // Protect error: install can allocate and calling it
                    // many times during pir2rir might in principle cause
                    // some of these to be gc'd.. But unlikely and only
                    // possible in debug mode
                    return Rf_install(ss.str().c_str());
#else
                    return nullptr;
#endif
                };

                auto moveToTOS = [&](size_t offset) {
                    cb.add(BC::pick(offset));
                };

                auto copyToTOS = [&](size_t offset) {
                    cb.add(BC::pull(offset));
                };

                auto getFromStack = [&](Value* what, size_t argNumber) {
                    auto lastUse = alloc.lastUse(instr, argNumber);
                    auto offset =
                        alloc.getStackOffset(instr, removedStackValues, what,
                                             lastUse) +
                        pushedStackValues;
                    if (lastUse)
                        moveToTOS(offset);
                    else
                        copyToTOS(offset);
                };

                auto loadEnv = [&](Value* what, size_t argNumber) {
                    if (what == Env::notClosed()) {
                        cb.add(BC::parentEnv());
                    } else if (what == Env::nil()) {
                        cb.add(BC::push(R_NilValue));
                    } else if (Env::isStaticEnv(what)) {
                        auto env = Env::Cast(what);
                        cb.add(BC::push(env->rho));
                    } else {
                        if (!alloc.hasSlot(what)) {
                            std::cerr << "Don't know how to load the env ";
                            what->printRef(std::cerr);
                            std::cerr << " (" << tagToStr(what->tag) << ")\n";
                            assert(false);
                        }
                        if (alloc.onStack(what)) {
                            getFromStack(what, argNumber);
                        } else {
                            cb.add(BC::ldloc(alloc[what]),
                                   debugAddVariableName(what));
                        }
                    }
                    pushedStackValues++;
                };

                auto adjustRefcount = refcount.beforeUse.find(instr);
                auto loadArg = [&](Value* what, size_t argNumber) {
                    if (what == UnboundValue::instance()) {
                        cb.add(BC::push(R_UnboundValue));
                    } else if (what == MissingArg::instance()) {
                        cb.add(BC::push(R_MissingArg));
                    } else if (what == True::instance()) {
                        cb.add(BC::push(R_TrueValue));
                    } else if (what == False::instance()) {
                        cb.add(BC::push(R_FalseValue));
                    } else if (what == NaLogical::instance()) {
                        cb.add(BC::push(R_LogicalNAValue));
                    } else {
                        if (!alloc.hasSlot(what)) {
                            std::cerr << "Don't know how to load the arg ";
                            what->printRef(std::cerr);
                            std::cerr << " (" << tagToStr(what->tag) << ")\n";
                            assert(false);
                        }
                        if (alloc.onStack(what)) {
                            getFromStack(what, argNumber);
                        } else {
                            cb.add(BC::ldloc(alloc[what]),
                                   debugAddVariableName(what));
                        }

                        if (auto j = Instruction::Cast(what->followCasts())) {
                            if (adjustRefcount != refcount.beforeUse.end()) {
                                auto adjust = adjustRefcount->second.find(j);
                                if (adjust != adjustRefcount->second.end()) {
                                    if (adjust->second ==
                                        NeedsRefcountAdjustment::SetShared)
                                        cb.add(BC::setShared());
                                    else if (adjust->second ==
                                             NeedsRefcountAdjustment::
                                                 EnsureNamed)
                                        cb.add(BC::ensureNamed());
                                }
                            }
                        }
                    }
                    pushedStackValues++;
                };

                auto loadPhiArg = [&](Phi* phi) {
                    if (!alloc.hasSlot(phi)) {
                        std::cerr << "Don't know how to load the phi arg ";
                        phi->printRef(std::cerr);
                        std::cerr << " (" << tagToStr(phi->tag) << ")\n";
                        assert(false);
                    }
                    if (alloc.onStack(phi)) {
                        auto offset = alloc.stackPhiOffset(instr, phi);
                        moveToTOS(offset);
                    } else {
                        cb.add(BC::ldloc(alloc[phi]),
                               debugAddVariableName(phi));
                    }
                };

                // Remove values from the stack that are dead here
                auto toDrop = alloc.sa->toDrop(instr);
                for (auto val : VisitorHelpers::reverse(toDrop)) {
                    // If not actually allocated on stack, do nothing
                    if (!alloc.onStack(val))
                        continue;

                    auto offset = alloc.getStackOffset(
                        instr, removedStackValues, val, true);
                    moveToTOS(offset);
                    cb.add(BC::pop());
                }

                if (auto phi = Phi::Cast(instr)) {
                    loadPhiArg(phi);
                } else {
                    size_t argNumber = 0;
                    instr->eachArg([&](Value* what) {
                        if (what == Env::elided() ||
                            what->tag == Tag::Tombstone ||
                            what->type == NativeType::context) {
                            argNumber++;
                            return;
                        }

                        if (instr->hasEnv() && instr->env() == what) {
                            if (LastEnv::explicitEnvValue(instr)) {
                                loadEnv(what, argNumber);
                            } else {
                                auto env = instr->env();
                                if (!lastEnv.envStillValid(instr)) {
                                    loadEnv(env, argNumber);
                                    cb.add(BC::setEnv());
                                } else {
                                    if (alloc.hasSlot(env) &&
                                        alloc.onStack(env) &&
                                        alloc.lastUse(instr, argNumber)) {
                                        auto offset =
                                            alloc.getStackOffset(
                                                instr, removedStackValues, env,
                                                true) +
                                            pushedStackValues;
                                        moveToTOS(offset);
                                        cb.add(BC::pop());
                                    }
                                }
                            }
                        } else {
                            loadArg(what, argNumber);
                        }
                        argNumber++;
                    });

                    // This is needed to prevent builtin calls from
                    // materializing env stubs
                    if (CallSafeBuiltin::Cast(instr) ||
                        (instr->mayHaveEnv() &&
                         instr->env() == Env::elided())) {
                        auto cur = lastEnv.currentEnv(instr);
                        if (!cur ||
                            (MkEnv::Cast(cur) && MkEnv::Cast(cur)->stub)) {
                            cb.add(BC::push(R_BaseNamespace));
                            cb.add(BC::setEnv());
                        }
                    }
                }
            }

            auto compileCallDots =
                [&](CallInstruction* call, unsigned srcIdx,
                    const std::function<void()>& loadCallee,
                    const std::function<SEXP(size_t)>& getName) {
                    bool hasDotsArgs = false;
                    call->eachCallArg([&](Value* v) {
                        if (ExpandDots::Cast(v))
                            hasDotsArgs = true;
                    });
                    if (!hasDotsArgs)
                        return false;

                    std::vector<SEXP> names;
                    size_t p = 0;
                    call->eachCallArg([&](Value* v) {
                        if (ExpandDots::Cast(v))
                            names.push_back(R_DotsSymbol);
                        else
                            names.push_back(getName(p));
                        p++;
                    });
                    loadCallee();
                    cb.add(BC::callDots(call->nCallArgs(), names,
                                        Pool::get(srcIdx),
                                        call->inferAvailableAssumptions()));
                    return true;
                };

            switch (instr->tag) {
            case Tag::LdDots: {
                SLOWASSERT(instr->usesAreOnly(bb, {Tag::ExpandDots}));
                auto mkenv = MkEnv::Cast(instr->env());
                if (mkenv && mkenv->stub) {
                    cb.add(
                        BC::ldvarNoForceStubbed(mkenv->indexOf(R_DotsSymbol)));
                    break;
                }
                cb.add(BC::push(R_DotsSymbol));
                break;
            }

            case Tag::ExpandDots: {
                // handled in calls
                break;
            }

            case Tag::DotsList: {
                auto d = DotsList::Cast(instr);
                cb.add(BC::mkDotlist(d->names));
                break;
            }

            case Tag::RecordDeoptReason: {
                cb.add(BC::recordDeopt(RecordDeoptReason::Cast(instr)->reason));
                break;
            }

            case Tag::LdConst: {
                cb.add(BC::push_from_pool(LdConst::Cast(instr)->idx));
                break;
            }

            case Tag::LdFun: {
                auto ldfun = LdFun::Cast(instr);
                cb.add(BC::ldfun(ldfun->varName));
                break;
            }

            case Tag::LdVar: {
                auto ldvar = LdVar::Cast(instr);
                auto key =
                    CachePosition::NameAndEnv(ldvar->varName, ldvar->env());
                auto mkenv = MkEnv::Cast(ldvar->env());
                if (mkenv && mkenv->stub) {
                    cb.add(BC::ldvarNoForceStubbed(
                        mkenv->indexOf(ldvar->varName)));
                } else if (needsLdVarForUpdate.count(instr)) {
                    if (cache.isCached(key)) {
                        cb.add(BC::ldvarForUpdateCached(ldvar->varName,
                                                        cache.indexOf(key)));
                    } else {
                        cb.add(BC::ldvarForUpdate(ldvar->varName));
                    }
                } else {
                    if (cache.isCached(key)) {
                        cb.add(BC::ldvarNoForceCached(ldvar->varName,
                                                      cache.indexOf(key)));
                    } else {
                        cb.add(BC::ldvarNoForce(ldvar->varName));
                    }
                }
                break;
            }

            case Tag::ForSeqSize: {
                cb.add(BC::forSeqSize());
                // TODO: currently we always pop the sequence, since we
                // cannot deal with instructions that do not pop the value
                // after use.
                cb.add(BC::swap());
                cb.add(BC::pop());
                break;
            }

            case Tag::LdArg: {
                auto ld = LdArg::Cast(instr);
                cb.add(BC::ldarg(ld->id));
                break;
            }

            case Tag::StVarSuper: {
                auto stvar = StVarSuper::Cast(instr);
                // In case this assert fails it means we start supporting nested
                // stub environments. In that case we should translate the store
                // super to a special rir instruction that does the store in the
                // stubbed parent.
                assert(!(MkEnv::Cast(stvar->env()) &&
                         MkEnv::Cast(stvar->env())->stub &&
                         MkEnv::Cast(MkEnv::Cast(stvar->env())->lexicalEnv()) &&
                         MkEnv::Cast(MkEnv::Cast(stvar->env())->lexicalEnv())
                             ->stub));
                cb.add(BC::stvarSuper(stvar->varName));
                break;
            }

            case Tag::LdVarSuper: {
                auto ldvar = LdVarSuper::Cast(instr);
                cb.add(BC::ldvarNoForceSuper(ldvar->varName));
                break;
            }

            case Tag::StVar: {
                auto stvar = StVar::Cast(instr);
                auto key =
                    CachePosition::NameAndEnv(stvar->varName, stvar->env());
                auto mkenv = MkEnv::Cast(stvar->env());
                assert(!MkEnv::Cast(stvar->arg(0).val()));
                if (stvar->isStArg) {
                    if (mkenv && mkenv->stub) {
                        cb.add(
                            BC::stargStubbed(mkenv->indexOf(stvar->varName)));
                    } else if (cache.isCached(key)) {
                        cb.add(BC::stargCached(stvar->varName,
                                               cache.indexOf(key)));
                    } else {
                        cb.add(BC::starg(stvar->varName));
                    }
                } else {
                    if (mkenv && mkenv->stub) {
                        cb.add(
                            BC::stvarStubbed(mkenv->indexOf(stvar->varName)));
                    } else if (cache.isCached(key)) {
                        cb.add(BC::stvarCached(stvar->varName,
                                               cache.indexOf(key)));
                    } else {
                        cb.add(BC::stvar(stvar->varName));
                    }
                }
                break;
            }

            case Tag::MkArg: {
                auto mk = MkArg::Cast(instr);
                auto p = mk->prom();
                unsigned id = ctx.cs().addPromise(getPromise(ctx, p));
                promMap[p] = id;
                if (mk->isEager()) {
                    cb.add(BC::mkEagerPromise(id));
                } else {
                    // Remove the UnboundValue argument pushed by loadArg, this
                    // will be cleaned up by the peephole opts
                    cb.add(BC::pop());
                    cb.add(BC::mkPromise(id));
                }
                break;
            }

            case Tag::UpdatePromise: {
                cb.add(BC::updatePromise());
                break;
            }

            case Tag::MkFunCls: {
                // TODO: would be nice to compile the function here. But I am
                // not sure if our compiler backend correctly deals with not
                // closed closures.
                auto mkfuncls = MkFunCls::Cast(instr);
                auto cls = mkfuncls->cls;
                cb.add(BC::push(cls->formals().original()));
                cb.add(BC::push(mkfuncls->originalBody->container()));
                cb.add(BC::push(cls->srcRef()));
                cb.add(BC::close());
                break;
            }

            case Tag::Missing: {
                auto m = Missing::Cast(instr);
                cb.add(BC::missing(m->varName));
                break;
            }

            case Tag::Is: {
                auto is = Is::Cast(instr);
                cb.add(BC::is(is->sexpTag));
                break;
            }

            case Tag::IsType: {
                auto is = IsType::Cast(instr);
                cb.add(BC::isType(is->typeChecks()));
                break;
            }

            case Tag::ColonInputEffects: {
                cb.add(BC::colonInputEffects(), instr->srcIdx);
                // TODO: We might want to add some mechanism in PIR to
                // distinguish between popped and just observed arguments.
                // This reads 2 args but pops 0 - PIR doesn't know and loads all
                // args, so we must manually pop them.
                cb.add(BC::put(2));
                cb.add(BC::popn(2));
                break;
            }

            case Tag::ColonCastRhs: {
                cb.add(BC::colonCastRhs());
                // This reads 2 args but only pops 1 - PIR doesn't know and
                // loads all args, so we must manually pop the other.
                cb.add(BC::swap());
                cb.add(BC::pop());
                break;
            }

#define EMPTY(Name)                                                            \
    case Tag::Name: {                                                          \
        break;                                                                 \
    }
                EMPTY(CastType);
                EMPTY(Nop);
                EMPTY(PirCopy);
#undef EMPTY

#define SIMPLE(Name, Factory)                                                  \
    case Tag::Name: {                                                          \
        cb.add(BC::Factory());                                                 \
        break;                                                                 \
    }
                SIMPLE(LdFunctionEnv, getEnv);
                SIMPLE(Visible, visible);
                SIMPLE(Invisible, invisible);
                SIMPLE(Names, names);
                SIMPLE(SetNames, setNames);
                SIMPLE(Identical, identicalNoforce);
                SIMPLE(IsEnvStub, isstubenv);
                SIMPLE(LOr, lglOr);
                SIMPLE(LAnd, lglAnd);
                SIMPLE(Inc, inc);
                SIMPLE(XLength, xlength_);
                SIMPLE(Force, force);
                SIMPLE(AsTest, asbool);
                SIMPLE(ChkMissing, checkMissing);
                SIMPLE(ChkClosure, checkClosure);
                SIMPLE(MkCls, close);
                SIMPLE(ColonCastLhs, colonCastLhs);
#define V(V, name, Name) SIMPLE(Name, name);
                SIMPLE_INSTRUCTIONS(V, _);
#undef V
#undef SIMPLE

#define SIMPLE_WITH_SRCIDX(Name, Factory)                                      \
    case Tag::Name: {                                                          \
        cb.add(BC::Factory(), instr->srcIdx);                                  \
        break;                                                                 \
    }
                SIMPLE_WITH_SRCIDX(Add, add);
                SIMPLE_WITH_SRCIDX(Sub, sub);
                SIMPLE_WITH_SRCIDX(Mul, mul);
                SIMPLE_WITH_SRCIDX(Div, div);
                SIMPLE_WITH_SRCIDX(IDiv, idiv);
                SIMPLE_WITH_SRCIDX(Mod, mod);
                SIMPLE_WITH_SRCIDX(Pow, pow);
                SIMPLE_WITH_SRCIDX(Lt, lt);
                SIMPLE_WITH_SRCIDX(Gt, gt);
                SIMPLE_WITH_SRCIDX(Lte, le);
                SIMPLE_WITH_SRCIDX(Gte, ge);
                SIMPLE_WITH_SRCIDX(Eq, eq);
                SIMPLE_WITH_SRCIDX(Neq, ne);
                SIMPLE_WITH_SRCIDX(Colon, colon);
                SIMPLE_WITH_SRCIDX(AsLogical, asLogical);
                SIMPLE_WITH_SRCIDX(Plus, uplus);
                SIMPLE_WITH_SRCIDX(Minus, uminus);
                SIMPLE_WITH_SRCIDX(Not, not_);
                SIMPLE_WITH_SRCIDX(Extract1_1D, extract1_1);
                SIMPLE_WITH_SRCIDX(Extract2_1D, extract2_1);
                SIMPLE_WITH_SRCIDX(Extract1_2D, extract1_2);
                SIMPLE_WITH_SRCIDX(Extract2_2D, extract2_2);
                SIMPLE_WITH_SRCIDX(Extract1_3D, extract1_3);
                SIMPLE_WITH_SRCIDX(Subassign1_1D, subassign1_1);
                SIMPLE_WITH_SRCIDX(Subassign2_1D, subassign2_1);
                SIMPLE_WITH_SRCIDX(Subassign1_2D, subassign1_2);
                SIMPLE_WITH_SRCIDX(Subassign2_2D, subassign2_2);
                SIMPLE_WITH_SRCIDX(Subassign1_3D, subassign1_3);
#undef SIMPLE_WITH_SRCIDX

            case Tag::Call: {
                auto call = Call::Cast(instr);
                if (compileCallDots(call, call->srcIdx, []() {},
                                    [](size_t) { return R_NilValue; }))
                    break;

                cb.add(BC::call(call->nCallArgs(), Pool::get(call->srcIdx),
                                call->inferAvailableAssumptions()));
                break;
            }

            case Tag::NamedCall: {
                auto call = NamedCall::Cast(instr);
                if (compileCallDots(call, call->srcIdx, []() {},
                                    [&](size_t i) { return call->names[i]; }))
                    break;

                cb.add(BC::call(call->nCallArgs(), call->names,
                                Pool::get(call->srcIdx),
                                call->inferAvailableAssumptions()));
                break;
            }

            case Tag::StaticCall: {
                auto call = StaticCall::Cast(instr);
                call->eachArg([](Value* v) { assert(!ExpandDots::Cast(v)); });
                if (call->cls()->hasOriginClosure()) {
                    SEXP originalClosure = call->cls()->rirClosure();
                    auto dt = DispatchTable::unpack(BODY(originalClosure));
                    if (auto trg = call->tryOptimisticDispatch()) {
                        // Avoid recursivly compiling the same closure
                        auto fun = compiler.alreadyCompiled(trg);
                        SEXP funCont = nullptr;

                        if (fun) {
                            funCont = fun->container();
                        } else if (!compiler.isCompiling(trg)) {
                            fun = compiler.compile(trg, dryRun);
                            funCont = fun->container();
                            Protect p(funCont);
                            assert(originalClosure &&
                                   "Cannot compile synthetic closure");
                            dt->insert(fun);
                        }
                        auto bc = BC::staticCall(
                            call->nCallArgs(), Pool::get(call->srcIdx),
                            originalClosure, funCont,
                            call->inferAvailableAssumptions());
                        auto hint =
                            bc.immediate.staticCallFixedArgs.versionHint;
                        cb.add(std::move(bc));
                        if (!funCont)
                            compiler.needsPatching(trg, hint);
                    } else {
                        // Something went wrong with dispatching, let's put the
                        // baseline there
                        cb.add(BC::staticCall(
                            call->nCallArgs(), Pool::get(call->srcIdx),
                            originalClosure, dt->baseline()->container(),
                            call->inferAvailableAssumptions()));
                    }
                } else {
                    assert(call->runtimeClosure());
                    cb.add(BC::call(call->nCallArgs(), Pool::get(call->srcIdx),
                                    call->inferAvailableAssumptions()));
                }
                break;
            }

            case Tag::CallBuiltin: {
                auto blt = CallBuiltin::Cast(instr);
                if (compileCallDots(blt, blt->srcIdx,
                                    [&]() {
                                        cb.add(BC::push(blt->blt));
                                        cb.add(BC::put(blt->nCallArgs()));
                                    },
                                    [](size_t) { return R_NilValue; }))
                    break;
                cb.add(BC::callBuiltin(blt->nCallArgs(), Pool::get(blt->srcIdx),
                                       blt->blt));
                break;
            }

            case Tag::CallSafeBuiltin: {
                auto blt = CallSafeBuiltin::Cast(instr);
                if (compileCallDots(blt, blt->srcIdx,
                                    [&]() {
                                        cb.add(BC::push(blt->blt));
                                        cb.add(BC::put(blt->nCallArgs()));
                                    },
                                    [](size_t) { return R_NilValue; }))
                    break;
                cb.add(BC::callBuiltin(blt->nargs(), Pool::get(blt->srcIdx),
                                       blt->blt));
                break;
            }

            case Tag::PushContext: {
                if (!pushContexts.count(instr))
                    pushContexts[instr] = ctx.cs().mkLabel();
                cb.add(BC::pushContext(pushContexts.at(instr)));
                break;
            }

            case Tag::PopContext: {
                auto push = PopContext::Cast(instr)->push();
                if (!pushContexts.count(push))
                    pushContexts[push] = ctx.cs().mkLabel();
                auto label = pushContexts.at(push);
                cb.add(label);
                pushContextsPopped.insert(label);
                cb.add(BC::popContext(INT_MAX));
                break;
            }

            case Tag::MkEnv: {
                auto mkenv = MkEnv::Cast(instr);
                cb.add(BC::mkEnv(mkenv->varName, mkenv->missing, mkenv->context,
                                 mkenv->stub));
                cache.ifCacheRange(mkenv, [&](CachePosition::StartSize range) {
                    if (range.second > 0)
                        cb.add(
                            BC::clearBindingCache(range.first, range.second));
                });
                break;
            }

            case Tag::MaterializeEnv: {
                cb.add(BC::materializeEnv());
                break;
            }

            case Tag::Phi: {
                // Phi functions are no-ops, because after allocation on
                // CSSA form, all arguments and the funcion itself are
                // allocated to the same place
                auto phi = Phi::Cast(instr);
                phi->eachArg([&](BB*, Value* arg) {
                    assert(((alloc.onStack(phi) && alloc.onStack(arg)) ||
                            (alloc[phi] == alloc[arg])) &&
                           "Phi inputs must all be allocated in 1 slot");
                });
                break;
            }

            // BB exitting instructions
            case Tag::Branch: {
                assert(bb->isBranch());
                auto trueBranch = bb->trueBranch();
                auto falseBranch = bb->falseBranch();
                if (trueBranch->id == order.front()) {
                    cb.add(BC::brfalse(bbLabels[falseBranch]));
                    cb.add(BC::br(bbLabels[trueBranch]));
                } else {
                    cb.add(BC::brtrue(bbLabels[trueBranch]));
                    cb.add(BC::br(bbLabels[falseBranch]));
                }
                // This is the end of this BB
                return;
            }

            case Tag::Return: {
                cb.add(BC::ret());
                // end of this BB
                return;
            }

            case Tag::ScheduledDeopt: {
                auto deopt = ScheduledDeopt::Cast(instr);

                size_t nframes = deopt->frames.size();

                SEXP store =
                    Rf_allocVector(RAWSXP, sizeof(DeoptMetadata) +
                                               nframes * sizeof(FrameInfo));
                auto m = new (DATAPTR(store)) DeoptMetadata;
                m->numFrames = nframes;

                size_t i = 0;
                // Frames in the ScheduledDeopt are in pir argument order
                // (from left to right). On the other hand frames in the rir
                // deopt_ instruction are in stack order, from tos down.
                for (auto fi = deopt->frames.rbegin();
                     fi != deopt->frames.rend(); fi++)
                    m->frames[i++] = *fi;

                cb.add(BC::deopt(store));
                // deopt is exit
                return;
            }

            // Invalid, should've been lowered away
            case Tag::FrameState:
            case Tag::Deopt:
            case Tag::Assume:
            case Tag::Checkpoint: {
                assert(false && "Deopt instructions must be lowered into "
                                "standard branches and scheduled deopt, "
                                "before pir_2_rir");
                break;
            }

            // Values, not instructions
#define V(Value) case Tag::Value:
            COMPILER_VALUES(V) {
#undef V
                break;
            }

            // Dummy sentinel enum item
            case Tag::_UNUSED_: {
                break;
            }
            }

            auto adjust = refcount.atCreation.find(instr);
            if (adjust != refcount.atCreation.end()) {
                if (adjust->second == NeedsRefcountAdjustment::SetShared)
                    cb.add(BC::setShared());
                else if (adjust->second == NeedsRefcountAdjustment::EnsureNamed)
                    cb.add(BC::ensureNamed());
            }

            // Check the return type
            if (pir::Parameter::RIR_CHECK_PIR_TYPES > 0 &&
                instr->type != PirType::voyd() &&
                instr->type != RType::expandedDots &&
                instr->type != NativeType::context && !CastType::Cast(instr) &&
                Visitor::check(code->entry, [&](Instruction* i) {
                    if (auto cast = CastType::Cast(i)) {
                        if (cast->arg<0>().val() == instr)
                            return false;
                    }
                    return true;
                })) {
                int instrStr;
                if (pir::Parameter::RIR_CHECK_PIR_TYPES > 1) {
                    std::stringstream instrPrint;
                    instr->printRecursive(instrPrint, 2);
                    instrStr =
                        Pool::insert(Rf_mkString(instrPrint.str().c_str()));
                } else {
                    instrStr = -1;
                }
                cb.add(BC::assertType(instr->type, instrStr));
            }

            // Store the result
            if (alloc.sa->dead(instr)) {
                cb.add(BC::pop());
            } else if (instr->producesRirResult()) {
                if (!alloc.hasSlot(instr)) {
                    cb.add(BC::pop());
                } else if (!alloc.onStack(instr)) {
                    cb.add(BC::stloc(alloc[instr]));
                }
            }
        }

        // This BB has exactly one successor
        assert(bb->isJmp());
        auto next = bb->next();
        cb.add(BC::br(bbLabels[next]));
    });

    // Edge case: if the push context was deadcode removed (e.g. since we
    // certainly deopt before ending a context) the pushContext label is not
    // bound, which trips the code writer. We artificially bind the label to
    // point to an int3 instruction.
    {
        bool fixed = false;
        for (auto pc : pushContexts) {
            auto l = pc.second;
            if (!pushContextsPopped.count(l)) {
                fixed = true;
                // must be unreachable
                cb.add(l);
                cb.add(BC::int3());
            }
        }
        if (fixed)
            cb.add(BC::ret());
    }

    cb.flush();

    auto localsCnt = alloc.slots();
    auto res = ctx.finalizeCode(localsCnt, cache.size());
    if (PIR_NATIVE_BACKEND) {
        LowerLLVM native;
        if (auto n = native.tryCompile(cls, code, promMap, refcount,
                                       needsLdVarForUpdate, log.out())) {
            res->nativeCode = (NativeCode)n;
            if (native.registerMap)
                res->pirRegisterMap(native.registerMap);
        }
    }
    return res;
}

static bool coinFlip() {
    static std::mt19937 gen(Parameter::DEOPT_CHAOS_SEED);
    static std::bernoulli_distribution coin(0.03);
    return coin(gen);
};

void Pir2Rir::lower(Code* code) {

    Visitor::runPostChange(code->entry, [&](BB* bb) {
        auto it = bb->begin();
        while (it != bb->end()) {
            auto next = it + 1;
            if (auto call = CallInstruction::CastCall(*it))
                call->clearFrameState();
            if (auto b = CallSafeBuiltin::Cast(*it)) {
                if (b->builtinId == blt("length") && next != bb->end()) {
                    if (auto t = IsType::Cast(*(it + 1))) {
                        if (t->typeTest.isA(
                                PirType::simpleScalarInt().notNAOrNaN()) &&
                            t->arg(0).val() == b) {
                            // Type test follows, let's cheat and load this as
                            // an integer already. this avoids boxing in the
                            // native backend. NOTE: don't move this to an
                            // earlier pass, since otherwise the check will be
                            // optimized away!
                            b->type = PirType::simpleScalarInt().notNAOrNaN();
                            break;
                        }
                    }
                }
            } else if (auto ldfun = LdFun::Cast(*it)) {
                // The guessed binding in ldfun is just used as a temporary
                // store. If we did not manage to resolve ldfun by now, we
                // have to remove the guess again, since apparently we
                // were not sure it is correct.
                if (ldfun->guessedBinding())
                    ldfun->clearGuessedBinding();
            } else if (auto ld = LdVar::Cast(*it)) {
                while (true) {
                    auto mk = MkEnv::Cast(ld->env());
                    if (mk && mk->stub && !mk->contains(ld->varName))
                        ld->env(mk->lexicalEnv());
                    else
                        break;
                }
            } else if (auto st = StVar::Cast(*it)) {
                auto mk = MkEnv::Cast(st->env());
                if (mk && mk->stub)
                    assert(mk->contains(st->varName));
            } else if (auto deopt = Deopt::Cast(*it)) {
                // Lower Deopt instructions + their FrameStates to a
                // ScheduledDeopt.
                auto newDeopt = new ScheduledDeopt();
                newDeopt->consumeFrameStates(deopt);
                bb->replace(it, newDeopt);
            } else if (auto expect = Assume::Cast(*it)) {
                auto expectation = expect->assumeTrue;
                std::string debugMessage;
                if (Parameter::DEBUG_DEOPTS) {
                    debugMessage = "DEOPT, assumption ";
                    {
                        std::stringstream dump;
                        if (auto i = Instruction::Cast(expect->condition())) {
                            dump << "\n";
                            i->printRecursive(dump, 4);
                            dump << "\n";
                        } else {
                            expect->condition()->printRef(dump);
                        }
                        debugMessage += dump.str();
                    }
                    debugMessage += " failed\n";
                }
                BBTransform::lowerExpect(
                    code, bb, it, expect, expectation,
                    expect->checkpoint()->bb()->falseBranch(), debugMessage,
                    Parameter::DEOPT_CHAOS && coinFlip());
                // lowerExpect splits the bb from current position. There
                // remains nothing to process. Breaking seems more robust
                // than trusting the modified iterator.
                break;
            }

            it = next;
        }
    });

    Visitor::run(code->entry, [&](BB* bb) {
        auto it = bb->begin();
        while (it != bb->end()) {
            auto next = it + 1;
            if (FrameState::Cast(*it)) {
                next = bb->remove(it);
            } else if (Checkpoint::Cast(*it)) {
                next = bb->remove(it);
                bb->convertBranchToJmp(true);
            }
            it = next;
        }
    });

    BBTransform::mergeRedundantBBs(code);

    // Insert Nop into all empty blocks to make life easier
    Visitor::run(code->entry, [&](BB* bb) {
        if (bb->isEmpty())
            bb->append(new Nop());
    });

}


void Pir2Rir::toCSSA(Code* code) {

    // For each Phi, insert copies
    BreadthFirstVisitor::run(code->entry, [&](BB* bb) {
        // TODO: move all phi's to the beginning, then insert the copies not
        // after each phi but after all phi's?
        for (auto it = bb->begin(); it != bb->end(); ++it) {
            auto instr = *it;
            if (auto phi = Phi::Cast(instr)) {

                for (size_t i = 0; i < phi->nargs(); ++i) {
                    BB* pred = phi->inputAt(i);
                    // If pred is branch insert a new split block
                    if (!pred->isJmp()) {
                        assert(pred->isBranch());
                        BB* split = nullptr;
                        if (pred->trueBranch() == phi->bb())
                            split = pred->trueBranch();
                        else if (pred->falseBranch() == phi->bb())
                            split = pred->falseBranch();
                        assert(split &&
                               "Don't know where to insert a phi input copy.");
                        pred = BBTransform::splitEdge(code->nextBBId++, pred,
                                                      split, code);
                    }
                    if (Instruction* iav =
                            Instruction::Cast(phi->arg(i).val())) {
                        auto copy = pred->insert(pred->end(), new PirCopy(iav));
                        phi->arg(i).val() = *copy;
                    } else {

                        auto val = phi->arg(i).val()->asRValue();
                        auto copy = pred->insert(pred->end(), new LdConst(val));

                        phi->arg(i).val() = *copy;

                        if (phi->arg(i).type() == NativeType::test) {
                            (*copy)->type = phi->arg(i).type();
                        }
                    }
                }
                auto phiCopy = new PirCopy(phi);
                phi->replaceUsesWith(phiCopy);
                it = bb->insert(it + 1, phiCopy);
            }
        }
    });
}

rir::Code* Pir2Rir::getPromise(Context& ctx, Promise* p) {
    if (!promises.count(p)) {
        ctx.push(src_pool_at(globalContext(), p->srcPoolIdx()));
        promises[p] = compileCode(ctx, p);
    }
    return promises.at(p);
}

rir::Function* Pir2Rir::finalize() {
    // TODO: keep track of source ast indices in the source pool
    // (for now, calls, promises and operators do)
    // + how to deal with inlined stuff?

    FunctionWriter function;
    Context ctx(function);

    FunctionSignature signature(FunctionSignature::Environment::CalleeCreated,
                                FunctionSignature::OptimizationLevel::Optimized,
                                cls->assumptions());

    // PIR does not support default args currently.
    for (size_t i = 0; i < cls->nargs(); ++i) {
        function.addArgWithoutDefault();
        signature.pushDefaultArgument();
    }

    assert(signature.formalNargs() == cls->nargs());
    ctx.push(R_NilValue);
    auto body = compileCode(ctx, cls);
    log.finalPIR(cls);
    function.finalize(body, signature);

    function.function()->inheritFlags(cls->owner()->rirFunction());
#ifdef ENABLE_SLOWASSERT
    CodeVerifier::verifyFunctionLayout(function.function()->container(),
                                       globalContext());
#endif
    log.finalRIR(function.function());
#ifdef ENABLE_EVENT_COUNTERS
    if (ENABLE_EVENT_COUNTERS)
        EventCounters::instance().count(ClosuresCompiled, cls->inlinees + 1);
#endif
    return function.function();
}

} // namespace

rir::Function* Pir2RirCompiler::compile(ClosureVersion* cls, bool dryRun) {
    auto& log = logger.get(cls);
    done[cls] = nullptr;
    Pir2Rir pir2rir(*this, cls, dryRun, log);
    auto fun = pir2rir.finalize();
    done[cls] = fun;
    log.flush();
    if (fixup.count(cls)) {
        auto fixups = fixup.find(cls);
        for (auto idx : fixups->second)
            Pool::patch(idx, fun->container());
        fixup.erase(fixups);
    }
    return fun;
}

bool Parameter::DEBUG_DEOPTS = getenv("PIR_DEBUG_DEOPTS") &&
                               0 == strncmp("1", getenv("PIR_DEBUG_DEOPTS"), 1);
bool Parameter::DEOPT_CHAOS = getenv("PIR_DEOPT_CHAOS") &&
                              0 == strncmp("1", getenv("PIR_DEOPT_CHAOS"), 1);
bool Parameter::DEOPT_CHAOS_SEED = getenv("PIR_DEOPT_CHAOS_SEED")
                                       ? atoi(getenv("PIR_DEOPT_CHAOS_SEED"))
                                       : std::random_device()();

} // namespace pir
} // namespace rir
