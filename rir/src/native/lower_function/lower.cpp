#include "native/lower_function.h"

#include "native/builtins.h"
#include "native/jit.h"
#include "native/phi_builder.h"
#include "native/representation.h"
#include "native/types.h"
#include "native/variable.h"

#include "R/BuiltinIds.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "compiler/analysis/liveness.h"
#include "compiler/analysis/reference_count.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/util/visitor.h"
#include "interpreter/LazyEnvironment.h"
#include "interpreter/builtins.h"
#include "interpreter/instance.h"
#include "runtime/DispatchTable.h"
#include "utils/Pool.h"

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace rir {
namespace pir {

using namespace llvm;

static LLVMContext& C = rir::pir::Jit::C;

bool LowerFunction::tryCompile() {
    std::unordered_map<BB*, BasicBlock*> blockMapping_;
    auto getBlock = [&](BB* bb) {
        auto b = blockMapping_.find(bb);
        if (b != blockMapping_.end()) {
            return b->second;
        }
        std::stringstream ss;
        ss << "BB" << bb->id;
        return blockMapping_[bb] = BasicBlock::Create(C, ss.str(), fun);
    };
    entryBlock = BasicBlock::Create(C, "", fun);
    builder.SetInsertPoint(entryBlock);
    nodestackPtrAddr = convertToPointer(&R_BCNodeStackTop,
                                        PointerType::get(t::stackCellPtr, 0));
    {
        SmallSet<std::pair<Value*, SEXP>> bindings;
        Visitor::run(code->entry, [&](Instruction* i) {
            SEXP varName = nullptr;
            if (auto l = LdVar::Cast(i))
                varName = l->varName;
            else if (auto l = StVar::Cast(i))
                varName = l->varName;
            else if (LdDots::Cast(i))
                varName = R_DotsSymbol;

            if (varName) {
                auto e = MkEnv::Cast(i->env());
                if (e && !e->stub) {
                    bindings.insert(std::pair<Value*, SEXP>(i->env(), varName));
                }
            }
        });
        size_t idx = 0;
        for (auto& b : bindings) {
            bindingsCache[b.first][b.second] = idx++;
        }
        bindingsCacheBase = topAlloca(t::SEXP, idx);
    }

    std::unordered_map<Instruction*, Instruction*> phis;
    {
        basepointer = nodestackPtr();
        auto needsVariable = [](Instruction* v) {
            return v->producesRirResult() && !LdConst::Cast(v) &&
                   !CastType::Cast(v);
        };
        auto createVariable = [&](Instruction* i, bool mut) {
            if (representationOf(i) == Representation::Sexp) {
                if (mut)
                    variables[i] = Variable::MutableRVariable(
                        i, numLocals++, builder, basepointer);
                else
                    variables[i] = Variable::RVariable(i, numLocals++, builder,
                                                       basepointer);
            } else {
                if (mut)
                    variables[i] =
                        Variable::Mutable(i, topAlloca(representationOf(i)));
                else
                    variables[i] = Variable::Immutable(i);
            }
        };

        auto arg = fun->arg_begin();
        for (size_t i = 0; i < argNames.size(); ++i) {
            args.push_back(arg);
            args.back()->setName(argNames[i]);
            arg++;
        }

        constantpool = builder.CreateIntToPtr(c(globalContext()), t::SEXP_ptr);
        constantpool = builder.CreateGEP(constantpool, c(1));

        Visitor::run(code->entry, [&](BB* bb) {
            for (auto i : *bb) {
                if (auto phi = Phi::Cast(i)) {
                    createVariable(phi, true);
                    phi->eachArg([&](BB*, Value* v) {
                        auto i = Instruction::Cast(v);
                        assert(i);
                        phis[i] = phi;
                    });
                }
            }
        });

        Visitor::run(code->entry, [&](Instruction* i) {
            if (auto pop = PopContext::Cast(i)) {
                auto res = pop->result();
                auto push = pop->push();
                auto resStore = topAlloca(representationOf(res));
                auto rcntxt = topAlloca(t::RCNTXT);
                contexts[push] = {rcntxt, resStore,
                                  BasicBlock::Create(C, "", fun)};

                // Everything which is live at the Push context needs to be
                // mutable, to be able to restore on restart
                Visitor::run(code->entry, [&](Instruction* j) {
                    if (needsVariable(j)) {
                        if (representationOf(j) == t::SEXP &&
                            liveness.live(push, j)) {
                            contexts[push].savedSexpPos[j] = numLocals++;
                        }
                        if (!liveness.live(push, j) && liveness.live(pop, j))
                            escapesInlineContext.insert(j);
                        if (!variables.count(j) &&
                            (liveness.live(push, j) || liveness.live(pop, j)))
                            createVariable(j, true);
                    }
                });
            }
        });
        Visitor::run(code->entry, [&](Instruction* i) {
            if (needsVariable(i) && !variables.count(i))
                createVariable(i, false);
        });
    }

    numLocals += MAX_TEMPS;
    if (numLocals > 0)
        incStack(numLocals, true);

    std::unordered_map<BB*, int> blockInPushContext;
    blockInPushContext[code->entry] = 0;

    LoweringVisitor::run(code->entry, [&](BB* bb) {
        if (!success)
            return;

        builder.SetInsertPoint(getBlock(bb));
        inPushContext = blockInPushContext.at(bb);

        for (auto it = bb->begin(); it != bb->end(); ++it) {
            auto i = *it;
            if (!success)
                return;

            auto needsAdjust = refcount.beforeUse.find(i);
            if (needsAdjust != refcount.beforeUse.end()) {
                for (auto& adjust : needsAdjust->second) {
                    if (representationOf(adjust.first) == t::SEXP) {
                        if (adjust.second == NeedsRefcountAdjustment::SetShared)
                            ensureShared(load(adjust.first));
                        else if (adjust.second ==
                                 NeedsRefcountAdjustment::EnsureNamed)
                            ensureNamed(load(adjust.first));
                    }
                }
            }

            currentInstr = i;
            switch (i->tag) {
            case Tag::ExpandDots:
                // handled in calls
                break;

            case Tag::DotsList: {
                auto mk = DotsList::Cast(i);
                auto arglist = constant(R_NilValue, t::SEXP);
                mk->eachElementRev([&](SEXP name, Value* v) {
                    auto val = loadSxp(v);
                    incrementNamed(val);
                    arglist = call(NativeBuiltins::consNr, {val, arglist});
                    setTag(arglist, constant(name, t::SEXP), false);
                });
                setSexptype(arglist, DOTSXP);
                setVal(i, arglist);
                break;
            }

            case Tag::RecordDeoptReason: {
                auto rec = RecordDeoptReason::Cast(i);
                auto reason = llvm::ConstantStruct::get(
                    t::DeoptReason, {
                                        c(rec->reason.reason, 32),
                                        convertToPointer(rec->reason.srcCode),
                                        c(rec->reason.originOffset),
                                    });
                call(NativeBuiltins::recordDeopt,
                     {loadSxp(rec->arg<0>().val()), globalConst(reason)});
                break;
            }

            case Tag::PushContext: {
                compilePushContext(i);
                break;
            }

            case Tag::PopContext: {
                compilePopContext(i);
                break;
            }

            case Tag::PirCopy: {
                auto c = PirCopy::Cast(i);
                auto in = c->arg<0>().val();
                if (Phi::Cast(in))
                    setVal(i, load(in, representationOf(i)));
                break;
            }

            case Tag::Phi:
                break;

            case Tag::LdArg:
                setVal(i, argument(LdArg::Cast(i)->id));
                break;

            case Tag::LdFunctionEnv:
                setVal(i, paramEnv());
                break;

            case Tag::Invisible:
                setVisible(0);
                break;

            case Tag::Visible:
                setVisible(1);
                break;

            case Tag::Identical: {
                auto a = depromise(load(i->arg(0).val()));
                auto b = depromise(load(i->arg(1).val()));
                setVal(i,
                       builder.CreateZExt(builder.CreateICmpEQ(a, b), t::Int));
                break;
            }

            case Tag::CallSafeBuiltin: {
                auto b = CallSafeBuiltin::Cast(i);
                if (compileDotcall(
                        b, [&]() { return constant(b->blt, t::SEXP); },
                        [&](size_t i) { return R_NilValue; })) {
                    break;
                }
                std::vector<Value*> args;
                b->eachCallArg([&](Value* v) { args.push_back(v); });

                auto callTheBuiltin = [&]() -> llvm::Value* {
                    // Some "safe" builtins still look up functions in the base
                    // env
                    return callRBuiltin(b->blt, args, i->srcIdx, b->builtin,
                                        constant(R_BaseEnv, t::SEXP));
                };

                if (!tryInlineBuiltin(b, callTheBuiltin)) {
                    setVal(i, callTheBuiltin());
                }

                break;
            }

            case Tag::CallBuiltin: {
                auto b = CallBuiltin::Cast(i);
                if (compileDotcall(
                        b, [&]() { return constant(b->blt, t::SEXP); },
                        [&](size_t i) { return R_NilValue; })) {
                    break;
                }
                std::vector<Value*> args;
                b->eachCallArg([&](Value* v) { args.push_back(v); });
                setVal(i, callRBuiltin(b->blt, args, i->srcIdx, b->builtin,
                                       b->hasEnv()
                                           ? loadSxp(b->env())
                                           : constant(R_BaseEnv, t::SEXP)));
                break;
            }

            case Tag::Call: {
                auto b = Call::Cast(i);

                if (compileDotcall(
                        b, [&]() { return loadSxp(b->cls()); },
                        [&](size_t i) { return R_NilValue; })) {
                    break;
                }

                std::vector<Value*> args;
                b->eachCallArg([&](Value* v) { args.push_back(v); });
                Assumptions asmpt = b->inferAvailableAssumptions();
                setVal(i, withCallFrame(args, [&]() -> llvm::Value* {
                           return call(NativeBuiltins::call,
                                       {paramCode(), c(b->srcIdx),
                                        loadSxp(b->cls()), loadSxp(b->env()),
                                        c(b->nCallArgs()), c(asmpt.toI())});
                       }));
                break;
            }

            case Tag::NamedCall: {
                auto b = NamedCall::Cast(i);
                if (compileDotcall(
                        b, [&]() { return loadSxp(b->cls()); },
                        [&](size_t i) { return b->names[i]; })) {
                    break;
                }
                std::vector<Value*> args;
                b->eachCallArg([&](Value* v) { args.push_back(v); });
                Assumptions asmpt = b->inferAvailableAssumptions();

                std::vector<BC::PoolIdx> names;
                for (size_t i = 0; i < b->names.size(); ++i)
                    names.push_back(Pool::insert((b->names[i])));
                auto namesConst = c(names);
                auto namesStore = globalConst(namesConst);

                setVal(i, withCallFrame(args, [&]() -> llvm::Value* {
                           return call(
                               NativeBuiltins::namedCall,
                               {
                                   paramCode(),
                                   c(b->srcIdx),
                                   loadSxp(b->cls()),
                                   loadSxp(b->env()),
                                   c(b->nCallArgs()),
                                   builder.CreateBitCast(namesStore, t::IntPtr),
                                   c(asmpt.toI()),
                               });
                       }));
                break;
            }

            case Tag::StaticCall: {
                auto calli = StaticCall::Cast(i);
                calli->eachArg([](Value* v) { assert(!ExpandDots::Cast(v)); });
                auto target = calli->tryDispatch();
                auto bestTarget = calli->tryOptimisticDispatch();
                std::vector<Value*> args;
                calli->eachCallArg([&](Value* v) { args.push_back(v); });
                Assumptions asmpt = calli->inferAvailableAssumptions();

                if (!target->owner()->hasOriginClosure()) {
                    setVal(i, withCallFrame(args, [&]() -> llvm::Value* {
                               return call(NativeBuiltins::call,
                                           {paramCode(), c(calli->srcIdx),
                                            loadSxp(calli->runtimeClosure()),
                                            loadSxp(calli->env()),
                                            c(calli->nCallArgs()),
                                            c(asmpt.toI())});
                           }));
                    break;
                }

                if (target == bestTarget) {
                    auto callee = target->owner()->rirClosure();
                    auto dt = DispatchTable::check(BODY(callee));
                    assert(cls);
                    rir::Function* nativeTarget = nullptr;
                    for (size_t i = 0; i < dt->size(); i++) {
                        auto entry = dt->get(i);
                        if (entry->signature().assumptions ==
                                target->assumptions() &&
                            entry->signature().numArguments >= args.size()) {
                            nativeTarget = entry;
                        }
                    }
                    if (nativeTarget) {
                        llvm::Value* trg = Jit::get(target);
                        auto nativeCode = nativeTarget->body()->nativeCode;
                        if (!trg && nativeCode) {
                            trg = builder.CreateIntToPtr(c((void*)nativeCode),
                                                         t::nativeFunctionPtr);
                        }
                        if (trg &&
                            target->properties.includes(
                                ClosureVersion::Property::NoReflection)) {
                            auto code = builder.CreateIntToPtr(
                                c(nativeTarget->body()), t::voidPtr);
                            llvm::Value* arglist = nodestackPtr();
                            auto rr = withCallFrame(args, [&]() {
                                return builder.CreateCall(
                                    trg, {code, arglist, loadSxp(i->env()),
                                          constant(callee, t::SEXP)});
                            });
                            setVal(i, rr);
                            break;
                        }

                        assert(
                            asmpt.includes(Assumption::StaticallyArgmatched));
                        auto res = withCallFrame(args, [&]() {
                            return call(NativeBuiltins::nativeCallTrampoline,
                                        {
                                            constant(callee, t::SEXP),
                                            builder.CreateIntToPtr(
                                                c(nativeTarget), t::voidPtr),
                                            c(calli->srcIdx),
                                            loadSxp(calli->env()),
                                            c(args.size()),
                                            c(asmpt.toI()),
                                        });
                        });
                        setVal(i, res);
                        break;
                    }
                }

                assert(asmpt.includes(Assumption::StaticallyArgmatched));
                setVal(i, withCallFrame(args, [&]() -> llvm::Value* {
                           return call(
                               NativeBuiltins::call,
                               {
                                   paramCode(),
                                   c(calli->srcIdx),
                                   builder.CreateIntToPtr(
                                       c(calli->cls()->rirClosure()), t::SEXP),
                                   loadSxp(calli->env()),
                                   c(calli->nCallArgs()),
                                   c(asmpt.toI()),
                               });
                       }));
                break;
            }

            case Tag::Inc: {
                auto arg = i->arg(0).val();
                llvm::Value* res = nullptr;
                if (representationOf(arg) == Representation::Integer) {
                    res = load(arg, Representation::Integer);
                    res = builder.CreateAdd(res, c(1), "", true, true);
                } else {
                    success = false;
                }
                setVal(i, res);
                break;
            }

            case Tag::LdConst:
            case Tag::Nop:
                break;

            case Tag::ForSeqSize: {
                llvm::Value* res = call(NativeBuiltins::forSeqSize,
                                        {loadSxp(i->arg(0).val())});
                if (representationOf(i) == Representation::Real)
                    res = builder.CreateSIToFP(res, t::Double);
                else if (representationOf(i) == Representation::Sexp)
                    res = boxInt(res);
                setVal(i, res);
                break;
            }

            case Tag::Branch: {
                auto cond = load(i->arg(0).val(), Representation::Integer);
                cond = builder.CreateICmpNE(cond, c(0));

                auto t = bb->trueBranch();
                auto f = bb->falseBranch();
                MDNode* weight = nullptr;
                if (t->isDeopt() || (t->isJmp() && t->next()->isDeopt()))
                    weight = branchAlwaysFalse;
                else if (f->isDeopt() || (f->isJmp() && f->next()->isDeopt()))
                    weight = branchAlwaysTrue;
                builder.CreateCondBr(cond, getBlock(bb->trueBranch()),
                                     getBlock(bb->falseBranch()), weight);
                break;
            }

            case Tag::ScheduledDeopt: {
                // TODO, this is copied from pir_2_rir... rather ugly
                DeoptMetadata* m = nullptr;
                {
                    auto deopt = ScheduledDeopt::Cast(i);
                    size_t nframes = deopt->frames.size();
                    SEXP store =
                        Rf_allocVector(RAWSXP, sizeof(DeoptMetadata) +
                                                   nframes * sizeof(FrameInfo));
                    m = new (DATAPTR(store)) DeoptMetadata;
                    m->numFrames = nframes;

                    size_t i = 0;
                    // Frames in the ScheduledDeopt are in pir argument
                    // order (from left to right). On the other hand frames
                    // in the rir deopt_ instruction are in stack order,
                    // from tos down.
                    for (auto fi = deopt->frames.rbegin();
                         fi != deopt->frames.rend(); fi++)
                        m->frames[i++] = *fi;
                    Pool::insert(store);
                }

                std::vector<Value*> args;
                i->eachArg([&](Value* v) { args.push_back(v); });
                llvm::CallInst* res;
                withCallFrame(args, [&]() {
                    res = call(NativeBuiltins::deopt,
                               {paramCode(), paramClosure(),
                                convertToPointer(m), paramArgs()});
                    return res;
                });
                res->setTailCall(true);
                builder.CreateUnreachable();
                break;
            }

            case Tag::MkEnv: {
                auto mkenv = MkEnv::Cast(i);
                auto parent = loadSxp(mkenv->env());

                std::vector<BC::PoolIdx> names;
                for (size_t i = 0; i < mkenv->nLocals(); ++i) {
                    auto n = mkenv->varName[i];
                    if (mkenv->missing[i])
                        n = CONS_NR(n, R_NilValue);
                    names.push_back(Pool::insert(n));
                }
                auto namesConst = c(names);
                auto namesStore = globalConst(namesConst);

                if (mkenv->stub) {
                    auto env =
                        call(NativeBuiltins::createStubEnvironment,
                             {parent, c((int)mkenv->nLocals()),
                              builder.CreateBitCast(namesStore, t::IntPtr),
                              c(mkenv->context)});
                    size_t pos = 0;
                    mkenv->eachLocalVar([&](SEXP name, Value* v, bool miss) {
                        envStubSet(env, pos++, loadSxp(v), mkenv->nLocals(),
                                   false);
                    });
                    setVal(i, env);
                    break;
                }

                auto arglist = constant(R_NilValue, t::SEXP);
                mkenv->eachLocalVarRev([&](SEXP name, Value* v, bool miss) {
                    if (miss) {
                        arglist = call(
                            NativeBuiltins::createMissingBindingCell,
                            {loadSxp(v), constant(name, t::SEXP), arglist});
                    } else {
                        arglist = call(
                            NativeBuiltins::createBindingCell,
                            {loadSxp(v), constant(name, t::SEXP), arglist});
                    }
                });

                setVal(i, call(NativeBuiltins::createEnvironment,
                               {parent, arglist, c(mkenv->context)}));

                if (bindingsCache.count(i))
                    for (auto b : bindingsCache.at(i))
                        builder.CreateStore(
                            convertToPointer(nullptr, t::SEXP),
                            builder.CreateGEP(bindingsCacheBase, c(b.second)));
                break;
            }

            case Tag::MaterializeEnv: {
                auto materialize = MaterializeEnv::Cast(i);
                setVal(i, call(NativeBuiltins::materializeEnvironment,
                               {loadSxp(materialize->env())}));
                break;
            }

            case Tag::Add:
                compileBinop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateAdd(a, b, "", false, true);
                    },
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFAdd(a, b);
                    },
                    BinopKind::ADD);
                break;
            case Tag::Sub:
                compileBinop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateSub(a, b, "", false, true);
                    },
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFSub(a, b);
                    },
                    BinopKind::SUB);
                break;
            case Tag::Mul:
                compileBinop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateMul(a, b, "", false, true);
                    },
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFMul(a, b);
                    },
                    BinopKind::MUL);
                break;
            case Tag::Div:
                compileBinop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateSDiv(a, b);
                    },
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFDiv(a, b);
                    },
                    BinopKind::DIV);
                break;
            case Tag::Pow:
                compileBinop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateIntrinsic(
                            Intrinsic::powi, {a->getType(), b->getType()},
                            {a, b});
                    },
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateIntrinsic(
                            Intrinsic::pow, {a->getType(), b->getType()},
                            {a, b});
                    },
                    BinopKind::POW);
                break;

            case Tag::Neq:
                compileRelop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateICmpNE(a, b);
                    },
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFCmpUNE(a, b);
                    },
                    BinopKind::NE);
                break;

            case Tag::Minus: {
                compileUnop(
                    i, [&](llvm::Value* a) { return builder.CreateNeg(a); },
                    [&](llvm::Value* a) { return builder.CreateFNeg(a); },
                    UnopKind::MINUS);
                break;
            }

            case Tag::Plus: {
                compileUnop(
                    i, [&](llvm::Value* a) { return a; },
                    [&](llvm::Value* a) { return a; }, UnopKind::PLUS);
                break;
            }

            case Tag::Not: {
                auto resultRep = representationOf(i);
                auto argument = i->arg(0).val();
                auto argumentRep = representationOf(argument);
                if (argumentRep == Representation::Sexp) {
                    auto argumentNative = loadSxp(argument);

                    llvm::Value* res = nullptr;
                    if (i->hasEnv()) {
                        res = call(
                            NativeBuiltins::notEnv,
                            {argumentNative, loadSxp(i->env()), c(i->srcIdx)});
                    } else {
                        res = call(NativeBuiltins::notOp, {argumentNative});
                    }
                    setVal(i, res);
                    break;
                }

                auto done = BasicBlock::Create(C, "", fun);
                auto isNa = BasicBlock::Create(C, "", fun);

                auto argumentNative = load(argument, argumentRep);

                nacheck(argumentNative, isNa);

                auto res = phiBuilder(t::Int);

                res.addInput(builder.CreateZExt(
                    builder.CreateICmpEQ(argumentNative, c(0)), t::Int));
                builder.CreateBr(done);

                builder.SetInsertPoint(isNa);
                // Maybe we need to model R_LogicalNAValue?
                res.addInput(c(NA_INTEGER));
                builder.CreateBr(done);
                builder.SetInsertPoint(done);

                if (resultRep == Representation::Sexp) {
                    setVal(i, boxLgl(res(), true));
                } else {
                    setVal(i, res());
                }
                break;
            }

            case Tag::Eq:
                compileRelop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateICmpEQ(a, b);
                    },
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFCmpUEQ(a, b);
                    },
                    BinopKind::EQ);
                break;

            case Tag::Lte:
                compileRelop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateICmpSLE(a, b);
                    },
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFCmpULE(a, b);
                    },
                    BinopKind::LTE);
                break;
            case Tag::Lt:
                compileRelop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateICmpSLT(a, b);
                    },
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFCmpULT(a, b);
                    },
                    BinopKind::LT);
                break;
            case Tag::Gte:
                compileRelop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateICmpSGE(a, b);
                    },
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFCmpUGE(a, b);
                    },
                    BinopKind::GTE);
                break;
            case Tag::Gt:
                compileRelop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateICmpSGT(a, b);
                    },
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateFCmpUGT(a, b);
                    },
                    BinopKind::GT);
                break;
            case Tag::LAnd:
                compileRelop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        a = builder.CreateZExt(builder.CreateICmpNE(a, c(0)),
                                               t::Int);
                        b = builder.CreateZExt(builder.CreateICmpNE(b, c(0)),
                                               t::Int);
                        return builder.CreateAnd(a, b);
                    },
                    [&](llvm::Value* a, llvm::Value* b) {
                        a = builder.CreateZExt(builder.CreateFCmpUNE(a, c(0.0)),
                                               t::Int);
                        b = builder.CreateZExt(builder.CreateFCmpUNE(b, c(0.0)),
                                               t::Int);
                        return builder.CreateAnd(a, b);
                    },
                    BinopKind::LAND);
                break;
            case Tag::LOr:
                compileRelop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        return builder.CreateOr(a, b);
                    },
                    [&](llvm::Value* a, llvm::Value* b) {
                        a = builder.CreateZExt(builder.CreateFCmpUNE(a, c(0.0)),
                                               t::Int);
                        b = builder.CreateZExt(builder.CreateFCmpUNE(b, c(0.0)),
                                               t::Int);
                        return builder.CreateOr(a, b);
                    },
                    BinopKind::LOR);
                break;
            case Tag::IDiv:
                compileBinop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        auto isZero = BasicBlock::Create(C, "", fun);
                        auto notZero = BasicBlock::Create(C, "", fun);
                        auto cnt = BasicBlock::Create(C, "", fun);
                        builder.CreateCondBr(builder.CreateICmpEQ(b, c(0)),
                                             isZero, notZero,
                                             branchMostlyFalse);

                        auto res = phiBuilder(t::Int);

                        builder.SetInsertPoint(isZero);
                        res.addInput(c(NA_INTEGER));
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(notZero);
                        auto r = builder.CreateFDiv(
                            builder.CreateSIToFP(a, t::Double),
                            builder.CreateSIToFP(b, t::Double));
                        res.addInput(builder.CreateFPToSI(r, t::Int));
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(cnt);
                        return res();
                    },
                    [&](llvm::Value* a, llvm::Value* b) {
                        // from myfloor
                        auto q = builder.CreateFDiv(a, b);
                        auto isZero = BasicBlock::Create(C, "", fun);
                        auto notZero = BasicBlock::Create(C, "", fun);
                        auto cnt = BasicBlock::Create(C, "", fun);
                        builder.CreateCondBr(builder.CreateFCmpUEQ(b, c(0.0)),
                                             isZero, notZero,
                                             branchMostlyFalse);

                        auto res = phiBuilder(t::Double);

                        builder.SetInsertPoint(isZero);
                        res.addInput(q);
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(notZero);
                        auto fq = builder.CreateIntrinsic(Intrinsic::floor,
                                                          {t::Double}, {q});
                        auto tmp =
                            builder.CreateFSub(a, builder.CreateFMul(fq, b));
                        auto frem = builder.CreateIntrinsic(
                            Intrinsic::floor, {t::Double},
                            {builder.CreateFDiv(tmp, b)});
                        res.addInput(builder.CreateFAdd(fq, frem));
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(cnt);
                        return res();
                    },
                    BinopKind::IDIV);
                break;
            case Tag::Mod: {
                auto myfmod = [&](llvm::Value* a, llvm::Value* b) {
                    // from myfmod
                    auto isZero = BasicBlock::Create(C, "", fun);
                    auto notZero = BasicBlock::Create(C, "", fun);
                    auto cnt = BasicBlock::Create(C, "", fun);
                    auto res = phiBuilder(t::Double);
                    builder.CreateCondBr(builder.CreateFCmpUEQ(b, c(0.0)),
                                         isZero, notZero, branchMostlyFalse);

                    builder.SetInsertPoint(isZero);
                    res.addInput(c(R_NaN));
                    builder.CreateBr(cnt);

                    builder.SetInsertPoint(notZero);
                    auto q = builder.CreateFDiv(a, b);
                    auto fq = builder.CreateIntrinsic(Intrinsic::floor,
                                                      {t::Double}, {q});

                    auto absq = builder.CreateIntrinsic(Intrinsic::fabs,
                                                        {t::Double}, {q});
                    auto finite = builder.CreateFCmpUNE(
                        absq, c((double)0x7FF0000000000000));
                    auto gt =
                        builder.CreateFCmpUGT(absq, c(1 / R_AccuracyInfo.eps));

                    auto warn = BasicBlock::Create(C, "", fun);
                    auto noWarn = BasicBlock::Create(C, "", fun);
                    builder.CreateCondBr(builder.CreateAnd(finite, gt), warn,
                                         noWarn, branchMostlyFalse);

                    builder.SetInsertPoint(warn);
                    auto msg = builder.CreateGlobalString(
                        "probable complete loss of accuracy in modulus");
                    call(NativeBuiltins::warn,
                         {builder.CreateBitCast(msg, t::voidPtr)});
                    builder.CreateBr(noWarn);

                    builder.SetInsertPoint(noWarn);
                    auto tmp = builder.CreateFSub(a, builder.CreateFMul(fq, b));
                    auto frem =
                        builder.CreateIntrinsic(Intrinsic::floor, {t::Double},
                                                {builder.CreateFDiv(tmp, b)});
                    res.addInput(
                        builder.CreateFSub(tmp, builder.CreateFMul(frem, b)));
                    builder.CreateBr(cnt);

                    builder.SetInsertPoint(cnt);
                    return res();
                };

                compileBinop(
                    i,
                    [&](llvm::Value* a, llvm::Value* b) {
                        auto fast = BasicBlock::Create(C, "", fun);
                        auto fast1 = BasicBlock::Create(C, "", fun);
                        auto slow = BasicBlock::Create(C, "", fun);
                        auto cnt = BasicBlock::Create(C, "", fun);
                        auto res = phiBuilder(t::Int);
                        builder.CreateCondBr(builder.CreateICmpSGE(a, c(0)),
                                             fast1, slow, branchMostlyTrue);

                        builder.SetInsertPoint(fast1);
                        builder.CreateCondBr(builder.CreateICmpSGT(b, c(0)),
                                             fast, slow, branchMostlyTrue);

                        builder.SetInsertPoint(fast);
                        res.addInput(builder.CreateSRem(a, b));
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(slow);
                        res.addInput(builder.CreateFPToSI(
                            myfmod(builder.CreateSIToFP(a, t::Double),
                                   builder.CreateSIToFP(b, t::Double)),
                            t::Int));
                        builder.CreateBr(cnt);

                        builder.SetInsertPoint(cnt);
                        return res();
                    },
                    myfmod, BinopKind::MOD);
                break;
            }
            case Tag::Colon: {
                assert(representationOf(i) == t::SEXP);
                auto a = loadSxp(i->arg(0).val());
                auto b = loadSxp(i->arg(1).val());
                llvm::Value* res;
                if (i->hasEnv()) {
                    auto e = loadSxp(i->env());
                    res =
                        call(NativeBuiltins::binopEnv,
                             {a, b, e, c(i->srcIdx), c((int)BinopKind::COLON)});
                } else {
                    res = call(NativeBuiltins::binop,
                               {a, b, c((int)BinopKind::COLON)});
                }
                setVal(i, res);
                break;
            }

            case Tag::CastType: {
                // Scheduled on use
                break;
            }

            case Tag::Return: {
                auto res = loadSxp(Return::Cast(i)->arg<0>().val());
                if (numLocals > 0)
                    decStack(numLocals);
                builder.CreateRet(res);
                break;
            }

            case Tag::IsEnvStub: {
                auto arg = loadSxp(i->arg(0).val());
                auto env = MkEnv::Cast(i->env());

                auto isStub = BasicBlock::Create(C, "", fun);
                auto isNotMaterialized = BasicBlock::Create(C, "", fun);
                auto isNotStub = BasicBlock::Create(C, "", fun);
                auto done = BasicBlock::Create(C, "", fun);

                auto r = representationOf(i);
                auto res = phiBuilder(r);

                builder.CreateCondBr(isExternalsxp(arg, LAZY_ENVIRONMENT_MAGIC),
                                     isStub, isNotStub, branchAlwaysTrue);

                builder.SetInsertPoint(isStub);
                auto materialized = envStubGet(arg, -2, env->nLocals());
                builder.CreateCondBr(
                    builder.CreateICmpEQ(materialized,
                                         convertToPointer(nullptr, t::SEXP)),
                    isNotMaterialized, isNotStub, branchAlwaysTrue);

                builder.SetInsertPoint(isNotMaterialized);
                res.addInput(constant(R_TrueValue, r));
                builder.CreateBr(done);

                builder.SetInsertPoint(isNotStub);
                res.addInput(constant(R_FalseValue, r));
                builder.CreateBr(done);

                builder.SetInsertPoint(done);

                setVal(i, res());
                break;
            }

            case Tag::MkFunCls: {
                auto mkFunction = MkFunCls::Cast(i);
                auto closure = mkFunction->cls;
                auto srcRef = constant(closure->srcRef(), t::SEXP);
                auto formals = constant(closure->formals().original(), t::SEXP);
                auto body =
                    constant(mkFunction->originalBody->container(), t::SEXP);
                assert(DispatchTable::check(
                    mkFunction->originalBody->container()));
                setVal(i, call(NativeBuiltins::createClosure,
                               {body, formals, loadSxp(mkFunction->env()),
                                srcRef}));
                break;
            }

            case Tag::MkCls: {
                auto mk = MkCls::Cast(i);
                auto formals = loadSxp(mk->arg(0).val());
                auto body = loadSxp(mk->arg(1).val());
                auto srcRef = loadSxp(mk->arg(2).val());
                auto env = loadSxp(mk->arg(3).val());
                setVal(i, call(NativeBuiltins::createClosure,
                               {body, formals, env, srcRef}));
                break;
            }

            case Tag::IsType: {
                if (representationOf(i) != Representation::Integer) {
                    success = false;
                    break;
                }

                auto t = IsType::Cast(i);
                auto arg = i->arg(0).val();
                if (representationOf(arg) == Representation::Sexp) {
                    auto a = loadSxp(arg);
                    if (arg->type.maybePromiseWrapped() &&
                        t->typeTest.maybePromiseWrapped())
                        a = depromise(a);

                    if (t->typeTest.notPromiseWrapped() ==
                        PirType::simpleScalarInt()) {
                        setVal(i, builder.CreateZExt(isSimpleScalar(a, INTSXP),
                                                     t::Int));
                        break;
                    } else if (t->typeTest.notPromiseWrapped() ==
                               PirType::simpleScalarLogical()) {
                        setVal(i, builder.CreateZExt(isSimpleScalar(a, LGLSXP),
                                                     t::Int));
                        break;
                    } else if (t->typeTest.notPromiseWrapped() ==
                               PirType::simpleScalarReal()) {
                        setVal(i, builder.CreateZExt(isSimpleScalar(a, REALSXP),
                                                     t::Int));
                        break;
                    }

                    llvm::Value* res = nullptr;
                    if (t->typeTest.noAttribs().isA(
                            PirType(RType::logical).orPromiseWrapped())) {
                        res = builder.CreateICmpEQ(sexptype(a), c(LGLSXP));
                    } else if (t->typeTest.noAttribs().isA(
                                   PirType(RType::integer)
                                       .orPromiseWrapped())) {
                        res = builder.CreateICmpEQ(sexptype(a), c(INTSXP));
                    } else if (t->typeTest.noAttribs().isA(
                                   PirType(RType::real).orPromiseWrapped())) {
                        res = builder.CreateICmpEQ(sexptype(a), c(REALSXP));
                    } else {
                        assert(arg->type.notMissing().notLazy().noAttribs().isA(
                            t->typeTest));
                        res = builder.CreateICmpNE(
                            a, constant(R_UnboundValue, t::SEXP));
                    }
                    if (t->typeTest.isScalar() && !arg->type.isScalar()) {
                        assert(a->getType() == t::SEXP);
                        res = builder.CreateAnd(res, isScalar(a));
                    }
                    if (arg->type.maybeHasAttrs() &&
                        !t->typeTest.maybeHasAttrs()) {
                        res = builder.CreateAnd(res, fastVeceltOkNative(a));
                    }
                    if (arg->type.maybeObj() && !t->typeTest.maybeObj()) {
                        res =
                            builder.CreateAnd(res, builder.CreateNot(isObj(a)));
                    }
                    setVal(i, builder.CreateZExt(res, t::Int));
                } else {
                    setVal(i, c(1));
                }
                break;
            }

            case Tag::Is: {
                assert(representationOf(i) == Representation::Integer);
                auto is = Is::Cast(i);
                auto arg = i->arg(0).val();
                llvm::Value* res;
                if (representationOf(arg) == Representation::Sexp) {
                    auto argNative = loadSxp(arg);
                    auto expectedTypeNative = c(is->sexpTag);
                    auto typeNative = sexptype(argNative);
                    switch (is->sexpTag) {
                    case NILSXP:
                    case LGLSXP:
                    case REALSXP:
                        res = builder.CreateICmpEQ(typeNative,
                                                   expectedTypeNative);
                        break;

                    case VECSXP: {
                        auto operandLhs =
                            builder.CreateICmpEQ(typeNative, c(VECSXP));
                        auto operandRhs =
                            builder.CreateICmpEQ(typeNative, c(LISTSXP));
                        res = builder.CreateOr(operandLhs, operandRhs);
                        break;
                    }

                    case LISTSXP: {
                        auto operandLhs =
                            builder.CreateICmpEQ(typeNative, c(LISTSXP));
                        auto operandRhs =
                            builder.CreateICmpEQ(typeNative, c(NILSXP));
                        res = builder.CreateOr(operandLhs, operandRhs);
                        break;
                    }

                    default:
                        assert(false);
                        res = builder.getFalse();
                        success = false;
                        break;
                    }
                } else {
                    assert(i->type.isA(RType::integer) ||
                           i->type.isA(RType::logical) ||
                           i->type.isA(RType::real));
                    assert(representationOf(i) == Representation::Integer ||
                           representationOf(i) == Representation::Real);

                    bool matchInt =
                        (is->sexpTag == INTSXP) && i->type.isA(RType::integer);
                    bool matchLgl =
                        (is->sexpTag == LGLSXP) && i->type.isA(RType::logical);
                    bool matchReal =
                        (is->sexpTag == REALSXP) && i->type.isA(RType::real);

                    res = (matchInt || matchLgl || matchReal)
                              ? builder.getTrue()
                              : builder.getFalse();
                }
                setVal(i, builder.CreateZExt(res, t::Int));
                break;
            }

            case Tag::AsTest: {
                assert(representationOf(i) == Representation::Integer);

                auto arg = i->arg(0).val();
                if (auto lgl = AsLogical::Cast(arg))
                    arg = lgl->arg(0).val();

                if (representationOf(arg) == Representation::Sexp) {
                    auto a = loadSxp(arg);
                    setVal(i, call(NativeBuiltins::asTest, {a}));
                    break;
                }

                auto r = representationOf(arg);

                auto done = BasicBlock::Create(C, "", fun);
                auto isNa = BasicBlock::Create(C, "asTestIsNa", fun);

                if (r == Representation::Real) {
                    auto narg = load(arg, r);
                    auto isNotNa = builder.CreateFCmpUEQ(narg, narg);
                    narg = builder.CreateFPToSI(narg, t::Int);
                    setVal(i, narg);
                    builder.CreateCondBr(isNotNa, done, isNa, branchMostlyTrue);
                } else {
                    auto narg = load(arg, Representation::Integer);
                    auto isNotNa = builder.CreateICmpNE(narg, c(NA_INTEGER));
                    setVal(i, narg);
                    builder.CreateCondBr(isNotNa, done, isNa, branchMostlyTrue);
                }

                builder.SetInsertPoint(isNa);
                call(NativeBuiltins::error, {});
                builder.CreateRet(builder.CreateIntToPtr(c(nullptr), t::SEXP));

                builder.SetInsertPoint(done);
                break;
            }

            case Tag::AsLogical: {
                auto arg = i->arg(0).val();

                auto r1 = representationOf(arg);
                auto r2 = representationOf(i);

                assert(r2 == Representation::Integer);

                llvm::Value* res;
                if (r1 == Representation::Sexp) {
                    res = call(NativeBuiltins::asLogicalBlt, {loadSxp(arg)});
                } else if (r1 == Representation::Real) {
                    auto phi = phiBuilder(t::Int);
                    auto in = load(arg, Representation::Integer);
                    auto nin = load(arg, Representation::Real);

                    auto done = BasicBlock::Create(C, "", fun);
                    auto isNaBr = BasicBlock::Create(C, "isNa", fun);
                    auto notNaBr = BasicBlock::Create(C, "", fun);
                    nacheck(nin, isNaBr, notNaBr);

                    builder.SetInsertPoint(isNaBr);
                    phi.addInput(c(NA_INTEGER));
                    builder.CreateBr(done);

                    builder.SetInsertPoint(notNaBr);
                    phi.addInput(in);
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);
                    res = phi();
                } else {
                    assert(r1 == Representation::Integer);
                    res = load(arg, Representation::Integer);
                }

                setVal(i, res);
                break;
            }

            case Tag::Force: {
                auto f = Force::Cast(i);
                auto arg = loadSxp(f->arg<0>().val());
                if (!f->effects.includes(Effect::Force)) {
                    auto res = depromise(arg);
                    setVal(i, res);
#ifdef ENABLE_SLOWASSERT
                    insn_assert(builder.CreateICmpNE(
                                    constant(R_UnboundValue, t::SEXP), res),
                                "Expected evaluated promise");
#endif
                } else {
                    setVal(i, force(i, arg));
                }
                break;
            }

            case Tag::LdFun: {
                auto ld = LdFun::Cast(i);
                auto res =
                    call(NativeBuiltins::ldfun,
                         {constant(ld->varName, t::SEXP), loadSxp(ld->env())});
                setVal(i, res);
                setVisible(1);
                break;
            }

            case Tag::MkArg: {
                auto p = MkArg::Cast(i);
                setVal(i, call(NativeBuiltins::createPromise,
                               {paramCode(), c(promMap.at(p->prom())),
                                loadSxp(p->env()), loadSxp(p->eagerArg())}));
                break;
            }

            case Tag::UpdatePromise: {
                setCar(loadSxp(i->arg(0).val()), loadSxp(i->arg(1).val()));
                break;
            }

            case Tag::LdVarSuper: {
                auto ld = LdVarSuper::Cast(i);

                auto env = cdr(loadSxp(ld->env()));

                auto res = call(NativeBuiltins::ldvar,
                                {constant(ld->varName, t::SEXP), env});
                res->setName(CHAR(PRINTNAME(ld->varName)));

                checkMissing(res);
                checkUnbound(res);
                setVal(i, res);
                break;
            }

            case Tag::LdDots:
            case Tag::LdVar: {
                auto maybeLd = LdVar::Cast(i);
                auto varName = maybeLd ? maybeLd->varName : R_DotsSymbol;

                auto env = MkEnv::Cast(i->env());
                if (env && env->stub) {
                    auto e = loadSxp(env);
                    llvm::Value* res =
                        envStubGet(e, env->indexOf(varName), env->nLocals());
                    if (env->argNamed(varName).val() ==
                        UnboundValue::instance()) {
                        res = builder.CreateSelect(
                            builder.CreateICmpEQ(
                                res, constant(R_UnboundValue, t::SEXP)),
                            // if unsassigned in the stub, fall through
                            call(NativeBuiltins::ldvar,
                                 {constant(varName, t::SEXP),
                                  envStubGet(e, -1, env->nLocals())}),
                            res);
                    }
                    setVal(i, res);
                    break;
                }

                llvm::Value* res;
                if (bindingsCache.count(i->env())) {
                    auto phi = phiBuilder(t::SEXP);
                    auto offset = bindingsCache.at(i->env()).at(varName);

                    auto cachePtr =
                        builder.CreateGEP(bindingsCacheBase, c(offset));
                    llvm::Value* cache = builder.CreateLoad(cachePtr);

                    auto hit1 = BasicBlock::Create(C, "", fun);
                    auto hit2 = BasicBlock::Create(C, "", fun);
                    auto miss = BasicBlock::Create(C, "", fun);
                    auto done = BasicBlock::Create(C, "", fun);

                    builder.CreateCondBr(
                        builder.CreateICmpULE(
                            builder.CreatePtrToInt(cache, t::i64), c(1, 64)),
                        miss, hit1, branchMostlyFalse);
                    builder.SetInsertPoint(hit1);
                    auto val = car(cache);
                    builder.CreateCondBr(
                        builder.CreateICmpEQ(val,
                                             constant(R_UnboundValue, t::SEXP)),
                        miss, hit2, branchMostlyFalse);
                    builder.SetInsertPoint(hit2);
                    ensureNamed(val);
                    phi.addInput(val);
                    builder.CreateBr(done);

                    builder.SetInsertPoint(miss);
                    auto res0 = call(NativeBuiltins::ldvarCacheMiss,
                                     {constant(varName, t::SEXP),
                                      loadSxp(i->env()), cachePtr});
                    if (needsLdVarForUpdate.count(i))
                        ensureShared(res0);
                    phi.addInput(res0);
                    builder.CreateBr(done);
                    builder.SetInsertPoint(done);
                    res = phi();
                } else {
                    auto setter = needsLdVarForUpdate.count(i)
                                      ? NativeBuiltins::ldvarForUpdate
                                      : NativeBuiltins::ldvar;
                    res = call(setter,
                               {constant(varName, t::SEXP), loadSxp(i->env())});
                }
                res->setName(CHAR(PRINTNAME(varName)));

                if (maybeLd) {
                    checkMissing(res);
                    checkUnbound(res);
                }
                setVal(i, res);
                break;
            }

            case Tag::Extract1_1D: {
                auto extract = Extract1_1D::Cast(i);
                auto vector = loadSxp(extract->vec());

                bool fastcase = !extract->vec()->type.maybe(RType::vec) &&
                                !extract->vec()->type.maybeObj() &&
                                vectorTypeSupport(extract->vec()) &&
                                extract->idx()->type.isA(
                                    PirType::intReal().notObject().scalar());
                BasicBlock* done;
                auto res = phiBuilder(representationOf(i));

                if (fastcase) {
                    auto fallback = BasicBlock::Create(C, "", fun);
                    done = BasicBlock::Create(C, "", fun);

                    llvm::Value* vector = load(extract->vec());

                    if (representationOf(extract->vec()) == t::SEXP) {
                        auto hit2 = BasicBlock::Create(C, "", fun);
                        builder.CreateCondBr(isAltrep(vector), fallback, hit2,
                                             branchMostlyFalse);
                        builder.SetInsertPoint(hit2);

                        if (extract->vec()->type.maybeHasAttrs()) {
                            auto hit3 = BasicBlock::Create(C, "", fun);
                            builder.CreateCondBr(fastVeceltOkNative(vector),
                                                 hit3, fallback,
                                                 branchMostlyTrue);
                            builder.SetInsertPoint(hit3);
                        }
                    }

                    llvm::Value* index =
                        computeAndCheckIndex(extract->idx(), vector, fallback);
                    auto res0 =
                        extract->vec()->type.isScalar()
                            ? vector
                            : accessVector(vector, index, extract->vec()->type);
                    res.addInput(convert(res0, i->type));
                    builder.CreateBr(done);

                    builder.SetInsertPoint(fallback);
                }

                auto env = constant(R_NilValue, t::SEXP);
                if (extract->hasEnv())
                    env = loadSxp(extract->env());
                auto idx = loadSxp(extract->idx());
                auto res0 = call(NativeBuiltins::extract11,
                                 {vector, idx, env, c(extract->srcIdx)});

                res.addInput(convert(res0, i->type));
                if (fastcase) {
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);
                }

                setVal(i, res());
                break;
            }

            case Tag::Extract1_2D: {
                auto extract = Extract1_2D::Cast(i);

                bool fastcase = !extract->vec()->type.maybe(RType::vec) &&
                                !extract->vec()->type.maybeObj() &&
                                vectorTypeSupport(extract->vec()) &&
                                extract->idx1()->type.isA(
                                    PirType::intReal().notObject().scalar()) &&
                                extract->idx2()->type.isA(
                                    PirType::intReal().notObject().scalar());

                BasicBlock* done;
                auto res = phiBuilder(representationOf(i));

                if (fastcase) {
                    auto fallback = BasicBlock::Create(C, "", fun);
                    done = BasicBlock::Create(C, "", fun);

                    llvm::Value* vector = load(extract->vec());

                    if (representationOf(extract->vec()) == t::SEXP) {
                        auto hit2 = BasicBlock::Create(C, "", fun);
                        builder.CreateCondBr(isAltrep(vector), fallback, hit2,
                                             branchMostlyFalse);
                        builder.SetInsertPoint(hit2);

                        if (extract->vec()->type.maybeHasAttrs()) {
                            auto hit3 = BasicBlock::Create(C, "", fun);
                            builder.CreateCondBr(fastVeceltOkNative(vector),
                                                 hit3, fallback,
                                                 branchMostlyTrue);
                            builder.SetInsertPoint(hit3);
                        }
                    }

                    auto ncol = builder.CreateZExt(
                        call(NativeBuiltins::matrixNcols, {vector}), t::i64);
                    auto nrow = builder.CreateZExt(
                        call(NativeBuiltins::matrixNrows, {vector}), t::i64);
                    llvm::Value* index1 = computeAndCheckIndex(
                        extract->idx1(), vector, fallback, nrow);
                    llvm::Value* index2 = computeAndCheckIndex(
                        extract->idx2(), vector, fallback, ncol);

                    llvm::Value* index =
                        builder.CreateMul(nrow, index2, "", true, true);
                    index = builder.CreateAdd(index, index1, "", true, true);

                    auto res0 =
                        extract->vec()->type.isScalar()
                            ? vector
                            : accessVector(vector, index, extract->vec()->type);

                    res.addInput(convert(res0, i->type));
                    builder.CreateBr(done);

                    builder.SetInsertPoint(fallback);
                }

                auto vector = loadSxp(extract->vec());
                auto idx1 = loadSxp(extract->idx1());
                auto idx2 = loadSxp(extract->idx2());
                auto res0 = call(NativeBuiltins::extract12,
                                 {vector, idx1, idx2, loadSxp(extract->env()),
                                  c(extract->srcIdx)});

                res.addInput(convert(res0, i->type));
                if (fastcase) {
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);
                }
                setVal(i, res());
                break;
            }

            case Tag::Extract2_1D: {
                auto extract = Extract2_1D::Cast(i);
                // TODO: Extend a fastPath for generic vectors.
                bool fastcase = vectorTypeSupport(extract->vec()) &&
                                extract->idx()->type.isA(
                                    PirType::intReal().notObject().scalar());

                BasicBlock* done;
                auto res = phiBuilder(representationOf(i));

                if (fastcase) {
                    auto fallback = BasicBlock::Create(C, "", fun);
                    auto hit2 = BasicBlock::Create(C, "", fun);
                    done = BasicBlock::Create(C, "", fun);

                    llvm::Value* vector = load(extract->vec());

                    if (representationOf(extract->vec()) == t::SEXP) {
                        builder.CreateCondBr(isAltrep(vector), fallback, hit2,
                                             branchMostlyFalse);
                        builder.SetInsertPoint(hit2);
                    }

                    llvm::Value* index =
                        computeAndCheckIndex(extract->idx(), vector, fallback);
                    auto res0 =
                        extract->vec()->type.isScalar()
                            ? vector
                            : accessVector(vector, index, extract->vec()->type);
                    res.addInput(convert(res0, i->type));
                    builder.CreateBr(done);

                    builder.SetInsertPoint(fallback);
                }

                auto irep = representationOf(extract->idx());
                llvm::Value* res0;

                if (irep != t::SEXP) {
                    NativeBuiltin getter;
                    if (irep == t::Int) {
                        getter = NativeBuiltins::extract21i;
                    } else {
                        assert(irep == t::Double);
                        getter = NativeBuiltins::extract21r;
                    }
                    auto vector = loadSxp(extract->vec());
                    res0 = call(getter,
                                {vector, load(extract->idx()),
                                 loadSxp(extract->env()), c(extract->srcIdx)});
                } else {
                    auto vector = loadSxp(extract->vec());
                    auto idx = loadSxp(extract->idx());
                    res0 = call(NativeBuiltins::extract21,
                                {vector, idx, loadSxp(extract->env()),
                                 c(extract->srcIdx)});
                }

                res.addInput(convert(res0, i->type));
                if (fastcase) {
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);
                }
                setVal(i, res());
                break;
            }

            case Tag::Extract1_3D: {
                auto extract = Extract1_3D::Cast(i);
                auto vector = loadSxp(extract->vec());
                auto idx1 = loadSxp(extract->idx1());
                auto idx2 = loadSxp(extract->idx2());
                auto idx3 = loadSxp(extract->idx3());

                // We should implement the fast cases (known and primitive
                // types) speculatively here
                auto env = constant(R_NilValue, t::SEXP);
                if (extract->hasEnv())
                    env = loadSxp(extract->env());

                auto res =
                    call(NativeBuiltins::extract13,
                         {vector, idx1, idx2, idx3, env, c(extract->srcIdx)});
                setVal(i, res);

                break;
            }

            case Tag::Extract2_2D: {
                auto extract = Extract2_2D::Cast(i);

                bool fastcase = vectorTypeSupport(extract->vec()) &&
                                extract->idx1()->type.isA(
                                    PirType::intReal().notObject().scalar()) &&
                                extract->idx2()->type.isA(
                                    PirType::intReal().notObject().scalar());

                BasicBlock* done;
                auto res = phiBuilder(representationOf(i));

                if (fastcase) {
                    auto fallback = BasicBlock::Create(C, "", fun);
                    auto hit2 = BasicBlock::Create(C, "", fun);
                    done = BasicBlock::Create(C, "", fun);

                    llvm::Value* vector = load(extract->vec());

                    if (representationOf(extract->vec()) == t::SEXP) {
                        builder.CreateCondBr(isAltrep(vector), fallback, hit2,
                                             branchMostlyFalse);
                        builder.SetInsertPoint(hit2);
                    }

                    auto ncol = builder.CreateZExt(
                        call(NativeBuiltins::matrixNcols, {vector}), t::i64);
                    auto nrow = builder.CreateZExt(
                        call(NativeBuiltins::matrixNrows, {vector}), t::i64);
                    llvm::Value* index1 = computeAndCheckIndex(
                        extract->idx1(), vector, fallback, nrow);
                    llvm::Value* index2 = computeAndCheckIndex(
                        extract->idx2(), vector, fallback, ncol);

                    llvm::Value* index =
                        builder.CreateMul(nrow, index2, "", true, true);
                    index = builder.CreateAdd(index, index1, "", true, true);

                    auto res0 =
                        extract->vec()->type.isScalar()
                            ? vector
                            : accessVector(vector, index, extract->vec()->type);

                    res.addInput(convert(res0, i->type));
                    builder.CreateBr(done);

                    builder.SetInsertPoint(fallback);
                }

                auto irep = representationOf(extract->idx1());
                llvm::Value* res0;

                if (irep != t::SEXP &&
                    representationOf(extract->idx2()) == irep) {
                    NativeBuiltin getter;
                    if (irep == t::Int) {
                        getter = NativeBuiltins::extract22ii;
                    } else {
                        assert(irep == t::Double);
                        getter = NativeBuiltins::extract22rr;
                    }

                    auto vector = loadSxp(extract->vec());
                    res0 = call(getter,
                                {vector, load(extract->idx1()),
                                 load(extract->idx2()), loadSxp(extract->env()),
                                 c(extract->srcIdx)});
                } else {

                    auto vector = loadSxp(extract->vec());
                    auto idx1 = loadSxp(extract->idx1());
                    auto idx2 = loadSxp(extract->idx2());
                    res0 = call(NativeBuiltins::extract22,
                                {vector, idx1, idx2, loadSxp(extract->env()),
                                 c(extract->srcIdx)});
                }

                res.addInput(convert(res0, i->type));
                if (fastcase) {
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);
                }
                setVal(i, res());
                break;
            }

            case Tag::Subassign1_3D: {
                auto subAssign = Subassign1_3D::Cast(i);
                auto vector = loadSxp(subAssign->lhs());
                auto val = loadSxp(subAssign->rhs());
                auto idx1 = loadSxp(subAssign->idx1());
                auto idx2 = loadSxp(subAssign->idx2());
                auto idx3 = loadSxp(subAssign->idx3());

                // We should implement the fast cases (known and primitive
                // types) speculatively here
                auto res =
                    call(NativeBuiltins::subassign13,
                         {vector, idx1, idx2, idx3, val,
                          loadSxp(subAssign->env()), c(subAssign->srcIdx)});
                setVal(i, res);
                break;
            }

            case Tag::Subassign1_2D: {
                auto subAssign = Subassign1_2D::Cast(i);
                auto vector = loadSxp(subAssign->lhs());
                auto val = loadSxp(subAssign->rhs());
                auto idx1 = loadSxp(subAssign->idx1());
                auto idx2 = loadSxp(subAssign->idx2());

                // We should implement the fast cases (known and primitive
                // types) speculatively here
                auto res =
                    call(NativeBuiltins::subassign12,
                         {vector, idx1, idx2, val, loadSxp(subAssign->env()),
                          c(subAssign->srcIdx)});
                setVal(i, res);
                break;
            }

            case Tag::Subassign2_2D: {
                auto subAssign = Subassign2_2D::Cast(i);

                auto idx1Type = subAssign->idx1()->type;
                auto idx2Type = subAssign->idx2()->type;
                auto valType = subAssign->rhs()->type;
                auto vecType = subAssign->lhs()->type;

                BasicBlock* done = nullptr;
                auto res = phiBuilder(representationOf(i));

                // Missing cases: store int into double matrix / store double
                // into int matrix
                auto fastcase =
                    idx1Type.isA(PirType::intReal().notObject().scalar()) &&
                    idx2Type.isA(PirType::intReal().notObject().scalar()) &&
                    valType.isScalar() && !vecType.maybeObj() &&
                    ((vecType.isA(RType::integer) &&
                      valType.isA(RType::integer)) ||
                     (vecType.isA(RType::real) && valType.isA(RType::real)));

                if (fastcase) {
                    auto fallback = BasicBlock::Create(C, "", fun);
                    auto hit = BasicBlock::Create(C, "", fun);
                    done = BasicBlock::Create(C, "", fun);

                    llvm::Value* vector = load(subAssign->lhs());
                    if (representationOf(subAssign->lhs()) == t::SEXP) {
                        builder.CreateCondBr(shared(vector), fallback, hit,
                                             branchMostlyFalse);
                        builder.SetInsertPoint(hit);
                    }

                    auto ncol = builder.CreateZExt(
                        call(NativeBuiltins::matrixNcols, {vector}), t::i64);
                    auto nrow = builder.CreateZExt(
                        call(NativeBuiltins::matrixNrows, {vector}), t::i64);
                    llvm::Value* index1 = computeAndCheckIndex(
                        subAssign->idx1(), vector, fallback, nrow);
                    llvm::Value* index2 = computeAndCheckIndex(
                        subAssign->idx2(), vector, fallback, ncol);

                    auto val = load(subAssign->rhs());
                    if (representationOf(i) == Representation::Sexp) {
                        llvm::Value* index =
                            builder.CreateMul(nrow, index2, "", true, true);
                        index =
                            builder.CreateAdd(index, index1, "", true, true);
                        assignVector(vector, index, val, vecType);
                        res.addInput(convert(vector, i->type));
                    } else {
                        res.addInput(convert(val, i->type));
                    }

                    builder.CreateBr(done);

                    builder.SetInsertPoint(fallback);
                }

                auto idx1 = loadSxp(subAssign->idx1());
                auto idx2 = loadSxp(subAssign->idx2());

                llvm::Value* assign = nullptr;
                auto irep = representationOf(subAssign->idx1());
                auto vrep = representationOf(subAssign->rhs());
                if (representationOf(subAssign->idx2()) == irep &&
                    irep != t::SEXP && vrep != t::SEXP) {
                    NativeBuiltin setter;
                    if (irep == t::Int && vrep == t::Int)
                        setter = NativeBuiltins::subassign22iii;
                    else if (irep == t::Double && vrep == t::Int)
                        setter = NativeBuiltins::subassign22rri;
                    else if (irep == t::Int && vrep == t::Double)
                        setter = NativeBuiltins::subassign22iir;
                    else {
                        assert(irep == t::Double && vrep == t::Double);
                        setter = NativeBuiltins::subassign22rrr;
                    }

                    assign = call(
                        setter,
                        {loadSxp(subAssign->lhs()), load(subAssign->idx1()),
                         load(subAssign->idx2()), load(subAssign->rhs()),
                         loadSxp(subAssign->env()), c(subAssign->srcIdx)});
                } else {
                    assign =
                        call(NativeBuiltins::subassign22,
                             {loadSxp(subAssign->lhs()), idx1, idx2,
                              loadSxp(subAssign->rhs()),
                              loadSxp(subAssign->env()), c(subAssign->srcIdx)});
                }

                res.addInput(assign);
                if (fastcase) {
                    builder.CreateBr(done);
                    builder.SetInsertPoint(done);
                }
                setVal(i, res());

                break;
            }

            case Tag::Subassign1_1D: {
                auto subAssign = Subassign1_1D::Cast(i);

                // TODO: Extend a fastPath for generic vectors.
                // TODO: Support type conversions
                auto vecType = subAssign->vector()->type;
                auto valType = subAssign->val()->type;
                auto idxType = subAssign->idx()->type;

                BasicBlock* done = nullptr;
                auto resultRep = representationOf(i);
                auto res = phiBuilder(resultRep);

                // Missing cases: store int into double vect / store double into
                // int vect
                bool fastcase =
                    idxType.isA(PirType::intReal().notObject().scalar()) &&
                    valType.isScalar() && !vecType.maybeObj() &&
                    ((vecType.isA(RType::integer) &&
                      valType.isA(RType::integer)) ||
                     (vecType.isA(RType::real) && valType.isA(RType::real)));

                if (fastcase) {
                    auto fallback = BasicBlock::Create(C, "", fun);
                    done = BasicBlock::Create(C, "", fun);

                    llvm::Value* vector = load(subAssign->vector());
                    if (representationOf(subAssign->vector()) == t::SEXP) {
                        auto hit1 = BasicBlock::Create(C, "", fun);
                        builder.CreateCondBr(isAltrep(vector), fallback, hit1,
                                             branchMostlyFalse);
                        builder.SetInsertPoint(hit1);

                        if (vecType.maybeHasAttrs()) {
                            auto hit2 = BasicBlock::Create(C, "", fun);
                            builder.CreateCondBr(fastVeceltOkNative(vector),
                                                 hit2, fallback,
                                                 branchMostlyTrue);
                            builder.SetInsertPoint(hit2);
                        }

                        auto hit3 = BasicBlock::Create(C, "", fun);
                        builder.CreateCondBr(shared(vector), fallback, hit3,
                                             branchMostlyFalse);
                        builder.SetInsertPoint(hit3);
                    }

                    llvm::Value* index = computeAndCheckIndex(subAssign->idx(),
                                                              vector, fallback);

                    auto val = load(subAssign->val());
                    if (representationOf(i) == Representation::Sexp) {
                        assignVector(vector, index, val,
                                     subAssign->vector()->type);
                        res.addInput(convert(vector, i->type));
                    } else {
                        res.addInput(convert(val, i->type));
                    }

                    builder.CreateBr(done);

                    builder.SetInsertPoint(fallback);
                }

                llvm::Value* res0 =
                    call(NativeBuiltins::subassign11,
                         {loadSxp(subAssign->vector()),
                          loadSxp(subAssign->idx()), loadSxp(subAssign->val()),
                          loadSxp(subAssign->env()), c(subAssign->srcIdx)});

                res.addInput(convert(res0, i->type));
                if (fastcase) {
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);
                }
                setVal(i, res());
                break;
            }

            case Tag::Subassign2_1D: {
                auto subAssign = Subassign2_1D::Cast(i);

                // TODO: Extend a fastPath for generic vectors.
                // TODO: Support type conversions
                auto vecType = subAssign->vector()->type;
                auto valType = subAssign->val()->type;
                auto idxType = subAssign->idx()->type;

                BasicBlock* done = nullptr;
                auto resultRep = representationOf(i);
                auto res = phiBuilder(resultRep);

                // Missing cases: store int into double vect / store double into
                // int vect
                bool fastcase =
                    idxType.isA(PirType::intReal().notObject().scalar()) &&
                    valType.isScalar() && !vecType.maybeObj() &&
                    ((vecType.isA(RType::integer) &&
                      valType.isA(RType::integer)) ||
                     (vecType.isA(RType::real) && valType.isA(RType::real)));

                if (fastcase) {
                    auto fallback = BasicBlock::Create(C, "", fun);
                    done = BasicBlock::Create(C, "", fun);

                    llvm::Value* vector = load(subAssign->vector());
                    if (representationOf(subAssign->vector()) == t::SEXP) {
                        auto hit1 = BasicBlock::Create(C, "", fun);
                        builder.CreateCondBr(isAltrep(vector), fallback, hit1,
                                             branchMostlyFalse);
                        builder.SetInsertPoint(hit1);

                        auto hit3 = BasicBlock::Create(C, "", fun);
                        builder.CreateCondBr(shared(vector), fallback, hit3,
                                             branchMostlyFalse);
                        builder.SetInsertPoint(hit3);
                    }

                    llvm::Value* index = computeAndCheckIndex(subAssign->idx(),
                                                              vector, fallback);

                    auto val = load(subAssign->val());
                    if (representationOf(i) == Representation::Sexp) {
                        assignVector(vector, index, val,
                                     subAssign->vector()->type);
                        res.addInput(convert(vector, i->type));
                    } else {
                        res.addInput(convert(val, i->type));
                    }

                    builder.CreateBr(done);

                    builder.SetInsertPoint(fallback);
                }

                llvm::Value* res0 = nullptr;
                auto irep = representationOf(subAssign->idx());
                auto vrep = representationOf(subAssign->val());
                if (irep != t::SEXP && vrep != t::SEXP) {
                    NativeBuiltin setter;
                    if (irep == t::Int && vrep == t::Int)
                        setter = NativeBuiltins::subassign21ii;
                    else if (irep == t::Double && vrep == t::Int)
                        setter = NativeBuiltins::subassign21ri;
                    else if (irep == t::Int && vrep == t::Double)
                        setter = NativeBuiltins::subassign21ir;
                    else {
                        assert(irep == t::Double && vrep == t::Double);
                        setter = NativeBuiltins::subassign21rr;
                    }

                    res0 =
                        call(setter,
                             {loadSxp(subAssign->vector()),
                              load(subAssign->idx()), load(subAssign->val()),
                              loadSxp(subAssign->env()), c(subAssign->srcIdx)});
                } else {
                    res0 = call(
                        NativeBuiltins::subassign21,
                        {loadSxp(subAssign->vector()),
                         loadSxp(subAssign->idx()), loadSxp(subAssign->val()),
                         loadSxp(subAssign->env()), c(subAssign->srcIdx)});
                }

                res.addInput(convert(res0, i->type));
                if (fastcase) {
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);
                }
                setVal(i, res());
                break;
            }

            case Tag::StVar: {
                auto st = StVar::Cast(i);
                auto environment = MkEnv::Cast(st->env());

                if (environment && environment->stub) {
                    auto idx = environment->indexOf(st->varName);
                    auto e = loadSxp(environment);
                    BasicBlock* done = BasicBlock::Create(C, "", fun);
                    auto cur = envStubGet(e, idx, environment->nLocals());

                    if (representationOf(st->val()) != t::SEXP) {
                        auto fastcase = BasicBlock::Create(C, "", fun);
                        auto fallback = BasicBlock::Create(C, "", fun);

                        auto expected = representationOf(st->val()) == t::Int
                                            ? INTSXP
                                            : REALSXP;
                        auto reuse = builder.CreateAnd(
                            builder.CreateNot(isObj(cur)),
                            builder.CreateAnd(
                                builder.CreateNot(shared(cur)),
                                builder.CreateAnd(
                                    builder.CreateICmpEQ(sexptype(cur),
                                                         c(expected)),
                                    isScalar(cur))));
                        builder.CreateCondBr(reuse, fastcase, fallback,
                                             branchMostlyTrue);

                        builder.SetInsertPoint(fastcase);
                        auto store =
                            vectorPositionPtr(cur, c(0), st->val()->type);
                        builder.CreateStore(load(st->val()), store);
                        builder.CreateBr(done);

                        builder.SetInsertPoint(fallback);
                    }

                    auto val = loadSxp(st->val());
                    if (representationOf(st->val()) == t::SEXP) {
                        auto same = BasicBlock::Create(C, "", fun);
                        auto different = BasicBlock::Create(C, "", fun);
                        builder.CreateCondBr(builder.CreateICmpEQ(val, cur),
                                             same, different);

                        builder.SetInsertPoint(same);
                        ensureNamed(val);
                        if (!st->isStArg)
                            envStubSetNotMissing(e, idx);
                        builder.CreateBr(done);

                        builder.SetInsertPoint(different);
                        incrementNamed(val);
                        envStubSet(e, idx, val, environment->nLocals(),
                                   !st->isStArg);
                        builder.CreateBr(done);
                    } else {
                        ensureNamed(val);
                        envStubSet(e, idx, val, environment->nLocals(),
                                   !st->isStArg);
                    }

                    builder.CreateBr(done);
                    builder.SetInsertPoint(done);
                    break;
                }

                auto setter = NativeBuiltins::stvar;
                if (st->isStArg)
                    setter = NativeBuiltins::starg;

                if (bindingsCache.count(environment)) {
                    auto offset = bindingsCache.at(environment).at(st->varName);
                    auto cachePtr =
                        builder.CreateGEP(bindingsCacheBase, c(offset));
                    llvm::Value* cache = builder.CreateLoad(cachePtr);

                    auto hit1 = BasicBlock::Create(C, "", fun);
                    auto hit2 = BasicBlock::Create(C, "", fun);
                    auto hit3 = BasicBlock::Create(C, "", fun);
                    auto identical = BasicBlock::Create(C, "", fun);
                    auto miss = BasicBlock::Create(C, "", fun);
                    auto done = BasicBlock::Create(C, "", fun);

                    auto newVal = loadSxp(st->arg<0>().val());

                    builder.CreateCondBr(
                        builder.CreateICmpULE(
                            builder.CreatePtrToInt(cache, t::i64), c(1, 64)),
                        miss, hit1, branchMostlyFalse);

                    builder.SetInsertPoint(hit1);
                    auto val = car(cache);
                    builder.CreateCondBr(
                        builder.CreateICmpEQ(val,
                                             constant(R_UnboundValue, t::SEXP)),
                        miss, hit2, branchMostlyFalse);

                    builder.SetInsertPoint(hit2);
                    builder.CreateCondBr(builder.CreateICmpEQ(val, newVal),
                                         identical, hit3, branchMostlyFalse);

                    builder.SetInsertPoint(hit3);
                    incrementNamed(newVal);
                    assert(cache->getType() == t::SEXP);
                    assert(newVal->getType() == t::SEXP);
                    setCar(cache, newVal);
                    builder.CreateBr(done);

                    builder.SetInsertPoint(identical);
                    // In the fast case (where the value is not updated) we
                    // still need to ensure it is named.
                    ensureNamed(val);
                    builder.CreateBr(done);

                    builder.SetInsertPoint(miss);
                    call(setter, {constant(st->varName, t::SEXP), newVal,
                                  loadSxp(st->env())});
                    builder.CreateBr(done);

                    builder.SetInsertPoint(done);

                } else {
                    call(setter,
                         {constant(st->varName, t::SEXP),
                          loadSxp(st->arg<0>().val()), loadSxp(st->env())});
                }
                break;
            }

            case Tag::StVarSuper: {
                auto st = StVarSuper::Cast(i);
                auto environment = MkEnv::Cast(st->env());
                if (environment) {
                    auto parent = MkEnv::Cast(environment->lexicalEnv());
                    if (environment->stub || (parent && parent->stub)) {
                        success = false;
                        break;
                    }
                }

                // In case we statically knew the parent PIR already converted
                // super assigns to standard stores
                call(NativeBuiltins::defvar,
                     {constant(st->varName, t::SEXP),
                      loadSxp(st->arg<0>().val()), loadSxp(st->env())});
                break;
            }

            case Tag::Missing: {
                assert(representationOf(i) == Representation::Integer);
                auto missing = Missing::Cast(i);
                setVal(i, call(NativeBuiltins::isMissing,
                               {constant(missing->varName, t::SEXP),
                                loadSxp(i->env())}));
                break;
            }

            case Tag::ChkMissing: {
                auto arg = i->arg(0).val();
                if (representationOf(arg) == Representation::Sexp)
                    checkMissing(loadSxp(arg));
                setVal(i, load(arg, representationOf(i)));
                break;
            }

            case Tag::ChkClosure: {
                auto arg = loadSxp(i->arg(0).val());
                call(
                    NativeBuiltins::chkfun,
                    {constant(Rf_install(ChkClosure::Cast(i)->name()), t::SEXP),
                     arg});
                setVal(i, arg);
                break;
            }

            case Tag::Length: {
                llvm::Value* l;
                if (representationOf(i) == Representation::Sexp) {
                    l = vectorLength(loadSxp(i->arg(0).val()));
                } else if (representationOf(i) == Representation::Real) {
                    l = c((double)1);
                } else {
                    assert(representationOf(i) == Representation::Integer);
                    l = c(1);
                }
                setVal(i, l);
                break;
            }

            case Tag::ColonInputEffects:
                setVal(i, call(NativeBuiltins::colonInputEffects,
                               {loadSxp(i->arg(0).val()),
                                loadSxp(i->arg(1).val()), c(i->srcIdx)}));
                break;

            case Tag::ColonCastLhs:
                setVal(i, call(NativeBuiltins::colonCastLhs,
                               {loadSxp(i->arg(0).val())}));
                break;

            case Tag::ColonCastRhs:
                setVal(i, call(NativeBuiltins::colonCastRhs,
                               {loadSxp(i->arg(0).val()),
                                loadSxp(i->arg(1).val())}));
                break;

            case Tag::Int3:
            case Tag::PrintInvocation:
                success = false;
                break;

            case Tag::_UNUSED_:
                assert(false && "Invalid instruction tag");
                success = false;
                break;

            case Tag::FrameState:
            case Tag::Checkpoint:
            case Tag::Assume:
            case Tag::Deopt:
                assert(false && "Expected scheduled deopt");
                success = false;
                break;

#define V(Value) case Tag::Value:
                COMPILER_VALUES(V)
#undef V
                assert(false && "Values should not occur in instructions");
                success = false;
                break;
            }

            if (!success) {
                // std::cerr << "Can't compile ";
                // i->print(std::cerr, true);
                // std::cerr << "\n";
            }

            if (!success)
                return;

            Instruction* origin = i;
            llvm::Value* sexpResult = nullptr;

            if (phis.count(i)) {
                auto phi = phis.at(i);
                auto r = representationOf(phi);
                auto inp = PirCopy::Cast(i) ? i->arg(0).val() : i;
                auto inpv = load(inp, r);

                // If we box this phi input we need to make sure that we are not
                // missing an ensure named, because the original value would
                // have gotten an ensureNamed if it weren't unboxed.
                if (representationOf(inp) != t::SEXP && r == t::SEXP) {
                    if (auto ii = Instruction::Cast(inp->followCasts())) {
                        sexpResult = inpv;
                        origin = ii;
                    }
                }
                variables.at(phi).update(builder, inpv);
            }

            if (sexpResult ||
                (representationOf(origin) == t::SEXP &&
                 variables.count(origin) && variables.at(origin).initialized)) {

                auto adjust = refcount.atCreation.find(origin);
                if (adjust != refcount.atCreation.end()) {
                    if (!sexpResult)
                        sexpResult = load(origin);

                    if (adjust->second == NeedsRefcountAdjustment::SetShared)
                        ensureShared(sexpResult);
                    else if (adjust->second ==
                             NeedsRefcountAdjustment::EnsureNamed)
                        ensureNamed(sexpResult);
                }
            }

            numTemps = 0;
        }

        if (bb->isJmp())
            builder.CreateBr(getBlock(bb->next()));

        for (auto suc : bb->succsessors())
            blockInPushContext[suc] = inPushContext;
    });

    // Delayed insertion of the branch, so we can still easily add instructions
    // to the entry block while compiling
    builder.SetInsertPoint(entryBlock);
    builder.CreateBr(getBlock(code->entry));

    if (success) {
        // outs() << "Compiled " << fun->getName() << "\n";
        // fun->dump();
        // code->printCode(std::cout, true, true);
    }
    return success;
}

} // namespace pir
} // namespace rir