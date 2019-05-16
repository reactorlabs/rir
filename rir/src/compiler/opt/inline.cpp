#include "../../api.h"
#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/safe_builtins_list.h"
#include "../util/visitor.h"
#include "R/Funtab.h"
#include "R/Symbols.h"
#include "R/r.h"
#include "compiler/parameter.h"
#include "pass_definitions.h"
#include "utils/Pool.h"

#include <algorithm>
#include <unordered_map>

namespace {

using namespace rir::pir;

class TheInliner {
  public:
    ClosureVersion* version;
    explicit TheInliner(ClosureVersion* version) : version(version) {}

    void operator()() {
        size_t fuel = Parameter::INLINER_INITIAL_FUEL;

        if (version->size() > Parameter::INLINER_MAX_SIZE)
            return;

        Visitor::run(version->entry, [&](BB* bb) {
            // Dangerous iterater usage, works since we do only update it in
            // one place.
            for (auto it = bb->begin(); it != bb->end() && fuel; it++) {
                if (!CallInstruction::CastCall(*it))
                    continue;

                Closure* inlineeCls = nullptr;
                ClosureVersion* inlinee = nullptr;
                Value* staticEnv = nullptr;

                const FrameState* callerFrameState = nullptr;
                if (auto call = Call::Cast(*it)) {
                    auto mkcls =
                        MkFunCls::Cast(call->cls()->followCastsAndForce());
                    if (!mkcls)
                        continue;
                    inlineeCls = mkcls->cls;
                    if (inlineeCls->rirFunction()->uninlinable)
                        continue;
                    inlinee = call->tryDispatch(inlineeCls);
                    if (!inlinee)
                        continue;
                    if (inlinee->nargs() -
                            inlinee->assumptions().numMissing() !=
                        call->nCallArgs())
                        continue;
                    staticEnv = mkcls->lexicalEnv();
                    callerFrameState = call->frameState();
                } else if (auto call = StaticCall::Cast(*it)) {
                    inlineeCls = call->cls();
                    if (inlineeCls->rirFunction()->uninlinable)
                        continue;
                    inlinee = call->tryDispatch();
                    if (!inlinee)
                        continue;
                    if (inlinee->nargs() -
                            inlinee->assumptions().numMissing() !=
                        call->nCallArgs())
                        continue;
                    // if we don't know the closure of the inlinee, we can't
                    // inline.
                    if (inlineeCls->closureEnv() == Env::notClosed() &&
                        inlinee != version)
                        continue;
                    staticEnv = inlineeCls->closureEnv();
                    callerFrameState = call->frameState();
                } else {
                    continue;
                }

                if (inlineeCls->rirFunction()->uninlinable)
                    continue;

                enum SafeToInline {
                    Yes,
                    NeedsContext,
                    No,
                };

                // TODO: instead of blacklisting those, we could also create
                // contexts for inlined functions.
                SafeToInline allowInline = SafeToInline::Yes;
                auto updateAllowInline = [&](Code* code) {
                    Visitor::check(code->entry, [&](Instruction* i) {
                        if (auto ld = LdFun::Cast(i)) {
                            if (!SafeBuiltinsList::forInlineByName(
                                    ld->varName)) {
                                allowInline = SafeToInline::No;
                                return false;
                            }
                        }
                        if (auto call = CallBuiltin::Cast(i)) {
                            if (!SafeBuiltinsList::forInline(call->builtinId)) {
                                allowInline = SafeToInline::No;
                                return false;
                            }
                        }
                        if (CallInstruction::CastCall(i)) {
                            allowInline = SafeToInline::NeedsContext;
                        }
                        return true;
                    });
                };

                // No recursive inlining
                if (inlinee->owner() == version->owner()) {
                    continue;
                } else if (inlinee->size() >
                           Parameter::INLINER_MAX_INLINEE_SIZE) {
                    inlineeCls->rirFunction()->uninlinable = true;
                    continue;
                } else {
                    updateAllowInline(inlinee);
                    inlinee->eachPromise(
                        [&](Promise* p) { updateAllowInline(p); });
                    if (allowInline == SafeToInline::No) {
                        inlineeCls->rirFunction()->uninlinable = true;
                        continue;
                    }
                }

                fuel--;

                BB* split =
                    BBTransform::split(version->nextBBId++, bb, it, version);
                auto theCall = *split->begin();
                auto theCallInstruction = CallInstruction::CastCall(theCall);
                std::vector<Value*> arguments;
                theCallInstruction->eachCallArg(
                    [&](Value* v) { arguments.push_back(v); });

                // Clone the version
                BB* copy = BBTransform::clone(inlinee->entry, version, version);

                bool needsEnvPatching = inlineeCls->closureEnv() != staticEnv;

                bool failedToInline = false;
                Visitor::run(copy, [&](BB* bb) {
                    auto ip = bb->begin();
                    while (!failedToInline && ip != bb->end()) {
                        auto next = ip + 1;
                        auto ld = LdArg::Cast(*ip);
                        Instruction* i = *ip;

                        if (auto sp = FrameState::Cast(i)) {
                            if (!callerFrameState) {
                                failedToInline = true;
                                return;
                            }

                            // When inlining a frameState we need to chain it
                            // with the frameStates after the call to the
                            // inlinee
                            if (!sp->next()) {
                                auto copyFromFs = callerFrameState;
                                auto cloneSp =
                                    FrameState::Cast(copyFromFs->clone());

                                ip = bb->insert(ip, cloneSp);
                                sp->next(cloneSp);

                                size_t created = 1;
                                while (copyFromFs->next()) {
                                    assert(copyFromFs->next() ==
                                           cloneSp->next());
                                    copyFromFs = copyFromFs->next();
                                    auto prevClone = cloneSp;
                                    cloneSp =
                                        FrameState::Cast(copyFromFs->clone());

                                    ip = bb->insert(ip, cloneSp);
                                    created++;

                                    prevClone->updateNext(cloneSp);
                                }

                                next = ip + created + 1;
                            }
                        }
                        // If the inlining resolved some env, we need to
                        // update. For example this happens if we inline an
                        // inner version. Then the lexical env is the current
                        // versions env.
                        if (needsEnvPatching && i->hasEnv() &&
                            i->env() == inlineeCls->closureEnv()) {
                            i->env(staticEnv);
                        }

                        // If we inline without context, then we need to update
                        // the mkEnv instructions in the inlinee, such that
                        // they do not update the (non-existing) context.
                        if (allowInline != SafeToInline::NeedsContext) {
                            if (auto mk = MkEnv::Cast(i)) {
                                mk->context--;
                            }
                        }

                        if (ld) {
                            Value* a = arguments[ld->id];
                            if (auto mk = MkArg::Cast(a)) {
                                if (!ld->type.maybePromiseWrapped()) {
                                    // This load already expects to load an
                                    // eager value. We can just discard the
                                    // promise altogether.
                                    assert(mk->isEager());
                                    a = mk->eagerArg();
                                } else {
                                    // We need to cast from a promise to a lazy
                                    // value
                                    auto type = mk->isEager()
                                                    ? mk->eagerArg()
                                                          ->type.forced()
                                                          .orPromiseWrapped()
                                                    : ld->type;
                                    auto cast = new CastType(a, RType::prom,
                                                             type.notMissing());
                                    ip = bb->insert(ip + 1, cast);
                                    ip--;
                                    a = cast;
                                }
                            }
                            ld->replaceUsesWith(a);
                            next = bb->remove(ip);
                        }
                        ip = next;
                    }
                });

                if (failedToInline) {
                    delete copy;
                    bb->overrideNext(split);
                    inlineeCls->rirFunction()->uninlinable = true;
                } else {
                    bb->overrideNext(copy);

                    // Copy over promises used by the inner version
                    std::vector<bool> copiedPromise(false);
                    std::vector<size_t> newPromId;
                    copiedPromise.resize(inlinee->promises().size(), false);
                    newPromId.resize(inlinee->promises().size());
                    Visitor::run(copy, [&](BB* bb) {
                        auto it = bb->begin();
                        while (it != bb->end()) {
                            MkArg* mk = MkArg::Cast(*it);
                            it++;
                            if (!mk)
                                continue;

                            size_t id = mk->prom()->id;
                            if (mk->prom()->owner == inlinee) {
                                assert(id < copiedPromise.size());
                                if (copiedPromise[id]) {
                                    mk->updatePromise(
                                        version->promises().at(newPromId[id]));
                                } else {
                                    Promise* clone = version->createProm(
                                        mk->prom()->srcPoolIdx());
                                    BB* promCopy = BBTransform::clone(
                                        mk->prom()->entry, clone, version);
                                    clone->entry = promCopy;
                                    newPromId[id] = clone->id;
                                    copiedPromise[id] = true;
                                    mk->updatePromise(clone);
                                }
                            }
                        }
                    });

                    auto inlineeRet = BBTransform::forInline(copy, split);
                    Value* inlineeRes = inlineeRet.first;
                    BB* inlineeReturnblock = inlineeRet.second;
                    if (allowInline == SafeToInline::NeedsContext) {
                        size_t insertPos = 0;
                        Value* op = nullptr;
                        if (auto call = Call::Cast(theCall)) {
                            op = call->cls();
                        } else if (auto call = StaticCall::Cast(theCall)) {
                            auto ld = new LdConst(call->cls()->rirClosure());
                            copy->insert(copy->begin(), ld);
                            op = ld;
                            insertPos++;
                        }
                        assert(op);
                        auto ast = new LdConst(rir::Pool::get(theCall->srcIdx));
                        auto ctx = new PushContext(ast, op, theCall->env());
                        copy->insert(copy->begin() + insertPos, ctx);
                        copy->insert(copy->begin() + insertPos, ast);
                        inlineeReturnblock->append(
                            new PopContext(inlineeRes, ctx));
                    }

                    theCall->replaceUsesWith(inlineeRes);

                    // Remove the call instruction
                    split->remove(split->begin());
                }

                bb = split;
                it = split->begin();

                // Can happen if split only contained the call instruction
                if (it == split->end())
                    break;
            }
        });
    }
};

} // namespace

namespace rir {
namespace pir {

// TODO: maybe implement something more resonable to pass in those constants.
// For now it seems a simple env variable is just fine.
size_t Parameter::INLINER_MAX_SIZE = getenv("PIR_INLINER_MAX_SIZE")
                                         ? atoi(getenv("PIR_INLINER_MAX_SIZE"))
                                         : 4000;
size_t Parameter::INLINER_MAX_INLINEE_SIZE =
    getenv("PIR_INLINER_MAX_INLINEE_SIZE")
        ? atoi(getenv("PIR_INLINER_MAX_INLINEE_SIZE"))
        : 100;
size_t Parameter::INLINER_INITIAL_FUEL =
    getenv("PIR_INLINER_INITIAL_FUEL")
        ? atoi(getenv("PIR_INLINER_INITIAL_FUEL"))
        : 5;

void Inline::apply(RirCompiler&, ClosureVersion* version, LogStream&) const {
    TheInliner s(version);
    s();
}
} // namespace pir
} // namespace rir
