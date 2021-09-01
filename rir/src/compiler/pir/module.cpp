#include "module.h"

#include "pir_impl.h"
#include "runtime/TypeFeedback.h"
#include "utils/Pool.h"
#include "values.h"

namespace rir {
namespace pir {

void Module::print(std::ostream& out, bool tty) {
    eachPirClosure([&](Closure* c) {
        out << "================================\n";
        c->print(out, tty);
    });
}

Closure* Module::getOrDeclareRirFunction(const std::string& name,
                                         rir::Function* f, SEXP formals,
                                         SEXP src, Context userContext) {
    auto env = Env::notClosed();
    if (!closures.count(Idx(f, env))) {
        closures[Idx(f, env)] = new Closure(name, f, formals, src, userContext);
    }
    return closures.at(Idx(f, env));
}

Closure* Module::getOrDeclareRirClosure(const std::string& name, SEXP closure,
                                        rir::Function* f, Context userContext) {
    // For Identification we use the real env, but for optimization we only use
    // the real environment if this is not an inner function. When it is an
    // inner function, then the env is expected to change over time.
    auto id = Idx(f, getEnv(CLOENV(closure)));
    auto env = f->flags.contains(Function::InnerFunction)
                   ? Env::notClosed()
                   : getEnv(CLOENV(closure));
    if (!closures.count(id))
        closures[id] = new Closure(name, closure, f, env, userContext);
    assert(closures.at(id)->rirClosure() == closure);
    return closures.at(id);
}

void Module::eachPirClosure(PirClosureIterator it) {
    for (auto& c : closures)
        it(c.second);
}

void Module::eachPirClosureVersion(PirClosureVersionIterator it) {
    for (auto& c : closures)
        c.second->eachVersion(it);
}

Env* Module::getEnv(SEXP rho) {
    if (rho == R_NilValue)
        return Env::nil();
    if (rho == R_GlobalEnv)
        return Env::global();

    if (environments.count(rho))
        return environments.at(rho);

    assert(TYPEOF(rho) == ENVSXP);
    Env* parent = getEnv(ENCLOS(rho));
    Env* env = new Env(rho, parent);
    environments[rho] = env;
    return env;
}

Module::~Module() {
    for (auto& e : environments)
        delete e.second;
    for (auto& cs : closures)
        delete cs.second;
    for (auto dr : deoptReasons)
        delete dr;
    for (auto c : constants)
        delete c.second;
}

DeoptReasonWrapper* Module::deoptReasonValue(const DeoptReason& reason) {
    for (auto dr : deoptReasons)
        if (dr->reason == reason)
            return dr;
    return *deoptReasons.emplace(new DeoptReasonWrapper(reason)).first;
}

Value* Module::c(SEXP s) {
    if (IS_SIMPLE_SCALAR(s, LGLSXP)) {
        if (*LOGICAL(s) == NA_LOGICAL)
            return NaLogical::instance();
        return *LOGICAL(s) ? (Value*)True::instance() : False::instance();
    }
    auto idx = Pool::insert(s);
    return c(idx, PirType(s));
}

Const* Module::c(int i) {
    auto idx = Pool::getInt(i);
    auto t = PirType(RType::integer).simpleScalar();
    if (i != NA_INTEGER)
        t = t.notNAOrNaN();
    return c(idx, t);
}

Const* Module::c(double d) {
    auto idx = Pool::getNum(d);
    auto t = PirType(RType::real).simpleScalar();
    if (d == d)
        t = t.notNAOrNaN();
    return c(idx, t);
}

Const* Module::c(BC::PoolIdx idx, PirType t) {
    auto f = constants.find(idx);
    if (f != constants.end())
        return f->second;
    return constants.emplace(idx, new Const(idx, t)).first->second;
}
}
}
