#include "../analysis/available_checkpoints.h"
#include "../pir/pir_impl.h"
#include "../translations/pir_translator.h"
#include "../util/cfg.h"
#include "../util/visitor.h"

#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

struct ALoad {
    // cppcheck-suppress noExplicitConstructor
    ALoad(LdVar* ld) : origin(ld), env(ld->env()), name(ld->varName) {}
    // cppcheck-suppress noExplicitConstructor
    ALoad(LdFun* ld) : origin(ld), env(ld->env()), name(ld->varName) {}
    Instruction* origin;
    Value* env;
    SEXP name;
    bool operator==(const ALoad& other) const {
        return origin == other.origin && env == other.env && name == other.name;
    }
    bool same(Instruction* i) const {
        if (auto ld = LdVar::Cast(i))
            return LdVar::Cast(origin) && ld->env() == env &&
                   ld->varName == name;
        else if (auto ld = LdFun::Cast(i))
            return LdFun::Cast(origin) && ld->env() == env &&
                   ld->varName == name;
        else
            return false;
    }
    void print(std::ostream& out, bool) {
        out << CHAR(PRINTNAME(name)) << "@";
        env->printRef(out);
    }
};

struct AvailableLoads : public StaticAnalysis<IntersectionSet<ALoad>> {
    AvailableLoads(ClosureVersion* cls, LogStream& log)
        : StaticAnalysis("AvailableLoads", cls, cls, log) {}
    AbstractResult apply(IntersectionSet<ALoad>& state, Instruction* i) const {
        AbstractResult res;
        if (auto ld = LdVar::Cast(i)) {
            for (auto& ald : state.available) {
                if (ald.same(ld)) {
                    if (ld->type.isA(ald.origin->type)) {
                        ald.origin->type = ld->type;
                        res.update();
                    }
                    return res;
                }
            }
            state.available.insert(ld);
            res.update();
        } else if (i->changesEnv()) {
            state.available.clear();
            res.taint();
        }
        return res;
    }

    Instruction* get(Instruction* i) const {
        auto res = StaticAnalysis::at<
            StaticAnalysis::PositioningStyle::BeforeInstruction>(i);
        for (auto dld : res.available) {
            if (dld.same(i))
                return dld.origin;
        }
        return nullptr;
    }
};

void LoadElision::apply(RirCompiler&, ClosureVersion* function,
                        LogStream& log) const {
    AvailableLoads loads(function, log);

    Visitor::runPostChange(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;
            auto instr = *ip;

            if (LdVar::Cast(instr) || LdFun::Cast(instr)) {
                if (auto domld = loads.get(instr)) {
                    instr->replaceUsesWith(domld);
                    next = bb->remove(ip);
                }
            }

            ip = next;
        }
    });
}

} // namespace pir
} // namespace rir
