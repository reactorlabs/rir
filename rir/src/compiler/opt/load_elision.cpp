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
  private:
    ALoad() : origin(NULL), type(), env(NULL), name(NULL) {}

  public:
    static ALoad empty() { return ALoad(); }

    // cppcheck-suppress noExplicitConstructor
    ALoad(LdVar* ld)
        : origin(ld), type(ld->type), env(ld->env()), name(ld->varName) {}
    // cppcheck-suppress noExplicitConstructor
    ALoad(LdFun* ld)
        : origin(ld), type(ld->type), env(ld->env()), name(ld->varName) {}
    Instruction* origin;
    PirType type;
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
                    if (ld->type.isA(ald.type)) {
                        ald.type = ld->type;
                        res.update();
                    }
                    return res;
                }
            }
            state.available.insert(ld);
            res.update();
        } else if (i->changesEnv()) {
            if (auto store = StVar::Cast(i)) {
                for (auto& load : state.available) {
                    if (load.name == store->varName)
                        state.available.erase(state.available.find(load));
                }
            } else {
                state.available.clear();
                res.taint();
            }
        }
        return res;
    }

    ALoad get(Instruction* i) const {
        auto res = StaticAnalysis::at<
            StaticAnalysis::PositioningStyle::BeforeInstruction>(i);
        for (auto dld : res.available) {
            if (dld.same(i))
                return dld;
        }
        return ALoad::empty();
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
                auto domald = loads.get(instr);
                if (auto domld = domald.origin) {
                    domld->type = domald.type;
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
