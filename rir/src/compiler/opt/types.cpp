#include "../pir/pir_impl.h"
#include "../util/visitor.h"
#include "R/Funtab.h"
#include "compiler/analysis/cfg.h"

#include "../analysis/abstract_value.h"
#include "../analysis/range.h"

#include "pass_definitions.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

bool TypeInference::apply(Compiler&, ClosureVersion* cls, Code* code,
                          LogStream& log, size_t) const {

    RangeAnalysis rangeAnalysis(cls, code, log);

    std::unordered_map<Instruction*, PirType> types;
    {
        bool done = false;
        auto apply = [&]() {
            Visitor::run(code->entry, [&](Instruction* i) {
                if (!i->type.isRType())
                    return;

                auto getType = [&](Value* v) {
                    if (auto arg = Instruction::Cast(v)) {
                        if (types.count(arg)) {
                            return types.at(arg);
                        } else {
                            done = false;
                        }
                    } else {
                        return v->type;
                    }
                    return PirType::bottom();
                };

                PirType inferred = i->inferType(getType);
                switch (i->tag) {
                case Tag::Extract1_1D: {
                    auto e = Extract1_1D::Cast(i);
                    if (!inferred.isSimpleScalar() &&
                        getType(e->vec()).isA(PirType::num()) &&
                        // named arguments produce named result
                        !getType(e->vec()).maybeHasAttrs() &&
                        getType(e->idx()).isSimpleScalar()) {
                        auto range = rangeAnalysis.before(e).range;
                        if (range.count(e->idx())) {
                            if (range.at(e->idx()) > 0) {
                                // Negative numbers as indices make the
                                // extract return a vector. Only
                                // positive are safe.
                                inferred = inferred.simpleScalar();
                            }
                        }
                    }
                    break;
                }
                default: {
                }
                }

                // inference should never generate less precise type
                inferred = inferred & i->type;

                if (!types.count(i) || types.at(i) != inferred) {
                    done = false;
                    types[i] = inferred;
                }
            });
        };
        while (!done) {
            done = true;
            apply();
        }
    }

    Visitor::run(code->entry, [&](Instruction* i) {
        if (!i->type.isRType())
            return;
        auto t = types.find(i);
        if (t != types.end()) {
            // Inferring void can legitimately happen with unreachable
            // instructions. For example ChkMissing(missingArg) might infer
            // void, since it will always error. However we do not want this to
            // happen as it is guaranteed to cause problems downstream, e.g. in
            // code generation.
            assert(!t->second.isVoid() && "Inference must not reutrn void");
            i->type = t->second;
        }
    });

    return false;
}

} // namespace pir
} // namespace rir
