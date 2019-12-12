#ifndef PIR_RANGE_ANALYSIS
#define PIR_RANGE_ANALYSIS

#include "../analysis/generic_static_analysis.h"
#include "../pir/closure.h"
#include "../pir/closure_version.h"
#include "../pir/pir.h"
#include "abstract_value.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

static int max(int a, int b) {
    if (b > a)
        return b;
    return a;
}

static int min(int a, int b) {
    if (b < a)
        return b;
    return a;
}

static constexpr int MIN = -20;
static constexpr int MAX = 20;

typedef std::pair<int, int> Range;
struct RangeAnalysisState {
    std::unordered_map<Value*, Range> range;
    std::unordered_set<Phi*> seen;

    void print(std::ostream& out, bool tty) const {
        for (auto i : range) {
            i.first->printRef(out);
            out << ": [" << i.second.first << ", " << i.second.second << "]\n";
        }
    }
    AbstractResult mergeExit(const RangeAnalysisState& other) {
        return merge(other);
    }
    AbstractResult merge(const RangeAnalysisState& other) {
        AbstractResult res = AbstractResult::None;
        for (auto o = other.range.begin(); o != other.range.end(); o++) {
            auto m = range.find(o->first);
            if (m == range.end()) {
                range.insert(*o);
                res.update();
            } else {
                auto& mine = m->second;
                auto their = o->second;
                auto mi = min(mine.first, their.first);
                if (mi < MIN)
                    mi = MIN;
                if (mine.first > mi) {
                    mine.first = mi;
                    res.update();
                }
                auto ma = max(mine.second, their.second);
                if (ma > MAX)
                    ma = MAX;
                if (mine.second < ma) {
                    mine.second = ma;
                    res.update();
                }
            }
        }
        if (seen != other.seen) {
            res.update();
            seen.insert(other.seen.begin(), other.seen.end());
        }
        return res;
    }
};

class RangeAnalysis : public StaticAnalysis<RangeAnalysisState, DummyState,
                                            true, AnalysisDebugLevel::None> {
  public:
    RangeAnalysis(ClosureVersion* cls, LogStream& log)
        : StaticAnalysis("Range", cls, cls, log) {}

    AbstractResult apply(RangeAnalysisState& state,
                         Instruction* i) const override {
        AbstractResult res = AbstractResult::None;

        auto branching = [&]() {
            if (i != *i->bb()->begin() || !i->bb()->hasSinglePred())
                return;

            auto pred = *i->bb()->predecessors().begin();
            if (pred->isEmpty())
                return;

            auto br = Branch::Cast(pred->last());
            if (!br)
                return;

            auto t = AsTest::Cast(br->arg(0).val());
            if (!t)
                return;

            Instruction* condition = Instruction::Cast(t->arg(0).val());
            if (!condition)
                return;

            bool holds = i->bb() == pred->trueBranch();
            if (auto n = Not::Cast(condition)) {
                holds = false;
                condition = Instruction::Cast(n->arg(0).val());
            }
            if (!condition)
                return;

            auto applyCond = [&](std::function<Range(Range, Range)> getLhs,
                                 std::function<Range(Range, Range)> getRhs) {
                if (condition->effects.contains(Effect::ExecuteCode))
                    return;
                auto lhs = condition->arg(0).val();
                auto rhs = condition->arg(1).val();
                if (!holds) {
                    auto t = lhs;
                    lhs = rhs;
                    rhs = t;
                }

                bool hasA = state.range.count(lhs);
                bool hasB = state.range.count(rhs);

                if (!hasA && !hasB)
                    return;

                Range& a = state.range[lhs];
                Range& b = state.range[rhs];
                if (!hasA)
                    a = Range(MIN, MAX);
                if (!hasB)
                    b = Range(MIN, MAX);

                auto lhsApp = getLhs(a, b);
                auto rhsApp = getRhs(a, b);
                auto& lhsCur = state.range[lhs];
                auto& rhsCur = state.range[rhs];
                if (lhsApp.first > lhsCur.first) {
                    res.update();
                    lhsCur.first = lhsApp.first;
                }
                if (lhsApp.second < lhsCur.second) {
                    res.update();
                    lhsCur.second = lhsApp.second;
                }
                if (rhsApp.first > rhsCur.first) {
                    res.update();
                    rhsCur.first = rhsApp.first;
                }
                if (rhsApp.second < rhsCur.second) {
                    res.update();
                    rhsCur.second = rhsApp.second;
                }
            };

            switch (condition->tag) {
            case Tag::Lte:
            case Tag::Lt:
                applyCond(
                    [](std::pair<int, int> a, std::pair<int, int> b) {
                        return Range(min(a.first, b.second),
                                     min(a.second, b.second));
                    },
                    [](std::pair<int, int> a, std::pair<int, int> b) {
                        return Range(max(a.first, b.first),
                                     max(a.first, b.second));
                    });
                break;

            case Tag::Gte:
            case Tag::Gt:
                applyCond(
                    [](std::pair<int, int> a, std::pair<int, int> b) {
                        return Range(max(a.first, b.first),
                                     max(a.second, b.first));
                    },
                    [](std::pair<int, int> a, std::pair<int, int> b) {
                        return Range(min(a.second, b.first),
                                     max(a.second, b.second));
                    });
                break;
            default: {}
            }
        };
        branching();

        auto binop = [&](const std::function<int(int, int)> apply) {
            if (i->effects.contains(Effect::ExecuteCode))
                return;
            if (state.range.count(i->arg(0).val()) &&
                state.range.count(i->arg(1).val())) {
                auto a = state.range.at(i->arg(0).val());
                auto b = state.range.at(i->arg(1).val());

                auto up =
                    Range(apply(a.first, b.first), apply(a.second, b.second));

                if (up.first < MIN)
                    up.first = MIN;

                if (up.second > MAX)
                    up.second = MAX;

                auto& cur = state.range[i];
                if (a.first > MIN && b.first > MIN && cur.first != up.first) {
                    cur.first = up.first;
                    res.update();
                }
                if (a.second < MAX && b.second < MAX &&
                    cur.second != up.second) {
                    cur.second = up.second;
                    res.update();
                }
            }
        };

        switch (i->tag) {
        case Tag::LdConst: {
            auto ld = LdConst::Cast(i);
            if (IS_SIMPLE_SCALAR(ld->c(), INTSXP)) {
                auto r = INTEGER(ld->c())[0];
                state.range[i] = {r, r};
            } else if (IS_SIMPLE_SCALAR(ld->c(), REALSXP)) {
                auto r = REAL(ld->c())[0];
                state.range[i] = {floor(r), ceil(r)};
            }
            break;
        }

        case Tag::Add:
            binop([&](int a, int b) { return a + b; });
            break;
        case Tag::Sub:
            binop([&](int a, int b) { return a - b; });
            break;
        case Tag::Mul:
            binop([&](int a, int b) { return a * b; });
            break;
        case Tag::Phi: {
            int mi = MAX;
            int ma = MIN;
            auto p = Phi::Cast(i);
            p->eachArg([&](BB*, Value* v) {
                if (state.range.count(v)) {
                    auto r = state.range.at(v);
                    if (r.first < mi)
                        mi = r.first;
                    if (r.second > ma)
                        ma = r.second;
                } else {
                    if (state.seen.count(p))
                        state.range.emplace(v, Range(MIN, MAX));
                }
            });
            state.range[i] = {mi, ma};
            if (!state.seen.count(p)) {
                res.update();
                state.seen.insert(p);
            }
            break;
        }

        default: {}
        }

        return res;
    }
};

} // namespace pir
} // namespace rir

#endif
