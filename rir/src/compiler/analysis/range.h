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

class Range {
  private:
    Range(int a, int b) : begin_(a), end_(b) {}

    int begin_;
    int end_;

  public:
    int begin() const { return begin_; }
    int end() const { return end_; }

    bool operator!=(const Range& other) const {
        return begin_ != other.begin_ || end_ != other.end_;
    }
    bool operator==(const Range& other) const {
        return begin_ == other.begin_ && end_ == other.end_;
    }

    bool operator>(int other) const { return begin_ > other; }

    static Range MAX;
    static Range NEG;
    static Range POS;
    static Range ABOVE0;
    static Range ZERO;
    static Range ONE;

    static Range get(double a, double b) {
        int ia, ib;
        if (a <= (double)INT_MIN)
            ia = INT_MIN;
        else
            ia = floor(a);
        if (b >= (double)INT_MAX)
            ib = INT_MAX;
        else
            ib = ceil(b);
        return get(ia, ib);
    }

    static Range get(int a, int b) {
        for (auto r : {ZERO, ONE, NEG, ABOVE0, POS})
            if (a >= r.begin_ && b <= r.end_)
                return r;
        return MAX;
    }

    Range merge(const Range& other) const {
        if (*this == MAX)
            return MAX;
        if (other.begin_ <= begin_ && other.end_ >= end_)
            return other;
        if (begin_ <= other.begin_ && end_ >= other.end_)
            return *this;
        for (auto r : {NEG, ABOVE0, POS}) {
            if (begin_ >= r.begin_ && other.begin_ >= r.begin_ &&
                end_ <= r.end_ && other.end_ <= r.end_)
                return r;
        }
        return MAX;
    }
};

struct RangeAnalysisState {
    std::unordered_map<Value*, Range> range;
    std::unordered_set<Phi*> seen;

    void print(std::ostream& out, bool tty) const {
        for (auto i : range) {
            i.first->printRef(out);
            out << ": [" << i.second.begin() << ", " << i.second.end() << "]\n";
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
                auto mi = mine.merge(their);
                if (mine != mi) {
                    mine = mi;
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
    RangeAnalysis(ClosureVersion* cls, Code* code, LogStream& log)
        : StaticAnalysis("Range", cls, code, log) {}

    AbstractResult applyEntry(RangeAnalysisState& state,
                              BB* bb) const override {
        AbstractResult res = AbstractResult::None;

        if (!bb->hasSinglePred())
            return res;

        auto pred = *bb->predecessors().begin();
        if (pred->isEmpty())
            return res;

        auto br = Branch::Cast(pred->last());
        if (!br)
            return res;

        auto t = CheckTrueFalse::Cast(br->arg(0).val());
        if (!t)
            return res;

        bool holds = bb == pred->trueBranch();
        Instruction* condition = Instruction::Cast(t->arg(0).val());
        if (!condition)
            return res;

        if (auto n = Not::Cast(condition)) {
            holds = !holds;
            condition = Instruction::Cast(n->arg(0).val());
            }
            if (!condition)
                return res;

            auto applyCond =
                [&](std::function<Range(int, int, int, int)> getLhs,
                    std::function<Range(int, int, int, int)> getRhs) {
                    if (condition->effects.contains(Effect::ExecuteCode))
                        return;
                    auto lhs = condition->arg(0).val();
                    auto rhs = condition->arg(1).val();
                    if (!holds) {
                        auto t = lhs;
                        lhs = rhs;
                        rhs = t;
                    }

                    if (!state.range.count(lhs))
                        state.range.emplace(lhs, Range::MAX);
                    if (!state.range.count(rhs))
                        state.range.emplace(rhs, Range::MAX);

                    auto& lhsCur = state.range.at(lhs);
                    auto& rhsCur = state.range.at(rhs);

                    auto i1 = lhsCur.begin();
                    auto i2 = lhsCur.end();
                    auto i3 = rhsCur.begin();
                    auto i4 = rhsCur.end();
                    auto lhsNew = getLhs(i1, i2, i3, i4);
                    auto rhsNew = getRhs(i1, i2, i3, i4);
                    if (lhsCur != lhsNew) {
                        lhsCur = lhsNew;
                        res.update();
                    }
                    if (rhsCur != rhsNew) {
                        res.update();
                        rhsCur = rhsNew;
                    }
                };

            switch (condition->tag) {
            case Tag::Lte:
            case Tag::Lt:
                // [a,b] < [c,d] ==> [min(a,d), min(b,d)] [max(a,c), max(a, d)]
                applyCond(
                    [&](int a, int b, int c, int d) {
                        auto bound = d;
                        // The lower bound a in [a,b] represents all doubles in
                        // the interval [a, a+1), therefore in most cases we
                        // cannot be more precise for Lt than for Lte. But if
                        // the interval is the singleton [n,n], then we know
                        // that the lower bound is exactly n.
                        // same bellow for the other cases.
                        if (condition->tag == Tag::Lt && c == d)
                            bound--;
                        return Range::get(min(a, bound), min(b, bound));
                    },
                    [&](int a, int b, int c, int d) {
                        auto bound = a;
                        if (condition->tag == Tag::Lte && a == b)
                            bound++;
                        return Range::get(max(bound, c), max(bound, d));
                    });
                break;

            case Tag::Gte:
            case Tag::Gt:
                // [a,b] > [c,d] ==> [max(a,c), max(b,c)] [min(b,c), min(b, d)]
                applyCond(
                    [&](int a, int b, int c, int d) {
                        auto bound = c;
                        if (condition->tag == Tag::Gt && c == d)
                            bound++;
                        return Range::get(max(a, bound), max(b, bound));
                    },
                    [&](int a, int b, int c, int d) {
                        auto bound = b;
                        if (condition->tag == Tag::Gte && c == d)
                            bound--;
                        return Range::get(min(bound, c), min(bound, d));
                    });
                break;
            default: {}
            }

            return res;
    }

    AbstractResult apply(RangeAnalysisState& state,
                         Instruction* i) const override {
        AbstractResult res = AbstractResult::None;
        auto binop = [&](const std::function<int(int, int)> apply) {
            if (i->effects.contains(Effect::ExecuteCode))
                return;
            if (state.range.count(i->arg(0).val()) &&
                state.range.count(i->arg(1).val())) {
                auto a = state.range.at(i->arg(0).val());
                auto b = state.range.at(i->arg(1).val());

                auto lower = a.begin() == INT_MIN || b.begin() == INT_MIN
                                 ? INT_MIN
                                 : apply(a.begin(), b.begin());
                auto upper = a.end() == INT_MAX || b.begin() == INT_MAX
                                 ? INT_MAX
                                 : apply(a.end(), b.end());
                auto up = Range::get(lower, upper);

                if (state.range.count(i)) {
                    auto& cur = state.range.at(i);
                    if (cur != up) {
                        cur = up;
                        res.update();
                    }
                } else {
                    state.range.emplace(i, up);
                    res.update();
                }
            }
        };

        switch (i->tag) {
        case Tag::LdConst: {
            auto ld = LdConst::Cast(i);
            if (!state.range.count(i)) {
                if (IS_SIMPLE_SCALAR(ld->c(), INTSXP)) {
                    auto r = INTEGER(ld->c())[0];
                    state.range.emplace(i, Range::get(r, r));
                    res.update();
                } else if (IS_SIMPLE_SCALAR(ld->c(), REALSXP)) {
                    auto r = REAL(ld->c())[0];
                    state.range.emplace(i, Range::get(r, r));
                    res.update();
                }
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
            auto p = Phi::Cast(i);
            auto m = Range::MAX;
            bool first = true;

            p->eachArg([&](BB*, Value* v) {
                if (state.range.count(v)) {
                    auto r = state.range.at(v);
                    if (first)
                        m = r;
                    else
                        m = m.merge(r);
                } else {
                    if (state.seen.count(p)) {
                        state.range.emplace(v, Range::MAX);
                        m = Range::MAX;
                    }
                }
                first = false;
            });
            if (!state.seen.count(p)) {
                res.update();
                state.range.emplace(p, m);
                state.seen.insert(p);
            } else if (state.range.at(p) != m) {
                state.range.at(p) = m;
            }
            break;
        }

        default: {}
        }

        return res;
    }

  private:
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
};

} // namespace pir
} // namespace rir

#endif
