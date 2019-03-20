#ifndef PIR_VISIBILITY_H
#define PIR_VISIBILITY_H

#include "abstract_value.h"
#include "generic_static_analysis.h"

namespace rir {
namespace pir {

class LastVisibilityUpdate {
  public:
    std::unordered_set<Instruction*> observable;
    Instruction* last;

    AbstractResult mergeExit(const LastVisibilityUpdate& other) {
        return merge(other);
    }

    AbstractResult merge(const LastVisibilityUpdate& other) {
        AbstractResult res;
        for (auto& v : other.observable) {
            if (!observable.count(v)) {
                observable.insert(v);
                res.update();
            }
        }
        if (last != other.last) {
            if (last && !observable.count(last)) {
                observable.insert(last);
                res.update();
            }
            if (other.last && !observable.count(other.last)) {
                observable.insert(other.last);
                res.update();
            }
        }
        return res;
    }

    void print(std::ostream& out, bool tty) const {
        if (last) {
            out << "Last Update : ";
            last->printRef(std::cout);
            out << "\n";
        }
        out << "Observable: ";
        for (auto& o : observable) {
            o->printRef(out);
            out << " ";
        }
        out << "\n";
    };
};

class VisibilityAnalysis : public StaticAnalysis<LastVisibilityUpdate> {
  public:
    VisibilityAnalysis(ClosureVersion* cls, LogStream& log)
        : StaticAnalysis("VisibilityAnalysis", cls, cls, log) {}

    AbstractResult apply(LastVisibilityUpdate& vis,
                         Instruction* i) const override final;

    bool observed(Instruction* i) { return result().observable.count(i); }
};

} // namespace pir
} // namespace rir

#endif
