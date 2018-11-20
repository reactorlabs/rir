#ifndef PIR_ABSTRACT_RESULT_H
#define PIR_ABSTRACT_RESULT_H

namespace rir {
namespace pir {

struct AbstractResult {
    enum Kind { None, Updated, LostPrecision, Tainted };
    Kind kind;
    AbstractResult(Kind kind) : kind(kind) {}
    AbstractResult() : kind(None) {}

    bool operator>(const AbstractResult& other) const {
        return kind > other.kind;
    }
    bool operator>=(const AbstractResult& other) const {
        return kind >= other.kind;
    }
    bool operator==(const AbstractResult& other) const {
        return kind == other.kind;
    }

    void max(const AbstractResult& other) {
        if (kind < other.kind)
            kind = other.kind;
    }

    void lostPrecision() { max(LostPrecision); }

    void taint() { max(Tainted); }

    void update() { max(Updated); }
};

} // namespace pir
} // namespace rir

#endif
