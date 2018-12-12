#ifndef PIR_ABSTRACT_RESULT_H
#define PIR_ABSTRACT_RESULT_H

namespace rir {
namespace pir {

struct AbstractResult {
    enum Kind { None, Updated, LostPrecision, Tainted };
    Kind kind;
    bool keepSnapshot = false;

    // cppcheck-suppress noExplicitConstructor
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

    const AbstractResult& max(const AbstractResult& other) {
        if (kind < other.kind)
            kind = other.kind;
        return *this;
    }

    void lostPrecision() { max(LostPrecision); }

    void taint() { max(Tainted); }

    void update() { max(Updated); }
};

} // namespace pir
} // namespace rir

#endif
