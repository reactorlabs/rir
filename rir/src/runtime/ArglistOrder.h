#ifndef ARGLIST_ORDER_H
#define ARGLIST_ORDER_H

#include "RirRuntimeObject.h"

#include <iostream>
#include <vector>

namespace rir {

#pragma pack(push)
#pragma pack(1)

constexpr static size_t ARGLIST_ORDER_MAGIC = 0xaabbccdd;

struct Code;

struct ArglistOrder
    : public RirRuntimeObject<ArglistOrder, ARGLIST_ORDER_MAGIC> {

    using CallId = size_t;
    using ArgIdx = size_t;
    using CallArglistOrder = std::vector<ArgIdx>;

    static constexpr ArgIdx ARG_NAMED_MASK = 1ULL << ((8 * sizeof(ArgIdx)) - 1);
    static constexpr ArgIdx STATIC_DOTS_MASK = 1ULL
                                               << ((8 * sizeof(ArgIdx)) - 2);
    static constexpr CallId NOT_REORDERED = -1;

    static ArgIdx encodeArg(ArgIdx val, bool named,
                            bool staticallyMatchedDots) {
        auto res = val;
        if (named)
            res = res | ARG_NAMED_MASK;
        if (staticallyMatchedDots)
            res = res | STATIC_DOTS_MASK;
        return res;
    }

    static ArgIdx decodeArg(ArgIdx val) {
        return val & ~(ARG_NAMED_MASK | STATIC_DOTS_MASK);
    }

    static bool isArgNamed(ArgIdx val) { return val & ARG_NAMED_MASK; }
    static bool isStaticallyMatchedDots(ArgIdx val) {
        return val & STATIC_DOTS_MASK;
    }

    static size_t size(std::vector<CallArglistOrder> const& reordering) {
        size_t sz = 0;
        for (auto const& r : reordering)
            sz += r.size();
        return sizeof(ArglistOrder) +
               (2 * reordering.size() + sz) * sizeof(*data);
    }

    size_t size() const {
        size_t sz = 0;
        for (size_t i = 0; i < nCalls; i++) {
            sz += originalArglistLength(i);
        }
        return sizeof(ArglistOrder) + (2 * nCalls + sz) * sizeof(*data);
    }

    static ArglistOrder* New(std::vector<CallArglistOrder> const& reordering) {
        SEXP cont = Rf_allocVector(EXTERNALSXP, size(reordering));
        ArglistOrder* res = new (DATAPTR(cont)) ArglistOrder(reordering);
        return res;
    }

    explicit ArglistOrder(std::vector<CallArglistOrder> const& reordering)
        : ArglistOrder(reordering.size()) {
        auto offset = nCalls * 2;
        for (size_t i = 0; i < nCalls; i++) {
            data[2 * i] = offset;
            auto n = reordering[i].size();
            data[2 * i + 1] = n;
            memcpy(data + offset, reordering[i].data(), n * sizeof(*data));
            offset += n;
        }
    }

    ArgIdx index(CallId callId, size_t i) const {
        assert(callId < nCalls);
        assert(i < originalArglistLength(callId));
        auto offset = data[callId * 2];
        return data[offset + i];
    }

    ArgIdx originalArglistLength(CallId callId) const {
        assert(callId < nCalls);
        return data[callId * 2 + 1];
    }

    static ArglistOrder* deserialize(__attribute__((unused)) SEXP refTable, R_inpstream_t inp);
    void serialize(__attribute__((unused)) SEXP refTable, R_outpstream_t out) const;

    /*
     * Layout of data[] is nCalls * (offset, length), followed by
     * nCalls * (variable length list of indices)
     */
    size_t nCalls;
    ArgIdx data[];

  private:
    explicit ArglistOrder(size_t nCalls)
        : RirRuntimeObject(0, 0), nCalls(nCalls) {}
};

#pragma pack(pop)

} // namespace rir

#endif
