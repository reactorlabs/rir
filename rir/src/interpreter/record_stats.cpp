#include "record_stats.h"

#ifdef RIR_RECORD_STATS

#include <cstdio>
#include <string>

namespace rir {

RecordSkipStats g_recStats;

// Format an integer with thousands separators, e.g. 1234567 -> "1,234,567".
static std::string commafy(uint64_t n) {
    std::string digits = std::to_string(n);
    std::string out;
    size_t len = digits.size();
    for (size_t i = 0; i < len; ++i) {
        if (i != 0 && (len - i) % 3 == 0)
            out.push_back(',');
        out.push_back(digits[i]);
    }
    return out;
}

// value / total as a percentage (0 if total is 0).
static double pct(uint64_t value, uint64_t total) {
    return total ? 100.0 * (double)value / (double)total : 0.0;
}

// Percentage as a string; if it rounds up to 100.0% but is not exactly
// complete (value < total), prefix ≈ so a near-100% is not read as a true 100%.
static std::string pctStr(uint64_t value, uint64_t total) {
    char buf[16];
    snprintf(buf, sizeof(buf), "%.1f%%", pct(value, total));
    std::string s = buf;
    if (s == "100.0%" && value < total)
        s = "≈" + s;
    return s;
}

RecordSkipStats::~RecordSkipStats() {
    // ldvar leaves
    uint64_t leafRec = leafAlwaysRec + leafOnceRec;
    uint64_t leafSkip = leafOnceSkip + noRecordSkip;
    uint64_t leafShould = leafRec + leafSkip;
    // inner nodes
    uint64_t innerRec = rootInnerRec + innerNodeRec;
    uint64_t innerSkip = rootInnerSkip + innerNodeSkip;
    uint64_t innerShould = innerRec + innerSkip;
    // untracked records (record_type_): always fire, never skipped
    uint64_t untrackedShould = untrackedRec;
    // totals
    uint64_t recordedTotal = leafRec + innerRec + untrackedRec;
    uint64_t skippedTotal = leafSkip + innerSkip;
    uint64_t baselineTotal = recordedTotal + skippedTotal;
    if (baselineTotal == 0)
        return; // trivial invocation, stay quiet

    const int W = 15; // value column width (fits ~ billions w/ separators)
    auto col = [&](uint64_t v) { return commafy(v); };
    const char* dash = "------------------------";

    fprintf(stderr, "\n=== RIR type-record stats ===\n\n");

    // Table 1: overall, by site (vs. record-everything baseline).
    fprintf(stderr, "Overall (vs. record-everything baseline)\n");
    fprintf(stderr, "  %-21s | %*s | %*s | %*s | %6s\n", "site", W, "should", W,
            "recorded", W, "skipped", "skip%");
    fprintf(stderr, "  ----------------------+-%.*s-+-%.*s-+-%.*s-+-------\n",
            W, dash, W, dash, W, dash);
    fprintf(stderr, "  %-21s | %*s | %*s | %*s | %6s\n", "ldvar leaves", W,
            col(leafShould).c_str(), W, col(leafRec).c_str(), W,
            col(leafSkip).c_str(), pctStr(leafSkip, leafShould).c_str());
    fprintf(stderr, "  %-21s | %*s | %*s | %*s | %6s\n", "inner nodes", W,
            col(innerShould).c_str(), W, col(innerRec).c_str(), W,
            col(innerSkip).c_str(), pctStr(innerSkip, innerShould).c_str());
    fprintf(stderr, "  %-21s | %*s | %*s | %*s | %6s\n",
            "untracked/unoptimized", W, col(untrackedShould).c_str(), W,
            col(untrackedRec).c_str(), W, col(0).c_str(),
            pctStr(0, untrackedShould).c_str());
    fprintf(stderr, "  %-21s | %*s | %*s | %*s | %6s\n", "TOTAL", W,
            col(baselineTotal).c_str(), W, col(recordedTotal).c_str(), W,
            col(skippedTotal).c_str(),
            pctStr(skippedTotal, baselineTotal).c_str());

    const int Ws = 22; // should column: "value (share%)"

    // Table 2: ldvar leaves by compiler classification. The should column
    // carries each class's share of all ldvar leaves in parens.
    auto leafShouldCol = [&](uint64_t v) {
        return commafy(v) + " (" + pctStr(v, leafShould) + ")";
    };
    uint64_t onceShould = leafOnceRec + leafOnceSkip;
    fprintf(stderr, "\nldvar leaves (total): %s\n", col(leafShould).c_str());
    fprintf(stderr, "  %-18s | %*s | %*s | %*s | %6s\n", "class", Ws, "should",
            W, "recorded", W, "skip", "skip%");
    fprintf(stderr, "  -------------------+-%.*s-+-%.*s-+-%.*s-+-------\n", Ws,
            dash, W, dash, W, dash);
    fprintf(stderr, "  %-18s | %*s | %*s | %*s | %6s\n", "RecordAlways", Ws,
            leafShouldCol(leafAlwaysRec).c_str(), W, col(leafAlwaysRec).c_str(),
            W, col(0).c_str(), pctStr(0, leafAlwaysRec).c_str());
    fprintf(stderr, "  %-18s | %*s | %*s | %*s | %6s\n", "RecordOnce", Ws,
            leafShouldCol(onceShould).c_str(), W, col(leafOnceRec).c_str(), W,
            col(leafOnceSkip).c_str(),
            pctStr(leafOnceSkip, onceShould).c_str());
    fprintf(stderr, "  %-18s | %*s | %*s | %*s | %6s\n", "NoRecord (elided)",
            Ws, leafShouldCol(noRecordSkip).c_str(), W, col(0).c_str(), W,
            col(noRecordSkip).c_str(),
            pctStr(noRecordSkip, noRecordSkip).c_str());

    // Table 3: inner nodes by opcode. "skip" = suppressed via shouldNotRecord
    // (the expression-tree elision). Share is of all inner-node executions.
    auto innerShouldCol = [&](uint64_t v) {
        return commafy(v) + " (" + pctStr(v, innerShould) + ")";
    };
    uint64_t rootInnerShould = rootInnerRec + rootInnerSkip;
    uint64_t innerNodeShould = innerNodeRec + innerNodeSkip;
    fprintf(stderr, "\ninner nodes (total): %s\n", col(innerShould).c_str());
    fprintf(stderr, "  %-18s | %*s | %*s | %*s | %6s\n", "class", Ws, "should",
            W, "recorded", W, "suppressed", "skip%");
    fprintf(stderr, "  -------------------+-%.*s-+-%.*s-+-%.*s-+-------\n", Ws,
            dash, W, dash, W, dash);
    fprintf(stderr, "  %-18s | %*s | %*s | %*s | %6s\n", "root_inner", Ws,
            innerShouldCol(rootInnerShould).c_str(), W,
            col(rootInnerRec).c_str(), W, col(rootInnerSkip).c_str(),
            pctStr(rootInnerSkip, rootInnerShould).c_str());
    fprintf(stderr, "  %-18s | %*s | %*s | %*s | %6s\n", "inner_node", Ws,
            innerShouldCol(innerNodeShould).c_str(), W,
            col(innerNodeRec).c_str(), W, col(innerNodeSkip).c_str(),
            pctStr(innerNodeSkip, innerNodeShould).c_str());
}

} // namespace rir

#endif // RIR_RECORD_STATS
