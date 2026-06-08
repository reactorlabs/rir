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

RecordSkipStats::~RecordSkipStats() {
    // Authoritative partition of the record-everything baseline: every record
    // opportunity is one of these four (sums to the baseline total).
    uint64_t recordedTotal = typeAlways + typeOnceRec;
    uint64_t skippedTotal = typeOnceSkip + noRecordSkip;
    uint64_t baselineTotal = recordedTotal + skippedTotal;
    if (baselineTotal == 0)
        return; // trivial invocation, stay quiet

    // Split by site. All skipping happens at ldvar leaves (NoRecord/RecordOnce
    // only classify variable loads); inner-node records are never skipped here.
    uint64_t recordedLdvar = ldvarRec;
    uint64_t recordedInner =
        (recordedTotal >= ldvarRec) ? recordedTotal - ldvarRec : 0;
    uint64_t shouldLdvar = recordedLdvar + skippedTotal;
    uint64_t shouldInner = recordedInner;

    const int W = 15; // value column width (fits ~ billions w/ separators)
    auto col = [&](uint64_t v) { return commafy(v); };

    fprintf(stderr, "\n=== RIR type-record stats ===\n\n");

    // Table 1: overall, by site (vs. record-everything baseline).
    fprintf(stderr, "Overall (vs. record-everything baseline)\n");
    fprintf(stderr, "  %-12s | %*s | %*s | %*s | %6s\n", "site", W, "should", W,
            "recorded", W, "skipped", "skip%");
    fprintf(stderr, "  -------------+-%.*s-+-%.*s-+-%.*s-+-------\n", W,
            "-----------------", W, "-----------------", W,
            "-----------------");
    fprintf(stderr, "  %-12s | %*s | %*s | %*s | %5.1f%%\n", "ldvar leaves", W,
            col(shouldLdvar).c_str(), W, col(recordedLdvar).c_str(), W,
            col(skippedTotal).c_str(), pct(skippedTotal, shouldLdvar));
    fprintf(stderr, "  %-12s | %*s | %*s | %*s | %5.1f%%\n", "inner nodes", W,
            col(shouldInner).c_str(), W, col(recordedInner).c_str(), W,
            col(0).c_str(), 0.0);
    fprintf(stderr, "  %-12s | %*s | %*s | %*s | %5.1f%%\n", "TOTAL", W,
            col(baselineTotal).c_str(), W, col(recordedTotal).c_str(), W,
            col(skippedTotal).c_str(), pct(skippedTotal, baselineTotal));

    // Table 2: the ldvar leaves broken down by their compiler classification.
    // The should column carries its share of all ldvar leaves in parens.
    uint64_t onceShould = typeOnceRec + typeOnceSkip; // RecordOnce executions
    uint64_t alwaysShould =
        (ldvarRec >= typeOnceRec) ? ldvarRec - typeOnceRec : 0; // RecordAlways
    const int Ws = 22; // should column: "value (share%)"
    auto shouldCol = [&](uint64_t v) {
        char buf[16];
        snprintf(buf, sizeof(buf), "%.1f%%", pct(v, shouldLdvar));
        return commafy(v) + " (" + buf + ")";
    };
    // Percentage as a string; if it rounds up to 100.0% but is not exactly
    // complete (value < total), prefix ≈ so a near-100% is not read as a true
    // 100%.
    auto pctStr = [&](uint64_t value, uint64_t total) {
        char buf[16];
        snprintf(buf, sizeof(buf), "%.1f%%", pct(value, total));
        std::string s = buf;
        if (s == "100.0%" && value < total)
            s = "≈" + s; // ≈100.0%
        return s;
    };
    fprintf(stderr, "\nldvar leaves (total): %s\n", col(shouldLdvar).c_str());
    fprintf(stderr, "  %-18s | %*s | %*s | %*s | %6s\n", "class", Ws, "should",
            W, "recorded", W, "skip", "skip%");
    fprintf(stderr, "  -------------------+-%.*s-+-%.*s-+-%.*s-+-------\n", Ws,
            "------------------------", W, "-----------------", W,
            "-----------------");
    fprintf(stderr, "  %-18s | %*s | %*s | %*s | %6s\n", "RecordAlways", Ws,
            shouldCol(alwaysShould).c_str(), W, col(alwaysShould).c_str(), W,
            col(0).c_str(), pctStr(0, alwaysShould).c_str());
    fprintf(stderr, "  %-18s | %*s | %*s | %*s | %6s\n", "RecordOnce", Ws,
            shouldCol(onceShould).c_str(), W, col(typeOnceRec).c_str(), W,
            col(typeOnceSkip).c_str(),
            pctStr(typeOnceSkip, onceShould).c_str());
    fprintf(stderr, "  %-18s | %*s | %*s | %*s | %6s\n", "NoRecord (elided)",
            Ws, shouldCol(noRecordSkip).c_str(), W, col(0).c_str(), W,
            col(noRecordSkip).c_str(),
            pctStr(noRecordSkip, noRecordSkip).c_str());
}

} // namespace rir

#endif // RIR_RECORD_STATS
