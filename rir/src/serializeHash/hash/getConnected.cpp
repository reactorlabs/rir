//
// Created by Jakob Hain on 7/23/23.
//

#include "getConnected.h"
#include "R/r.h"
#include "compiler/parameter.h"
#include "hashRoot_getConnected_common.h"
#include "runtime/Code.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "runtime/LazyArglist.h"
#include "runtime/LazyEnvironment.h"
#include "utils/Pool.h"
#include "utils/measuring.h"

namespace rir {

static std::unordered_set<SEXP> globalsSet = []{
    std::unordered_set<SEXP> set;
    for (auto g : globals) {
        set.insert(g);
    }
    return set;
}();

// Will hash sexp if it's an instance of CLS
template <typename CLS>
static inline bool tryAddConnected(SEXP sexp, ConnectedCollector& collector) {
    if (CLS* b = CLS::check(sexp)) {
        b->addConnected(collector);
        return true;
    } else {
        return false;
    }
}

static inline void addConnectedRir(SEXP sexp, ConnectedCollector& collector) {
    if (!tryAddConnected<DispatchTable>(sexp, collector) &&
        !tryAddConnected<Function>(sexp, collector) &&
        !tryAddConnected<Code>(sexp, collector) &&
        !tryAddConnected<ArglistOrder>(sexp, collector) &&
        !tryAddConnected<LazyArglist>(sexp, collector) &&
        !tryAddConnected<LazyEnvironment>(sexp, collector) &&
        !tryAddConnected<PirTypeFeedback>(sexp, collector)) {
        std::cerr << "couldn't add connected in EXTERNALSXP: ";
        Rf_PrintValue(sexp);
        assert(false);
    }
}

static void addConnectedBc1(SEXP sexp, ConnectedCollector& collector,
                            std::queue<SEXP>& bcWorklist) {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_INTERNING, "getConnected.cpp: addConnectedBc1", sexp, [&] {
       SEXP code = R_bcDecode(BCODE_CODE(sexp));
       collector.add(code);
       auto consts = BCODE_CONSTS(sexp);
       auto n = LENGTH(consts);
       for (auto i = 0; i < n; i++) {
           auto c = VECTOR_ELT(consts, i);
           // Adds to collector either way, but bcWorklist may (?) be faster
           // (this weird function structure is what R does with serialization)
           if (TYPEOF(c) == BCODESXP) {
               bcWorklist.push(c);
           } else {
               collector.add(c);
           }
       }
   });
}

static void addConnectedBc(SEXP sexp, ConnectedCollector& collector) {
    std::queue<SEXP> bcWorklist;
    bcWorklist.push(sexp);
    while (!bcWorklist.empty()) {
        sexp = bcWorklist.front();
        bcWorklist.pop();

        addConnectedBc1(sexp, collector, bcWorklist);
    }
}

static void addConnected(SEXP sexp, ConnectedCollector& collector) {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_INTERNING, "getConnected.cpp: addConnected", sexp, [&] {
        auto type = TYPEOF(sexp);
        if (ALTREP(sexp)) {
            auto info = ALTREP_SERIALIZED_CLASS(sexp);
            auto state = ALTREP_SERIALIZED_STATE(sexp);
            auto attrib = ATTRIB(sexp);
            if (info != nullptr && state != nullptr) {
                collector.add(info);
                collector.add(state);
                collector.add(attrib);
                return;
            }
            /* else fall through to standard processing */
        } else if (globalsSet.count(sexp)) {
            return;
        }

        // With the CHARSXP cache chains maintained through the ATTRIB
        // field the content of that field must not be serialized, so
        // we treat it as not there.
        auto hasAttr = (type != CHARSXP && ATTRIB(sexp) != R_NilValue);
        if (hasAttr) {
            collector.add(ATTRIB(sexp));
        }

        switch (type) {
        case NILSXP:
        case SYMSXP:
            break;
        case LISTSXP:
        case LANGSXP:
        case PROMSXP:
        case DOTSXP:
            if (hasTag(sexp)) {
                collector.add(TAG(sexp));
            }
            if (BNDCELL_TAG(sexp)) {
                assert(false && "TODO R_expand_binding_value isn't public");
            }
            collector.add(CAR(sexp));
            // ???: use goto tailcall like R for perf boost?
            collector.add(CDR(sexp));
            break;
        case CLOSXP:
            collector.add(CLOENV(sexp));
            collector.add(FORMALS(sexp));
            // ???: use goto tailcall like R for perf boost?
            collector.add(BODY(sexp));
            break;
        case EXTPTRSXP:
            collector.add(EXTPTR_PROT(sexp));
            collector.add(EXTPTR_TAG(sexp));
            break;
        case WEAKREFSXP:
            break;
        case ENVSXP:
            if (!R_IsPackageEnv(sexp) && !R_IsNamespaceEnv(sexp)) {
                collector.add(ENCLOS(sexp));
                collector.add(FRAME(sexp));
                collector.add(HASHTAB(sexp));
                collector.add(ATTRIB(sexp));
            }
            break;
        case SPECIALSXP:
        case BUILTINSXP:
        case CHARSXP:
        case LGLSXP:
        case INTSXP:
        case REALSXP:
        case CPLXSXP:
        case RAWSXP:
        case STRSXP:
            break;
        case VECSXP:
        case EXPRSXP: {
            auto n = XLENGTH(sexp);
            for (int i = 0; i < n; ++i) {
                collector.add(VECTOR_ELT(sexp, i));
            }
            break;
        }
        case S4SXP:
            break;
        case BCODESXP: {
            addConnectedBc(sexp, collector);
            break;
        }
        case EXTERNALSXP:
            addConnectedRir(sexp, collector);
            break;
        default:
            Rf_error("hashChild: unknown type %i", type);
        }
    });
}

void ConnectedCollector::addConstant(unsigned idx) {
    add(Pool::get(idx));
}

void ConnectedCollector::addSrc(unsigned idx) {
    add(src_pool_at(idx));
}

ConnectedSet getConnected(SEXP root) {
    return Measuring::timeEventIf<ConnectedSet>(pir::Parameter::PIR_MEASURE_INTERNING, "getConnected", root, [&] {
        ConnectedSet set;
        std::queue<SEXP> worklist;
        worklist.push(root);
        ConnectedCollector collector{set, worklist};

        while (!worklist.empty()) {
            auto sexp = worklist.front();
            worklist.pop();

            addConnected(sexp, collector);
        }
        return set;
    });
}

} // namespace rir