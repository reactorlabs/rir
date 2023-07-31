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
       auto consts = BCODE_CONSTS(sexp);
       auto n = LENGTH(consts);
       for (auto i = 0; i < n; i++) {
           auto c = VECTOR_ELT(consts, i);
           // Adds to collector either way, but bcWorklist may (?) be faster
           // (this weird function structure is what R does with serialization)
           if (TYPEOF(c) == BCODESXP) {
               bcWorklist.push(c);
           } else {
               collector.add(c, false);
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

static void addConnected(SEXP sexp, bool isChild, ConnectedCollector& collector) {
    Measuring::timeEventIf(pir::Parameter::PIR_MEASURE_INTERNING, "getConnected.cpp: addConnected", sexp, [&] {
        auto type = TYPEOF(sexp);
        if (ALTREP(sexp)) {
            auto info = ALTREP_SERIALIZED_CLASS(sexp);
            auto state = ALTREP_SERIALIZED_STATE(sexp);
            auto attrib = ATTRIB(sexp);
            if (info != nullptr && state != nullptr) {
                collector.add(info, false);
                collector.add(state, false);
                collector.add(attrib, false);
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
            collector.add(ATTRIB(sexp), false);
        }

        switch (type) {
        case NILSXP:
        case SYMSXP:
            break;
        case LISTSXP:
        // LANGSXP can contain RIR objects (perhaps in its tag)
        case LANGSXP:
        case PROMSXP:
        case DOTSXP:
            if (hasTag(sexp)) {
                collector.add(TAG(sexp), false);
            }
            if (BNDCELL_TAG(sexp)) {
                assert(false && "TODO R_expand_binding_value isn't public");
            }
            collector.add(CAR(sexp), isChild);
            // ???: use goto tailcall like R for perf boost?
            collector.add(CDR(sexp), isChild);
            break;
        case CLOSXP:
            collector.add(CLOENV(sexp), false);
            collector.add(FORMALS(sexp), isChild);
            // ???: use goto tailcall like R for perf boost?
            collector.add(BODY(sexp), isChild);
            break;
        case EXTPTRSXP:
            collector.add(EXTPTR_PROT(sexp), false);
            collector.add(EXTPTR_TAG(sexp), false);
            break;
        case WEAKREFSXP:
            break;
        case ENVSXP:
            if (!R_IsPackageEnv(sexp) && !R_IsNamespaceEnv(sexp)) {
                collector.add(ENCLOS(sexp), false);
                collector.add(FRAME(sexp), false);
                collector.add(HASHTAB(sexp), false);
                collector.add(ATTRIB(sexp), false);
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
        case EXPRSXP:
        case VECSXP: {
            auto n = XLENGTH(sexp);
            for (int i = 0; i < n; ++i) {
                collector.add(VECTOR_ELT(sexp, i), isChild);
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
    add(Pool::get(idx), false);
}

void ConnectedCollector::addSrc(unsigned idx) {
    add(src_pool_at(idx), false);
}

ConnectedSet getConnected(SEXP root) {
    return Measuring::timeEventIf<ConnectedSet>(pir::Parameter::PIR_MEASURE_INTERNING, "getConnected", root, [&] {
        ConnectedSet set;
        std::queue<ConnectedElem> worklist;
        worklist.push({root, false});
        ConnectedCollector collector{set, worklist};

        while (!worklist.empty()) {
            auto elem = worklist.front();
            worklist.pop();

            addConnected(elem.sexp, elem.isChild, collector);
        }
        return set;
    });
}

} // namespace rir