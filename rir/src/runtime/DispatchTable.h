#ifndef RIR_DISPATCH_TABLE_H
#define RIR_DISPATCH_TABLE_H

#include "Function.h"
#include "R/Serialize.h"
#include "RirRuntimeObject.h"
#include "utils/random.h"
#include "utils/serializerData.h"
namespace rir {
#define PRINT_LINKING_STATUS 1
#define PRINT_LINKING_STATUS_OVERRIDE 1

#define DISPATCH_TABLE_MAGIC (unsigned)0xd7ab1e00

typedef SEXP DispatchTableEntry;

/*
 * A dispatch table (vtable) for functions.
 *
 */
#pragma pack(push)
#pragma pack(1)
struct DispatchTable
    : public RirRuntimeObject<DispatchTable, DISPATCH_TABLE_MAGIC> {

    size_t size() const { return size_; }

    Function* get(size_t i) const {
        assert(i < capacity());
        return Function::unpack(getEntry(i));
    }

    Function* best() const {
        if (size() > 1)
            return get(1);
        return get(0);
    }
    Function* baseline() const {
        auto f = Function::unpack(getEntry(0));
        assert(f->signature().envCreation ==
               FunctionSignature::Environment::CallerProvided);
        return f;
    }

    Function* dispatch(Context a) const {
        if (!a.smaller(userDefinedContext_)) {
#ifdef DEBUG_DISPATCH
            std::cout << "DISPATCH trying: " << a
                      << " vs annotation: " << userDefinedContext_ << "\n";
#endif
            Rf_error("Provided context does not satisfy user defined context");
        }

        for (size_t i = 1; i < size(); ++i) {
#ifdef DEBUG_DISPATCH
            std::cout << "DISPATCH trying: " << a << " vs " << get(i)->context()
                      << "\n";
#endif
            auto e = get(i);
            if (a.smaller(e->context()) && !e->disabled())
                return e;
        }
        return baseline();
    }

    void baseline(Function* f) {
        assert(f->signature().optimization ==
               FunctionSignature::OptimizationLevel::Baseline);
        if (size() == 0)
            size_++;
        else
            assert(baseline()->signature().optimization ==
                   FunctionSignature::OptimizationLevel::Baseline);
        setEntry(0, f->container());
    }

    bool contains(const Context& assumptions) const {
        for (size_t i = 0; i < size(); ++i)
            if (get(i)->context() == assumptions)
                return !get(i)->disabled();
        return false;
    }

    void remove(Code* funCode) {
        size_t i = 1;
        for (; i < size(); ++i) {
            if (get(i)->body() == funCode)
                break;
        }
        if (i == size())
            return;
        for (; i < size() - 1; ++i) {
            setEntry(i, getEntry(i + 1));
        }
        setEntry(i, nullptr);
        size_--;
    }

    // insert function ordered by increasing number of assumptions
    void insert(Function* fun) {
        // TODO: we might need to grow the DT here!
        assert(size() > 0);
        assert(fun->signature().optimization !=
               FunctionSignature::OptimizationLevel::Baseline);
        auto assumptions = fun->context();
        size_t i;
        for (i = size() - 1; i > 0; --i) {
            auto old = get(i);
            if (old->context() == assumptions) {
                if (i != 0) {
                    // Remember deopt counts across recompilation to avoid
                    // deopt loops
                    fun->addDeoptCount(old->deoptCount());
                    setEntry(i, fun->container());
                    assert(get(i) == fun);
                }
                return;
            }
            if (!(assumptions < get(i)->context())) {
                break;
            }
        }
        i++;
        assert(!contains(fun->context()));
        if (size() == capacity()) {
#ifdef DEBUG_DISPATCH
            std::cout << "Tried to insert into a full Dispatch table. Have: \n";
            for (size_t i = 0; i < size(); ++i) {
                auto e = getEntry(i);
                std::cout << "* " << Function::unpack(e)->context() << "\n";
            }
            std::cout << "\n";
            std::cout << "Tried to insert: " << assumptions << "\n";
            Rf_error("dispatch table overflow");
#endif
            // Evict one element and retry
            auto pos = 1 + (Random::singleton()() % (size() - 1));
            size_--;
            while (pos < size()) {
                setEntry(pos, getEntry(pos + 1));
                pos++;
            }
            return insert(fun);
        }

        for (size_t j = size(); j > i; --j)
            setEntry(j, getEntry(j - 1));
        size_++;
        setEntry(i, fun->container());

#ifdef DEBUG_DISPATCH
        std::cout << "Added version to DT, new order is: \n";
        for (size_t i = 0; i < size(); ++i) {
            auto e = getEntry(i);
            std::cout << "* " << Function::unpack(e)->context() << "\n";
        }
        std::cout << "\n";
        for (size_t i = 0; i < size() - 1; ++i) {
            assert(get(i)->context() < get(i + 1)->context());
            assert(get(i)->context() != get(i + 1)->context());
            assert(!(get(i + 1)->context() < get(i)->context()));
        }
        assert(contains(fun->context()));
#endif
        SEXP optMap = Pool::get(OPT_UNLOCK_MAP);
        if (unlockSym && unlockSym != R_NilValue && Rf_findVarInFrame(optMap, unlockSym) != R_UnboundValue && Rf_findVarInFrame(optMap, unlockSym) != R_NilValue) {
            SEXP unlockVec = Rf_findVarInFrame(optMap, unlockSym);
            bool allDone = true;
            for (int i = 0; i < Rf_length(unlockVec); i++) {
                SEXP curr = VECTOR_ELT(unlockVec, i);
                if (curr != R_NilValue) {
                    if (tryResolvingOptimisticLinks( VECTOR_ELT(curr, 0), VECTOR_ELT(curr, 1) )) {
                        SET_VECTOR_ELT(unlockVec, i, R_NilValue);
                    } else {
                        allDone = false;
                    }
                }
            }

            if (allDone) {
                Rf_defineVar(unlockSym, R_NilValue, optMap);
                unlockSym = nullptr;
            }
        }
    }

    static DispatchTable* create(size_t capacity = 20) {
        size_t sz =
            sizeof(DispatchTable) + (capacity * sizeof(DispatchTableEntry));
        SEXP s = Rf_allocVector(EXTERNALSXP, sz);
        return new (INTEGER(s)) DispatchTable(capacity);
    }

    size_t capacity() const { return info.gc_area_length; }

    static DispatchTable* deserialize(SEXP refTable, R_inpstream_t inp) {
        DispatchTable* table = create();
        PROTECT(table->container());
        AddReadRef(refTable, table->container());
        table->size_ = InInteger(inp);
        for (size_t i = 0; i < table->size(); i++) {
            table->setEntry(i,
                            Function::deserialize(refTable, inp)->container());
        }
        UNPROTECT(1);
        return table;
    }

    void serialize(SEXP refTable, R_outpstream_t out) const {
        HashAdd(container(), refTable);
        OutInteger(out, 1);
        baseline()->serialize(refTable, out);
    }

    Context userDefinedContext() const { return userDefinedContext_; }
    DispatchTable* newWithUserContext(Context udc) {

        auto clone = create(this->capacity());
        clone->setEntry(0, this->getEntry(0));

        auto j = 1;
        for (size_t i = 1; i < size(); i++) {
            if (get(i)->context().smaller(udc)) {
                clone->setEntry(j, getEntry(i));
                j++;
            }
        }

        clone->size_ = j;
        clone->userDefinedContext_ = udc;
        return clone;
    }

    Context combineContextWith(Context anotherContext) {
        return userDefinedContext_ | anotherContext;
    }

    bool disableFurtherSpecialization = false;
    SEXP unlockSym = nullptr;

    bool tryResolvingOptimisticLinks(SEXP vtabContainer, SEXP hSym) {
        if (!DispatchTable::check(vtabContainer)) {
            Rf_error("optimistic dispatch link failed, corrupted vtable!");
        }
        DispatchTable* vtable = DispatchTable::unpack(vtabContainer);

        SEXP serMap = Pool::get(HAST_DEPENDENCY_MAP);
        if (serMap == R_NilValue) return true;


        SEXP tabMap = Pool::get(HAST_VTAB_MAP);
        if (tabMap == R_NilValue) {
            SEXP tmp;
            PROTECT(tmp = R_NewEnv(R_EmptyEnv,0,0));
            Pool::patch(HAST_VTAB_MAP, tmp);
            UNPROTECT(1);
            tabMap = Pool::get(HAST_VTAB_MAP);
        }

        if (Rf_findVarInFrame(serMap, hSym) != R_UnboundValue && Rf_findVarInFrame(serMap, hSym) != R_NilValue) {
            vtable->disableFurtherSpecialization = true;
            #if PRINT_LINKING_STATUS == 1  || PRINT_LINKING_STATUS_OVERRIDE == 1
            std::cout << "[opt]symbolExistsInMap: " << CHAR(PRINTNAME(hSym)) << std::endl;
            #endif
            SEXP hastEnvMap = Rf_findVarInFrame(serMap, hSym);

            bool allOffsetsDone = true;

            serializerData::iterateOverOffsets(hastEnvMap, [&] (SEXP offsetSymbol, SEXP offsetEnv) {
                if (offsetEnv != R_NilValue) {
                    bool offsetCompletelyLinked = true;
                    #if PRINT_LINKING_STATUS == 1  || PRINT_LINKING_STATUS_OVERRIDE == 1
                    std::cout << "  offset: " << CHAR(PRINTNAME(offsetSymbol)) << std::endl;
                    #endif
                    int reqOffset = std::stoi(CHAR(PRINTNAME(offsetSymbol)));
                    serializerData::iterateOverContexts(offsetEnv, [&] (SEXP contextSym, SEXP cData) {
                        if (cData != R_NilValue) {
                            #if PRINT_LINKING_STATUS == 1  || PRINT_LINKING_STATUS_OVERRIDE == 1
                            std::cout << "    context: " << CHAR(PRINTNAME(contextSym)) << std::endl;
                            #endif
                            bool allSatisfied = true;
                            for (int j = 1; j < Rf_length(cData); j++) {
                                SEXP dep = VECTOR_ELT(cData, j);
                                if (dep == R_NilValue) continue;
                                if (TYPEOF(dep) == SYMSXP) {
                                    // normal case
                                    if (Rf_findVarInFrame(tabMap, dep) != R_UnboundValue) {
                                        SET_VECTOR_ELT(cData, j, R_NilValue);
                                    } else {
                                        allSatisfied = false;
                                    }
                                } else {
                                    // optimistic dispatch cast
                                    SEXP depHastSym = VECTOR_ELT(dep, 0);
                                    unsigned long* depCon = (unsigned long *) DATAPTR(VECTOR_ELT(dep, 1));
                                    unsigned* numArgs = (unsigned *) DATAPTR(VECTOR_ELT(dep, 2));
                                    if (Rf_findVarInFrame(tabMap, depHastSym) != R_UnboundValue) {
                                        // check if optimistic dispatch will succeed?
                                        DispatchTable * dt = DispatchTable::unpack(Rf_findVarInFrame(tabMap, depHastSym));

                                        bool entryFound = false;

                                        for (size_t i = 0; i < dt->size(); i++) {
                                            auto entry = dt->get(i);
                                            if (entry->context().toI() == *depCon &&
                                                entry->signature().numArguments >= *numArgs) {
                                                    entryFound = true;
                                            }
                                        }

                                        if (entryFound) {
                                            #if PRINT_LINKING_STATUS == 1  || PRINT_LINKING_STATUS_OVERRIDE == 1
                                            std::cout << "      [optimistic dispatch success]" << std::endl;
                                            #endif
                                            SET_VECTOR_ELT(cData, j, R_NilValue);
                                        } else {

                                            allSatisfied = false;
                                        }

                                    } else {
                                        allSatisfied = false;
                                    }
                                }
                            }
                            if (allSatisfied) {
                                SEXP functionContainer = VECTOR_ELT(cData, 0);

                                DispatchTable * requiredVtab;
                                if (reqOffset == 1) {
                                    requiredVtab = vtable;
                                } else {
                                    int idx = 0;
                                    // int aaa = 0;
                                    // vtable->baseline()->body()->printSource(true, aaa);
                                    // std::cout << "ReqSrc: " << reqOffset << std::endl;
                                    SEXP requiredVtabContainer = vtable->baseline()->body()->getTabAtOffset(true, idx, reqOffset);
                                    requiredVtab = DispatchTable::unpack(requiredVtabContainer);
                                }

                                Function * function = Function::unpack(functionContainer);
                                function->body()->populateSrcIdxData();

                                function->inheritFlags(requiredVtab->baseline());
                                requiredVtab->insert(function);
                                #if PRINT_LINKING_STATUS == 1  || PRINT_LINKING_STATUS_OVERRIDE == 1
                                std::cout << "      Linking Success!" << std::endl;
                                #endif
                                // do not need to link this again, linking happens only once.
                                Rf_defineVar(contextSym, R_NilValue, offsetEnv);
                            } else {
                                offsetCompletelyLinked = false;
                                #if PRINT_LINKING_STATUS == 1  || PRINT_LINKING_STATUS_OVERRIDE == 1
                                std::cout << "      Not Linked Yet!" << std::endl;
                                #endif
                            }
                        }

                    });
                    if (offsetCompletelyLinked) {
                        // we never need to look into this anymore
                        Rf_defineVar(offsetSymbol, R_NilValue, hastEnvMap);
                    } else {
                        allOffsetsDone = false;
                    }
                }


            });

            if (allOffsetsDone) {
                return true;
                vtable->disableFurtherSpecialization = false;
                Rf_defineVar(hSym, R_NilValue, serMap);
            }

            return false;
        } else {
            return true;
        }
    }

    void addUnlockDependency(SEXP dep, SEXP vTabContainer, SEXP hSym) {
        SEXP optMap = Pool::get(OPT_UNLOCK_MAP);
        if (optMap == R_NilValue) {
            SEXP tmp;
            PROTECT(tmp = R_NewEnv(R_EmptyEnv,0,0));
            Pool::patch(OPT_UNLOCK_MAP, tmp);
            UNPROTECT(1);
            optMap = Pool::get(OPT_UNLOCK_MAP);
        }

        if (!unlockSym) {
            unlockSym = dep;
        }



        if (Rf_findVarInFrame(optMap, dep) != R_UnboundValue && Rf_findVarInFrame(optMap, dep) != R_NilValue) {

            SEXP oldVec = Rf_findVarInFrame(optMap, dep);
            auto oldSize = Rf_length(oldVec);

            SEXP newWorkVec;
            PROTECT(newWorkVec = Rf_allocVector(VECSXP, oldSize + 1));
            memcpy(DATAPTR(newWorkVec), DATAPTR(oldVec), oldSize * sizeof(SEXP));

            SEXP data;
            PROTECT(data = Rf_allocVector(VECSXP, 2));
            SET_VECTOR_ELT(data, 0, vTabContainer);
            SET_VECTOR_ELT(data, 1, hSym);

            SET_VECTOR_ELT(newWorkVec, oldSize, data);
            UNPROTECT(1);

            Rf_defineVar(dep, newWorkVec, optMap);

            UNPROTECT(1);

        } else {

            SEXP workVector;
            PROTECT(workVector = Rf_allocVector(VECSXP, 2));
            SEXP data;
            PROTECT(data = Rf_allocVector(VECSXP, 2));
            SET_VECTOR_ELT(data, 0, vTabContainer);
            SET_VECTOR_ELT(data, 1, hSym);

            SET_VECTOR_ELT(workVector, 0, data);
            UNPROTECT(1);
            Rf_defineVar(dep, workVector, optMap);
            UNPROTECT(1);

        }
        // std::cout << "      [OUL] " << CHAR(PRINTNAME(dep)) << " -> " << CHAR(PRINTNAME(hSym)) << std::endl;
        // SEXP vec = Rf_findVarInFrame(optMap, dep);
        // std::cout << "      { ";
        // for (int i = 0; i < Rf_length(vec); i++) {
        //     SEXP ele = VECTOR_ELT(vec, i);
        //     if (ele != R_NilValue) {
        //         std::cout << "(" << VECTOR_ELT(ele, 0) << "," << CHAR(PRINTNAME(VECTOR_ELT(ele, 1))) << ") ";
        //     } else {
        //         std::cout << "0 ";
        //     }
        // }
        // std::cout << "}" << std::endl;
        // add an entry
    }

  private:
    DispatchTable() = delete;
    explicit DispatchTable(size_t cap)
        : RirRuntimeObject(
              // GC area starts at the end of the DispatchTable
              sizeof(DispatchTable),
              // GC area is just the pointers in the entry array
              cap) {}

    size_t size_ = 0;
    Context userDefinedContext_;
};
#pragma pack(pop)
} // namespace rir

#endif
