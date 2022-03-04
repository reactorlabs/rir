#ifndef RIR_UMAP_
#define RIR_UMAP_

#include <iostream>
#include <string>
#include <unistd.h>
#include <R/Protect.h>

#include "VecOpr.h"
#include "ir/BC_inc.h"
#include "R/r.h"
#include "interpreter/instance.h"

#include "runtimePatches.h"

class UMap {
  public:

  static SEXP createMap() {
    rir::Protect p;
    SEXP mainListHolder;
    SEXP keys;
    SEXP envObj;
    // initialize a vector of size 2
    p(mainListHolder = Rf_allocVector(VECSXP, 2));
    // vector of active keys(symbols) in the map
    p(keys = Rf_allocVector(VECSXP, 0));
    // env object is used to store the bindings
    p(envObj = R_NewEnv(R_EmptyEnv,0,0));

    SET_VECTOR_ELT(mainListHolder, 0, keys);
    SET_VECTOR_ELT(mainListHolder, 1, envObj);
    return mainListHolder;
  }

  static rir::BC::PoolIdx createMapInCp() {
    return rir::Pool::insert(createMap());
  }

  static void createMapInCp(rir::BC::PoolIdx idx) {
    rir::Protect p;
    SEXP mainListHolder;
    SEXP keys;
    SEXP envObj;
    // initialize a vector of size 2
    p(mainListHolder = Rf_allocVector(VECSXP, 2));
    // vector of active keys(symbols) in the map
    p(keys = Rf_allocVector(VECSXP, 0));
    // env object is used to store the bindings
    p(envObj = R_NewEnv(R_EmptyEnv,0,0));

    SET_VECTOR_ELT(mainListHolder, 0, keys);
    SET_VECTOR_ELT(mainListHolder, 1, envObj);
    rir::Pool::patch(idx, mainListHolder);
  }

  static SEXP getMapFromCP(rir::BC::PoolIdx idx) {
    // return an existing Map from the Constant Pool
    return rir::Pool::get(idx);
  }

  static void insert(SEXP map, SEXP key, SEXP val) {
    assert(TYPEOF(map) == VECSXP && TYPEOF(key) == SYMSXP);
    rir::Protect p;

    SEXP env = VECTOR_ELT(map, 1);
    SEXP keys = VECTOR_ELT(map, 0);
    if (!symbolExistsInEnv(key, env)) {
      p(keys = VecOpr::insertElementAndGrow(keys, key));
      SET_VECTOR_ELT(map, 0, keys);
    }
    Rf_defineVar(key, val, env);
  }

  static void remove(SEXP map, SEXP key) {
    assert(TYPEOF(map) == VECSXP && TYPEOF(key) == SYMSXP);
    SEXP keys = VECTOR_ELT(map, 0);
    SEXP env = VECTOR_ELT(map, 1);
    VecOpr::remove(keys, key);
    Rf_defineVar(key, R_NilValue, env);
  }

  static SEXP get(SEXP map, SEXP key) {
    assert(TYPEOF(map) == VECSXP && TYPEOF(key) == SYMSXP);
    SEXP env = VECTOR_ELT(map, 1);
    return Rf_findVarInFrame(env, key);
  }

  static void print(SEXP map) {
    assert(TYPEOF(map) == VECSXP);
    SEXP keys = VECTOR_ELT(map, 0);
    std::cout << "Printing UMap: " << std::endl;
    for (int i = 0; i < Rf_length(keys); i++) {
      auto key = VECTOR_ELT(keys, i);
      if (TYPEOF(key) == SYMSXP) {
        std::cout << "key [" << CHAR(PRINTNAME(key)) << "]" << std::endl;
        printAST(0,get(map, key));
      }
    }
  }
  static bool symbolExistsInEnv(SEXP sym, SEXP env) {

    if (Rf_findVarInFrame3(env, sym, FALSE) == R_UnboundValue || Rf_findVarInFrame(env, sym) == R_NilValue) {
      return false;
    }
    // return Rf_findVarInFrame3(env, sym, FALSE) != R_UnboundValue;
    return true;
  }

  static bool symbolExistsInMap(SEXP sym, SEXP map) {
    SEXP env = VECTOR_ELT(map, 1);
    if (Rf_findVarInFrame3(env, sym, FALSE) == R_UnboundValue || Rf_findVarInFrame(env, sym) == R_NilValue) {
      return false;
    }
    return true;
  }
};

class DeserialDataMap : UMap {
  public:
  static SEXP getHastEnv(SEXP map, SEXP hSym) {
    assert(TYPEOF(map) == VECSXP);
    rir::Protect p;
    SEXP currentHastEnv;
    if (!symbolExistsInMap(hSym, map)) {
      p(currentHastEnv = createMap());
      insert(map, hSym, currentHastEnv);
    } else {
      currentHastEnv = get(map, hSym);
    }
    return currentHastEnv;
  }

  static SEXP getContextDataVector(SEXP map, SEXP hSym, SEXP cSym) {
    assert(TYPEOF(map) == VECSXP);
    rir::Protect p;
    SEXP currentHastEnv = getHastEnv(map, hSym);
    SEXP currentContextVec;
    if (!symbolExistsInMap(cSym, currentHastEnv)) {
      p(currentContextVec = Rf_allocVector(VECSXP, 0));
      insert(currentHastEnv, cSym, currentContextVec);
    } else {
      currentContextVec = get(currentHastEnv, cSym);
    }
    return currentContextVec;
  }

  static SEXP getContextDataVector(SEXP map, size_t hast, unsigned long context) {
    std::stringstream hastSym;
    hastSym << hast;
    auto hSym = Rf_install(hastSym.str().c_str());

    std::stringstream conSym;
    conSym << context;
    auto cSym = Rf_install(conSym.str().c_str());

    return getContextDataVector(map, hSym, cSym);
  }

  static void setContextDataVector(SEXP map, SEXP hastSym, SEXP conSym, SEXP v) {
    assert(TYPEOF(map) == VECSXP);

    rir::Protect p;

    SEXP currentHastEnv = getHastEnv(map, hastSym);
    insert(currentHastEnv, conSym, v);
  }

  static void setContextDataVector(SEXP map, size_t hast, unsigned long context, SEXP v) {
    std::stringstream hastSym;
    hastSym << hast;
    auto hSym = Rf_install(hastSym.str().c_str());

    std::stringstream conSym;
    conSym << context;
    auto cSym = Rf_install(conSym.str().c_str());

    return setContextDataVector(map, hSym, cSym, v);
  }

  static SEXP getFunctionPtr(SEXP map, size_t hast, unsigned long context) {
    SEXP vec = getContextDataVector(map, hast, context);
    return VECTOR_ELT(vec, 0);
  }

  static void addFunctionPtr(SEXP map, size_t hast, unsigned long context, SEXP function) {
    rir::Protect p;
    SEXP vec = getContextDataVector(map, hast, context);
    if (Rf_length(vec) == 0) {
      p(vec = VecOpr::insertElementAndGrow(vec, function));
      setContextDataVector(map, hast, context, vec);
    }
    SET_VECTOR_ELT(vec, 0, function);
  }

  static void addDependencies(SEXP map, size_t hast, unsigned long context, std::vector<size_t> & hasts) {
    SEXP func = getFunctionPtr(map, hast, context);

    auto unlockMap = rir::Pool::get(HAST_UNLOCK_MAP);

    rir::Protect p;

    SEXP vec;
    p(vec = Rf_allocVector(VECSXP, hasts.size() + 1));

    SET_VECTOR_ELT(vec, 0, func);

    std::stringstream hs;
    hs << hast;

    SEXP hSym = Rf_install(hs.str().c_str());

    for (long unsigned int i = 0; i < hasts.size(); i++) {
      std::stringstream ss;
      ss << hasts[i];
      SEXP depSym = Rf_install(ss.str().c_str());
      if (depSym == hSym) {
        // std::cout << "dont add yourself as dependency" << std::endl;
        continue;
      }
      SET_VECTOR_ELT(vec, i+1, depSym);

      // create unlock map
      SEXP unlockMapVec = R_NilValue;
      if (!symbolExistsInMap(depSym, unlockMap)) {
        p(unlockMapVec = Rf_allocVector(VECSXP, 1));
        SET_VECTOR_ELT(unlockMapVec, 0, hSym);
      } else {
        unlockMapVec = get(unlockMap, depSym);
        p(unlockMapVec = VecOpr::insertUniqueSymbolElementAndGrow(unlockMapVec, hSym));
      }
      insert(unlockMap, depSym, unlockMapVec);
    }
    setContextDataVector(map, hast, context, vec);
  }

  static void printDependencies() {
    SEXP map = rir::Pool::get(HAST_DEPENDENCY_MAP);
    SEXP keys = VECTOR_ELT(map, 0);

    std::cout << "Dependency Map" << std::endl;

    for (int i = 0; i < Rf_length(keys); i++) {
      SEXP currHast = VECTOR_ELT(keys, i);
      std::cout << "Hast(" << i << "): " << CHAR(PRINTNAME(currHast)) << std::endl;

      SEXP contextMap = get(map, currHast);
      SEXP contexts = VECTOR_ELT(contextMap, 0);

      for (int j = 0; j < Rf_length(contexts); j++) {
        auto currContext = VECTOR_ELT(contexts, j);
        std::cout << "    (" << j << ") " << CHAR(PRINTNAME(currContext)) << ": [ ";

        auto contextVector = getContextDataVector(map, currHast, currContext);

        for (int k = 0; k < Rf_length(contextVector); k++) {
          auto ele = VECTOR_ELT(contextVector, k);
          if (TYPEOF(ele) == SYMSXP) {
            std::cout << CHAR(PRINTNAME(ele)) << " ";
          } else {
            std::cout << "Type(" << TYPEOF(ele) << ") ";
          }
        }

        std::cout << "]" << std::endl;
      }
    }

  }

  static void printUnlockMap() {
    SEXP map = rir::Pool::get(HAST_UNLOCK_MAP);
    SEXP keys = VECTOR_ELT(map, 0);
    std::cout << "Unlock Map" << std::endl;
    for (int i = 0; i < Rf_length(keys); i++) {
      SEXP currHast = VECTOR_ELT(keys, i);
      std::cout << "Hast(" << i << "): " << CHAR(PRINTNAME(currHast)) << " [ ";
      SEXP vec = get(map, currHast);
      for (int j = 0; j < Rf_length(vec); j++) {
        auto ele = VECTOR_ELT(vec, j);
        if (TYPEOF(ele) == SYMSXP) {
          std::cout << CHAR(PRINTNAME(ele)) << " ";
        } else {
          std::cout << "Type(" << TYPEOF(ele) << ") ";
        }
      }
      std::cout << "]" << std::endl;

    }

  }


  private:
};

#endif
