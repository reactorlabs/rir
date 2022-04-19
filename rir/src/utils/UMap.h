#ifndef RIR_UMAP_
#define RIR_UMAP_

#include <iostream>
#include <string>
#include <unistd.h>
#include <R/Protect.h>

#include "ir/BC_inc.h"
#include "R/r.h"
#include "interpreter/instance.h"
#include "utils/Pool.h"
class REnvHandler {
  public:
    REnvHandler(SEXP c) : _container(c) {
      assert(TYPEOF(c) == ENVSXP);
    }

    REnvHandler(uint32_t cPoolIndex) {
      SEXP c = rir::Pool::get(cPoolIndex);
      if (c == R_NilValue) {
        PROTECT(c = R_NewEnv(R_EmptyEnv,0,0));
        rir::Pool::patch(cPoolIndex, c);
        UNPROTECT(1);
      }
      assert(TYPEOF(c) == ENVSXP);
      _container = c;
    }

    // container
    SEXP container() {
      return _container;
    }

    // size
    int size() {
      int size = 0;
      iterate([&] (SEXP key, SEXP val) {
        size++;
      });
      return size;
    }

    // empty check
    bool isEmpty() {
      bool empty = true;
      iterate([&] (SEXP key, SEXP val) {
        empty = false;
      });
      return empty;

    }

    // removes a binding from the environment
    void remove(SEXP key) {
      R_removeVarFromFrame(key, _container);
    }

    void remove(const std::string & key) {
      remove(Rf_install(key.c_str()));
    }

    void remove(const char * key) {
      remove(Rf_install(key));
    }

    // setters: binds a key to a value
    void set(SEXP key, SEXP val) {
      assert(TYPEOF(key) == SYMSXP);
      Rf_defineVar(key, val, _container);
    }

    void set(const std::string & key, SEXP val) {
      set(Rf_install(key.c_str()), val);
    }

    void set(const char * key, SEXP val) {
      set(Rf_install(key), val);
    }

    // getters: returns nullptr if the key does not exist
    SEXP get(SEXP key) {
      assert(TYPEOF(key) == SYMSXP);
      SEXP binding = Rf_findVarInFrame(_container, key);
      return (binding == R_UnboundValue) ? nullptr : binding;
    }

    SEXP get(const std::string & key) {
      return get(Rf_install(key.c_str()));
    }

    SEXP get(const char * key) {
      return get(Rf_install(key));
    }

    SEXP operator[](SEXP key) {
      return get(key);
    }

    SEXP operator[](const std::string & key) {
      return get(Rf_install(key.c_str()));
    }

    SEXP operator[](const char * key) {
      return get(Rf_install(key));
    }

    // iterator
    void iterate(const std::function< void(SEXP, SEXP) >& callback) {
      SEXP offsetBindings = R_lsInternal(_container, (Rboolean) false);
      for (int i = 0; i < Rf_length(offsetBindings); i++) {
          SEXP key = Rf_install(CHAR(STRING_ELT(offsetBindings, i)));
          SEXP binding = Rf_findVarInFrame(_container, key);
          // skip inactive bindings
          if (binding == R_UnboundValue) continue;
          callback(key, binding);
      }
    }


  private:
    SEXP _container;
};

#endif
