#ifndef PRECIOUS_H
#define PRECIOUS_H

#include "RDefs.h"
#include <set>

namespace rir {

class Precious {
  public:
    static void add(SEXP s) { singleton().add_(s); }

    static void remove(SEXP s) { singleton().remove_(s); }

    static void gcCallback(void (*forward_node)(SEXP)) {
        singleton().doGcCallback(forward_node);
    }

  private:
    static Precious& singleton() {
        static Precious p;
        return p;
    }

    void add_(SEXP s) { precious.insert(s); }

    void remove_(SEXP s) { precious.erase(s); }

    void doGcCallback(void (*forward_node)(SEXP)) {
        for (SEXP e : precious) {
            forward_node(e);
        }
    }

    std::set<SEXP> precious;
};
}

#endif // PRECIOUS_H
