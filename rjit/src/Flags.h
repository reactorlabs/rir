#ifndef FLAGS_H
#define FLAGS_H

namespace rjit {

class Flag {
  public:
    static Flag& singleton() {
        static Flag flag;
        return flag;
    }

    bool recordTypes = false;
    bool recompileHot = false;
    bool staticNamedArgMatch = true;
    bool unsafeOpt = false;
    bool unsafeNA = false;
    bool printIR = false;
    bool printOptIR = false;
};
}

#endif
