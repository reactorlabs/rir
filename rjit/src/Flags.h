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
    bool useTypefeedback = false;
    bool unsafeNA = false;
    bool unsafeOpt = false;
    bool printIR = false;
    bool printOptIR = false;
    bool staticNamedArgMatch = true;
};
}

#endif
