#pragma once

#include "type.h"
#include <vector>

namespace rir {
namespace pir {

/**
 * A function signature for PIR function assertions.
 */
class PirSignature {
    enum class Form { Regular, Any, Nothing };

    static const unsigned MAX_NUM_ARGS = 4;

    Form form;
    PirType args[MAX_NUM_ARGS];
    unsigned numArgs;
    PirType result;

    explicit PirSignature(Form form)
        : form(form), args(), numArgs((unsigned)-1), result() {}

  public:
    static PirSignature any();
    static PirSignature nothing();
    static PirSignature parse(const std::string& inp);

    PirSignature(std::vector<PirType> args, PirType result);
    bool accepts(std::vector<PirType> inArgs) const;
    bool isAny() const { return form == Form::Any; }
    bool isNothing() const { return form == Form::Nothing; }
    PirSignature operator|(const PirSignature& other) const;
    void print(std::ostream& out) const;
};

std::ostream& operator<<(std::ostream& out, const PirSignature& sig);

} // namespace pir
} // namespace rir