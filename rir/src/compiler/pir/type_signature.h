#pragma once

#include "type.h"
#include <vector>

namespace rir {
namespace pir {

/**
 * A function signature for PIR function assertions.
 */
class TypeSignature {
    enum class Form { Regular, Any, Nothing };

    static const unsigned MAX_NUM_ARGS = 4;

    Form form;
    PirType args[MAX_NUM_ARGS];
    unsigned numArgs;
    PirType result;

    explicit TypeSignature(Form form)
        : form(form), args(), numArgs((unsigned)-1), result() {}

  public:
    static TypeSignature any();
    static TypeSignature nothing();
    static TypeSignature parse(const std::string& inp);

    TypeSignature(std::vector<PirType> args, PirType result);
    bool accepts(std::vector<PirType> inArgs) const;
    bool isAny() const { return form == Form::Any; }
    bool isNothing() const { return form == Form::Nothing; }
    TypeSignature operator|(const TypeSignature& other) const;
    void print(std::ostream& out) const;
};

std::ostream& operator<<(std::ostream& out, const TypeSignature& sig);

} // namespace pir
} // namespace rir