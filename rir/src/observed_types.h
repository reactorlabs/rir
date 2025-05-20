#ifndef OBSERVED_TYPES_H
#define OBSERVED_TYPES_H

#include "compiler/pir/type.h"
#include <cstdint>
#include <string>
#include <vector>

namespace rir {
namespace pir {

class Value;
namespace OT {

enum Opt : uint8_t {
    None,
    constantfold,
    eager_calls,
    force_dominance,
    inline_,
    load_elision,
    overflow,
    scope_resolution,
};

std::ostream& operator<<(std::ostream& os, Opt opt);

enum Origin : uint8_t {
    Default,
    Context,
    Inferred,

    FromOpt,
    FromValue,
};

std::ostream& operator<<(std::ostream& os, Origin origin);

struct GraphNode {
    GraphNode(PirType type, Origin origin, std::string instr_name,
              Opt optimization)
        : type(std::move(type)), instr_name(std::move(instr_name)),
          origin(origin), optimization(optimization) {}

    PirType type;
    std::string instr_name;
    Origin origin : 3;
    Opt optimization : 4;
};

void print_node(std::ostream& os, size_t idx, size_t depth = 0);

//------------------------------------------------------

size_t new_node(PirType type, OT::Origin origin, const std::string& instr_name,
                OT::Opt opt = None);

size_t new_value_node(Value* value);


//------------------------------------------------------

std::vector<size_t>& get_parents(size_t idx);

bool has_parents(size_t idx);

GraphNode get_node(size_t idx);

//------------------------------------------------------

void reset();

} // namespace OT
} // namespace pir
} // namespace rir

#endif // OBSERVED_TYPES_H
