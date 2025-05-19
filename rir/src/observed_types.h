#ifndef OBSERVED_TYPES_H
#define OBSERVED_TYPES_H

#include "compiler/pir/type.h"
#include <cstdint>
#include <string>
#include <vector>

namespace rir {
namespace pir {

namespace OT {

enum Opt {
    None,
    constantfold,
    eager_calls,
    force_dominance,
    inline_,
    load_elision,
    overflow,
    scope_resolution,
    types,
};

std::ostream& operator<<(std::ostream& os, Opt opt);

enum Origin : uint8_t {
    Default,
    Context,
    FromOpt,
    Inferred,
    Value,
};

std::ostream& operator<<(std::ostream& os, Origin origin);

struct GraphNode {
    GraphNode(PirType type, Origin origin, std::string instr_name,
              Opt optimization)
        : type(std::move(type)), origin(origin),
          instr_name(std::move(instr_name)), optimization(optimization) {}

    PirType type;
    Origin origin;
    std::string instr_name;
    Opt optimization;
};

size_t new_node(PirType type, OT::Origin origin, const std::string& instr_name, OT::Opt opt = None);

std::vector<size_t>& get_parents(size_t idx);

bool has_parents(size_t idx);

GraphNode get_node(size_t idx);

void print_node(std::ostream& os, size_t idx, size_t depth = 0);

void reset();

} // namespace OT
} // namespace pir
} // namespace rir

#endif // OBSERVED_TYPES_H
