#include "compiler/pir/instruction.h"
#include "utils/Terminal.h"
#include <observed_types.h>
#include <unordered_map>

namespace rir {
namespace pir {
namespace OT {

std::vector<GraphNode> NODES;
std::unordered_map<size_t, std::vector<size_t>> PARENTS;

void reset() {
    NODES.clear();
    PARENTS.clear();
}

//------------------------------------------------------

size_t new_node(PirType type, OT::Origin origin, const std::string& instr_name,
                OT::Opt opt) {
    size_t idx = NODES.size();
    NODES.emplace_back(type, origin, instr_name, opt);
    return idx;
}

size_t new_value_node(Value* value) {
    if (auto inst = Instruction::Cast(value)) {
        return inst->getOriginIdx();
    } else {
        return new_node(value->type, FromValue, tagToStr(value->tag));
    }
}

void new_node_assume(CastType* cast, Value* speculateOn, PirType typeFeedback) {
    cast->originIdx_ = new_node(cast->type, OT::FromAssume, cast->name());
    cast->originSet_ = 1;

    auto& pars = get_parents(cast->originIdx_);
    pars.push_back(new_node(typeFeedback, FromTypeFeedback, ""));
    pars.push_back(new_value_node(speculateOn));
}

//------------------------------------------------------

std::vector<size_t>& get_parents(size_t idx) {
    assert(idx < NODES.size());
    return PARENTS[idx];
}

bool has_parents(size_t idx) { return PARENTS.count(idx) > 0; }

GraphNode get_node(size_t idx) {
    assert(idx < NODES.size());
    return NODES[idx];
}

//------------------------------------------------------

std::ostream& operator<<(std::ostream& os, Opt opt) {
    switch (opt) {
    case None:
        return os << "None";

    case constantfold:
        return os << "constantfold";

    case eager_calls:
        return os << "eager_calls";

    case force_dominance:
        return os << "force_dominance";

    case inline_:
        return os << "inline_";

    case load_elision:
        return os << "load_elision";

    case overflow:
        return os << "overflow";

    case scope_resolution:
        return os << "scope_resolution";
    }
    assert(false);
}

std::ostream& operator<<(std::ostream& os, Origin origin) {
    switch (origin) {
    case Default:
        return os << "Default";
    case Context:
        return os << "Context";
    case Inferred:
        return os << "Inferred";

    case FromOpt:
        return os << "Optimization";
    case FromValue:
        return os << "Value";
    case FromAssume:
        return os << "Assume";
    case FromTypeFeedback:
        return os << "TypeFeedback";
    }
    assert(false);
}

void print_node(std::ostream& os, size_t idx, size_t depth) {
    auto node = get_node(idx);

    for (size_t i = 0; i < depth; i++) {
        os << "| ";
    }

    ConsoleColor::bold(os);
    os << node.type;
    ConsoleColor::clear(os);
    os << " " << node.instr_name << " (" << node.origin;
    if (node.origin == FromOpt) {
        os << ": " << node.optimization;
    }
    os << ")\n";

    if (has_parents(idx)) {
        auto pars = get_parents(idx);
        assert(pars.size() > 0);
        for (auto p : pars) {
            print_node(os, p, depth + 1);
        }
    }
}

} // namespace OT
} // namespace pir
} // namespace rir
