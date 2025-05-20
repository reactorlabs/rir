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

//------------------------------------------------------

DependencyNode get_node(size_t idx) {
    assert(idx < NODES.size());
    auto gnode = NODES[idx];
    DependencyNode node {
        .type = gnode.type,
        .instr_name = gnode.instr_name,
        .origin = gnode.origin,
        .optimization = gnode.optimization
    };

    if (has_parents(idx)){
        auto pars =  get_parents(idx);
        node.parents.reserve(pars.size());
        for (auto p : pars) {
            node.parents.push_back(get_node(p));
        }
    }

    return node;
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

void DependencyNode::print(std::ostream& os, size_t depth) const {
    for (size_t i = 0; i < depth; i++) {
        os << "| ";
    }

    if (ConsoleColor::isTTY(os)){
        ConsoleColor::bold(os);
    }
    os << type;
    if (ConsoleColor::isTTY(os)){
        ConsoleColor::clear(os);
    }
    os << " " << instr_name << " (" << origin;
    if (origin == FromOpt) {
        os << ": " << optimization;
    }
    os << ")\n";

    for (auto p : parents) {
        p.print(os, depth + 1);
    }
}

std::ostream& operator<<(std::ostream& os, const DependencyNode& node) {
    node.print(os);
    return os;
}

} // namespace OT
} // namespace pir
} // namespace rir
