#include "gnur2pir.h"
#include "../rir2pir/insert_cast.h"
#include "R/BuiltinIds.h"
#include "R/Funtab.h"
#include "R/RList.h"
#include "R/Symbols.h"
#include "compiler/analysis/cfg.h"
#include "compiler/analysis/query.h"
#include "compiler/analysis/verifier.h"
#include "compiler/opt/pass_definitions.h"
#include "compiler/pir/builder.h"
#include "compiler/pir/pir_impl.h"
#include "compiler/util/arg_match.h"
#include "compiler/util/visitor.h"
#include "ir/BC.h"
#include "ir/Compiler.h"
#include "runtime/ArglistOrder.h"
#include "simple_instruction_list.h"
#include "utils/FormalArgs.h"

#include <sstream>
#include <unordered_map>
#include <vector>

namespace rir {
namespace pir {

typedef std::pair<BB*, Value*> ReturnSite;

struct State {
    bool seen = false;
    BB* entryBB = nullptr;
    size_t entryPC = 0;

    State() {}
    State(State&&) = default;
    State(const State&) = delete;
    State(const State& other, bool seen, BB* entryBB, size_t entryPC)
        : seen(seen), entryBB(entryBB), entryPC(entryPC), stack(other.stack){};

    void operator=(const State&) = delete;
    State& operator=(State&&) = default;

    void mergeIn(const State& incom, BB* incomBB);
    void createMergepoint(Builder&);

    void clear() {
        stack.clear();
        entryBB = nullptr;
        entryPC = 0;
    }

    RirStack stack;
};

void State::createMergepoint(Builder& insert) {
    BB* oldBB = insert.getCurrentBB();
    insert.createNextBB();
    for (size_t i = 0; i < stack.size(); ++i) {
        auto v = stack.at(i);
        auto p = insert(new Phi);
        p->addInput(oldBB, v);
        stack.at(i) = p;
    }
}

void State::mergeIn(const State& incom, BB* incomBB) {
    assert(stack.size() == incom.stack.size());

    for (size_t i = 0; i < stack.size(); ++i) {
        Phi* p = Phi::Cast(stack.at(i));
        assert(p);
        Value* in = incom.stack.at(i);
        if (in != Tombstone::unreachable())
            p->addInput(incomBB, in);
    }
    incomBB->setNext(entryBB);
}

#define GNUR_BC(V)                                                             \
    V(BCMISMATCH_OP, 0)                                                        \
    V(RETURN_OP, 0)                                                            \
    V(GOTO_OP, 1)                                                              \
    V(BRIFNOT_OP, 2)                                                           \
    V(POP_OP, 0)                                                               \
    V(DUP_OP, 0)                                                               \
    V(PRINTVALUE_OP, 0)                                                        \
    V(STARTLOOPCNTXT_OP, 2)                                                    \
    V(ENDLOOPCNTXT_OP, 1)                                                      \
    V(DOLOOPNEXT_OP, 0)                                                        \
    V(DOLOOPBREAK_OP, 0)                                                       \
    V(STARTFOR_OP, 3)                                                          \
    V(STEPFOR_OP, 1)                                                           \
    V(ENDFOR_OP, 0)                                                            \
    V(SETLOOPVAL_OP, 0)                                                        \
    V(INVISIBLE_OP, 0)                                                         \
    V(LDCONST_OP, 1)                                                           \
    V(LDNULL_OP, 0)                                                            \
    V(LDTRUE_OP, 0)                                                            \
    V(LDFALSE_OP, 0)                                                           \
    V(GETVAR_OP, 1)                                                            \
    V(DDVAL_OP, 1)                                                             \
    V(SETVAR_OP, 1)                                                            \
    V(GETFUN_OP, 1)                                                            \
    V(GETGLOBFUN_OP, 1)                                                        \
    V(GETSYMFUN_OP, 1)                                                         \
    V(GETBUILTIN_OP, 1)                                                        \
    V(GETINTLBUILTIN_OP, 1)                                                    \
    V(CHECKFUN_OP, 0)                                                          \
    V(MAKEPROM_OP, 1)                                                          \
    V(DOMISSING_OP, 0)                                                         \
    V(SETTAG_OP, 1)                                                            \
    V(DODOTS_OP, 0)                                                            \
    V(PUSHARG_OP, 0)                                                           \
    V(PUSHCONSTARG_OP, 1)                                                      \
    V(PUSHNULLARG_OP, 0)                                                       \
    V(PUSHTRUEARG_OP, 0)                                                       \
    V(PUSHFALSEARG_OP, 0)                                                      \
    V(CALL_OP, 1)                                                              \
    V(CALLBUILTIN_OP, 1)                                                       \
    V(CALLSPECIAL_OP, 1)                                                       \
    V(MAKECLOSURE_OP, 1)                                                       \
    V(UMINUS_OP, 1)                                                            \
    V(UPLUS_OP, 1)                                                             \
    V(ADD_OP, 1)                                                               \
    V(SUB_OP, 1)                                                               \
    V(MUL_OP, 1)                                                               \
    V(DIV_OP, 1)                                                               \
    V(EXPT_OP, 1)                                                              \
    V(SQRT_OP, 1)                                                              \
    V(EXP_OP, 1)                                                               \
    V(EQ_OP, 1)                                                                \
    V(NE_OP, 1)                                                                \
    V(LT_OP, 1)                                                                \
    V(LE_OP, 1)                                                                \
    V(GE_OP, 1)                                                                \
    V(GT_OP, 1)                                                                \
    V(AND_OP, 1)                                                               \
    V(OR_OP, 1)                                                                \
    V(NOT_OP, 1)                                                               \
    V(DOTSERR_OP, 0)                                                           \
    V(STARTASSIGN_OP, 1)                                                       \
    V(ENDASSIGN_OP, 1)                                                         \
    V(STARTSUBSET_OP, 2)                                                       \
    V(DFLTSUBSET_OP, 0)                                                        \
    V(STARTSUBASSIGN_OP, 2)                                                    \
    V(DFLTSUBASSIGN_OP, 0)                                                     \
    V(STARTC_OP, 2)                                                            \
    V(DFLTC_OP, 0)                                                             \
    V(STARTSUBSET2_OP, 2)                                                      \
    V(DFLTSUBSET2_OP, 0)                                                       \
    V(STARTSUBASSIGN2_OP, 2)                                                   \
    V(DFLTSUBASSIGN2_OP, 0)                                                    \
    V(DOLLAR_OP, 2)                                                            \
    V(DOLLARGETS_OP, 2)                                                        \
    V(ISNULL_OP, 0)                                                            \
    V(ISLOGICAL_OP, 0)                                                         \
    V(ISINTEGER_OP, 0)                                                         \
    V(ISDOUBLE_OP, 0)                                                          \
    V(ISCOMPLEX_OP, 0)                                                         \
    V(ISCHARACTER_OP, 0)                                                       \
    V(ISSYMBOL_OP, 0)                                                          \
    V(ISOBJECT_OP, 0)                                                          \
    V(ISNUMERIC_OP, 0)                                                         \
    V(VECSUBSET_OP, 1)                                                         \
    V(MATSUBSET_OP, 1)                                                         \
    V(VECSUBASSIGN_OP, 1)                                                      \
    V(MATSUBASSIGN_OP, 1)                                                      \
    V(AND1ST_OP, 2)                                                            \
    V(AND2ND_OP, 1)                                                            \
    V(OR1ST_OP, 2)                                                             \
    V(OR2ND_OP, 1)                                                             \
    V(GETVAR_MISSOK_OP, 1)                                                     \
    V(DDVAL_MISSOK_OP, 1)                                                      \
    V(VISIBLE_OP, 0)                                                           \
    V(SETVAR2_OP, 1)                                                           \
    V(STARTASSIGN2_OP, 1)                                                      \
    V(ENDASSIGN2_OP, 1)                                                        \
    V(SETTER_CALL_OP, 2)                                                       \
    V(GETTER_CALL_OP, 1)                                                       \
    V(SWAP_OP, 0)                                                              \
    V(DUP2ND_OP, 0)                                                            \
    V(SWITCH_OP, 4)                                                            \
    V(RETURNJMP_OP, 0)                                                         \
    V(STARTSUBSET_N_OP, 2)                                                     \
    V(STARTSUBASSIGN_N_OP, 2)                                                  \
    V(VECSUBSET2_OP, 1)                                                        \
    V(MATSUBSET2_OP, 1)                                                        \
    V(VECSUBASSIGN2_OP, 1)                                                     \
    V(MATSUBASSIGN2_OP, 1)                                                     \
    V(STARTSUBSET2_N_OP, 2)                                                    \
    V(STARTSUBASSIGN2_N_OP, 2)                                                 \
    V(SUBSET_N_OP, 2)                                                          \
    V(SUBSET2_N_OP, 2)                                                         \
    V(SUBASSIGN_N_OP, 2)                                                       \
    V(SUBASSIGN2_N_OP, 2)                                                      \
    V(LOG_OP, 1)                                                               \
    V(LOGBASE_OP, 1)                                                           \
    V(MATH1_OP, 2)                                                             \
    V(DOTCALL_OP, 2)                                                           \
    V(COLON_OP, 1)                                                             \
    V(SEQALONG_OP, 1)                                                          \
    V(SEQLEN_OP, 1)                                                            \
    V(BASEGUARD_OP, 2)                                                         \
    V(INCLNK_OP, 0)                                                            \
    V(DECLNK_OP, 0)                                                            \
    V(DECLNK_N_OP, 1)

class RBC {
  public:
    enum Id {
#define V(BC, _) BC,
        GNUR_BC(V)
#undef V
    };

    const Id id;

    bool valid() const {
        switch (id) {
#define V(BC, I) case BC:
            GNUR_BC(V)
#undef V
            return true;
        default: {}
        }
        return false;
    }

    size_t imm() const {
        switch (id) {
#define V(BC, I)                                                               \
    case BC:                                                                   \
        return I;
            GNUR_BC(V)
#undef V
        }
        assert(false);
        return 0;
    }
    int imm(size_t pos) const {
        assert(pos < parsed);
        return args[pos];
    }

    explicit RBC(const int* pc) : id((Id)pc[0]) {
        for (; parsed < imm(); ++parsed)
            args[parsed] = pc[parsed + 1];
        assert(valid());
    };

    bool operator==(int bc) const { return id == bc; }

    bool jumps() const {
        return id == RBC::BRIFNOT_OP || id == RBC::GOTO_OP ||
               id == RBC::STARTFOR_OP || id == RBC::STEPFOR_OP;
    }

    size_t jump() const {
        if (id == RBC::BRIFNOT_OP)
            return imm(1);
        if (id == RBC::GOTO_OP)
            return imm(0);
        if (id == RBC::STARTFOR_OP)
            return imm(2);
        if (id == RBC::STEPFOR_OP)
            return imm(0);
        assert(false);
        return 0;
    }

    bool falls() const {
        return id != RBC::RETURN_OP && id != RBC::GOTO_OP &&
               id != RBC::STARTFOR_OP;
    }

    friend std::ostream& operator<<(std::ostream& out, Id id) {
        switch (id) {
#define V(BC, _)                                                               \
    case BC:                                                                   \
        out << #BC;                                                            \
        break;
            GNUR_BC(V)
#undef V
        }
        return out;
    }

    friend std::ostream& operator<<(std::ostream& out, RBC bc) {
        out << bc.id;
        if (bc.imm() > 0) {
            out << "(";
            for (unsigned i = 0; i < bc.imm(); ++i) {
                out << bc.imm(i);
                if (i < bc.imm() - 1)
                    out << ", ";
            }
            out << ")";
        }
        return out;
    }

  private:
    unsigned parsed = 0;
    std::array<int, 4> args;
};

class RBCCode {
  private:
    SEXP body;
    Preserve p;
    int* stream() { return INTEGER(body); }
    const int* stream() const { return INTEGER(body); }

  public:
    explicit RBCCode(SEXP body) : body(R_bcDecode(BCODE_CODE(body))) {
        p(body);
    }
    size_t length() const { return XLENGTH(body); }
    int version() const { return stream()[0]; }

    RBC operator[](size_t pos) const { return RBC(stream() + pos); }

    struct Iterator {
        const int* pos;
        explicit Iterator(const int* pos) : pos(pos) {}

        RBC operator*() { return RBC(pos); }
        void operator++() { pos += this->operator*().imm() + 1; }
        bool operator!=(const Iterator& other) const {
            return pos != other.pos;
        }
        bool operator==(const Iterator& other) const {
            return pos == other.pos;
        }

        size_t label(const Iterator& begin) const {
            return pos - begin.pos + 1;
        }
        Iterator operator+(size_t n) const {
            auto pos = *this;
            for (size_t i = 0; i < n; ++i)
                ++pos;
            return pos;
        }
        std::list<Iterator> successors() {
            auto bc = this->operator*();
            if (bc.jumps() && bc.falls())
                return {*this + bc.jump(), *this + 1};
            if (bc.jumps())
                return {*this + bc.jump()};
            if (bc.falls())
                return {*this + 1};
            return {};
        }
    };
    Iterator begin() const { return Iterator(stream() + 1); }
    Iterator end() const { return Iterator(stream() + length()); }

    friend std::ostream& operator<<(std::ostream& out, const RBCCode& code) {
        out << "Length : " << code.length() << "\n";
        out << "Version : " << code.version() << "\n";
        for (auto pc = code.begin(); pc != code.end(); ++pc) {
            out << std::left << std::setw(6) << pc.label(code.begin()) << *pc
                << "\n";
        }
        return out;
    }
};

struct Stack {
  private:
    std::stack<Value*> stack;

  public:
    void push(Value* v) { stack.push(v); }
    Value* pop() {
        auto v = stack.top();
        stack.pop();
        return v;
    }
    ~Stack() { assert(stack.empty()); }
};

struct CompilerInfo {
    CompilerInfo(SEXP src, Stack& stack, Builder& insert)
        : src(src), stack(stack), insert(insert) {}
    SEXP src;
    Stack& stack;
    Builder& insert;
    std::unordered_set<size_t> mergepoints;
    std::unordered_map<size_t, size_t> jumps;
};

static void findMerges(const RBCCode& bytecode, CompilerInfo& info) {
    std::unordered_map<size_t, std::unordered_set<size_t>> incom;
    // Mark incoming jmps
    for (auto pc = bytecode.begin(); pc != bytecode.end(); ++pc) {
        auto l = pc.label(bytecode.begin());
        auto bc = *pc;
        if (bc == RBC::BRIFNOT_OP)
            incom[bc.imm(1)].insert(l);
        else if (bc == RBC::GOTO_OP)
            incom[bc.imm(0)].insert(l);
    }

    // Add fall-through cases
    for (auto pc = bytecode.begin(); pc != bytecode.end(); ++pc) {
        if (!(*pc).falls())
            continue;
        auto next = pc + 1;
        auto next_label = next.label(bytecode.begin());
        auto next_merge = incom.find(next_label);
        if (next_merge != incom.end()) {
            next_merge->second.insert(pc.label(bytecode.begin()));
        }
    }

    // Create mergepoints
    for (auto m : incom)
        // The first position must also be considered a mergepoint in case it
        // has only one incoming (a jump)
        if (m.first == 1 || m.second.size() > 1)
            info.mergepoints.insert(m.first);
}

struct BCCompiler {
    CompilerInfo& cmp;
    explicit BCCompiler(CompilerInfo& cmp) : cmp(cmp) {}

    void push(Value* v) { cmp.stack.push(v); }

    Value* pop() { return cmp.stack.pop(); }

    Instruction* insertPush(Instruction* i) {
        push(insert(i));
        return i;
    }

    Instruction* insert(Instruction* i) {
        cmp.insert(i);
        return i;
    }

    SEXP cnst(int i) { return VECTOR_ELT(BCODE_CONSTS(BODY(cmp.src)), i); }

    Value* env() { return cmp.insert.env; }

    template <RBC::Id BC>
    void compile(const RBC&);
};

// Start instructions translation

template <>
void BCCompiler::compile<RBC::GETVAR_OP>(const RBC& bc) {
    auto v = insert(new LdVar(cnst(bc.imm(0)), env()));
    insertPush(new Force(v, env(), Tombstone::framestate()));
}

template <>
void BCCompiler::compile<RBC::RETURN_OP>(const RBC& bc) {
    insert(new Return(pop()));
}

template <>
void BCCompiler::compile<RBC::LDCONST_OP>(const RBC& bc) {
    push(insert(new LdConst(cnst(bc.imm(0)))));
}

template <>
void BCCompiler::compile<RBC::ADD_OP>(const RBC& bc) {
    auto a = pop();
    auto b = pop();
    insertPush(new Add(a, b, env(), bc.imm(0)));
}

// End instructions translation

pir::ClosureVersion* Gnur2Pir::compile(SEXP src, const std::string& name) {
    SEXP body = BODY(src);
    RBCCode bytecode(body);

    std::cout << bytecode;

    auto c = m.getOrDeclareGnurClosure(name, src, Context());
    auto v = c->declareVersion(Compiler::defaultContext, true, nullptr);

    Stack stack;
    Builder insert(v, c->closureEnv());

    CompilerInfo info(src, stack, insert);

    findMerges(bytecode, info);

    std::cout << "CFG merges at: ";
    for (auto& m : info.mergepoints) {
        std::cout << m << " ";
    }
    std::cout << "\n";

    BCCompiler bccompiler(info);

#define SUPPORTED_INSTRUCTIONS(V)                                              \
    V(RBC::GETVAR_OP)                                                          \
    V(RBC::RETURN_OP)                                                          \
    V(RBC::LDCONST_OP)                                                         \
    V(RBC::ADD_OP)

    for (auto pc = bytecode.begin(); pc != bytecode.end(); ++pc) {
        auto bc = *pc;
        switch (bc.id) {
#define V(BC)                                                                  \
    case BC:                                                                   \
        bccompiler.compile<BC>(bc);                                            \
        break;
            SUPPORTED_INSTRUCTIONS(V)
#undef V

        default:
            std::cerr << "Could not compile " << *pc << "\n";
            assert(false);
            return nullptr;
        }
    }
    return v;
}

} // namespace pir
} // namespace rir
