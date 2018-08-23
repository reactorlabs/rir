#ifndef RIR_2_PIR_H
#define RIR_2_PIR_H

#include "../../util/builder.h"
#include "../pir_translator.h"
#include "rir_2_pir_compiler.h"

#include <unordered_set>

namespace rir {
namespace pir {

class Rir2Pir {
  public:
    Rir2Pir(Rir2PirCompiler& cmp, rir::Function* srcFunction)
        : compiler(cmp), srcFunction(srcFunction) {}

    // Tries to compile the srcCode. Return value indicates failure. Builder
    // has to be discarded, if compilation fails!
    bool tryCompile(rir::Code* srcCode, Builder& insert)
        __attribute__((warn_unused_result)) {
        return translate<bool>(srcCode, insert,
                               [&](Value* v) {
                                   finalize(v, insert);
                                   return true;
                               },
                               []() { return false; });
    }

  private:
    bool tryCompilePromise(rir::Code* prom, Builder& insert) const
        __attribute__((warn_unused_result)) {
        return Rir2Pir(compiler, srcFunction).tryCompile(prom, insert);
    }

    template <typename T>
    using Maybe = std::function<T()>;
    template <typename T>
    using MaybeVal = std::function<T(Value*)>;

    // Try to translate. On success and on fail "callbacks" are passed
    void translate(rir::Code* srcCode, Builder& insert, MaybeVal<void> success,
                   Maybe<void> fail) const;

    // Try to translate. On fail is silent.
    void translate(rir::Code* srcCode, Builder& insert,
                   MaybeVal<void> success) const {
        translate(srcCode, insert, success, []() {});
    }

    // Try to translate. Either fail or success is invoked. The final result is
    // the return value of either of those.
    template <typename T>
    T translate(rir::Code* srcCode, Builder& insert, MaybeVal<T> success,
                Maybe<T> fail) const {
        T res;
        translate(srcCode, insert, [&](Value* v) { res = success(v); },
                  [&]() { res = fail(); });
        return res;
    }

    void finalize(Value*, Builder& insert);

    bool finalized = false;

    Rir2PirCompiler& compiler;
    rir::Function* srcFunction;

    struct State {
        bool seen = false;
        BB* entryBB = nullptr;
        Opcode* entryPC = 0;

        State() {}
        State(State&&) = default;
        State(const State&) = delete;
        State(const State& other, bool seen, BB* entryBB, Opcode* entryPC)
            : seen(seen), entryBB(entryBB), entryPC(entryPC),
              stack(other.stack){};

        void operator=(const State&) = delete;
        State& operator=(State&&) = default;

        void push(Value* v) { stack.push_back(v); }
        Value* pop() {
            auto v = stack.back();
            stack.pop_back();
            return v;
        }
        Value*& at(unsigned i) { return stack[stack.size() - 1 - i]; }
        Value* at(unsigned i) const { return stack[stack.size() - 1 - i]; }
        Value* top() const { return stack.back(); }
        bool empty() const { return stack.empty(); }
        size_t size() const { return stack.size(); }
        void clear() {
            stack.clear();
            entryBB = nullptr;
            entryPC = nullptr;
        }

        void mergeIn(const State& incom, BB* incomBB);
        void createMergepoint(Builder&);

        std::deque<Value*> stack;
    };

    bool interpret(BC bc, Opcode* pos, rir::Code* srcCode, State&,
                   Builder&) const;
    std::unordered_set<Opcode*> findMergepoints(rir::Code*) const;
};

} // namespace pir
} // namespace rir

#endif
