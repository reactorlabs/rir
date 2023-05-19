#include "recording.h"
#include "compiler/pir/module.h"
#include "compiler/pir/pir.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include <memory>
#include <ostream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

extern "C" {
SEXP Rf_deparse1(SEXP, Rboolean, int);
}

namespace rir {
namespace recording {

static std::unordered_map<std::string, FunRecorder> recordings_;

std::string sexp_address(const SEXP s) {
    char* caddress;
    if (asprintf(&caddress, "%p", (void*)s) == -1) {
        Rf_error("Getting address of SEXP failed");
    }

    return caddress;
}

std::string deparse_r_code(const SEXP s) {
    // TODO: are the opts = 0 OK?
    auto r_str = Rf_deparse1(s, FALSE, 0);
    std::ostringstream res;

    for (auto i = 0; i < XLENGTH(r_str); i++) {
        res << CHAR(STRING_ELT(r_str, i));
        res << std::endl;
    }

    return res.str();
}

void record_closure_speculative_context(const Code* code,
                                        std::vector<SpeculativeContext>& ctx) {
    auto end = code->endCode();
    auto pc = code->code();
    while (pc < end) {
        switch (*pc) {
            // TODO: nested functions
            // TODO: promises
        case Opcode::close_: {
            break;
        }
        case Opcode::record_call_: {
            ObservedCallees* feedback = (ObservedCallees*)pc;
            ctx.push_back(*feedback);
            pc += sizeof(ObservedCallees);
            break;
        }
        case Opcode::record_test_: {
            ObservedTest* feedback = (ObservedTest*)pc;
            ctx.push_back(*feedback);
            pc += sizeof(ObservedTest);
            break;
        }
        case Opcode::record_type_: {
            ObservedValues* feedback = (ObservedValues*)pc;
            ctx.push_back(*feedback);
            pc += sizeof(ObservedValues);
            break;
        }
        default: {
            pc = BC::next(pc);
            break;
        }
        }
    }
}

void record_compile(const SEXP cls, const std::string& name) {
    auto address = sexp_address(cls);
    auto r = recordings_.insert({address, FunRecorder{}});
    auto& v = r.first->second;

    if (r.second) {
        v.name = name;
        // TODO: serialize the CLOSXP instead
        v.r_code = deparse_r_code(cls);
    }

    CompilationEvent event;

    auto dt = DispatchTable::unpack(BODY(cls));
    auto fun = dt->baseline();
    auto code = fun->body();
    std::vector<SpeculativeContext> sc;

    record_closure_speculative_context(code, sc);

    event.add_speculative_context(std::move(sc));

    v.events.push_back(std::make_unique<CompilationEvent>(std::move(event)));

    std::cerr << "Compilation " << address << std::endl << v;
}

void record_deopt(const SEXP cls) {
    auto address = sexp_address(cls);
    auto r = recordings_.find(address);
    if (r == recordings_.end()) {
        return;
    }

    auto& v = r->second;
    DeoptEvent event;

    v.events.push_back(std::make_unique<DeoptEvent>(std::move(event)));

    std::cerr << "Deopt " << address << std::endl;
}

void CompilationEvent::print(std::ostream& out) const {
    out << "Compilation" << std::endl;
    out << "context: " << dispatch_context << std::endl;
    for (auto& fun : speculative_contexts) {
        for (auto& ctx : fun) {
            out << "speculative contexts: " << ctx << std::endl;
        }
    }
}

std::ostream& operator<<(std::ostream& out, const FunRecorder& fr) {
    out << "Recording of: " << fr.name << std::endl;
    out << fr.r_code << std::endl;
    out << "Log:" << std::endl;
    for (const auto& e : fr.events) {
        out << *e << std::endl;
    }
    return out;
}

std::ostream& operator<<(std::ostream& out, const Event& e) {
    e.print(out);
    return out;
}

std::ostream& operator<<(std::ostream& out, const SpeculativeContext& ctx) {
    switch (ctx.type) {
    case SpeculativeContextType::Callees:
        out << "callees";
        break;
    case SpeculativeContextType::Test:
        out << "Tests";
        break;
    case SpeculativeContextType::Values:
        out << "values";
        break;
    }
    return out;
}
} // namespace recording
} // namespace rir
