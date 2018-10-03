#ifndef RIR_FUNCTION_HANDLE_H
#define RIR_FUNCTION_HANDLE_H

#include "interpreter/interp.h"
#include "interpreter/runtime.h"
#include "ir/BC_inc.h"

#include "R/Preserve.h"
#include "ir/CodeVerifier.h"
#include "runtime/Function.h"
#include "utils/Pool.h"

#include <algorithm>
#include <iostream>
#include <vector>

namespace rir {

class FunctionWriter {
  private:
    Function* function_;

  public:
    typedef unsigned PcOffset;

    size_t functionSize; // size of the Function, including pointers to its code
                         // objects

    std::vector<SEXP> codeVec;

    Preserve preserve;

    FunctionWriter() : function_(nullptr), functionSize(sizeof(Function)) {}

    ~FunctionWriter() {}

    Function* function() {
        assert(function_ && "FunctionWriter has not been finalized");
        return function_;
    }

    void finalize() {
        assert(function_ == nullptr && "Trying to finalize a second time");

        size_t dataSize = codeVec.size() * sizeof(SEXP);
        assert(functionSize == sizeof(Function) + dataSize);
        assert(functionSize % sizeof(int) == 0);

        SEXP store = Rf_allocVector(EXTERNALSXP, functionSize);
        void* payload = INTEGER(store);
        Function* fun = new (payload) Function(functionSize, codeVec);
        preserve(store);

        assert(fun->info.magic == FUNCTION_MAGIC);

        function_ = fun;
    }

    Code* writeCode(SEXP ast, void* bc, unsigned originalCodeSize,
                    const std::map<PcOffset, BC::PoolIdx>& sources,
                    const std::map<PcOffset, BC::Label>& patchpoints,
                    const std::map<PcOffset, std::vector<BC::Label>>& labels,
                    bool markDefaultArg, size_t localsCnt) {
        assert(function_ == nullptr &&
               "Trying to add more code after finalizing");

        size_t numberOfSources = 0;
        size_t skippedPatchpoints = 0;
        size_t codeSize = 0;

        auto buffer = std::make_unique<Opcode[]>(originalCodeSize);
        auto sourcesBuffer =
            std::make_unique<Code::SrclistEntry[]>(sources.size());

        // Since we are removing instructions from the BC stream, we need to
        // update labels and patchpoint offsets.
        std::vector<PcOffset> updatedLabel2Pos;
        std::unordered_map<PcOffset, BC::Label> updatedPatchpoints;
        {
            Opcode* from = (Opcode*)bc;
            Opcode* to = buffer.get();
            const Opcode* from_start = (Opcode*)bc;
            const Opcode* to_start = buffer.get();
            const Opcode* from_end = from + originalCodeSize;

            // Since those are ordered maps, the elements appear in order. Our
            // strategy is thus, to wait for the next element to show up in the
            // source stream, and transfer them to the updated maps for the
            // target code stream.
            auto source = sources.begin();
            auto patchpoint = patchpoints.begin();
            auto label = labels.begin();

            while (from != from_end) {
                assert(to < to_start + originalCodeSize);

                unsigned bcSize = BC::size(from);
                PcOffset fromOffset = from - from_start;
                PcOffset fromOffsetAfter = fromOffset + bcSize;
                PcOffset toOffset = to - to_start;

                // Look for labels. If we have a label in the 'from' stream,
                // when we will need to note the position of that label in the
                // 'to' stream.
                if (label != labels.end()) {
                    auto nextLabelPos = label->first;
                    assert(nextLabelPos >= fromOffset);
                    if (nextLabelPos == fromOffset) {
                        for (auto labelNr : label->second) {
                            if ((unsigned)labelNr >= updatedLabel2Pos.size())
                                updatedLabel2Pos.resize(labelNr + 1, -1);
                            updatedLabel2Pos[labelNr] = toOffset;
                        }
                        label++;
                    }
                }

                // We skip nops (and maybe potentially other instructions)
                if (*from == Opcode::nop_) {
                    from++;
                    continue;
                }

                if (*from == Opcode::br_) {
                    auto t = patchpoints.find(fromOffset + 1)->second;
                    if (labels.count(fromOffsetAfter)) {
                        auto v = labels.at(fromOffsetAfter);
                        if (std::find(v.begin(), v.end(), t) != v.end()) {
                            patchpoint++;
                            skippedPatchpoints++;
                            from += bcSize;
                            continue;
                        }
                    }
                }

                // Copy the bytecode from 'from' to 'to'
                memcpy(to, from, bcSize);

                // The code stream stores sources after the instruction, but in
                // the BC we actually need the index before the instruction.
                // If the current BC in the code stream has a source attached,
                // we add it to the sources list of the code object.
                if (source != sources.end()) {
                    assert(source->first >= fromOffsetAfter);
                    if (source->first == fromOffsetAfter) {
                        sourcesBuffer[numberOfSources].pcOffset = toOffset;
                        sourcesBuffer[numberOfSources].srcIdx = source->second;
                        numberOfSources++;
                        source++;
                    }
                }

                // Patchpoints can occur anywhere within BCs. If there is a
                // patchpoint in the 'from' BC, we need to update it, such
                // that it references the correct place in the 'to' BC.
                if (patchpoint != patchpoints.end()) {
                    auto patchpointPos = patchpoint->first;
                    assert(patchpointPos >= fromOffset);
                    auto patchpointDistance = patchpointPos - fromOffset;
                    if (patchpointDistance < bcSize) {
                        updatedPatchpoints[toOffset + patchpointDistance] =
                            patchpoint->second;
                        patchpoint++;
                    }
                }

                from += bcSize;
                to += bcSize;
            }
            codeSize = to - to_start;
        }
        assert(patchpoints.size() ==
               updatedPatchpoints.size() + skippedPatchpoints);
        assert(sources.size() == numberOfSources);

        // Patch jumps with actual offset in bytes
        for (auto p : updatedPatchpoints) {
            unsigned pos = p.first;
            unsigned labelNr = p.second;
            assert(labelNr < updatedLabel2Pos.size() &&
                   "Jump to missing label");
            unsigned target = updatedLabel2Pos[labelNr];
            assert(target != (unsigned)-1 && "Jump to missing label");
            BC::Jmp j = target - pos - sizeof(BC::Jmp);
            *(BC::Jmp*)((uintptr_t)buffer.get() + pos) = j;
        }

        unsigned totalSize = Code::size(codeSize, sources.size());
        size_t index = codeVec.size();

        SEXP store = Rf_allocVector(EXTERNALSXP, totalSize);
        void* payload = INTEGER(store);
        Code* code =
            new (payload) Code(nullptr, index, ast, codeSize, sources.size(),
                               markDefaultArg, localsCnt);
        preserve(store);
        codeVec.push_back(store);
        functionSize += sizeof(SEXP);

        auto to = code->code();
        memcpy(to, buffer.get(), codeSize);
        memcpy(to + pad4(codeSize), sourcesBuffer.get(),
               sizeof(Code::SrclistEntry) * sources.size());

        return code;
    }
};
}

#endif
