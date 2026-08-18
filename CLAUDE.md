# CLAUDE.md

This repo is a fork of Ř (RIR/PIR), the R JIT built on the RIR bytecode interpreter.

## Recordless

The "recordless" work in this repo (reducing type-feedback recording overhead in
the RIR interpreter) is documented in full in
[recordless-design.md](recordless-design.md) — read it before making changes to
type-feedback recording, the expression-tree leaf/inner-node optimizations, or
force-behavior recording (`rir/src/runtime/TypeFeedback.h`,
`rir/src/bc/Compiler.cpp`, `rir/src/bc/DefUseAnalysis.h`,
`rir/src/interpreter/interp.cpp`, `rir/src/interpreter/record_stats.*`).

That document covers the problem motivation, the core mechanism (leaf
optimizations, inner-node optimizations, and how they're unified), the formal
justification for def-site subsumption, alternatives considered and rejected,
open questions, current implementation status, and benchmark/evaluation notes
— including an important caveat about codegen-artifact noise in this codebase's
benchmarks. It is a living document; check its "Current implementation status"
section for what's stable vs. still in flux. When in doubt , scan the source code instead
of relying on the doc.
