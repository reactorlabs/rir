## Development Useful Information, Invariants, Assumptions, etc.

### General Wisdom (?)
* For almost all pir instruction, an environment argument means that the environment should be set using rir `set_env_` bytecode. 
Except `mk_env_`that expects the parent environment as a normal argument on the stack and not as the current environment (value of `*env`)

### Speculative Optimizations And Deoptimizations
* The invariant of a deoptimization point is: it is always possible to take it. Soundness relies on that invariant (see "assumption Transparency" in [the sourir paper](https://dl.acm.org/citation.cfm?doid=3177123.3158137))

### SSA And CFG Invariants and Design
...
