# Recordless Optimization Tests

Tests for the recordless type-feedback optimization in RIR.

## Running Tests

```bash
cd /home/skrynski/rsh-recordLess
./build/release/bin/R --slave --no-init-file -f tests/recordless/test_norecord.R
```

## Test Cases

### test_norecord.R

**Topic:** NoRecord optimization  
**Scenario:** Variable assigned from a recorded operation, then read once afterward  
**Expected:** The read does not produce a new `record_type` opcode; instead it uses `ldvar_cached_noRecordFB_` and creates a NoRecord dependency on the def's type slot.

**Bytecode pattern to verify:**
```
Type#0 (record_type_)          ← f() call result (assignment source)
ldvar_cached_noRecordFB_ x{0}  ← x read: NO Type# record after this
NoRecord Type#1 (dep: #0)      ← dependency on the def
```

**Why it matters:** The NoRecord optimization eliminates redundant recording when the type is already known from a prior def. This is the foundation of the recordless system.

---

## Planned Tests

- [ ] `test_opaque_result.R` — recordTypeOpaqueResult isolates subscript/call operands
- [ ] `test_promise_attribution.R` — promise loads don't leak into enclosing expression tree
- [ ] `test_builtin_speculate.R` — dual-path builtin speculation and its effect on classifications
- [ ] `test_recordonce_loop.R` — RecordOnce gate in loops with stable outer variables
