#!/bin/sh

sed 's/%struct.SEXPREC/SEXP/g' - | \
    sed 's/llvm.experimental.gc.statepoint[^(]*(/statepoint(/g' | \
    sed 's/.*llvm.experimental.gc.relocate.*//g' | \
    sed 's/.*patchpoint.*/  @patchpoint/g' | \
    sed '/^$/d' |  \
    sed 's/\.relocated[0-9]*//g' | \
    sed 's/call [^@]*/call /g' | \
    sed 's/llvm.experimental.gc.result[^(]*(/gc.result(/g' | \
    sed 's/addrspace(1)*//g' | \
    sed 's/@statepoint([^@]*/@statepoint(/g'
