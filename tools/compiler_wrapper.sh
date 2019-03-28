#!/bin/bash

# filter annoying messages
"${@}" 2>&1 | grep -v 'note: the ABI of passing union with long double has changed'

# return code of the compiler, rather than grep
exit "${PIPESTATUS[0]}"
