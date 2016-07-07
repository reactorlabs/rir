# Testing

Currently I have the following very brittle method of running the gnur tests:

        cd gnur

        # Add a system wide Rprofile which loads rir
        cat - >> library/base/R/Rprofile <<EOF
        dyn.load('~/path/to/rjit/librjit.so')
        sys.source('~/path/to/rjit/rjit/R/rjit.R')
        rir.enableJit()
        EOF

        # reset the tests folder and run the tests
        git clean -fx -e Makefile tests/ && make check
