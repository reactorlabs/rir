# RIR

# Installation

    git clone https://github.com/reactorlabs/rir
    cd rir
    cmake -DCMAKE_BUILD_TYPE=debug .
    # Fetch and build dependencies. This will build gnur from source, which
    # takes a while. Note that on Mac OSX, you will need to install a fortran
    # compiler (eg. brew install gcc). 
    make setup
    make

## Running tests

To run the basic test, just execute

    bin/tests

To run tests from gnur with rir enabled as a jit:

    bin/gnur-make check-devel

## Playing with RIR

To run R with RIR use

    bin/R

This loads a normal R environment with RIR replacing the R bytecode compiler
and interpreter. If you want to automatically compile functions when they
are executed use

    R_ENABLE_JIT=2 bin/R

Functions compiled to RIR can be inspected using `rir.disassemble`.

## Hacking

To make changes to this repository please open a pull request. Ask somebody to
review your changes and make sure travis is green.

Caveat: we use submodules. If you are in the habit of blindly `git commit .` you are up for surprises. Please make sure that you do not by accident commit an updated submodule reference for external/custom-r.

## PIR optimizer

To try out the PIR optimizer you can use `pir.compile` to optimize a RIR compiled closure.
Or you can pass the environment variable PIR_ENABLE, and set it to 'on' or 'force'.
Those flags will either use the PIR optimizer for hot RIR functions, or always.

To print intermediate debug information, `pir.compile` takes a `debugFlags` argument.
Debug flags can be created using `pir.debugFlags`, for example to debug the register allocator, you could use `pir.compile(f, debugFlags=pir.debugFlags(PrintFinalPir=TRUE,DebugAllocator=TRUE))`.
To change the default debug flags use `pir.setDebugFlags(pir.debugFlags(...))`.

### Off-Tree builds

You can have multiple builds at the same time.
If you want to use that feature, you need to *not* run cmake in the main directory.
Instead do this:

     git clone https://github.com/reactorlabs/rir
     cd rir
     mkdir -p build/debug build/release
     cd build/debug
     cmake -DCMAKE_BUILD_TYPE=debug ../..
     make setup && make
     cd ../release
     cmake -DCMAKE_BUILD_TYPE=release ../..

### Building with ninja

For faster build use ninja. To generate ninja files instead of makefiles add `-GNinja` to the cmake commands.

### Making changes to gnur

R with RIR patches is a submodule under external/custom-r. This is how you edit:

    # Assuming you are making changes in you local RIR branch
    cd external/custom-r
    # By default submodules are checked out headless. We use a
    # branch to keep track of our changes to R, that is based on
    # one of the R version branches. If you want to make changes
    # you have to make sure to be on that branch locally, before
    # creating commits.
    git checkout rir-patch-3-3-branch
    git pull origin rir-patch-3-3-branch
    # edit some stuff ... 
    git commit
    git push origin rir-patch-3-3-branch
    cd ../..
    # now the updated submodule needs to be commited to rir 
    git commit external/custom-r -m "bump R module version"
    git push my-rir-remote my-rir-feature-branch
    # Now you can create a PR with the R changes & potential RIR 
    # changes in my-feature-branch

If you want to test your R changes on travis, before pushing to the main branch on the gnur repository you can also push to a feature branch on gnur first. E.g.:

    git checkout -b my-rir-feature-branch
    cd external/custom-r
    git checkout -b my-gnur-feature-branch
    # edit and commit. Need to push, or travis will not be able to access the submodule reference
    git push origin my-gnur-feature-branch
    cd ../..
    git commit external/custom-r -m "temp module version"
    git push my-rir-remote my-rir-feature-branch

    # Review....
    # Now, with travis green, before merging, change it back:

    cd external/custom-r
    git checkout rir-patch-3-3-branch
    git pull origin rir-patch-3-3-branch
    git merge --fast-forward-only my-gnur-feature-branch
    git push origin rir-patch-3-3-branch
    # delete old branch
    git push origin :my-gnur-feature-branch
    cd ../..
    git commit external/custom-r -m "bump R module version"
    git push my-rir-remote my-rir-feature-branch

    # Merge PR

Fetch updated R:

    git submodule update
    cd external/custom-r && make -j4 

Or use `make setup`

## Build Status

![travis](https://api.travis-ci.org/reactorlabs/rir.svg?branch=master) ([travis](https://travis-ci.org/reactorlabs/rir))

### Debugging a Travis build

Debug mode has been enabled for our repository. To simplify the process, we have
a script `tools/travis-debug.sh`.

To set up, go to your [Travis-CI.org profile](https://travis-ci.org/profile)
(note travis-ci.org, NOT travis-ci.com) and copy your API token. Paste it in
a file `.travis_token` in the repository root.

To use the debug script, first find the job you want to debug, e.g.:
https://travis-ci.org/reactorlabs/rir/jobs/<job id>

Now you can run `tools/travis-debug.sh <job id>` which will POST to the API
endpoint and restart the build. Then you can go back to the job on the Travis
website and use the SSH command to access the machine.

The debug VM will be in a state before the `before_install` phase runs. You will
have to manually run the build phases:

```
travis_run_before_install
travis_run_install
travis_run_before_script
travis_run_script
travis_run_after_success
travis_run_after_failure
travis_run_after_script
```

For more information, see:
https://docs.travis-ci.com/user/running-build-in-debug-mode/
