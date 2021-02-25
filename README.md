# Ř

To give it a spin try

    docker run -it registry.gitlab.com/rirvm/rir_mirror:master /opt/rir/build/release/bin/R

## Building from Source

This is currently only tested on Ubuntu.

    git clone https://github.com/reactorlabs/rir
    cd rir
    cmake -DCMAKE_BUILD_TYPE=build_type .    # possible build_types: release, debugopt, debug
    # Fetch and build dependencies. This will build gnur from source, which takes a while.
    # To build llvm from source (for debug symbols) set the following. This can take a long time to build.
    # export BUILD_LLVM_FROM_SRC=1
    make setup
    # Run cmake again, this should now say "Found LLVM 11"
    cmake .
    make

Now run Ř with

    bin/R

If you want to develop Ř then it is highly recommended to use off-tree builds and ninja instead of make. See further down (Hacking).

### macOS

macOS has some extra prerequisites:

- A Fortran compiler (e.g. `brew install gcc`)
- Xcode Command line tools

## Running tests

To run the basic test, just execute

    bin/tests

To run tests from gnur with rir enabled as a jit:

    bin/gnur-make check-devel

## Are we fast?

Check out our [performance dashboard](https://speed.r-vm.net) to see how we compare to GNU R and FastR in terms of performance.
Select two jobs to compare against, to compare against GNU R, or FastR select `all` first.
We periodically [benchmark](documentation/benchmarking.md) the performance of the optimizer.

## PIR optimizer

The optimizer kicks in before the 3rd execution of a function (depending on R's bytecode-compile heuristic also 4th sometimes).

To print intermediate debug information set the `PIR_DEBUG` environment variable to a comma separated list of flags.
For instance to show all functions that are optimized use:

    PIR_DEBUG=PrintPirAfterOpt bin/R

## Hacking

To make changes to this repository please open a pull request. Ask somebody to
review your changes and make sure [CI](https://gitlab.com/rirvm/rir_mirror/pipelines) is green.

Caveat: we use submodules. If you are in the habit of blindly `git commit .` you are up for surprises. Please make sure that you do not by accident commit an updated submodule reference for external/custom-r.

### Off-Tree builds

It is highly recommended to use off-tree builds for hacking Ř.
You can have multiple builds at the same time.
Follow the build instructions as above, but run cmake in a separate directory.
For instance you can create a sub-directory called `build`, cd into that
directory, then replace `.` with `..` in all the cmake commands from the build instructions.
This will build Ř in the `build` directory. You can then have multiple build directories, with different build types.

### Building with ninja

For faster build use ninja. To generate ninja files instead of makefiles add `-GNinja` to the cmake commands.

Using ninja means GCC and Clang will disable color output. To force color, run cmake with `-DFORCE_COLORED_OUTPUT=true`.

### LLVM backend

If you need to debug issues in the llvm backend then it is useful to run `make setup` with `BUILD_LLVM_FROM_SRC=1` set. This will give you debug symbols for LLVM, which unfortunately increases linking time. To switch between source and prebuilt LLVM you can switch the `llvm-11` symlink between `llvm-11-build` and `clang+llvm...ubuntu-...`. After the switch a `make clean` is needed.

Assertions in native code are disabled in release builds.

If there are any issues with LLVM includes, you can `rm -rf external/llvm-11*` and then run `make setup` again.

### Building on macOS with GCC 9

By default macOS will build gnuR and rir with clang, while Linux always uses GCC. There might be some differences between the 2 compilers. However, you can force macOS to use GCC via the following:

- Install GCC 9 with homebrew (`brew install gcc@9`)
- Pass `-GMACOS_USE_GCC_9` to `cmake`

### Making changes to gnur

R with Ř patches is a submodule under external/custom-r. This is how you edit:

    # Assuming you are making changes in you local Ř branch
    cd external/custom-r
    # By default submodules are checked out headless. We use a
    # branch to keep track of our changes to R, that is based on
    # one of the R version branches. If you want to make changes
    # you have to make sure to be on that branch locally, before
    # creating commits.
    git checkout R-3.5.1-rir-patch
    git pull origin R-3.5.1-rir-patch
    # edit some stuff ... 
    git commit
    git push origin R-3.5.1-rir-patch
    cd ../..
    # now the updated submodule needs to be commited to rir 
    git commit external/custom-r -m "bump R module version"
    git push my-rir-remote my-rir-feature-branch
    # Now you can create a PR with the R changes & potential Ř 
    # changes in my-feature-branch

If you want to test your R changes on ci, before pushing to the main branch on the gnur repository you can also push to a feature branch on gnur first. E.g.:

    git checkout -b my-rir-feature-branch
    cd external/custom-r
    git checkout -b my-gnur-feature-branch
    # edit and commit. Need to push, or ci will not be able to access the submodule reference
    git push origin my-gnur-feature-branch
    cd ../..
    git commit external/custom-r -m "temp module version"
    git push my-rir-remote my-rir-feature-branch

    # Review....
    # Now, with ci green, before merging, change it back:

    cd external/custom-r
    git checkout R-3.5.1-rir-patch
    git pull origin R-3.5.1-rir-patch
    git merge --fast-forward-only my-gnur-feature-branch
    git push origin R-3.5.1-rir-patch
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
