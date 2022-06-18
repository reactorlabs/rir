# Ř

## Giving it a spin 

The easiest way to try Ř is using our pre-built docker container

    docker run -it registry.gitlab.com/rirvm/rir_mirror:master /opt/rir/build/release/bin/R

## Building

If you want to build Ř from source, we strongly recommend that you use Ubuntu.
This way, the build scripts can download pre-compiled LLVM binaries instead of compiling LLVM from scratch.
If you're not sure if your OS can benefit from pre-compiled LLVM binaries, check the tools/fetch-llvm.sh script.

Before we can begin, we must install the dependencies.
The optional ninja-build dependency improves the compilation time.
For the R build-dep step you may need to enable source code repositories (deb-src) via GNOME Software or /etc/apt/sources.list.

    sudo apt install build-essential cmake curl
    sudo apt install ninja-build
    sudo apt build-dep r-base

Then, we can proceed with the compilation:

    # Clone this repository
    git clone https://github.com/reactorlabs/rir
    cd rir

    # Create a directory for the build.
    # You can use any name.
    mkdir build
    cd build

    # Invoke cmake for the first time.
    # Possible build types: release, debugopt, debug
    cmake -GNinja -DCMAKE_BUILD_TYPE=release ..

    # Fetch and/or build LLVM and Gnu R.
    # On Ubuntu this downloads pre-compiled LLVM binaries, which takes around 10 minutes.
    # On other systems, or if you set BUILD_LLVM_FROM_SRC, this takes a very long time.
    ninja setup

    # Now we can build Ř itself.
    # The cmake should output "Found LLVM 12"
    cmake ..
    ninja

Congratulations! You can now run Ř with

    bin/R

### make vs ninja

If you prefer to use `make` instead of `ninja`, remove the `-GNinja` flag when you call `cmake`.
Then use `make` in all the places where we told you to use `ninja`.

## Running tests

To run the basic Ř tests, execute

    bin/tests

To run tests from GNU R with Ř enabled as a JIT:

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

Caveat: we use submodules. If you are in the habit of blindly `git commit .` you are up for surprises.
Please make sure that you do not by accident commit an updated submodule reference for external/custom-r.

### LLVM backend

If you need to debug issues in the LLVM backend then it might be useful to build it from source.
This way, you will have debug symbols available.
To build LLVM from source, set the `BUILD_LLVM_FROM_SRC` environment variable before `ninja setup`, as shown below.
However, be aware that compiling LLVM takes a long time and it also increases the linking times. 

    export BUILD_LLVM_FROM_SRC=1
    ninja setup

To switch between source and prebuilt LLVM you can switch the `llvm-12` symlink between `llvm-12-build` and `clang+llvm...ubuntu-...`.
After the switch a `ninja clean` is needed.
If there are any issues with LLVM includes, you can `rm -rf external/llvm-12*` and then run `make setup` again.

Assertions in native code are disabled in release builds.

### Making changes to GNU R

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

Or use `ninja setup`
