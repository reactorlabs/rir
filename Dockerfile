FROM ubuntu:latest
MAINTAINER "Roman Tsegelskyi" roman.tsegelskyi@gmail.com
RUN apt-get update -qq && apt-get dist-upgrade -y && apt-get install -y --no-install-recommends xdg-utils g++ gcc gfortran subversion cmake git make python r-base-dev liblzma-dev sed binutils curl ruby openjdk-7-jdk doxygen rsync
ENV LLVM_SRC_DIR=/usr/local/llvm/llvm-src-370
ENV LLVM_BUILD_DIR=/usr/local/llvm/llvm-build-370-release
RUN mkdir -p $LLVM_SRC_DIR
RUN mkdir -p $LLVM_BUILD_DIR
RUN svn co http://llvm.org/svn/llvm-project/llvm/tags/RELEASE_370/final/ ${LLVM_SRC_DIR}
RUN cd $LLVM_BUILD_DIR && cmake -G "Unix Makefiles" -DLLVM_ENABLE_RTTI=1 -DLLVM_TARGETS_TO_BUILD="X86;CppBackend" -DCMAKE_BUILD_TYPE=Release --enable-debug-symbols --with-oprofile ${LLVM_SRC_DIR} && make
RUN apt-get update -qq && apt-get dist-upgrade -y && apt-get install -y --no-install-recommends xorg-dev openbox
