FROM ubuntu:20.04
ARG CI_COMMIT_SHA
ADD . /opt/rir
RUN echo $CI_COMMIT_SHA > /opt/rir_version && \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get upgrade -y -qq && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y -qq curl git gcc gfortran g++ libreadline-dev libx11-dev libxt-dev zlib1g-dev libbz2-dev liblzma-dev libpcre3-dev libcurl4-openssl-dev libcairo2-dev make cmake libreadline8 && \
    cd /opt/rir && \
    tools/build-gnur.sh && \
    rm -rf external/custom-r/cache_recommended.tar .git && \
    find external -type f -name '*.o' -exec rm -f {} \; && \
    find external -type f -name '*.tar.gz' -exec rm -f {} \; && \
    find external -type f -name '*.tar.xz' -exec rm -f {} \; && \
    apt-get remove -y -qq  libreadline-dev libx11-dev libxt-dev zlib1g-dev libbz2-dev liblzma-dev libpcre3-dev libcurl4-openssl-dev libcairo2-dev && \
    apt-get autoremove -y -qq
RUN mkdir -p /opt/rir/build/release && \
    cd /opt/rir && \
    (curl 10.200.14.25:8080/clang+llvm-12.0.0-x86_64-linux-gnu-ubuntu-20.04.tar.xz > external/clang+llvm-12.0.0-x86_64-linux-gnu-ubuntu-20.04.tar.xz || true) && \
    tools/fetch-llvm.sh && \
    cd /opt/rir/build/release && \
    cmake -DCMAKE_BUILD_TYPE=release ../.. && \
    make -j4 && \
    rm -rf CMakeFiles external/clang+llvm*
