FROM registry.gitlab.com/rirvm/rir_mirror/base
ARG CI_COMMIT_SHA
ADD . /opt/rir
RUN echo $CI_COMMIT_SHA > /opt/rir_version && apt-get update && apt-get install -y lsb-release
RUN cd /opt/rir && (curl 10.200.14.25:8080/clang+llvm-11.0.0-x86_64-linux-gnu-ubuntu-20.04.tar.xz > external/clang+llvm-11.0.0-x86_64-linux-gnu-ubuntu-20.04.tar.xz || true) && tools/sync.sh && tools/build-gnur.sh custom-r && rm -rf external/custom-r/cache_recommended.tar .git && find external -type f -name '*.o' -exec rm -f {} \; &&\
    mkdir -p /opt/rir/build/release      && cd /opt/rir/build/release      && cmake -DCMAKE_BUILD_TYPE=release      -GNinja ../.. && ninja && bin/tests && \
    mkdir -p /opt/rir/build/releaseassert && cd /opt/rir/build/releaseassert && cmake -DCMAKE_BUILD_TYPE=releaseslowassert -GNinja ../.. && ninja && \
    rm -rf /opt/rir/external/libjit /opt/rir/external/clang+llvm-* /opt/rir/external/*.tar.xz /opt/rir/build/*/CMakeFiles /opt/rir/external/custom-r/src/main
