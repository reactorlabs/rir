FROM registry.gitlab.com/rirvm/rir_mirror/base
ARG CI_COMMIT_SHA
ADD . /opt/rir
RUN echo $CI_COMMIT_SHA > /opt/rir_version
RUN cd /opt/rir && tools/sync.sh && tools/build-gnur.sh custom-r && rm -rf external/custom-r/cache_recommended.tar .git && find external -type f -name '*.o' -exec rm -f {} \;
RUN mkdir -p /opt/rir/build/release      && cd /opt/rir/build/release      && cmake -DCMAKE_BUILD_TYPE=release      -GNinja ../.. && ninja && rm -rf build/*/CMakeFiles
RUN mkdir -p /opt/rir/build/fullverifier && cd /opt/rir/build/fullverifier && cmake -DCMAKE_BUILD_TYPE=fullverifier -GNinja ../.. && ninja && rm -rf build/*/CMakeFiles
RUN mkdir -p /opt/rir/build/debugopt     && cd /opt/rir/build/debugopt     && cmake -DCMAKE_BUILD_TYPE=debugopt     -GNinja ../.. && ninja && rm -rf build/*/CMakeFiles
RUN mkdir -p /opt/rir/build/sanitize     && cd /opt/rir/build/sanitize     && cmake -DCMAKE_BUILD_TYPE=sanitize     -GNinja ../.. && ninja && rm -rf build/*/CMakeFiles
