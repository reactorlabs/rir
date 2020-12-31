FROM registry.gitlab.com/rirvm/rir_mirror/base

ARG GRAAL_VERSION=20.3.0

RUN git clone --depth 1 https://github.com/smarr/ReBench.git /opt/ReBench && cd /opt/ReBench && pip3 install .
# this thing does not work...
RUN mv /usr/local/bin/rebench-denoise /usr/local/bin/rebench-denoise.bkp && cp /usr/bin/false /usr/local/bin/rebench-denoise
RUN git clone --depth 10 https://github.com/reactorlabs/rbenchmarking /opt/rbenchmarking && cd /opt/rbenchmarking && git checkout 95e5ee7d292f8978b5b06679c06c7839554313bd
RUN git clone --recursive https://github.com/reactorlabs/rir /opt/rir && cd /opt/rir && tools/sync.sh && git -C external/custom-r checkout R-3-6-2-branch && tools/build-gnur.sh custom-r && rm -rf custom-r/cache_recommended.tar custom-r/src .git
RUN curl --fail --silent --location --retry 3 https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-$GRAAL_VERSION/graalvm-ce-java11-linux-amd64-$GRAAL_VERSION.tar.gz | gunzip | tar x -C /opt/ && cd /opt && ln -s graalvm-ce-java11-$GRAAL_VERSION graal && cd /opt/graal/bin && ./gu install R
