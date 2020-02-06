FROM registry.gitlab.com/rirvm/rir_mirror/base

ARG GRAAL_VERSION=1.0.0-rc16

RUN git clone --depth 1 https://github.com/reactorlabs/ReBench.git -b envVarsSupport /opt/ReBench && cd /opt/ReBench && pip install .
RUN git clone --depth 10 https://github.com/reactorlabs/rbenchmarking /opt/rbenchmarking && cd /opt/rbenchmarking && git checkout 8647ab0080c3bd3034ed5c6a3e72917e1fae5e47
RUN git clone --recursive https://github.com/reactorlabs/rir /opt/rir && cd /opt/rir && tools/sync.sh && git -C external/custom-r checkout R-3-6-2-branch && tools/build-gnur.sh custom-r && rm -rf custom-r/cache_recommended.tar custom-r/src .git
RUN curl --fail --silent --location --retry 3 https://github.com/oracle/graal/releases/download/vm-$GRAAL_VERSION/graalvm-ce-$GRAAL_VERSION-linux-amd64.tar.gz | gunzip | tar x -C /opt/ && cd /opt && ln -s graalvm-ce-$GRAAL_VERSION graal && cd /opt/graal/bin && ./gu install R
