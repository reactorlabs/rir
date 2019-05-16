FROM registry.gitlab.com/rirvm/rir_mirror/base
RUN git clone --depth 1 https://github.com/charig/ReBench.git -b envVarsSupport /opt/ReBench && cd /opt/ReBench && pip install .
RUN git clone --depth 1 https://github.com/reactorlabs/rbenchmarking /opt/rbenchmarking
RUN git clone --recursive https://github.com/reactorlabs/rir /opt/rir && cd /opt/rir && tools/sync.sh && git -C external/custom-r checkout R-3.5.1 && tools/build-gnur.sh custom-r && rm -rf custom-r/cache_recommended.tar custom-r/src .git
