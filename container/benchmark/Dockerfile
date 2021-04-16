ARG CI_COMMIT_SHA
FROM registry.gitlab.com/rirvm/rir_mirror:$CI_COMMIT_SHA
RUN git clone --depth 1 https://github.com/smarr/ReBench.git /opt/ReBench && cd /opt/ReBench && pip3 install .
# this thing does not work...
RUN mv /usr/local/bin/rebench-denoise /usr/local/bin/rebench-denoise.bkp && cp /usr/bin/false /usr/local/bin/rebench-denoise
RUN git clone https://github.com/reactorlabs/rbenchmarking /opt/rbenchmarking && cd /opt/rbenchmarking && git fetch origin benchmarks-eagerPromsExperiment-mkPromiseTime && git checkout benchmarks-eagerPromsExperiment-mkPromiseTime && git pull && git checkout affe222494e1ac49da48f497f92691389507fddd
