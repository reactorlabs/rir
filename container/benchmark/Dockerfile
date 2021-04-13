ARG CI_COMMIT_SHA
FROM registry.gitlab.com/rirvm/rir_mirror:$CI_COMMIT_SHA
RUN git clone --depth 1 https://github.com/smarr/ReBench.git /opt/ReBench && cd /opt/ReBench && pip3 install .
# this thing does not work...
RUN mv /usr/local/bin/rebench-denoise /usr/local/bin/rebench-denoise.bkp && cp /usr/bin/false /usr/local/bin/rebench-denoise
RUN git clone --depth 10 https://github.com/reactorlabs/rbenchmarking /opt/rbenchmarking && cd /opt/rbenchmarking && git checkout 88a76410dc70eeff8c9a0cbddaeb472a01510e4f
RUN git clone https://github.com/reactorlabs/rbenchmarking /opt/rbenchmarking && cd /opt/rbenchmarking && git fetch origin benchmarks-eagerPromsExperiment-GC && git checkout benchmarks-eagerPromsExperiment-GC && git pull && git checkout 306008c45e5b16e91d653c296a01243e4587ff42
