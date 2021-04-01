ARG CI_COMMIT_SHA
FROM registry.gitlab.com/rirvm/rir_mirror:$CI_COMMIT_SHA
RUN git clone --depth 1 https://github.com/smarr/ReBench.git /opt/ReBench && cd /opt/ReBench && pip3 install .
# this thing does not work...
RUN mv /usr/local/bin/rebench-denoise /usr/local/bin/rebench-denoise.bkp && cp /usr/bin/false /usr/local/bin/rebench-denoise
RUN git clone https://github.com/reactorlabs/rbenchmarking /opt/rbenchmarking && cd /opt/rbenchmarking && git fetch origin benchmarksForAnnotations && git checkout benchmarksForAnnotations && git pull && git checkout 991c9fe087521cd64906f9425ec65f288ef779cf
