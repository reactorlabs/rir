ARG CI_COMMIT_SHA
FROM registry.gitlab.com/rirvm/rir_mirror:$CI_COMMIT_SHA
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y -qq python3-pip && \
    apt-get clean && rm -rf /var/cache/apt/lists && \
    git clone --depth 1 https://github.com/smarr/ReBench.git /opt/ReBench && cd /opt/ReBench && pip3 install . && \
    mv /usr/local/bin/rebench-denoise /usr/local/bin/rebench-denoise.bkp && cp /usr/bin/false /usr/local/bin/rebench-denoise && \
    git clone --depth 10 https://github.com/reactorlabs/rbenchmarking /opt/rbenchmarking && cd /opt/rbenchmarking && git checkout 5977a8ab19d193eb4be262cdcd3ba375e5d436fd
