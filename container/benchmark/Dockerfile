ARG CI_COMMIT_SHA
FROM registry.gitlab.com/rirvm/rir_mirror:$CI_COMMIT_SHA
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y -qq python3-pip sudo && \
    apt-get clean && rm -rf /var/cache/apt/lists && \
    git clone --depth 1 https://github.com/smarr/ReBench.git /opt/ReBench && cd /opt/ReBench && pip3 install . && \
    git clone --depth 10 https://github.com/reactorlabs/rbenchmarking /opt/rbenchmarking && cd /opt/rbenchmarking && git checkout f51a2c0d66c7166611807d132440824a582d7a91
