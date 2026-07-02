ARG CI_COMMIT_SHA
FROM registry.gitlab.com/rirvm/rir_mirror:$CI_COMMIT_SHA
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y -qq python3-pip sudo time && \
    apt-get clean && rm -rf /var/cache/apt/lists && \
    git clone https://github.com/smarr/ReBench.git /opt/ReBench && \
    cd /opt/ReBench && \
    git checkout 1aaba13 && \
    pip3 install . && \
    git clone https://github.com/reactorlabs/rbenchmarking /opt/rbenchmarking && cd /opt/rbenchmarking && git checkout 3c6119d16e74b6141dbb4f3a7aa28deb45960dd2
