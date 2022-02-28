FROM ubuntu:20.04
ARG GRAAL_VERSION=22.0.0.2

RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get upgrade -y -qq && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y -qq curl git gcc gfortran g++ libreadline-dev libx11-dev libxt-dev zlib1g-dev libbz2-dev liblzma-dev libpcre3-dev libcurl4-openssl-dev libcairo2-dev make libreadline8 libncurses-dev xz-utils cmake python3-pip sudo time && \
    git clone --recursive https://github.com/reactorlabs/rir /opt/rir && cd /opt/rir && \
    DONT_SWITCH_TO_NAMED=1 GNUR_BRANCH=R-4-1-branch tools/build-gnur.sh && \
    rm -rf .git && \
    find external -type f -name '*.o' -exec rm -f {} \; && \
    find external -type f -name '*.tar.gz' -exec rm -f {} \; && \
    find external -type f -name '*.tar.xz' -exec rm -f {} \; && \
    curl --fail --silent --location --retry 3 https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-$GRAAL_VERSION/graalvm-ce-java11-linux-amd64-$GRAAL_VERSION.tar.gz | gunzip | tar x -C /opt/ && \
    cd /opt && ln -s graalvm-ce-java11-$GRAAL_VERSION graal && cd /opt/graal/bin && \
    ./gu install R && \
    git clone --depth 1 https://github.com/smarr/ReBench.git /opt/ReBench && cd /opt/ReBench && pip3 install . && \
    mv /usr/local/bin/rebench-denoise /usr/local/bin/rebench-denoise.bkp && cp /usr/bin/false /usr/local/bin/rebench-denoise && \
    git clone --depth 10 https://github.com/reactorlabs/rbenchmarking /opt/rbenchmarking && cd /opt/rbenchmarking && git checkout a92447b37a03e96f8da1e18eb3cd8ab3b46fbf89 && \
    apt-get clean && rm -rf /var/cache/apt/lists
