ARG CI_COMMIT_SHA
FROM registry.gitlab.com/rirvm/rir_mirror:$CI_COMMIT_SHA
RUN git clone --depth 1 https://github.com/reactorlabs/ReBench.git -b envVarsSupport /opt/ReBench && cd /opt/ReBench && pip install .
RUN git clone --depth 10 https://github.com/reactorlabs/rbenchmarking /opt/rbenchmarking && cd /opt/rbenchmarking && git checkout 8647ab0080c3bd3034ed5c6a3e72917e1fae5e47
