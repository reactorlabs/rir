ARG CI_COMMIT_SHA
FROM registry.gitlab.com/rirvm/rir_mirror:$CI_COMMIT_SHA
RUN git clone --depth 1 https://github.com/reactorlabs/ReBench.git -b envVarsSupport /opt/ReBench && cd /opt/ReBench && pip install .
RUN git clone --depth 10 https://github.com/reactorlabs/rbenchmarking /opt/rbenchmarking && cd /opt/rbenchmarking && git checkout de9b7b8c3122d6089b8c6e5d86e34e110de85e90
