#!/bin/sh

docker build -t registry.gitlab.com/rirvm/rir_mirror/benchmark-baseline .
docker push registry.gitlab.com/rirvm/rir_mirror/benchmark-baseline
