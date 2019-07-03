#!/bin/sh

docker build --no-cache -t registry.gitlab.com/rirvm/rir_mirror/benchmark-baseline .
docker push registry.gitlab.com/rirvm/rir_mirror/benchmark-baseline
