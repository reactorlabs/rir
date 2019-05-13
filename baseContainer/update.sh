#!/bin/sh

docker build -t registry.gitlab.com/rirvm/rir_mirror:base .
docker push registry.gitlab.com/rirvm/rir_mirror:base
