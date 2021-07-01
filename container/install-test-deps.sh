#!/bin/sh
apt-get update
DEBIAN_FRONTEND=noninteractive apt-get -o dir::cache::archives=apt-cache install -y -qq texlive-base xvfb
