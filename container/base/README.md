## What

This Dockerfile builds the base container with all dependencies preinstalled to compile and run R.

## How

To update the container at "registry.gitlab.com/rirvm/rir_mirror/base"

First

    docker login registry.gitlab.com

then just

    ./update.sh
