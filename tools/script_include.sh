function ncores {
    if [[ "$OSTYPE" == "darwin"* ]]; then
        CORES=`sysctl -n hw.ncpu || echo 8`
    else
        CORES=`nproc || echo 8`
    fi
    echo $CORES
}
