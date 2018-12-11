USING_OSX=0
if [[ "$OSTYPE" == "darwin"* ]]; then
    USING_OSX=1
fi

function ncores {
    if [[ "$OSTYPE" == "darwin"* ]]; then
        CORES=`sysctl -n hw.ncpu || echo 8`
    else
        CORES=`nproc || echo 8`
    fi
    echo $CORES
}

function build {
    (
        DIR=$1
        ROOT_DIR=$2
        TYPE=$3

        # Cmake being stupid cannot build out of tree, when there is already an in-tree build :(
        if [ -f $ROOT_DIR/CMakeCache.txt ]; then
            mv $ROOT_DIR/CMakeCache.txt $ROOT_DIR/CMakeCache.txt.disabled
            trap "mv $ROOT_DIR/CMakeCache.txt.disabled $ROOT_DIR/CMakeCache.txt" EXIT
        fi

        mkdir -p $DIR
        cd $DIR
        cmake $ROOT_DIR -DCMAKE_BUILD_TYPE=$TYPE -DNO_LOCAL_CONFIG=1
        cmake --build . -- -j `ncores`
    )
}
