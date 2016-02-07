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

# function to build freshr
# requires freshr directory, version and optimization level
function build_freshr {
    if [ "$1" ]; then
        FRESH_R_DIR=$1
    fi

    if [ "$2" ]; then
        FRESH_R_VERSION=$2
    fi

    if [ "$3" ]; then
        OPT=$3
    fi

    FRESH_R_LOG="${FRESH_R_DIR}/freshr-configure-make-log.txt"

    # create the freshr directory
    if [ ! -d ${FRESH_R_DIR} ]; then
        mkdir ${FRESH_R_DIR}
    fi

    cd ${FRESH_R_DIR}

    FRESH_R_VERS_F=R-${FRESH_R_VERS}-branch
    FRESH_R_SRC=${FRESH_R_DIR}/${FRESH_R_VERS_F}/

    # checkout R-3-2 from svn
    if [ ! -d ${FRESH_R_SRC} ]; then
        echo "-> checking out ${FRESH_R_VERS_F} to ${FRESH_R_SRC}"
        svn co https://svn.r-project.org/R/branches/${FRESH_R_VERS_F}/ ${FRESH_R_SRC}
    fi
    cd ${FRESH_R_SRC}

    # download the set of recommended packages for R-3-2
    echo "-> checking out the recommended packages"
    ./tools/rsync-recommended 

    # configure the make file
    echo "-> building the config file"
    if [ $USING_OSX ]; then
        # Mac OSX
        F77="gfortran -arch x86_64" FC="gfortran -arch x86_64" CXXFLAGS="$OPT -arch x86_64" CFLAGS="$OPT -arch x86_64" ./configure
    else
        ./configure
    fi

    # make the R-3-2
    echo "-> building a fresh copy of ${FRESH_R_VERS_F} to ${FRESH_R_DIR}"
    make 
}
