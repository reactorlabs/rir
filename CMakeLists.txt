cmake_minimum_required(VERSION 2.8.8)

project(rir)

# set R
set(R_HOME ${CMAKE_SOURCE_DIR}/external/custom-r)
set(R_LIBRARY_TREE ${CMAKE_SOURCE_DIR}/packages)
set(R_ROOT_DIR ${R_HOME})
set(R_INCLUDE_DIR ${R_HOME}/include)

set(R_COMMAND ${R_HOME}/bin/R)

include_directories(${R_INCLUDE_DIR})
include_directories(${CMAKE_SOURCE_DIR}/rir/src)

add_definitions(-g -fno-omit-frame-pointer)
set(CMAKE_CXX_FLAGS_RELEASE "-O1 -std=c++11")
set(CMAKE_CXX_FLAGS_DEBUG "-std=c++11")
set(CMAKE_CXX_FLAGS "-std=c++11")
set(CMAKE_C_FLAGS_RELEASE "-O1 -std=c99")
set(CMAKE_C_FLAGS_DEBUG "-std=c99")
set(CMAKE_C_FLAGS "-std=c99")


add_custom_target(dependencies
    COMMAND ${CMAKE_SOURCE_DIR}/tools/sync.sh
)

add_custom_target(gnur
    DEPENDS dependencies
    COMMAND cd ${CMAKE_SOURCE_DIR}/external/custom-r && make -j 8
)

add_custom_target(tests
    DEPENDS ${PROJECT_NAME}
    COMMAND ${CMAKE_SOURCE_DIR}/tools/tests
)

add_custom_target(setup
    DEPENDS dependencies
    DEPENDS gnur
)

if (APPLE) 
    find_library(libr 
                NAMES libR.dylib
                PATHS ${R_ROOT_DIR}/lib
                )
    message(STATUS "libR is found at ${libr}")
endif(APPLE)

set(MAKEVARS_SRC "SOURCES = $(wildcard *.cpp ir/*.cpp passes/codegen/*.cpp)\nOBJECTS = $(SOURCES:.cpp=.o)")

# build the shared library for the JIT
file(GLOB_RECURSE SRC "rir/src/*.cpp" "rir/src/*.c" "rir/src/*.h" "local/*" "rir/R/*.R" "rir/tests/*.R")
add_library(${PROJECT_NAME} SHARED ${SRC})

set(PACKAGE_NAME "rir_0.1.tar.gz")
add_custom_target(package
    COMMAND ${R_COMMAND} CMD build rir WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)

add_custom_target(package_check
    DEPENDS package
    COMMAND ${R_COMMAND} CMD check ${PACKAGE_NAME} --no-manual --no-vignettes
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)

add_custom_target(package_install
    DEPENDS package
    COMMAND ${CMAKE_COMMAND} -E make_directory ${R_LIBRARY_TREE} WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMAND ${R_COMMAND} CMD INSTALL -l ${R_LIBRARY_TREE} ${PACKAGE_NAME} WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)


file(GLOB_RECURSE BENCHMARKS "benchmarks/*.R" "benchmarks/*.r")

add_custom_target(benchmarks SOURCES ${BENCHMARKS})

# dummy target so that IDEs show the local folder in solution explorers. The local
# folder is ignored by git and can be used for local scripts and stuff
file(GLOB SCRIPTS "local/*.sh")
add_custom_target(scripts SOURCES ${SCRIPTS})

# dummy target so that IDEs show the tools folder in solution explorers
file(GLOB TOOLS "tools/*.*")
add_custom_target(tools SOURCES ${TOOLS})
