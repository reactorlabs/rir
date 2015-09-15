cmake_minimum_required(VERSION 2.8.8)

project(rjit)

# set LLVM

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
add_definitions(--std=c++11)

# set R

set(R_HOME ${CMAKE_SOURCE_DIR}/gnur)
set(R_LIBRARY_TREE ${CMAKE_SOURCE_DIR}/packages)
set(R_ROOT_DIR ${R_HOME})
set(R_INCLUDE_DIR ${R_HOME}/include)
set(R_COMMAND ${R_HOME}/bin/R)
include_directories(${R_INCLUDE_DIR})
include_directories(${CMAKE_SOURCE_DIR}/rjit/src)

# TODO for now, make all builds debug
set(CMAKE_BUILD_TYPE Debug)


# build the shared library for the JIT
file(GLOB_RECURSE SRC "rjit/src/*.cpp" "rjit/src/*.h" "local/.Rprofile" "rjit/R/*.R" "rjit/tests/*.R")
add_library(${PROJECT_NAME} SHARED ${SRC})

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs support core mcjit native irreader linker ipo)

# Link against LLVM libraries
target_link_libraries(${PROJECT_NAME} ${llvm_libs})

# now creating the package

set(PACKAGE_NAME "rjit_0.1.tar.gz")
add_custom_target(package
    COMMAND ${R_COMMAND} CMD build rjit WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)

add_custom_target(package_check
    DEPENDS package
    COMMAND ${R_COMMAND} CMD check ${PACKAGE_NAME} WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)

add_custom_target(package_install
    DEPENDS package
    COMMAND ${CMAKE_COMMAND} -E make_directory ${R_LIBRARY_TREE} WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMAND ${R_COMMAND} CMD INSTALL -l ${R_LIBRARY_TREE} ${PACKAGE_NAME} WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)


file(GLOB_RECURSE BENCHMARKS "benchmarks/*.R" "benchmarks/*.r")

add_custom_target(benchmarks SOURCES ${BENCHMARKS})

file(GLOB SCRIPTS "local/*.sh")
add_custom_target(scripts SOURCES ${SCRIPTS})


# Ubuntu comments
#
# llvm cmake files are not where they are expected by the find_package script. A symlink from /usr/share/llvm-3.6/cmake to /usr/lib/llvm-3.6/share/llvm/cmake has to be created
