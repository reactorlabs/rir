cmake_minimum_required(VERSION 2.8.8)

project(rir)

# set R
set(R_HOME ${CMAKE_SOURCE_DIR}/external/custom-r)
set(R_LIBRARY_TREE ${CMAKE_SOURCE_DIR}/packages)
set(R_ROOT_DIR ${R_HOME})
set(R_INCLUDE_DIR ${R_HOME}/include)

set(R_COMMAND ${R_HOME}/bin/R)

add_definitions(-g3)
set(CMAKE_CXX_FLAGS_RELEASE "-O2 -std=c++11")
set(CMAKE_CXX_FLAGS_DEBUG "-Wall -std=c++11 -DENABLE_SLOWASSERT")
set(CMAKE_CXX_FLAGS "-Wall -std=c++11")
set(CMAKE_C_FLAGS_RELEASE "-O2 -std=gnu99")
set(CMAKE_C_FLAGS_DEBUG "-std=gnu99 -DENABLE_SLOWASSERT")
set(CMAKE_C_FLAGS "-std=gnu99")

add_definitions(-DGNUR_COMPATIBILITY)

if(NOT DEFINED NO_LOCAL_CONFIG)
    #include any local configuration, overriding the default values above
    include(${CMAKE_SOURCE_DIR}/local/cmake.cmake OPTIONAL)
    set(NO_LOCAL_CONFIG 0)
endif(NOT DEFINED NO_LOCAL_CONFIG)
if(${NO_LOCAL_CONFIG})
    message(STATUS "Local configuration files ignored")
endif(${NO_LOCAL_CONFIG})



include_directories(${R_INCLUDE_DIR})
include_directories(${CMAKE_SOURCE_DIR}/rir/src)

message(STATUS "Using R from ${R_HOME}")

add_custom_target(setup-build-dir
    COMMAND ${CMAKE_SOURCE_DIR}/tools/setup-build-dir ${CMAKE_SOURCE_DIR} ${R_HOME}
)

add_custom_target(dependencies
    COMMAND ${CMAKE_SOURCE_DIR}/tools/sync.sh
)

add_custom_target(default-gnur
    DEPENDS dependencies
    COMMAND cd ${CMAKE_SOURCE_DIR}/external/custom-r && make -j 8
)

add_custom_target(setup
    DEPENDS dependencies
    DEPENDS default-gnur
)

add_custom_target(tests
  DEPENDS ${PROJECT_NAME}
  COMMAND ${CMAKE_SOURCE_DIR}/tools/tests
)

set(MAKEVARS_SRC "SOURCES = $(wildcard *.cpp ir/*.cpp passes/codegen/*.cpp)\nOBJECTS = $(SOURCES:.cpp=.o)")

# build the shared library for the JIT
file(GLOB_RECURSE SRC "rir/src/*.cpp" "rir/src/*.c" "rir/*/*.cpp" "rir/src/*.h" "local/*" "rir/R/*.R" "rir/tests/*.R")
add_library(${PROJECT_NAME} SHARED ${SRC})
add_dependencies(${PROJECT_NAME} setup-build-dir)

set(PACKAGE_NAME "rir_0.1.tar.gz")
add_custom_target(rpkg
    COMMAND ${R_COMMAND} CMD build rir WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)

add_custom_target(rpkg_check
    DEPENDS rpkg
    COMMAND ${R_COMMAND} CMD check ${PACKAGE_NAME} --no-manual --no-vignettes
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)

add_custom_target(rpkg_install
    DEPENDS rpkg
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

if(APPLE)
    set_target_properties(${PROJECT_NAME} PROPERTIES LINK_FLAGS "-Lexternal/custom-r/lib")
    target_link_libraries(${PROJECT_NAME} R)
endif(APPLE)
