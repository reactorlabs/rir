cmake_minimum_required(VERSION 2.8.8)

project(rir)

# set R
set(R_HOME ${CMAKE_SOURCE_DIR}/external/custom-r)
set(R_LIBRARY_TREE ${CMAKE_SOURCE_DIR}/packages)
set(R_ROOT_DIR ${R_HOME})
set(R_INCLUDE_DIR ${R_HOME}/include)

set(R_COMMAND ${R_HOME}/bin/R)

add_definitions(-g3)
set(CMAKE_CXX_FLAGS_RELEASE "-O2 -Werror")
set(CMAKE_CXX_FLAGS_DEBUG "-O0 -DENABLE_SLOWASSERT")
set(CMAKE_CXX_FLAGS_DEBUGOPT "-O2 -DENABLE_SLOWASSERT")
set(CMAKE_CXX_FLAGS "-Wall -Wuninitialized -Wundef -Winit-self -Wcast-align -Woverloaded-virtual -Wctor-dtor-privacy -Wmissing-include-dirs -Wstrict-overflow=5 -std=c++14 -fno-rtti")
set(CMAKE_C_FLAGS_RELEASE "-O2")
set(CMAKE_C_FLAGS_DEBUG "-O0 -DENABLE_SLOWASSERT")
set(CMAKE_C_FLAGS_DEBUGOPT "-O2 -DENABLE_SLOWASSERT")
set(CMAKE_C_FLAGS "-std=gnu99")

SET(CMAKE_CXX_FLAGS_SANITIZE "${CMAKE_CXX_FLAGS_DEBUGOPT} -fsanitize=undefined -fno-sanitize=alignment -fsanitize-undefined-trap-on-error -fstack-protector")
SET(CMAKE_C_FLAGS_SANITIZE "${CMAKE_C_FLAGS_DEBUGOPT} -fsanitize=undefined -fno-sanitize=alignment -fsanitize-undefined-trap-on-error -fstack-protector")
MARK_AS_ADVANCED(
    CMAKE_CXX_FLAGS_DEBUGOPT,
    CMAKE_C_FLAGS_DEBUGOPT,
    CMAKE_CXX_FLAGS_SANITIZE
    CMAKE_C_FLAGS_SANITIZE)

# Take from https://medium.com/@alasher/colored-c-compiler-output-with-ninja-clang-gcc-10bfe7f2b949
option(FORCE_COLORED_OUTPUT "Always produce ANSI-colored output (GNU/Clang only)." FALSE)
if (${FORCE_COLORED_OUTPUT})
    if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
       add_compile_options (-fdiagnostics-color=always)
    elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
       add_compile_options (-fcolor-diagnostics)
    endif ()
endif ()

# Create proxy scripts for the scripts in /tools
file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/.bin_create")
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/.bin_create/tests"           "#!/bin/sh\nRIR_BUILD=\"${CMAKE_CURRENT_BINARY_DIR}\" ${CMAKE_SOURCE_DIR}/tools/tests \"$@\"")
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/.bin_create/R"               "#!/bin/sh\nRIR_BUILD=\"${CMAKE_CURRENT_BINARY_DIR}\" ${CMAKE_SOURCE_DIR}/tools/R \"$@\"")
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/.bin_create/Rscript"         "#!/bin/sh\nRIR_BUILD=\"${CMAKE_CURRENT_BINARY_DIR}\" ${CMAKE_SOURCE_DIR}/tools/Rscript \"$@\"")
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/.bin_create/gnur-make"       "#!/bin/sh\nRIR_BUILD=\"${CMAKE_CURRENT_BINARY_DIR}\" ${CMAKE_SOURCE_DIR}/tools/gnur-make \"$@\"")
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/.bin_create/gnur-make-tests" "#!/bin/sh\nRIR_BUILD=\"${CMAKE_CURRENT_BINARY_DIR}\" ${CMAKE_SOURCE_DIR}/tools/gnur-make-tests \"$@\"")

file(GLOB BIN_IN "${CMAKE_CURRENT_BINARY_DIR}/.bin_create/*")
file(INSTALL ${BIN_IN} DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/bin" FILE_PERMISSIONS OWNER_EXECUTE OWNER_READ GROUP_READ GROUP_EXECUTE)
file(GLOB BIN "${CMAKE_CURRENT_BINARY_DIR}/bin/*")

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

add_custom_target(dependencies-vanilla
    COMMAND ${CMAKE_SOURCE_DIR}/tools/sync.sh  --vanilla
)

add_custom_target(vanilla-gnur
    DEPENDS dependencies-vanilla
    COMMAND cd ${CMAKE_SOURCE_DIR}/external/vanilla-r && make -j 8
)

add_custom_target(setup
    DEPENDS dependencies
    DEPENDS default-gnur
)

add_custom_target(tests
  DEPENDS ${PROJECT_NAME}
  COMMAND ${CMAKE_SOURCE_DIR}/tools/tests
)

set(MAKEVARS_SRC "SOURCES = $(wildcard *.cpp)\nOBJECTS = $(SOURCES:.cpp=.o)")

# build the shared library for the JIT
file(GLOB_RECURSE SRC "rir/src/*.cpp" "rir/src/*.c" "rir/*/*.cpp" "rir/src/*.h")
add_library(${PROJECT_NAME} SHARED ${SRC})
add_dependencies(${PROJECT_NAME} setup-build-dir)

# dummy target so that IDEs show the tools folder in solution explorers
add_custom_target(tools SOURCES ${BIN})

if(APPLE)
    set_target_properties(${PROJECT_NAME} PROPERTIES LINK_FLAGS "-L${R_HOME}/lib")
    target_link_libraries(${PROJECT_NAME} R)
endif(APPLE)
