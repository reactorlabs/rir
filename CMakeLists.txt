cmake_minimum_required(VERSION 2.8.8)

project(rir)

# set R
set(R_HOME ${CMAKE_SOURCE_DIR}/external/custom-r)
set(R_LIBRARY_TREE ${CMAKE_SOURCE_DIR}/packages)
set(R_ROOT_DIR ${R_HOME})
set(R_INCLUDE_DIR ${R_HOME}/include)
set(LLVM_DIR ${CMAKE_SOURCE_DIR}/external/llvm-12)

set(R_COMMAND ${R_HOME}/bin/R)

include(${LLVM_DIR}/lib/cmake/llvm/LLVMConfig.cmake OPTIONAL)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
set(LLVM_COMPONENTS_USED core orcjit native vectorize lto bitwriter)
option(LLVM_USE_PERF "Include LLVM support for perf profiling." FALSE)
if (${LLVM_USE_PERF})
    set(LLVM_COMPONENTS_USED "${LLVM_COMPONENTS_USED}" PerfJITEvents)
    add_definitions(-DPIR_USE_PERF)
endif ()
include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# use GCC 9 on macOS. Otherwise we use clang
if(APPLE)
    option(MACOS_USE_GCC_9 "Use GCC 9 on macOS." FALSE)
endif(APPLE)

if(${MACOS_USE_GCC_9})
    set(CMAKE_C_COMPILER /usr/local/bin/gcc-9 CACHE PATH "" FORCE)
    set(CMAKE_CXX_COMPILER /usr/local/bin/g++-9 CACHE PATH "" FORCE)
endif()

option(RECORDING "Enable recording of specific events" FALSE)
if(${RECORDING})
    message(STATUS "Enabled recording")
    add_definitions(-DRECORDING)
endif()

add_definitions(-g)
set(CMAKE_CXX_FLAGS_RELEASE "-O2 -Werror -DSWITCH_TO_NAMED=1")
set(CMAKE_CXX_FLAGS_RELEASENOASSERT "${CMAKE_CXX_FLAGS_RELEASE} -DNDEBUG")
set(CMAKE_CXX_FLAGS_FULLVERIFIER "${CMAKE_CXX_FLAGS_RELEASE} -DFULLVERIFIER")
set(CMAKE_CXX_FLAGS_RELEASESLOWASSERT "${CMAKE_CXX_FLAGS_RELEASE} -DENABLE_SLOWASSERT")
set(CMAKE_CXX_FLAGS_DEBUG "-O0 -DSWITCH_TO_NAMED=1 -DENABLE_SLOWASSERT")
set(CMAKE_CXX_FLAGS_DEBUGOPT "-Og -DSWITCH_TO_NAMED=1 -DENABLE_SLOWASSERT")
# with macOS GCC 9 we need to explicitly use libc++, since llvm does. See https://libcxx.llvm.org/docs/UsingLibcxx.html#using-libc-with-gcc
if(${MACOS_USE_GCC_9})
    set(CMAKE_CXX_FLAGS_LIBCXX "-nostdinc++ -nodefaultlibs -lc++ -lc++abi -lm -lc -lgcc_s.1 -lgcc")
else()
    set(CMAKE_CXX_FLAGS_LIBCXX "")
endif()
set(CMAKE_CXX_FLAGS "${LLVM_CXX_FLAGS} ${CMAKE_CXX_FLAGS_LIBCXX} -Wall -Wuninitialized -Wundef -Winit-self -Wcast-align -Woverloaded-virtual -Wmissing-include-dirs -Wstrict-overflow=3 -std=c++14 -fno-rtti -fno-exceptions -Wimplicit-fallthrough -Wno-deprecated-declarations")
set(CMAKE_C_FLAGS_RELEASE "-O2 -DSWITCH_TO_NAMED=1")
set(CMAKE_C_FLAGS_RELEASENOASSERT "${CMAKE_C_FLAGS_RELEASE} -DNDEBUG")
set(CMAKE_C_FLAGS_FULLVERIFIER "${CMAKE_CXX_FLAGS_RELEASE} -DFULLVERIFIER")
set(CMAKE_C_FLAGS_RELEASESLOWASSERT "${CMAKE_CXX_FLAGS_RELEASE} -DENABLE_SLOWASSERT")
set(CMAKE_C_FLAGS_DEBUG "-O0 -DSWITCH_TO_NAMED=1 -DENABLE_SLOWASSERT")
set(CMAKE_C_FLAGS_DEBUGOPT "-Og -DSWITCH_TO_NAMED=1 -DENABLE_SLOWASSERT")
set(CMAKE_C_FLAGS "-std=gnu99")

set(CMAKE_CXX_FLAGS_SANITIZE "${CMAKE_CXX_FLAGS_RELEASE} -g2 -fno-omit-frame-pointer -fsanitize=undefined -fsanitize=address -fsanitize=leak -fno-sanitize=alignment -shared-libasan -fvisibility=default")
set(CMAKE_C_FLAGS_SANITIZE "${CMAKE_C_FLAGS_RELEASE} -g2 -fno-omit-frame-pointer -fsanitize=undefined -fsanitize=address -fsanitize=leak -fno-sanitize=alignment -shared-libasan -fvisibility=default")

MARK_AS_ADVANCED(
    CMAKE_CXX_FLAGS_SANITIZE
    CMAKE_C_FLAGS_SANITIZE
    CMAKE_CXX_FLAGS_RELEASENOASSERT
    CMAKE_C_FLAGS_RELEASENOASSERT
    CMAKE_CXX_FLAGS_FULLVERIFIER
    CMAKE_C_FLAGS_FULLVERIFIER
)

# Currently not needed
# set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${CMAKE_SOURCE_DIR}/tools/compiler_wrapper.sh")

# Update the documentation string of CMAKE_BUILD_TYPE for GUIs
SET( CMAKE_BUILD_TYPE "${CMAKE_BUILD_TYPE}" CACHE STRING
    "Choose the type of build, options are: None Release DebugOpt Debug."
    FORCE )

# Force the Ninja compilation output to be colored.
#
# Due to the way ninja pipes compiler output, by default it has no color.
# To get colors we must manually set some compilation flags. See also:
#  * https://medium.com/@alasher/colored-c-compiler-output-with-ninja-clang-gcc-10bfe7f2b949
#  * https://github.com/ninja-build/ninja/issues/174
execute_process(
    COMMAND /usr/bin/test -t 1
    OUTPUT_FILE /dev/stdout
    RESULT_VARIABLE STDOUT_IS_TERMINAL)
if(STDOUT_IS_TERMINAL EQUAL 0)
    # Looks like we are in an Unix system and that a human invoked cmake from a terminal.
    # Let's make things pretty for them!
    if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
       add_compile_options (-fdiagnostics-color=always)
    elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
       add_compile_options (-fcolor-diagnostics)
    endif ()
endif()

# Create proxy scripts for the scripts in /tools
file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/.bin_create")
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/.bin_create/tests"           "#!/bin/sh\nRIR_BUILD=\"${CMAKE_CURRENT_BINARY_DIR}\" ${CMAKE_SOURCE_DIR}/tools/tests \"$@\"")
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/.bin_create/R"               "#!/bin/sh\nRIR_BUILD=\"${CMAKE_CURRENT_BINARY_DIR}\" ${CMAKE_SOURCE_DIR}/tools/R \"$@\"")
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/.bin_create/Rscript"         "#!/bin/sh\nRIR_BUILD=\"${CMAKE_CURRENT_BINARY_DIR}\" ${CMAKE_SOURCE_DIR}/tools/Rscript \"$@\"")
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/.bin_create/Rgnu"            "#!/bin/sh\n${CMAKE_SOURCE_DIR}/external/custom-r/bin/R \"$@\"")
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
# again we need to explicitly use libc++
if(${MACOS_USE_GCC_9})
    include_directories(/Library/Developer/CommandLineTools/usr/include/c++/v1)
endif(${MACOS_USE_GCC_9})
message(STATUS "Using R from ${R_HOME}")

add_custom_target(setup-build-dir
    COMMAND ${CMAKE_SOURCE_DIR}/tools/setup-build-dir ${CMAKE_SOURCE_DIR} ${R_HOME}
)

if(${MACOS_USE_GCC_9})
    add_custom_target(dependencies
        COMMAND ${CMAKE_SOURCE_DIR}/tools/build-gnur.sh --macos_gcc9
        COMMAND ${CMAKE_SOURCE_DIR}/tools/fetch-llvm.sh --macos_gcc9
    )
else()
    add_custom_target(dependencies
        COMMAND ${CMAKE_SOURCE_DIR}/tools/build-gnur.sh
        COMMAND ${CMAKE_SOURCE_DIR}/tools/fetch-llvm.sh
    )
endif()

add_custom_target(setup
    DEPENDS dependencies
)

add_custom_target(tests
  DEPENDS ${PROJECT_NAME}
  COMMAND ${CMAKE_SOURCE_DIR}/tools/tests
)

set(MAKEVARS_SRC "SOURCES = $(wildcard *.cpp)\nOBJECTS = $(SOURCES:.cpp=.o)")

# suppress macOS warning
if(APPLE)
    set(CMAKE_MACOSX_RPATH 1)
endif(APPLE)

# build the shared library for the JIT
file(GLOB_RECURSE SRC "rir/src/*.cpp" "rir/src/*.c" "rir/*/*.cpp" "rir/src/*.h")
add_library(${PROJECT_NAME} SHARED ${SRC})
add_dependencies(${PROJECT_NAME} setup-build-dir)

# dummy target so that IDEs show the tools folder in solution explorers
add_custom_target(tools SOURCES ${BIN})

if (DEFINED LLVM_PACKAGE_VERSION)
  llvm_map_components_to_libnames(LLVM_LIBS ${LLVM_COMPONENTS_USED})
  target_link_libraries(${PROJECT_NAME} ${LLVM_LIBS})
endif(DEFINED LLVM_PACKAGE_VERSION)

if(APPLE)
    set_target_properties(${PROJECT_NAME} PROPERTIES LINK_FLAGS "-L${R_HOME}/lib")
    target_link_libraries(${PROJECT_NAME} R)
    # to resolve build error from
    # https://www.gnu.org/software/gettext/FAQ.html#integrating_undefined
    target_link_libraries(${PROJECT_NAME} -lintl)
endif(APPLE)

set(CMAKE_EXPORT_COMPILE_COMMANDS on)

