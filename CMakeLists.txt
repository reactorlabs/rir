cmake_minimum_required(VERSION 2.8.8)

project(rjit)

set(CODEGEN_ARGS "noformat")

# first look for any local configuration, and include it, if found
include(${CMAKE_SOURCE_DIR}/local/cmake.cmake OPTIONAL)

# set LLVM

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
add_definitions(--std=c++11 -Wall -Werror)

# set R

set(R_LIBRARY_TREE ${CMAKE_SOURCE_DIR}/packages)
set(R_ROOT_DIR ${R_HOME})
set(R_INCLUDE_DIR ${R_HOME}/include)
set(R_COMMAND ${R_HOME}/bin/R)
include_directories(${R_INCLUDE_DIR})
include_directories(${CMAKE_SOURCE_DIR}/rjit/src)
message(STATUS "Found R ${R_ROOT_DIR}")

set(CMAKE_CXX_FLAGS_RELEASE -O2)

file(WRITE ${CMAKE_SOURCE_DIR}/.local.config  "BUILD_DIR=${CMAKE_BINARY_DIR}\nROOT_DIR=${CMAKE_SOURCE_DIR}\nR_HOME=${R_HOME}\nLLVM_CMAKE=${LLVM_DIR}\nTESTR_DIR=${TESTR_DIR}")

if (APPLE) 
    find_library(libr 
                NAMES libR.dylib
                PATHS ${R_ROOT_DIR}/lib
                )
    message(STATUS "libR is found at ${libr}")
endif(APPLE)

set(MAKEVARS_SRC "SOURCES = $(wildcard *.cpp ir/*.cpp passes/codegen/*.cpp)\nOBJECTS = $(SOURCES:.cpp=.o)")
set(LLVM_COMPONENTS_USED support core mcjit native irreader linker ipo)


function(JOIN VALUES GLUE OUTPUT)
    string (REPLACE ";" "${GLUE}" _TMP_STR "${VALUES}")
    set (${OUTPUT} "${_TMP_STR}" PARENT_SCOPE)
endfunction()
JOIN("${LLVM_COMPONENTS_USED}" " " LLVM_COMPONENTS_USED_STRING)
set(LLVM_CONFIG_BIN ${LLVM_TOOLS_BINARY_DIR}/llvm-config)

file(WRITE ${CMAKE_SOURCE_DIR}/rjit/src/Makevars  "${MAKEVARS_SRC}\nPKG_CXXFLAGS = `${LLVM_CONFIG_BIN} --cxxflags | sed 's/-Wcovered-switch-default//' | sed 's/-fcolor-diagnostics//'` -UNDEBUG -I.\nPKG_LIBS = `${LLVM_CONFIG_BIN} --ldflags --system-libs --libs ${LLVM_COMPONENTS_USED_STRING}`\n")

execute_process(COMMAND ${LLVM_CONFIG_BIN} --cxxflags
    OUTPUT_VARIABLE LLVM_CXX_FLAGS)
file(WRITE ${CMAKE_SOURCE_DIR}/.clang_complete "-I${CMAKE_SOURCE_DIR}/rjit/src\n${LLVM_CXX_FLAGS}--std=c++11\n-Wall\n")

# code generation
add_custom_target(codegen
    DEPENDS codegen_passes)
add_custom_target(codegen_passes_doxygen
    COMMAND doxygen rjit/Doxyfile WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
add_custom_target(codegen_passes_dir
    DEPENDS codegen_passes_doxygen
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_SOURCE_DIR}/rjit/src/passes/codegen)
add_custom_target(codegen_passes
    DEPENDS codegen_passes_dir
    COMMAND python3 tools/codegen_passes.py xml rjit/src/passes/codegen ${CODEGEN_ARGS} WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)

# build the shared library for the JIT
file(GLOB_RECURSE SRC "rjit/src/*.cpp" "rjit/src/*.h" "local/*" "rjit/R/*.R" "rjit/tests/*.R")
add_library(${PROJECT_NAME} SHARED ${SRC})

add_custom_target(codegen_was_run_before
    COMMAND ls ${CMAKE_SOURCE_DIR}/rjit/src/passes/codegen/* || $(echo && echo && echo "Error" && echo "Please run codegen and then cmake again before the first build" && echo && echo && exit 1))

#codegen is a dependency
add_dependencies(${PROJECT_NAME} codegen_was_run_before)
add_dependencies(${PROJECT_NAME} codegen)

add_dependencies(${PROJECT_NAME} check_gnur)
add_custom_target(check_gnur)
add_custom_command(TARGET check_gnur
    COMMAND ${CMAKE_SOURCE_DIR}/tools/check_gnur)

add_custom_target(tests
    DEPENDS ${PROJECT_NAME})
add_custom_command(TARGET tests
    COMMAND ${CMAKE_SOURCE_DIR}/tools/tests)

add_custom_target(integration_tests
    DEPENDS ${PROJECT_NAME})
add_custom_command(TARGET integration_tests
    COMMAND ${CMAKE_SOURCE_DIR}/tools/integration_tests.sh)

add_custom_target(gnur_tests
    DEPENDS ${PROJECT_NAME}
    DEPENDS package_install)
add_custom_command(TARGET gnur_tests
    COMMAND ${CMAKE_SOURCE_DIR}/tools/gnur_tests.sh)

add_custom_target(testr_tests
    DEPENDS ${PROJECT_NAME})
add_custom_command(TARGET testr_tests
    COMMAND ${CMAKE_SOURCE_DIR}/tools/testr_tests.sh)

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs ${LLVM_COMPONENTS_USED})

# Link against LLVM libraries
if(libr)
    target_link_libraries(${PROJECT_NAME} ${llvm_libs} ${libr})
else(libr)
    target_link_libraries(${PROJECT_NAME} ${llvm_libs})
endif(libr)
# now creating the package

set(PACKAGE_NAME "rjit_0.1.tar.gz")
add_custom_target(package
    COMMAND ${R_COMMAND} CMD build rjit WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
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


add_custom_target(distclean
    DEPENDS clean
    COMMAND rm -rf ${R_HOME}/library/rjit
    COMMAND rm -rf ${R_LIBRARY_TREE}/rjit
    COMMAND rm -rf ${CMAKE_SOURCE_DIR}/rjit/src/passes/codegen
    COMMAND rm -rf ${CMAKE_SOURCE_DIR}/rjit/src/ir/intrinsics.h
    COMMAND rm -rf ${CMAKE_SOURCE_DIR}/xml
    COMMAND rm -rf ${CMAKE_SOURCE_DIR}/doxygen_sqlite3.db
    COMMAND rm -rf ${CMAKE_SOURCE_DIR}/index.db
    COMMAND rm -rf ${CMAKE_SOURCE_DIR}/packages/rjit
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


# Ubuntu comments
#
# llvm cmake files are not where they are expected by the find_package script. A symlink from /usr/share/llvm-3.6/cmake to /usr/lib/llvm-3.6/share/llvm/cmake has to be created
