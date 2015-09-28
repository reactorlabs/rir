cmake_minimum_required(VERSION 2.8.8)

project(rjit)

# set LLVM

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
add_definitions(--std=c++11 -Wall)

# set R

set(R_LIBRARY_TREE ${CMAKE_SOURCE_DIR}/packages)
set(R_ROOT_DIR ${R_HOME})
set(R_INCLUDE_DIR ${R_HOME}/include)
set(R_COMMAND ${R_HOME}/bin/R)
include_directories(${R_INCLUDE_DIR})
include_directories(${CMAKE_SOURCE_DIR}/rjit/src)

# TODO for now, make all builds debug
set(CMAKE_BUILD_TYPE Debug)

file(WRITE ${CMAKE_SOURCE_DIR}/.local.config  "BUILD_DIR=${CMAKE_BINARY_DIR}\nROOT_DIR=${CMAKE_SOURCE_DIR}\nR_HOME=${R_HOME}\nLLVM_CMAKE=${LLVM_DIR}\n")

if (APPLE) 
    find_library(libr 
                NAMES libR.dylib
                PATHS ${R_ROOT_DIR}/lib
                )
    message(STATUS "libR is found at ${libr}")
endif(APPLE)

set(LLVM_COMPONENTS_USED support core mcjit orcjit native irreader linker ipo)


function(JOIN VALUES GLUE OUTPUT)
    string (REPLACE ";" "${GLUE}" _TMP_STR "${VALUES}")
    set (${OUTPUT} "${_TMP_STR}" PARENT_SCOPE)
endfunction()
JOIN("${LLVM_COMPONENTS_USED}" " " LLVM_COMPONENTS_USED_STRING)
set(LLVM_CONFIG_BIN ${LLVM_TOOLS_BINARY_DIR}/llvm-config)
file(WRITE ${CMAKE_SOURCE_DIR}/rjit/src/Makevars  "PKG_CXXFLAGS = `${LLVM_CONFIG_BIN} --cxxflags | sed 's/-Wcovered-switch-default//' | sed 's/-fcolor-diagnostics//'` -UNDEBUG\nPKG_LIBS = `${LLVM_CONFIG_BIN} --ldflags --system-libs --libs ${LLVM_COMPONENTS_USED_STRING}`\n")
execute_process(COMMAND ${LLVM_CONFIG_BIN} --cxxflags
    OUTPUT_VARIABLE LLVM_CXX_FLAGS)
file(WRITE ${CMAKE_SOURCE_DIR}/.clang_complete "-I${CMAKE_SOURCE_DIR}/rjit/src\n${LLVM_CXX_FLAGS}--std=c++11\n-Wall\n")

# code generation
add_custom_target(codegen
    COMMAND python3 intrinsicsGenerator.py ${R_ROOT_DIR}/src/main/eval.c WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/tools
)

# build the shared library for the JIT
file(GLOB_RECURSE SRC "rjit/src/*.cpp" "rjit/src/*.h" "local/.Rprofile" "rjit/R/*.R" "rjit/tests/*.R")
add_library(${PROJECT_NAME} SHARED ${SRC})

add_dependencies(${PROJECT_NAME} codegen)

add_custom_target(tests
    DEPENDS ${PROJECT_NAME})
add_custom_command(TARGET tests
    COMMAND ${CMAKE_SOURCE_DIR}/tools/tests)

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
    COMMAND ${R_COMMAND} CMD check ${PACKAGE_NAME} WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)

add_custom_target(package_install
    DEPENDS package
    COMMAND ${CMAKE_COMMAND} -E make_directory ${R_LIBRARY_TREE} WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMAND ${R_COMMAND} CMD INSTALL -l ${R_LIBRARY_TREE} ${PACKAGE_NAME} WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)

set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES "${R_HOME}/library/rjit;${R_LIBRARY_TREE}/rjit")

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
