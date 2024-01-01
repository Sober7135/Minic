# minimum required CMAKE version
CMAKE_MINIMUM_REQUIRED(VERSION 3.28 FATAL_ERROR)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE DEBUG) 
endif(NOT CMAKE_BUILD_TYPE)
 
# compiler must be 17
set(CMAKE_CXX_STANDARD 20)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)


# required if linking to static library
add_definitions(-DANTLR4CPP_STATIC)

# using /MD flag for antlr4_runtime (for Visual C++ compilers only)
set(ANTLR4_WITH_STATIC_CRT OFF)

# Specify the version of the antlr4 library needed for this project.
# By default the latest version of antlr4 will be used.  You can specify a
# specific, stable version by setting a repository tag value or a link
# to a zip file containing the libary source.
# set(ANTLR4_TAG 4.13.1)
# set(ANTLR4_ZIP_REPOSITORY https://github.com/antlr/antlr4/archive/refs/tags/4.13.1.zip)

# add external build for antlrcpp
include(ExternalAntlr4Cpp)
# add antrl4cpp artifacts to project environment
include_directories(${ANTLR4_INCLUDE_DIRS})

# set variable pointing to the antlr tool that supports C++
# this is not required if the jar file can be found under PATH environment
set(ANTLR_EXECUTABLE ${CMAKE_SOURCE_DIR}/thirdparty/antlr/antlr-4.13.1-complete.jar)
# add macros to generate ANTLR Cpp code from grammar
find_package(ANTLR REQUIRED)

# Call macro to add lexer and grammar to your build dependencies.
antlr_target(MinicGrammar ${CMAKE_SOURCE_DIR}/src/Antlr4/Minic.g4 LEXER PARSER VISITOR)
# include generated files in project environment
include_directories(${ANTLR_MinicGrammar_OUTPUT_DIR})

if(LINUX) 
    message(STATUS ">>> Linux")
    set(USE_LLVM /opt/llvm/bin/llvm-config)
    set(LLVM_DIR /opt/llvm/lib/cmake/llvm)
    # set(LLVM_DIR /usr/lib/cmake/llvm)
    message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
    message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
    message(STATUS "LLVM_LIBS = ${LLVM_LIBS}")
else(APPLE)
    message(STATUS ">>> APPLE")
    set(LLVM_DIR /opt/homebrew/opt/llvm/lib/cmake/llvm)
endif()
find_package(LLVM REQUIRED CONFIG)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -Ofast -Wall -Wextra -Wno-unused-parameter")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

file(GLOB_RECURSE SOURCES RELATIVE ${CMAKE_SOURCE_DIR} "src/*.cc" )
add_executable(minic ${SOURCES} ${ANTLR_MinicGrammar_CXX_OUTPUTS})

# Find the libraries that correspond to the LLVM components
# that we wish to use
#https://stackoverflow.com/questions/61188470/what-is-a-correct-way-to-solve-undefined-reference-to-undefined-reference-to-ll
llvm_map_components_to_libnames(llvm_libs core asmprinter ${LLVM_TARGETS_TO_BUILD}) 

# Link against LLVM libraries
target_link_libraries(minic ${llvm_libs} )
target_link_libraries(minic antlr4_static)