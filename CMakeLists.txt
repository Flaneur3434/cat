cmake_minimum_required(VERSION 3.0)
set(CMAKE_CXX_COMPILER "/usr/bin/g++")

# Export the compilation commands to a file
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

project(cat)

# Create the build directory
set(BUILD_DIR ${CMAKE_CURRENT_SOURCE_DIR}/build)
if(NOT EXISTS ${BUILD_DIR})
    file(MAKE_DIRECTORY ${BUILD_DIR})
endif()

# Add PEG library
add_subdirectory(PEGTL)

# Add the include directories for the PEG library
include_directories(PEGTL/include)

# Create the executable
add_executable(cat-cc cat-cc.cpp)

# Link the PEGTL library to the executable
target_link_libraries(cat-cc taocpp::pegtl)

# Set the output directory for the executable
set_target_properties(cat-cc PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${BUILD_DIR})

# Compiler flags for this target
add_compile_options(-std=c++20)
target_compile_options(cat-cc PRIVATE -Wall -Wextra -Wpedantic -Wconversion -Wshadow -Wnon-virtual-dtor -Wnull-dereference -Wuninitialized -Wuninitialized -Wstrict-aliasing -Wpessimizing-move -Wredundant-move)