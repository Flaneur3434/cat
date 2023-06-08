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

# Create the executable
add_executable(cat-cc cat-cc.cpp)
add_executable(cat-grammar-analysis cat-grammar-analysis.cpp)
add_executable(cat-grammar-tracer cat-grammar-tracer.cpp)

# Link ICU library for unicode support
find_package(ICU REQUIRED COMPONENTS uc)

target_link_libraries(cat-cc ICU::uc)
target_link_libraries(cat-grammar-analysis ICU::uc)
target_link_libraries(cat-grammar-tracer ICU::uc)

# Add PEG library
add_subdirectory(PEGTL)

# Add the include directories for the PEG library
include_directories(PEGTL/include)

# Link the PEGTL library to the executable
target_link_libraries(cat-cc taocpp::pegtl)
target_link_libraries(cat-grammar-analysis taocpp::pegtl)
target_link_libraries(cat-grammar-tracer taocpp::pegtl)

# Set the output directory for the executable
set_target_properties(cat-cc PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${BUILD_DIR})

# Compiler flags for this target
add_compile_options(-std=c++20)
target_compile_options(cat-cc PRIVATE -Wall -Wextra -Wpedantic -Wconversion -Wshadow -Wnon-virtual-dtor -Wnull-dereference -Wuninitialized -Wuninitialized -Wstrict-aliasing -Wpessimizing-move -Wredundant-move)
