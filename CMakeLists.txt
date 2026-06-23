cmake_minimum_required(VERSION 3.12)
project(headerlisp LANGUAGES CXX)

include(CTest)

option(ASAN "Build examples and tests with AddressSanitizer" OFF)

add_library(headerlisp INTERFACE)
target_include_directories(headerlisp INTERFACE "${CMAKE_CURRENT_SOURCE_DIR}")
target_compile_features(headerlisp INTERFACE cxx_std_17)

if(ASAN)
    target_compile_options(headerlisp INTERFACE -fsanitize=address)
    target_link_libraries(headerlisp INTERFACE -fsanitize=address)
endif()

function(add_headerlisp_example target source)
    add_executable(${target} ${source})
    target_link_libraries(${target} PRIVATE headerlisp)
    target_compile_options(${target} PRIVATE -Wall -Wextra -pedantic -Wshadow -march=native)
endfunction()

add_headerlisp_example(example_interp examples/interp.cpp)
add_headerlisp_example(example_interp_function examples/interp_func.cpp)
add_headerlisp_example(example_processing examples/processing.cpp)
add_headerlisp_example(example_serialize examples/serialize.cpp)
add_headerlisp_example(example_tests examples/tests.cpp)

if(BUILD_TESTING)
    add_test(NAME example_tests COMMAND example_tests)
endif()
