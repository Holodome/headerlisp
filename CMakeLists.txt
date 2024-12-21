cmake_minimum_required(VERSION 3.10)
project(headerlisp)

add_executable(headerlisp main.cpp)
target_compile_options(headerlisp PUBLIC -std=c++20 -Wall -Wextra -pedantic -Wshadow -march=native)

if (${ASAN})
    target_compile_options(headerlisp PUBLIC -fsanitize=address)
    target_link_libraries(headerlisp PUBLIC -fsanitize=address)
endif()