cmake_minimum_required(VERSION 3.10)
project(headerlisp)

if (${ASAN})
target_compile_options(headerlisp PUBLIC -fsanitize=address)
target_link_libraries(headerlisp PUBLIC -fsanitize=address)
endif()

add_executable(example_interp examples/interp.cpp)
target_include_directories(example_interp PUBLIC "${CMAKE_SOURCE_DIR}")
target_compile_options(example_interp PUBLIC -std=c++20 -Wall -Wextra -pedantic -Wshadow -march=native)

add_executable(example_interp_function examples/interp.cpp)
target_include_directories(example_interp_function PUBLIC "${CMAKE_SOURCE_DIR}")
target_compile_options(example_interp_function PUBLIC -std=c++20 -Wall -Wextra -pedantic -Wshadow -march=native)

add_executable(example_processing examples/processing.cpp)
target_include_directories(example_processing PUBLIC "${CMAKE_SOURCE_DIR}")
target_compile_options(example_processing PUBLIC -std=c++20 -Wall -Wextra -pedantic -Wshadow -march=native)

add_executable(example_serialize examples/serialize.cpp)
target_include_directories(example_serialize PUBLIC "${CMAKE_SOURCE_DIR}")
target_compile_options(example_serialize PUBLIC -std=c++20 -Wall -Wextra -pedantic -Wshadow -march=native)

add_executable(example_wire examples/wire.cpp)
target_include_directories(example_wire PUBLIC "${CMAKE_SOURCE_DIR}")
target_compile_options(example_wire PUBLIC -std=c++20 -Wall -Wextra -pedantic -Wshadow -march=native)