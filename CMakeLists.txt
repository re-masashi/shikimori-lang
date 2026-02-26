cmake_minimum_required(VERSION 3.20)
project(shikimori-lang VERSION 0.1.0 LANGUAGES CXX)

set(CMAKE_C_COMPILER "clang")
set(CMAKE_CXX_COMPILER "clang++")
set(CMAKE_EXE_LINKER_FLAGS "-fuse-ld=mold")

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# find_program(CLANG_TIDY_EXE NAMES clang-tidy PATHS /usr/bin)
# if(CLANG_TIDY_EXE)
#     set(CMAKE_CXX_CLANG_TIDY "${CLANG_TIDY_EXE}" "--quiet")
# endif()
#
option(SANITIZE "Enable Address and Undefined Sanitizers" ON)
option(BUILD_TESTS "Build the tests" ON)

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    add_compile_options(-Wall -Wextra -Wpedantic -g)
else()
  # add_compile_options(-Wall -Wextra -Wpedantic -O3)
  add_compile_options(-Wall -Wextra -Wpedantic -g)
endif()

add_executable(shikimori
  src/main.cpp
  src/parser/tokenizer.cpp
  src/parser/parser.cpp
  src/parser/parser_decl.cpp
  src/parser/parser_stmt.cpp
  src/parser/parser_expr.cpp
  src/ast/ast_printer.cpp
  src/span.cpp
)

target_include_directories(shikimori PRIVATE
    ${PROJECT_SOURCE_DIR}/src
)
