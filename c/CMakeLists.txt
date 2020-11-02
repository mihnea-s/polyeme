cmake_minimum_required(VERSION 3.12)

set(CMAKE_EXPORT_COMPILE_COMMANDS on)

project(Polyeme C)

add_subdirectory(dist)

add_compile_definitions(POLYEME_VERSION="0.1.0")
add_compile_definitions(POLYMER_VERSION="0.1.0")

set(POLYEME           "polyeme")
set(POLYEME_FILES     "source/array.c" "source/parser.c")
set(POLYEME_INCLUDE   "include/")

set(POLYMER_EXE     "polymer")
set(POLYMER_FILES   "polymer/main.c")
set(POLYMER_INCLUDE "include/")

add_library("${POLYEME}" "${POLYEME_FILES}")
target_include_directories("${POLYEME}" PUBLIC "${POLYEME_INCLUDE}" PRIVATE "${DIST_INCLUDE}")
target_link_libraries("${POLYEME}" "${DIST_LIBRARIES}")

add_executable("${POLYMER_EXE}" "${POLYMER_FILES}")
target_include_directories("${POLYMER_EXE}" PRIVATE "${POLYMER_INCLUDE}" "${DIST_INCLUDE}")
target_link_libraries("${POLYMER_EXE}" "${POLYEME}")
