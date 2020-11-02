cmake_minimum_required(VERSION 3.12)

project(Polyeme CXX)

set(POLYEME_NAME     "polyeme")
set(POLYEME_FILES    "source/polyeme.c++")
set(POLYEME_INCLUDE  "include/")

add_library("${POLYEME_NAME}" "${POLYEME_FILES}")
target_include_directories("${POLYEME_NAME}" PUBLIC "${POLYEME_INCLUDE}")

set(EXECUTABLE_NAME     "polymer")
set(EXECUTABLE_FILES    "polymer/main.c++")

add_executable("${EXECUTABLE_NAME}" "${EXECUTABLE_FILES}")
target_include_directories("${EXECUTABLE_NAME}" PRIVATE "${POLYEME_INCLUDE}")
target_link_libraries("${EXECUTABLE_NAME}" "${POLYEME_NAME}")