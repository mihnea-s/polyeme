cmake_minimum_required(VERSION 3.12)

set(DIST_INCLUDE "dist" PARENT_SCOPE)
set(DIST_LIBRARIES "LIBLINENOISE" PARENT_SCOPE)

add_library(LIBLINENOISE "linenoise/linenoise.c")
