cmake_minimum_required (VERSION 2.6)
project (open-SURFEX)
enable_language (Fortran)
file(GLOB src_SURFEX
    "./src/SURFEX/*.F90"
)
add_executable (final.exe ${src_SURFEX})