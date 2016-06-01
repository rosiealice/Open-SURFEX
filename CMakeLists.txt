cmake_minimum_required (VERSION 2.6)
project (open-SURFEX)
enable_language (Fortran)
file(GLOB src_SURFEX
    "./src/SURFEX/*.F90"
)
file(GLOB src_DRHOOK
    "./src/LIB/DRHOOK/BYPASS/*.F90")
file(GLOB src_ASSIM
    "./src/ASSIM/")
add_executable (final.exe ${src_DRHOOK} ${src_SURFEX})