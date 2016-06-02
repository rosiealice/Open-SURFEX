cmake_minimum_required (VERSION 2.6)
project (open-SURFEX)
enable_language (Fortran)

set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
find_package(grib_api REQUIRED)

#add_subdirectory(src/LIB/GELATO)

file(GLOB src_SURFEX
    "./src/SURFEX/*.F90")

file(GLOB src_DRHOOK
    "./src/LIB/DRHOOK/BYPASS/*.F90")

file(GLOB src_ASSIM
    "./src/ASSIM/*.F90")

file(GLOB src_OFFLIN
    "./src/OFFLIN/*.F90")

add_executable (final.exe ${src_OFFLIN} ${src_ASSIM} ${src_DRHOOK} ${src_SURFEX} src/LIB/GELATO/modd_types_glt.F90 src/LIB/GELATO/modd_glt_param.F90)
target_include_directories(final.exe PRIVATE ${GRIB_API_INCLUDE_DIRS})
target_link_libraries(final.exe LINK_PUBLIC GELATO ${GRIB_API_LIBRARIES})
