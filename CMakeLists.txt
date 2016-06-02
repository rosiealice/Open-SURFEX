cmake_minimum_required (VERSION 2.6)
project (open-SURFEX)
enable_language (Fortran)

set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
find_package(grib_api REQUIRED)

file(GLOB src_SURFEX
    "./src/SURFEX/*.F90"
)
file(GLOB src_DRHOOK
    "./src/LIB/DRHOOK/BYPASS/*.F90")
file(GLOB src_ASSIM
    "./src/ASSIM/")
add_executable (final.exe ${src_ASSIM} ${src_DRHOOK} ${src_SURFEX})
target_include_directories(final.exe PRIVATE ${GRIB_API_INCLUDE_DIRS})
target_link_libraries(final.exe ${GRIB_API_LIBRARIES})
