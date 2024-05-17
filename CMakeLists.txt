# this technique works on Windows.
# 1. download/extract Simply Fortran ZIP file
# 2. set app_root to the extracted folder

cmake_minimum_required(VERSION 3.19)

project(stacking LANGUAGES Fortran)

set(app_root ~/Downloads/simplyfortran-3.34.4181/SimplyFortran-3.34/mingw-w64)

if(WIN32)
  set(lib_path ${app_root}/x86_64-w64-mingw32/lib64)
  set(inc_path ${app_root}/lib/gcc/x86_64-w64-mingw32/13.1.0/finclude/)
endif()

if(WIN32)
  set(win32_lib gdi32 comdlg32 comctl32 uuid oleaut32 ole32)
endif()

find_library(appgraphics
NAMES appgraphics
PATHS ${lib_path}
NO_DEFAULT_PATH
REQUIRED
)

find_path(appgraphics_inc
NAMES appgraphics.mod
PATHS ${inc_path}
NO_DEFAULT_PATH
REQUIRED
)

add_executable(stacking src/main.f90 src/screen.f90 src/globals.f90 src/grid.f90 src/block.f90)

target_link_libraries(stacking PRIVATE ${appgraphics}
${win32_lib})

target_include_directories(stacking PRIVATE ${appgraphics_inc})

file(GENERATE OUTPUT .gitignore CONTENT "*")
