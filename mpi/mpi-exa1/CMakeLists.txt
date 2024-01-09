# https://blog.csdn.net/ljxfblog/article/details/81455998

cmake_minimum_required(VERSION 3.15)

message(STATUS "The CMAKE_VERSION is ${CMAKE_VERSION}.")

project(mpiexa1)

include(myFindMPI.cmake)

message(STATUS "PROJECT_NAME is ${PROJECT_NAME}")

if(CMAKE_SYSTEM_NAME MATCHES "Windows")
    # msvcpch.cmake的路径
    set(MSVCCMAKE_PCH ${CMAKE_SOURCE_DIR}/build/)

    # 预编译头文件和源文件的路径
    set(STDAFX_PCH_H ${CMAKE_SOURCE_DIR}/Protocol/stdafx.h)
    set(STDAFX_PCH_C ${CMAKE_SOURCE_DIR}/Protocol/stdafx.cpp)

    # 添加预编译cmake的路径
    LIST(APPEND CMAKE_MODULE_PATH ${MSVCCMAKE_PCH})

    # 包含msvcpch.cmake
    INCLUDE(msvcpch.cmake)
endif()

add_executable(${PROJECT_NAME} pi.cpp)
target_include_directories(${PROJECT_NAME} PUBLIC "${MPI_CXX_INCLUDE_DIRS}")
target_link_libraries(${PROJECT_NAME} PUBLIC "${MPI_CXX_LIBRARIES}")

# exe2
set(exe2 jacobi)
add_executable(${exe2} jacobi.cpp)
target_include_directories(${exe2} PUBLIC "${MPI_CXX_INCLUDE_DIRS}")
target_link_libraries(${exe2} PUBLIC "${MPI_CXX_LIBRARIES}")

# exe3 ; 添加源文件，生成可执行文件
set(exe3 mpiexa3)
add_executable(${exe3} mpiexa3.cpp ${STDAFX_PCH_C})
target_include_directories(${exe3} PUBLIC "${MPI_CXX_INCLUDE_DIRS}")
target_link_libraries(${exe3} PUBLIC "${MPI_CXX_LIBRARIES}")

# 此处是关键，添加我们在msvcpch.cmake定义的宏
USE_MSVC_PCH(${exe3} stdafx.h ${STDAFX_PCH_C})
