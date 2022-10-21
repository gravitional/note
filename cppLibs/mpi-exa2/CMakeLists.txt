cmake_minimum_required(VERSION 3.15)

message(STATUS "The CMAKE_VERSION is ${CMAKE_VERSION}.")

project(mpi-Matrix)

include(myFindMPI.cmake)

message(STATUS "PROJECT_NAME is ${PROJECT_NAME}")

add_executable(${PROJECT_NAME} matrix.cpp)
target_include_directories(${PROJECT_NAME} PUBLIC "${MPI_CXX_INCLUDE_DIRS}")
target_link_libraries(${PROJECT_NAME} PUBLIC "${MPI_CXX_LIBRARIES}")
