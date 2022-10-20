cmake_minimum_required(VERSION 3.15)

message(STATUS "The CMAKE_VERSION is ${CMAKE_VERSION}.")

project(mpiexa1)

include(myFindMPI.cmake)

message(STATUS "PROJECT_NAME is ${PROJECT_NAME}")

add_executable(${PROJECT_NAME} pi.cpp)
target_include_directories(${PROJECT_NAME} PUBLIC "${MPI_CXX_INCLUDE_DIRS}")
target_link_libraries(${PROJECT_NAME} PUBLIC "${MPI_CXX_LIBRARIES}")

# exe2
set(exe2 jacobi)
add_executable(${exe2} jacobi.cpp)
target_include_directories(${exe2} PUBLIC "${MPI_CXX_INCLUDE_DIRS}")
target_link_libraries(${exe2} PUBLIC "${MPI_CXX_LIBRARIES}")