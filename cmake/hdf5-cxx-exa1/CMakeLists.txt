cmake_minimum_required(VERSION 3.10.2)

# Set the project name
project(h5test CXX)

set(FIND_HDF_COMPONENTS CXX shared)

find_package(HDF5 NAMES "hdf5" COMPONENTS ${FIND_HDF_COMPONENTS} PATHS "C:/Program Files/HDF_Group/HDF5")

if(HDF5_FOUND)
    message(" HDF5 find")

    if(HDF5_shared_CXX_FOUND)
        set(LINK_LIBS ${LINK_LIBS} ${HDF5_CXX_SHARED_LIBRARY})
    endif()
else()
    message(FATAL_ERROR "Can't find  HDF5")
endif()

message("<< include: " ${HDF5_INCLUDE_DIR})
message("<< hdf5 include directory: " ${HDF5_INCLUDE_DIR})
message("<< hdf5 c++ shared library dir: " ${HDF5_CXX_SHARED_LIBRARY})
message("<< FIND_HDF_COMPONENTS: " ${FIND_HDF_COMPONENTS})

# Add your application HERE
add_executable(h5test1 "Possion1.cpp")

target_include_directories(h5test1 PRIVATE ${HDF5_INCLUDE_DIR})
target_compile_options(h5test1 PRIVATE "-I ${HDF5_INCLUDE_DIR}")

target_link_libraries(h5test1 PUBLIC ${LINK_LIBS})