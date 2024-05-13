cmake_minimum_required(VERSION 3.10.2)
project(myproj C CXX Fortran)

set(FIND_HDF_COMPONENTS C CXX Fortran shared)

find_package(HDF5 NAMES "hdf5" COMPONENTS ${FIND_HDF_COMPONENTS})

if(HDF5_FOUND)
    if(HDF5_shared_C_FOUND)
        set(LINK_LIBS ${LINK_LIBS} ${HDF5_C_SHARED_LIBRARY})
    endif()

    if(HDF5_shared_CXX_FOUND)
        set(LINK_LIBS ${LINK_LIBS} ${HDF5_CXX_SHARED_LIBRARY})
    endif()

    if(HDF5_shared_Fortran_FOUND)
        set(LINK_LIBS ${LINK_LIBS} ${HDF5_FORTRAN_SHARED_LIBRARY})
        INCLUDE_DIRECTORIES(${HDF5_INCLUDE_DIR_FORTRAN})
    endif()
else()
    message(FATAL_ERROR " HDF5 is Required")
endif()

INCLUDE_DIRECTORIES(${HDF5_INCLUDE_DIR})

# Add your application HERE
add_executable(runapp rwdset_fortran2003.f90)

target_link_libraries(runapp PRIVATE ${LINK_LIBS})