# ++ output message:
# message(STATUS "")
# message(STATUS "--- Enter subdirectory, Start to call ${CMAKE_CURRENT_LIST_FILE} ---")
# message(STATUS "This current binary directory is ${CMAKE_CURRENT_BINARY_DIR}.")
# message(STATUS "This current source directory is ${CMAKE_CURRENT_SOURCE_DIR}.")

if(CMAKE_SYSTEM_NAME MATCHES "Linux")
elseif(CMAKE_SYSTEM_NAME MATCHES "Windows")
    message(STATUS "WIN_COMPILER is ${WIN_COMPILER}")
    add_definitions(-D_CRT_SECURE_NO_WARNINGS) # 取消c语言中fopen等函数的警告
endif()

# ++
message(STATUS "The CMAKE_C_COMPILER is ${CMAKE_C_COMPILER}")

# message(STATUS "The CMAKE_CXX_COMPILER is ${CMAKE_CXX_COMPILER}")
# message(STATUS "The CMAKE_C_FLAGS is ${CMAKE_C_FLAGS}")
# message(STATUS "The CMAKE_CXX_FLAGS is ${CMAKE_CXX_FLAGS}")

# message(STATUS "The CFLAGS is ${CFLAGS}")
# message(STATUS "The CXXFLAGS is ${CXXFLAGS}")
# message(STATUS "The LDFLAGS is ${LDFLAGS}")

# message(STATUS "The CMAKE_C_FLAGS is ${CMAKE_C_FLAGS}")
# message(STATUS "The CMAKE_CXX_FLAGS is ${CMAKE_CXX_FLAGS}")
# message(STATUS "The CMAKE_EXE_LINKER_FLAGS is ${CMAKE_EXE_LINKER_FLAGS}")

# ++
if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    if(CMAKE_COMPILER_IS_GNUCXX)
        Set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-Bstatic, -Wl,-Bdynamic")
    else()
        Set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl")
    endif()
else()
    Set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,--no-as-needed -ldl")
endif()

# 连接器 flags
message(STATUS "The CMAKE_EXE_LINKER_FLAGS is ${CMAKE_EXE_LINKER_FLAGS}")

find_package(HDF5 REQUIRED)
include_directories("${HDF5_INCLUDE_DIRS}")
message(STATUS "The HDF5_INCLUDE_DIRS is ${HDF5_INCLUDE_DIRS}")
message(STATUS "The HDF5_LIBRARIES is ${HDF5_LIBRARIES}")

GetAllSubDir(dirList ${CMAKE_CURRENT_SOURCE_DIR})
message(STATUS "The dirList is ${dirList}")

set_source_group(HEAD_LIST SRC_LIST)

# . set executable name:
set(EXECUTABLE_TARGET_NAME "${CMAKE_PROJECT_NAME}")
message(STATUS "The executable name is ${EXECUTABLE_TARGET_NAME}.")

# . add an executable to the project using the specified source files and header files:
add_executable(${EXECUTABLE_TARGET_NAME} ${SRC_LIST} ${HEAD_LIST})
target_link_libraries(${EXECUTABLE_TARGET_NAME} ${HDF5_LIBRARIES})

install(TARGETS ${EXECUTABLE_TARGET_NAME} RUNTIME DESTINATION bin)

message(STATUS "The CMAKE_INSTALL_PREFIX is ${CMAKE_INSTALL_PREFIX}.")