# msvcpch.cmake
# 声明一个宏，参数如下
# USE_MSVC_PCH        ：宏名字
# PCH_TARGET          ：项目名称
# PCH_HEADER_FILE     ：预编译头文件名称（stdafx.h）
# PCH_SOURCE_FILE     ：预编译源文件名称（stdafx.cpp）
MACRO(USE_MSVC_PCH PCH_TARGET PCH_HEADER_FILE PCH_SOURCE_FILE)
    IF(MSVC)
        # 获取预编译头文件的文件名，通常是stdafx
        GET_FILENAME_COMPONENT(PCH_NAME ${PCH_HEADER_FILE} NAME_WE)

        # 生成预编译文件的路径
        IF(CMAKE_CONFIGURATION_TYPES)
            # 如果有配置选项（Debug/Release），路径添加以及配置选项
            SET(PCH_DIR "${CMAKE_CURRENT_BINARY_DIR}/PCH/${CMAKE_CFG_INTDIR}")
        ELSE(CMAKE_CONFIGURATION_TYPES)
            SET(PCH_DIR "${CMAKE_CURRENT_BINARY_DIR}/PCH")
        ENDIF(CMAKE_CONFIGURATION_TYPES)

        # 创建预编译文件的路径
        FILE(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/PCH)

        # 设置项目属性，使用预编译头文件
        SET_TARGET_PROPERTIES(${PCH_TARGET} PROPERTIES COMPILE_FLAGS
            "/Yu${PCH_HEADER_FILE} /FI${PCH_HEADER_FILE} /Fp${PCH_DIR}/${PCH_NAME}.pch")

        # 预编译源文件（stdafx.cpp）设置属性，创建预编译文件
        SET_SOURCE_FILES_PROPERTIES(${PCH_SOURCE_FILE} PROPERTIES COMPILE_FLAGS
            "/Yc${PCH_HEADER_FILE}")

        # 把预编译文件寄到清除列表
        SET_DIRECTORY_PROPERTIES(PROPERTIES
            ADDITIONAL_MAKE_CLEAN_FILES ${PCH_DIR}/${PCH_NAME}.pch)
    ENDIF(MSVC)
ENDMACRO(USE_MSVC_PCH)