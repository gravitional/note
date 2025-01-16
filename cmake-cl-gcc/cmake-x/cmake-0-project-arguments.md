# cmake 设置项目运行参数, .vcxproj 调试属性

[Adding command line arguments to project](https://stackoverflow.com/questions/30104520/adding-command-line-arguments-to-project)

除了在 Visual studio 等IDE 的GUI 属性页设置之外,
也可以在 CMakeLists.txt 中指定 target 属性

```sh
VS_DEBUGGER_COMMAND_ARGUMENTS # 为 Visual Studio C++ 目标设置本地 debugger 命令行参数.
VS_DEBUGGER_ENVIRONMENT # 为 Visual Studio C++ 目标设置本地 debugger 环境.
```

自 CMake 3.12 起, 它可与这些命令一起使用:

```sh
VS_DEBUGGER_COMMAND # 为 Visual Studio C++ 目标设置本地调试器命令.
VS_DEBUGGER_WORKING_DIRECTORY # 为 Visual Studio C++ 目标设置本地调试器的工作目录.
```

例如:

```sh
set_target_properties(HelloWorld
    PROPERTIES VS_DEBUGGER_COMMAND_ARGUMENTS "--gtest_filter=Test_Cases1*"
    )
```
