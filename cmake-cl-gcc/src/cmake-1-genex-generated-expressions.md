# cmake 生成表达式 genex, generated expressions

## Debugging

由于生成器表达式是在 generation of the buildsystem时 进行计算,
而不是在处理 `CMakeLists.txt` 文件时进行计算,
因此无法使用 `message()` 命令检查生成器表达式的结果.
生成调试信息的一种可行方法是添加自定义目标:

```cmake
add_custom_target(GenexDebug COMMAND ${CMAKE_COMMAND} -E echo "$<...>")
```

运行 `cmake` 后, 可以 build `GenexDebug` 目标,
以打印 `$<...>`表达式 的结果,
即在 build 目录下运行 `cmake --build . --target GenexDebug` 命令.

另一种方法是使用 `file(GENERATE)` 将调试信息写入文件:

```cmake
file(GENERATE OUTPUT filename CONTENT "$<...>")
```
