# cmake 别名库

## [Alias Libraries][def14], 别名库

```cmake
add_library(<name> ALIAS <target>)
```

创建 [Alias Target][def15], 在后续命令中, 可以使用 `<name>` 表示 `<target>`.
`<name>` 不会作为 make 目标出现在生成的 build系统中. `<target>` 不能是 `ALIAS`.

+ 3.11 版所增: `ALIAS` 可以指向 `GLOBAL` Imported Target
+ 在 3.18 版中新增:  ALIAS 可以指向 non-`GLOBAL` Imported Target.
此类别名的作用域为其创建时所在的目录及以下. `ALIAS_GLOBAL` 目标属性可用于检查别名是否是全局的.

`ALIAS` 目标可用作 linkable targets 和 读取属性的目标.
还可以使用常规的 [if(TARGET)][def16] 子命令测试目标是否存在.
`<name>` 不能用于修改 `<target>` 的属性, 也就是说,
不能用作 `set_property()`, `set_target_properties()`, `target_link_libraries()` 等命令的操作数.
`ALIAS` 目标不能被 installed or exported.

[def14]: https://cmake.org/cmake/help/latest/command/add_library.html#alias-libraries
[def15]: https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#alias-targets
[def16]: https://cmake.org/cmake/help/latest/command/if.html#target