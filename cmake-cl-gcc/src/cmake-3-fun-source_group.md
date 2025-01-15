# source gorup

在为 `IDE` 生成的项目文件中, 为源文件定义`group`.
有两种不同的签名来创建 `源文件组`.

```cmake
source_group(<name> [FILES <src>...] [REGULAR_EXPRESSION <regex>])
source_group(TREE <root> [PREFIX <prefix>] [FILES <src>...])
```

定义了一个组, 在项目文件中, 源文件将被放置到其中.
这是为了在 `Visual Studio` 中设置 file tabs.
`group` 被 scoped 于此命令被调用的目录, 并应用到该目录的 targets 的源文件.

选项如下:

+ `TREE`
*3.8版的新功能.*
CMake 将自动检测 ` <src>`中的文件路径中, 创建需要的 源文件组,
以保持 源文件组 的结构与项目中的 实际文件 和 目录结构 相似.
`<src>` 文件的路径将被剪切为 相对于 `<root>`.
如果 `src` 中的路径不是以 `root` 开头, 则该命令会失败.

+ `PREFIX`
*3.8版中的新内容.*
直接位于 `<root>` 路径中的源组和文件, 将被放在 `<prefix>` 源文件组 中.

+ `FILES`
任何明确指定的 `源文件` 将被放置在 `<name>` 组中.
`相对路径` 的解析相对于 `当前源目录`.

+ `REGULAR_EXPRESSION`
任何名称与 `正则表达式` 相匹配的源文件将被放置在 `<name>` 组中.
  + 如果一个 `源文件` 与 `多个组` 相匹配,
    `最后一个` 用 `FILES` 明确列出该文件的组将被优先考虑, 如果有的话.
  + 如果没有组明确列出该文件,
    `最后一个` 与该文件的 `正则表达式` 相匹配的组将被优先考虑.
  + 组的 `<name>` 和 `<prefix>` 参数可以包含 正斜线 或 反斜线 来指定子组.
    反斜线需要被适当地转义.

```cmake
source_group(base/subdir ...)
source_group(outer\\inner ...)
source_group(TREE <root> PREFIX sources\\inc ...)
```

*3.18版新增*: 允许使用 `正斜线`(`/`)来指定子组.

+ 为了向后兼容, 简写的签名

```cmake
source_group(<name> <regex>)
```

相当于

```cmake
source_group(<name> REGULAR_EXPRESSION <regex>)
```
