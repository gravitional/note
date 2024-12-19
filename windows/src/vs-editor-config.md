# visual studio .editorconfig

[Visual Studio 设置文件默认编码格式, 行尾符等](https://blog.csdn.net/weixin_45136016/article/details/142122625)

## EditorConfig配置

[使用 EditorConfig 定义一致的编码样式](https://learn.microsoft.com/zh-cn/visualstudio/ide/create-portable-custom-editor-options?view=vs-2022#supported-settings)

使用 `EditorConfig` 方式配置, 无需Visual Studio软件自带对 `EditorConfig` 的支持, 无需插件
将下面 `.editorconfig` 文件放在项目根目录下

```yaml
root = true                  # 所在目录是根目录, 此目录及子目录下保存的文件都会生效
[*]                          # 对所有文件生效
indent_style = tab           # 缩进风格
tab_width = 4                # 缩进宽度
charset = utf-8-bom          # 文件编码格式
end_of_line = lf             # 行尾格式
insert_final_newline = false # 文件结尾添加换行符, 以防警告
```

注:
勾选 `工具` > `选项` > `文本编辑器` > `遵循项目编码约定`

编辑 `EditorConfig` 文件后, 必须重载代码文件, 新设置才会生效
配置 `EditorConfig` 只会对新代码行的格式进行设置, 如果要对全部文件格式进行设置, 则需要进行代码清理

## 支持的设置

[EditorConfig](https://editorconfig.org/#supported-properties)

Visual Studio 中的编辑器支持 EditorConfig 属性的核心集:

```yaml
indent_style
indent_size
tab_width
end_of_line
charset
trim_trailing_whitespace
insert_final_newline
root
```

EditorConfig 支持所有 Visual Studio 支持的语言(XML 除外)均支持 EditorConfig 编辑器设置.
EditorConfig 支持适用于 C# 和 Visual Basic 的代码样式约定(包括语言, 格式设置)和命名约定.