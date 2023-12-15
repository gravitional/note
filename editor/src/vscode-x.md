# vscode.x

[code.visualstudio.com/docs](https://code.visualstudio.com/docs)
[codebasics](https://code.visualstudio.com/docs/editor/codebasics)

[vscode 下载加速方法](https://blog.csdn.net/qq_42074368/article/details/120410816)

[魔法注释]: https://github.com/James-Yu/LaTeX-Workshop/wiki/Compile#magic-comments
[latexmk]: https://personal.psu.edu/jcc8/software/latexmk/

[超链接]: https://code.visualstudio.com/docs/editor/integrated-terminal#_links
[错误检测]: https://code.visualstudio.com/docs/editor/tasks

官方的下载链接例如下:

```url
https://  az764295.vo.msecnd.net  /stable/83bd43bc519d15e50c4272c6cf5c1479df196a4d/code_1.60.1-1631294805_amd64.deb
```

将 `az764295.vo.msecnd.net` 替换为 `vscode.cdn.azure.cn`, 镜像下载快的飞起.

```url
https://vscode.cdn.azure.cn/stable/83bd43bc519d15e50c4272c6cf5c1479df196a4d/code_1.60.1-1631294805_amd64.deb
```

## 按文件名搜索文件

按快捷键`ctrl+p`可以弹出一个小窗, 在上面的输入框输入文件名, 下拉框点击一个文件.

## latex 编译调试

```powershell
$temp=latexmk -f -xelatex; Write-Output "++++++++++++" ;$temp | Where-Object {$_ -like "*tex:*"}
```

```powershell
$temp | Where-Object {$_ -like "*tex:*"}
```

## 快速打开文件

Keyboard Shortcut: `Ctrl+P`

### Open multiple files from Quick Open

你可以通过按 `右箭头` 从快速打开中打开多个文件.
这将在后台打开当前选定的文件, 而你可以继续从 "快速打开 "中选择文件.

### code 环境变量

[Variables Reference](https://code.visualstudio.com/docs/editor/variables-reference)

以下是预定义的变量:

+ `${workspaceFolder}` - the path of the folder opened in VS Code
+ `${workspaceFolderBasename}` - the name of the folder opened in VS Code without any slashes (/)
+ `${file}` - the current opened file
+ `${relativeFile}` - the current opened file relative to workspaceFolder
+ `${relativeFileDirname}` - the current opened file's dirname relative to workspaceFolder
+ `${fileBasename}` - the current opened file's basename
+ `${fileBasenameNoExtension}` - the current opened file's basename with no file extension
+ `${fileDirname}` - the current opened file's dirname
+ `${fileExtname}` - the current opened file's extension
+ `${cwd}` - the task runner's current working directory on startup
+ `${lineNumber}` - the current selected line number in the active file
+ `${selectedText}` - the current selected text in the active file
+ `${execPath}` - the path to the running VS Code executable
+ `${defaultBuildTask}` - the name of the default build task

## vscode 集成终端

### 集成终端

Visual Studio Code 包括一个功能齐全的集成终端, 可以方便地在工作区的根目录(root)启动.
它提供了与编辑器的集成, 以支持[超链接][] 和 [错误检测][] 等功能.

要打开终端.

+ 使用 `` Ctrl+` ``键盘快捷键与回车符.
+ 使用 `View > Terminal` 的菜单命令.
+ 从 `Command Palette (Ctrl+Shift+P)`, 使用 `View: Toggle Terminal` 命令.

![终端](https://code.visualstudio.com/assets/docs/editor/integrated-terminal/integrated-terminal.png)

>注意: 如果你喜欢在 VS Code 之外工作, 用 `Ctrl+Shift+C` 键盘快捷键打开一个外部终端.

### 管理终端

终端标签视图在终端视图的右侧.
每个终端都有一个条目, 包含其`名称`, `图标`, `颜色` 和 组别装饰(如果有的话).

![终端选项卡](https://code.visualstudio.com/assets/docs/editor/integrated-terminal/tabs.png)

>提示: 使用 `terminal.integrated.tabs.location` 设置来改变标签的位置.

通过点击 `TERMINAL` 面板右上方的 `+` 图标, 从终端下拉菜单中选择一个配置文件,
或通过触发`` Ctrl+Shift+` ``命令, 可以添加终端实例. 这个动作会在与该终端相关的标签列表中创建另一个条目.

删除终端实例可以通过将鼠标悬浮在一个标签上, 并选择 `垃圾桶` 按钮,
或选择标签项目并按下 `Delete`,
或使用 `Terminal: Kill the Active Terminal Instance ` 命令,
或者通过右键菜单.

在终端组之间的导航, 可以使用`Ctrl+PageDown` 将焦点移动到下一个,  `Ctrl+PageUp` 焦点上一个.

当一个终端的状态改变时, 图标可能会出现在标签上终端标题的右边.
一些例子是`铃铛`(macOS), 对于任务, 当没有错误时显示一个复选标记(check mark), 否则显示一个`X`.
鼠标悬浮到图标以阅读状态信息, 其中可能包含`动作`(actions).

#### 分组

通过以下方式分割终端:

+ 悬停时, 选择内联分割按钮.
+ 右键单击上下文菜单, 选择 `Split` 菜单选项.
+ `Alt` 点击标签, `+` 按钮, 或终端面板上的单个标签.
+ 触发 `Ctrl+Shift+5` 命令.

通过聚焦前一个窗格 -- `Alt+Left`, 和聚焦下一个窗格 -- `Alt+Right`, 在一个组中的终端之间导航.

标签支持拖放, 以允许重新排列.
将一个终端组中的条目拖入空的, 将从组中删除(例如, unsplit).
将一个标签拖入主终端区, 允许加入一个组.

通过触发 `Terminal: Unsplit Terminal` 来解除(unsplit)拆分终端命令.

## 自定义标签

通过 `右键上下文菜单` 或触发以下命令, 来改变终端的名称, 图标和标签的颜色.
命令 命令ID

+ `Terminal: Rename`    `workbench.action.terminal.rename`
+ `Terminal: Change`    `Icon workbench.action.terminal.changeIcon`
+ `Terminal: Change`    `Color workbench.action.terminal.changeColor`

>提示: 通过设置 `terminal.integrated.tabs.enabled:false` 回到旧版本

### 终端配置文件,profiles

[Terminal profiles](https://code.visualstudio.com/docs/editor/integrated-terminal#_terminal-profiles)

终端配置文件(profiles), 是特定于平台的 `shell` 配置, 由`可执行路径`, `参数` 和 其他 `自定义内容`组成.
配置文件的例子:

```json
{
  "terminal.integrated.profiles.windows": {
    "My PowerShell": {
      "path": "pwsh.exe",
      "args": ["-noexit", "-file", "${env:APPDATA}PowerShellmy-init-script.ps1"]
    }
  },
  "terminal.integrated.defaultProfile.windows": "My PowerShell"
}
```

你可以在终端配置文件中使用变量, 如上面的例子中的 `APPDATA` 环境变量.
你可以在 [变量参考][] 主题中找到可用变量的列表.

通过运行 `Terminal: Select Default Profile` 来配置你的默认集成终端.
该命令也可以通过终端下拉菜单访问.

![集成终端下拉菜单](https://code.visualstudio.com/assets/docs/editor/integrated-terminal/terminal-dropdown.png)

终端的 `shell` 在 `Linux` 和 `macOS` 上默认为 `$SHELL`,
在 `Windows` 上默认为 `PowerShell`. VS Code 会自动检测大多数标准的 `shell`, 然后可以将其配置为默认.

[变量参考]: https://code.visualstudio.com/docs/editor/variables-reference

### 配置 profiles

要创建一个新的 `profile`, 运行`Terminal: Select Default Profile` 命令,
并激活 shell 右侧的`配置`按钮以作为基础.
这将为你的设置添加一个新条目, 可以在 `settings.json` 文件中手动调整.

`Profiles`可以使用`path`或`source`, 以及一组可选的`参数`来创建.
`source`只在Windows上可用, 可以用来让 `VS Code` 检测 `PowerShell` 或 `Git Bash` 的安装.
另外, 也可以使用直接指向 `shell` 可执行文件的`path`. 下面是一些配置文件配置的例子.

```json
{
  "terminal.integrated.profiles.windows": {
    "PowerShell -NoProfile": {
      "source": "PowerShell",
      "args": ["-NoProfile"]
    }
  },
  "terminal.integrated.profiles.linux": {
    "zsh (login)": {
      "path": "zsh",
      "args": ["-l"]
    }
  }
}
```

配置文件中支持的其他参数包括.

+ `overrideName`; 布尔值, 表示是否用`静态profile的名称`来替换, 检测什么程序正在运行的动态终端`标题`.
+ `env`; 一个`map`, 定义环境变量及其值, 将变量设置为`null`以从环境中删除它.
    可以使用 `terminal.integrated.env.<platform>` setting 为所有 profiles 配置.
+ `icon`; 用于 profile 的图标ID.
+ `color`: 主题颜色的ID, 用来风格化图标.

>提示: `Path`, `args` 和 `env` 都支持[解析变量](https://code.visualstudio.com/docs/editor/variables-reference)
>
>Visual Studio Code支持 `Debugging` 和 `Task` 配置文件, 以及一些选择设置中的变量替换(variable substitution).
>在 `launch.json` 和 `tasks.json` 文件中的一些`键`和`值`字符串中, 支持使用 `${variableName}`语法进行变量替换.

可以通过 `terminal.integrated.defaultProfile.*` 设置手动定义 `default profile`, `*`应该替换成对应的操作系统名称.
应该把它被设置为现有配置文件的名称:

```json
{
  "terminal.integrated.profiles.windows": {
    "my-pwsh": {
      "source": "PowerShell",
      "args": ["-NoProfile"]
    }
  },
  "terminal.integrated.defaultProfile.windows": "my-pwsh"
}
```

>提示: 集成终端 `shell` 是以 `VS Code` 的权限运行的.
>如果你需要以高级(elevated, 管理员)或不同的权限运行 `shell` 命令, 请在终端中使用平台工具, 如 `runas.exe`.

### 移除内置配置文件

要从终端下拉菜单中删除条目, 请将配置文件的名称设为`null`.
例如, 要删除 `Windows` 上的 `Git Bash` 配置文件, 请使用此设置.

```json
{
  "terminal.integrated.profiles.windows": {
    "Git Bash": null
  }
}
```

### 配置 task/debug profile

默认情况下, `task/debug`功能将使用默认`profile`.
要覆盖它, 请使用 `terminal.integrated.automationShell.<platform>` settings.

```json
{
    "terminal.integrated.defaultProfile.osx": "fish",
    // 使用一个完全与 POSIX 兼容的 shell, 以避免运行复杂的 ~/.config/fish/config.fish
    //来进行 tasks and debug
    "terminal.integrated.automationShell.osx": "/bin/sh"
}
```

### 工作目录

默认情况下, 终端将在 `Explorer`(vscode 左侧边栏的目录树) 中打开的文件夹中打开.
`terminal.integrated.cwd` 设置允许指定一个自定义路径来代替打开.

```json
{
  "terminal.integrated.cwd": "/home/user"
}
```

Windows 上的分离式终端(Split terminals)将在父终端开始的目录中启动.
在 macOS 和 Linux 上, 分离式终端将继承父终端的当前工作目录.
这种行为可以通过 `terminal.integrated.splitCwd` 设置来改变.

```json
{
  "terminal.integrated.splitCwd": "workspaceRoot"
}
```

还有一些扩展可以提供更多的选项, 如 [Terminal Here](https://marketplace.visualstudio.com/items?itemName=Tyriar.vscode-terminal-here).

## LaTeX-Workshop

[LaTeX recipes](https://github.com/James-Yu/LaTeX-Workshop/wiki/Compile#latex-recipes)

### LaTeX配方

LaTeX配方(recipes), 是指LaTeX Workshop在构建LaTeX项目时按顺序执行的`命令序列`.
它是由 `latex-workshop.latex.recipes` 定义的.
默认情况下, LaTeX Workshop 包括两个基本的配方, 由变量 `latex-workshop.latex.recipes` 和 `latex-workshop.latex.tools` 定义.

+ 第一个是简单地依赖 `latexmk` 命令
+ 第二种运行以下命令序列: `pdflatex→bibtex→pdflatex→pdflatex`.

把下面的配置添加到你的 Vscode 的 json 配置文件中:

```json
"latex-workshop.latex.recipes": [
  {
    "name": "latexmk 🔃",
    "tools": [
      "latexmk"
    ]
  },
  {
    "name": "pdflatex ➞ bibtex ➞ pdflatex`×2",
    "tools": [
      "pdflatex",
      "bibtex",
      "pdflatex",
      "pdflatex"
    ]
  }
]
```

以及出现在`tools` field 中的每个`tool`都被定义在 `latex-workshop.latex.tools` 中.
它的默认值是:

```json
"latex-workshop.latex.tools": [
  {
    "name": "latexmk",
    "command": "latexmk",
    "args": [
      "-synctex=1",
      "-interaction=nonstopmode",
      "-file-line-error",
      "-pdf",
      "-outdir=%OUTDIR%",
      "%DOC%"
    ],
    "env": {}
  },
  {
    "name": "pdflatex",
    "command": "pdflatex",
    "args": [
      "-synctex=1",
      "-interaction=nonstopmode",
      "-file-line-error",
      "%DOC%"
    ],
    "env": {}
  },
  {
    "name": "bibtex",
    "command": "bibtex",
    "args": [
      "%DOCFILE%"
    ],
    "env": {}
  }
]
```

你可以用不同的`tools`创建多个`recipes`.
每个`配方`是`配置列表`中的一个对象, 由一个`name`字段和配方中要调用的`tools`列表组成.

配方中的`tools`可以在 `latex-workshop.latex.tools` 中定义, 其中每个命令就是一个`tool`.
每个`tool`都是一个对象, 由一个`name`, 一个要生成的`command`, 它的参数(`args`)和一些特定的环境变量(`env`)组成.
`env` 条目是一个字典. 想象一下, 你想使用一个 `texmf` 的子目录, 它处于你的主项目中, 只要写

```json
"env": {
    "TEXMFHOME": "%DIR%/texmf"
}
```

你也可以覆盖 `PATH` 环境变量. 注意, 在属性中, 只有占位符(placeholders) 例如`%DIR%`, 才会生效, 而其他变量, 例如 `$PATH`, **则不会被展开**.

要在配方中包括一个工具, 该工具的名称应包括在配方的工具列表中.

在构建项目时, 如果根文件中存在[魔法注释][], 就会使用它, 否则就使用第一个配方.
你可以通过 `latex-workshop.recipes` 命令用另一个配方进行编译. 默认情况下使用[latexmk][].
这个工具被捆绑在大多数LaTeX发行版中, 需要 `perl` 来执行.
对于非 `perl` 用户, MikTeX的以下 `texify` 工具链可能值得一试.

```json
"latex-workshop.latex.recipes": [{
  "name": "texify",
  "tools": [
    "texify"
  ]
}],
"latex-workshop.latex.tools": [{
  "name": "texify",
  "command": "texify",
  "args": [
    "--synctex",
    "--pdf",
    "--tex-option=\"-interaction=nonstopmode\"",
    "--tex-option=\"-file-line-error\"",
    "%DOC_EXT%"
  ],
  "env": {}
}]
```

`args` 和 `env` 参数可以包含由`%`包围的符号. 这些占位符会被即时替换(on-the-fly).

### 占位符

LaTeX Workshop 注册(registers)了以下占位符. 根文件值的是 LaTeX 主文件, 例如 `main.tex`.

占位符 代替的是

+ `%DOC%`;  不含扩展名的根文件`全路径`(tex root file)
+ `%DOC_W32%`;  根文件的`全路径`, 不含扩展名, 在Windows下使用`\`路径分隔符.
+ `%DOCFILE%`;  不含扩展名的根文件名
+ `%DOC_EXT%`;  带有`扩展名`的根文件`全路径`
+ `%DOC_EXT_W32%`;  根文件的完整路径, 带有扩展名, 在Windows下使用`\`路径分隔符
+ `%DOCFILE_EXT%`;  带有扩展名的根文件名
+ `%DIR%`;  根文件目录
+ `%DIR_W32%`;  根文件目录, 在Windows中使用`\`路径分隔符
+ `%TMPDIR%`;   用于存储辅助文件的临时文件夹
+ `%OUTDIR%`;   在 [latex-workshop.latex.outDir][] 中配置的输出目录.
+ `%OUTDIR_W32%`;   在 `latex-workshop.latex.outDir` 中配置的输出目录, 在Windows中使用`\`路径分隔符.
+ `%WORKSPACE_FOLDER%`; 当前`工作区`的路径
+ `%RELATIVE_DIR%`; 相对于`工作区`文件夹的根文件目录
+ `%RELATIVE_DOC%`; 相对于`工作区`文件夹的根文件路径

由于大多数 `LaTeX` 编译器接受没有扩展名的根文件名, `%DOC%`和`%DOCFILE%`不包括文件名扩展名.
同时, `texify` 工具需要完整的文件名及其扩展名, 因此在 `texify` 的配置中使用 `%DOC_EXT%`.

大多数命令都接受使用`/`路径分隔符, 甚至在Windows上, 大多数LaTeX工具甚至要求使用它.
相反, 一些Windows命令只能使用`\`路径分隔符. 因此, 我们提供两个版本的占位符.
所有不带`_W32`后缀的占位符, 即使在 `Windows` 上也总是使用`/`路径分隔符.
所有带`_W32`后缀的占位符在 `Windows` 上使用 `\` 路径分隔符.
注意在Linux和Unix系统上, 有和没有`_W32`后缀的占位符是相同的.

[latex-workshop.latex.outDir]: https://github.com/James-Yu/LaTeX-Workshop/wiki/View#latex-workshoplatexoutDir

### latex-workshop.latex.recipe.default

定义 `Build LaTeX` 项目命令所使用的配方. 它也适用于自动构建.
配方是以 `latex-workshop.latex.recipes` 中定义的名称来指代的. 注意有两个特殊的值.

+ `"first"`: 使用 `latex-workshop.latex.recipes` 中定义的第一个配方.
+ `"lastUsed"`: 使用 `LaTeX Workshop: Build with recipe` 命令上次使用过的配方.

类型 默认值

+ `string` `"first"`

### latex-workshop.latex.build.forceRecipeUsage

强制使用配方系统, 即使是在`魔法注释`中定义了`TeX命令`.
类型 默认值

+ `boolean` `false`

## 自动完成 括号匹配, 引号匹配 倒引号 `` ` ``

Editor: auto closing Brackets
Editor: auto closing Quotes

如需关闭打开, markdown 的倒引号自动补全(匹配),
可通过设置或关闭以上选项的值实现

## Wolfram Language extension

Wolfram语言的官方Visual Studio Code扩展

### 功能介绍

+ 语法高亮
+ 诊断和修复建议
+ 文件和选择的格式化
+ 语义高亮
+ 扩大和缩小选择范围
+ 大纲
+ 色标(Color swatches)
+ 符号参考(Symbol references)
+ 鼠标悬停时的文档(on hover)

## 语法高亮

支持整个 `Wolfram` 语言的语法和所有 `内置函数`.

### 安装

`LSP功能`(Language Server Protocal) 使用 Wolfram 内核, 作为语言服务器运行.
这需要 Wolfram System 12.1 或更高版本.

Wolfram语言扩展依赖于 [LSPServer paclet][] 来提供 `LSP` 功能.
通过运行此 `Wolfram Language` 代码来安装 `LSPServer paclet` 及其依赖:

```wolfram
PacletInstall["CodeParser"] .
PacletInstall["CodeInspector"]
PacletInstall["CodeFormatter"]
PacletInstall["LSPServer"]
```

如果设置正确, 你应该有 `Wolfram Language .wl` 文件的语法高亮和提示.
在新的 `.wl` 文件中以下内容并保存, 以测试:

```wolfram
Which[a, b, a, b]
```

你应该看到关于重复 clauses 的警告.

[LSPServer paclet]: https://github.com/WolframResearch/lspserver

### 设置

如果你把 Wolfram System安装在你系统的默认位置, 你可能不需要改变任何设置.

#### 如果 `Wolfram System` 不在默认位置, 那么请指定实际位置:

打开 `命令板`, 输入命令: `Preferences: Configure Language Specific Settings...`,
选择 `Wolfram`, 则打开 `settings.json` 文件,
添加 `wolfram.kernel` 设置:

```json
{
  ...

"wolfram.kernel": "/Applications/Mathematica.app/Contents/MacOS/WolframKernel"

...
}
```

而不要写在特定语言设置中:

```json
"[wolfram]": {

}
```

#### 你也可以改变用于启动 `server` 的命令:

```json
{
  ...

"wolfram.command": [
    "`kernel`",
    "-noinit",
    "-noprompt",
    "-nopaclet",
    "-noicon",
    "-nostartuppaclets",
    "-run",
    "Needs[\"LSPServer`\"];LSPServer`StartServer[]"
]

 ...
}
```

现在你应该可以对 `Wolfram .m` 和 `.wl` 文件的 `语法高亮` 和 `提示`了.
在新的 `.m` 文件中输入这些内容并保存, 以测试这一点.

```wolfram
Which[a, b, a, b]
```

你应该看到关于重复 Clauses 的警告.

### 其他设置

指定没有 `$` 字符不在 `单词分隔符` 范围是很方便的, `$` 字符在 `WL` 中是类字母的字符(普通符号):

```wolfram
"editor.wordSeparators": "`~!@#%^&*()-=+[{]}\\|:'\",.<>/?",
```

### 实验性设置

你可以启用实验性设置, 但不建议.

`implicitTokens` 控制隐含标记的显示, 如 ```  `` ``` 后的 `Null` 和隐含的 `Times` 字符`x`:

```json
{
  ...

  "wolfram.implicitTokens": ["*", ",", ";;", "?"]

  ...
}
```

`semanticTokens` 控制语义高亮, 如 `Module` 变量:

```json
{
  ...

  "wolfram.semanticTokens": true

  ...
}
```

### 故障排除

确保在你的系统中能够找到这些 `paclets`(小程序)

```wolfram
Needs["LSPServer`"]
```

## markdown 设置

### outline 隐藏链接 link

在 setting 中搜索 outline,
取消勾选 `Outline: Show Constants`,
即可隐藏 Explorer->Outline 中的 外链接项.

## vscode 离线安装 插件

[离线安装VSCode 插件](https://zixizixi.cn/vscode-extension-vsix-install)
[vscode离线安装包制作 及 批量安装插件](https://blog.csdn.net/pk0127/article/details/118558950)

在 [插件市场](https://marketplace.visualstudio.com/) 搜索需要下载的插件,
然后点击页面右侧的 Resources/Download Extension, 就会下载 `.vsix` 格式的插件包.
vscode 安装插件的相关命令是

```bash
# 该命令将打印当前的插件列表
code --list-extensions --show-versions

#安装制定版本的vim插件
code --install-extension  vscodevim.vim@1.21.5

#自动安装并强制更新至最新版本
code --install-extension --force  vscodevim.vim

#自动安装匹配的最新版本 此处也可以使用离线的vsix包
code --install-extension  vscodevim.vim

# 卸载指定插件
code --uninstall-extension
```

nushell 命令行

```nushell
let fs = (glob *.vsix); $fs | each {|it| code --install-extension ($it | path expand)}
```
