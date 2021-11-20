# vscode.x

[code.visualstudio.com/docs](https://code.visualstudio.com/docs)
[codebasics](https://code.visualstudio.com/docs/editor/codebasics)

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

[超链接]: https://code.visualstudio.com/docs/editor/integrated-terminal#_links
[错误检测]: https://code.visualstudio.com/docs/editor/tasks

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
[变量参考]: https://code.visualstudio.com/docs/editor/variables-reference

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
