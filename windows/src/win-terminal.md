# windows terminal

可以设置的选项非常多,参考
[Windows 终端中的全局设置](https://docs.microsoft.com/zh-cn/windows/terminal/customize-settings/global-settings)
[Windows 终端中的配置文件设置](https://docs.microsoft.com/zh-cn/windows/terminal/customize-settings/profile-settings#unique-identifier)

如果希望将某个设置应用于所有配置文件,可以将其添加到 `settings.json` 文件中的配置文件列表上方的 `defaults` 部分.

```json
"defaults":
{
    // SETTINGS TO APPLY TO ALL PROFILES
},
"list":
[
    // PROFILE OBJECTS
]
```

## 等宽字体

[Cascadia Code](https://docs.microsoft.com/zh-cn/windows/terminal/cascadia-code)

`Cascadia Code` 是 `Microsoft` 提供的一种新的等宽字体,可为命令行应用程序和文本编辑器提供全新的体验.
`Cascadia Code` 是与 `Windows` 终端一起开发的. 建议将此字体与终端应用程序和文本编辑器(如 `Visual Studio` 和 `Visual Studio Code`)一起使用.

编程连字是通过组合字符创建的字形.  它们在编写代码时最有用.  "Code"变体包含连字, 而"Mono"变体不包含连字.

![连字](https://docs.microsoft.com/zh-cn/windows/terminal/images/programming-ligatures.gif)

+ 简体中文等距更纱黑体+Nerd图标字体库:

[laishulu/Sarasa-Mono-SC-Nerd ](https://github.com/laishulu/Sarasa-Mono-SC-Nerd)

国内镜像:
[whjkd / Sarasa-Mono-SC-Nerd ](https://gitee.com/whjkd/Sarasa-Mono-SC-Nerd)

## 添加其他程序

[Adding profiles for third-party tools](https://github.com/microsoft/terminal/blob/main/doc/user-docs/ThirdPartyToolProfiles.md)

Git Bash

假设`Git Bash`安装到了`C:\\Program Files\\Git`:

```json
{
    "name": "Git Bash",
    "commandline": "C:\\Program Files\\Git\\bin\\bash.exe -li",
    "icon": "C:\\Program Files\\Git\\mingw64\\share\\git\\git-for-windows.ico",
    "startingDirectory": "%USERPROFILE%"
}
```

## 创建自己的配色方案

关于配色方案可以参考: [Windows 终端中的配色方案](https://docs.microsoft.com/zh-cn/windows/terminal/customize-settings/color-schemes)
可以在 `settings.json` 文件的 `schemes` 数组中定义配色方案. 它们是使用以下格式写入的:

```json
{
    "name" : "Campbell",
    "cursorColor": "#FFFFFF",
    "selectionBackground": "#FFFFFF",
    "background" : "#0C0C0C",
    "foreground" : "#CCCCCC",
    "black" : "#0C0C0C",
    "blue" : "#0037DA",
    "cyan" : "#3A96DD",
    "green" : "#13A10E",
    "purple" : "#881798",
    "red" : "#C50F1F",
    "white" : "#CCCCCC",
    "yellow" : "#C19C00",
    "brightBlack" : "#767676",
    "brightBlue" : "#3B78FF",
    "brightCyan" : "#61D6D6",
    "brightGreen" : "#16C60C",
    "brightPurple" : "#B4009E",
    "brightRed" : "#E74856",
    "brightWhite" : "#F2F2F2",
    "brightYellow" : "#F9F1A5"
},
```

除 `name` 以外,每个设置都接受十六进制格式(`"#rgb"` 或 `"#rrggbb"`)的字符串形式的颜色. `cursorColor` 和 `selectionBackground` 设置是可选的.

***
`terminal` 自带的配色方案
`Windows` 终端将这些配色方案包含在 `defaults.json` 文件中,可按住 `alt` 并选择设置按钮来访问该文件.
如果要在一个命令行配置文件中设置配色方案,请添加 `colorScheme` 属性,并将配色方案的 `name` 作为值.

```json
"colorScheme": "COLOR SCHEME NAME"
```

可以使用下列颜色方案

```json
Campbell
Campbell Powershell
Vintage
One Half Dark
One Half Light
Solarized Dark
Solarized Light
Tango Dark
Tango Light
```

### 终端提示

[Windows 终端提示与技巧](https://docs.microsoft.com/zh-cn/windows/terminal/tips-and-tricks)

可以通过按住 `ctrl` 和滚动来缩放 `Windows` 终端的文本窗口. 缩放后,终端会话将保持新的缩放效果.
如果要更改字体大小,可参阅配置文件设置页面.

可以通过按住 `ctrl+shift `和滚动来调整背景的不透明度. 调整后,终端会话将保持新的不透明度.
如果要更改配置文件的 `acrylic` 不透明度,可参阅配置文件设置页面.

## 可执行文件设置

***
命令行

这是在配置文件中使用的可执行文件.
属性名称:  `commandline`
必要性:  可选
接受:  字符串形式的可执行文件名
默认值:  `"cmd.exe"`

***
源

这会存储源自配置文件的配置文件生成器的名称.
此字段没有可发现的值. 有关动态配置文件的其他信息,请访问动态配置文件页.
属性名称:  `source`
必要性:  可选
接受:  字符串

***
起始目录

这是加载 `shell` 时所处的起始目录.
属性名称:  `startingDirectory`
必要性:  可选
接受:  字符串形式的文件夹位置
默认值:  `"%USERPROFILE%"`

备注
在为已安装的 WSL 分发设置打开时的起始目录时,应使用以下格式: `"startingDirectory": "//wsl$/"`,并用分发的名称进行替换.
例如,`"startingDirectory": "//wsl$/Ubuntu-20.04"`.

## 下拉列表设置

***
名称

这是将在下拉菜单中显示的配置文件的名称. 此值还用作在启动时传递给 shell 的"标题".
某些 shell(如 bash)可能会选择忽略此初始值,而其他 shell(Command Prompt, PowerShell)可能会在应用程序的生存期内使用此值.
可以使用 tabTitle 替代此"标题"行为.
属性名称:  `name`
必要性:  必需
接受:  字符串

***
图标

这会设置选项卡和下拉菜单中显示的图标.
属性名称:  `icon`
必要性:  可选
接受:  字符串形式的文件位置

***
隐藏下拉列表中的某个配置文件

如果 `hidden` 设置为 `true`,则配置文件将不会显示在配置文件列表中.
这可用于隐藏默认配置文件和动态生成的配置文件,同时并将它们保留在设置文件中. 若要详细了解动态配置文件,请访问动态配置文件页.
属性名称:  `hidden`
必要性:  可选
接受:  `true`,`false`
默认值:  `false`
这两个`value`都不带`"`

## 文本设置

***
字体

这是配置文件中使用的字体名称. 如果找不到或无效,终端将尝试回退到 `Consolas`.
若要了解默认字体 (`Cascadia Mono`) 的其他变体,请访问 `Cascadia Code` 页.
属性名称:  `fontFace`
必要性:  可选
接受:  字符串形式的字体名称.
默认值:  `"Cascadia Mono"`

***
字体大小

这将设置配置文件的字体大小(以磅为单位).
属性名称:  `fontSize`
必要性:  可选
接受:  整数
默认值:  `12`

***
字体粗细(预览)

此属性设置配置文件字体的粗细(笔画的粗细).
属性名称:  `fontWeight`
必要性:  可选
接受:  `"normal", "thin", "extra-light", "light", "semi-light", "medium", "semi-bold", "bold", "extra-bold", "black", "extra-black"`,或与 `OpenType` 字体粗细的数值表示形式相对应的整数
默认值:  `"normal"`
此功能仅在 Windows 终端预览中可用.

***
消除文本锯齿

此方法控制呈现器中文本的消除锯齿方式. 请注意,更改此设置将需要启动新的终端实例.
`Windows` 终端消除文本锯齿
属性名称:  `antialiasingMode`
必要性:  可选
接受:  `"grayscale", "cleartype", "aliased"`

默认值: `"grayscale"`

***
键盘设置

`AltGr` 别名(预览)
通过它可以控制 `Windows` 终端是否将 `ctrl+alt` 视为 `AltGr` 的别名.
属性名称:  `altGrAliasing`
必要性:  可选
接受:  `true`, `false`
默认值:  `true`
此功能仅在 Windows 终端预览中可用.

## 颜色设置

***
配色方案

这是配置文件中使用的配色方案名称. 配色方案是在 `schemes` 对象中定义的. 可以在配色方案页上找到更详细的信息.
属性名称:  `colorScheme`
必要性:  可选
接受:  字符串形式的配色方案的名称

默认值: `"Campbell"`

## Acrylic 设置

***
启用 `acrylic`

如果设置为 `true`,窗口将使用 `acrylic` 背景. 设置为 `false` 时,窗口将为普通的, 不带纹理的背景. 由于操作系统限制,透明度仅适用于焦点窗口.
属性名称:  `useAcrylic`
必要性:  可选
接受:  `true`, `false`
默认值:  `false`

***
`Acrylic` 不透明度

将 `useAcrylic` 设置为 `true` 时,这会设置该配置文件的窗口透明度. 接受的浮点值为 `0-1`.
属性名称:  `acrylicOpacity`
必要性:  可选
接受:  `0-1` 的浮点值的数字
默认值:  `0.5`

## 背景图像设置

***
设置背景图像

这将设置要在窗口背景上绘制的图像的文件位置. 背景图像可以是 `.jpg`, `.png `或 `.gif` 文件.
属性名称:  `backgroundImage`
必要性:  可选
接受:  字符串形式的文件位置

***
背景图像拉伸模式

这将设置如何调整背景图像的大小以填充窗口.
属性名称:  `backgroundImageStretchMode`
必要性:  可选
接受:  `"none", "fill", "uniform", "uniformToFill"`
默认值:  `"uniformToFill"`

***
背景图像对齐

这会设置背景图像与窗口边界对齐的方式.
属性名称:  `backgroundImageAlignment`
必要性:  可选
接受:  `"center", "left", "top", "right", "bottom", "topLeft", "topRight", "bottomLeft", "bottomRight"`
默认值:  `"center"`

***
背景图像不透明度

这会设置背景图像的透明度.
属性名称:  `backgroundImageOpacity`
必要性:  可选
接受:  `0-1` 的浮点值的数字
默认值:  `1.0`

## 滚动设置

滚动条可见性
这将设置滚动条的可见性.
属性名称:  `scrollbarState`
必要性:  可选
接受:  `"visible", "hidden"`

***
键入时滚动到输入行

如果设置为 `true`,则在键入时,窗口将滚动到命令输入行. 如果设置为 `false`,则在开始键入时,窗口不会滚动.
属性名称:  `snapOnInput`
必要性:  可选
接受:  `true`, `false`
默认值:  `true`

***
历史记录大小

这会设置在窗口显示的内容上方可以回滚的行数.
属性名称:  `historySize`
必要性:  可选
接受:  整数
默认值:  `9001`

## 退出时配置文件的关闭方式

这将设置配置文件如何响应终止或启动失败.
当键入 `exit` 或进程正常退出时,`"graceful"` 将关闭配置文件. `"always"` 将始终关闭配置文件,而 `"never"` 将永远不会关闭配置文件.
`true` 和 `false` 分别被接受为 `"graceful"` 和 `"never"` 的同义词.
属性名称:  `closeOnExit`
必要性:  可选
接受:  `"graceful", "always", "never", true, false`
默认值:  `"graceful"`

## 编码为UTF-8

[cmd,power shell设置默认编码为UTF-8](https://www.zhihu.com/question/54724102/answer/140852198)

注: 以下内容在非Windows平台上写的, 可能会有拼写错误, 如果有, 请指正, 我会尽快修正.
可以用Powershell的配置文件($PROFILE)来实现. $PROFILE默认文件不存在, 需要创建.

```Powershell
New-Item $PROFILE -ItemType File -Force此时会在文档下产生一个ps1文件,
```

该文件会在Powershell启动的时候加载. 在这个配置文件里加上一句:

```Powershell
[System.Console]::OutputEncoding=[System.Text.Encoding]::GetEncoding(65001)
```

当然, 这里就涉及到了Powershell的执行策略(Execution Policy)的问题,
你需要设置允许 Powershell执行脚本, 我是用的Unrestricted 策略:

```Powershell
Set-ExecutionPolicy Unrestricted
```
