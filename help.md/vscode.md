# vscode

如果没有特别说明, `C` for `Ctrl`, `A` for `Alt`, `S` for `Shift`.

## 常用设置

[User and Workspace Settings](https://code.visualstudio.com/docs/getstarted/settings)

***
字体
`ctrl+shfit+P` 打开命令面板, 输入`settings`, 查找到`Open Setting (UI)`
然后在设置面板查找`editor.fontFamily`这一项.
在`Editor: Font Family`下面设置字体, 注意有`用户设置`和`工作区设置`, 也就是可以对特定文件夹设置特定的字体.
工作区设置会覆盖用户设置.

可以设置三类字体, 盲猜应该分别是`Serif`,`Sans`,`mono`, 即衬线体, 无衬线体, 等宽字体.

比如设置为

```bash
'PingFang SC Bold', 'Courier New', 'monospace' # 例1
'Noto Serif CJK SC Bold','PingFang SC Bold','Noto Sans Mono CJK SC' # 例2
'PingFang SC Bold','Noto Sans CJK SC Bold','Noto Sans Mono CJK SC' # 例3
'Noto Serif CJK SC Bold','Noto Sans CJK SC Bold','Noto Sans Mono CJK SC' # 例4
'Noto Sans CJK SC Bold','Noto Sans CJK SC Bold','Noto Sans Mono CJK SC' # 例5
'Source Han Sans SC','Noto Sans CJK SC Bold','Noto Sans Mono CJK SC' # 例6
```

设置对应一个`json`文件, 在`json`中的写法是,

```json
  "editor.fontFamily": "'PingFang SC Bold','Noto Sans CJK SC Bold','Noto Sans Mono CJK SC'",
  "terminal.integrated.fontFamily": "Meslo LG M for Powerline"
```

## 各种常用命令

+ `toggle tab key moves focus`: 用 tab 控制焦点切换, 会影响终端补全.

## gitlens blame 显示

文件上方会有小字标示更改`blame`, 这个是`gitlen`插件的功能, 如果不需要, 在`gitlen`设置中关闭即可

## 常见概念

### 预览模式

当你在资源管理器中`单击`或选择一个文件时,它将以`预览模式`显示,并重复使用现存的 `标签`.
如果你正在快速浏览文件,并且不希望每个被访问的文件都有自己的标签,这很有用.
当你开始编辑文件或使用`双击`从资源管理器中打开文件时,将新建标签, 专门用于该文件.

*预览模式中,标签里的文件名是斜体字*

### 分屏

专业提示: 当鼠标悬停在右上角`工具条`上的`分割编辑器`动作时(图标是摊开书本的形状),
按住 `Alt` 可以变换分割屏幕的方向.这是一个快速的方法,可以向右或向下分割.

其他小技巧:

官方文档: [Visual Tricks](https://code.visualstudio.com/docs/getstarted/tips-and-tricks)

### 练习场地

在欢迎页面的右下方,有一个链接导向**互动操场**,在那里你可以互动地尝试`VS Code`的功能.
或者点击`Help > Interactive Playground`.

### 多光标编辑

1. `C+A+S+ 上下左右`, 或 `S+A+鼠标拖拽`
2. `C+A+Up`, `C+A+down`
3. `A+click`
4. `C+A+L` ; 选取所有相同实例

### 智能补全

`C+space`

### 行操作

由于按行处理文本是很常见的,我们提供了一套有用的快捷方式来帮助处理.

1. 复制一行并在当前位置上方或下方插入它; `S+A+down`或`S+A+up`.
1. 分别用`A+上`或`A+下`将整行或选定的行向上或向下移动.
1. 用`C+S+k`删除整行

### 批量重命名

重命名一个符号,如`函数名`或`变量名`,是很容易的.
按下 `F2` 键, 将寻找项目的所有文件中的实例.
你也可以在右键上下文菜单中看到`重构`.

### 自动缩进

如果没有一个好的`格式化工具`,要保持你的代码看起来很好是很难的.
可以用`S+A+F`对整个文档进行编辑.(可以右键查看,快捷键可能有所不同)
或者用 `C+K C+F` 对当前的选择进行处理.

### 代码折叠

在一个大文件中,`折叠`代码段, 增加可读性往往是有用的.
要做到这一点,你可以简单地按`C+S+[`,`C+S+]`,来折叠或展开.

折叠也可以通过左边沟槽(gutter,即行数旁边的竖条)中的`+/-`图标来完成.
要折叠所有部分,请使用`C+K C+0`,要展开所有部分,请使用`C+K C+J`.

### 错误提示

当你编辑你的代码时,错误和警告会以`波浪线`斜线的形式突出显示.
通过按`F8`,你可以依次浏览这些错误并看到详细的错误信息.

### 代码片段生成

通过使用片段(`snippets`),你可以大大加速你的编辑.

输入`try`, 并从建议列表中选择`trycatch`,然后按`tab`来创建一个`try->catch`块.
你的光标将被移动, 例如文字`error`上,以方便编辑.如果存在一个以上的参数,那么按`tab`就可以跳到它.

### 代码片段生成咒语

`Emmet`将`片段`(snippets)的想法提高到新水平.

你可以输入类似`CSS`的表达式,它们可以被动态解析,并根据你在缩写中输入的内容产生输出.

将光标放在合法的`Emmet`缩写或`片段`的末尾,在 `编辑`菜单中选择`Emmet: Expand Abbreviation`,扩展就会生效.
例如:

```css
ul>li.item$*5
```

### JavaScript类型检查

有时,对你的`JavaScript`代码进行`类型检查`, 可以帮助你发现未捕捉到的的错误.
你可以对现有的`JavaScript`代码运行`TypeScript类型检查器`,只需在文件的顶部添加一个`// @ts-check`注释.

```js
// @ts-nocheck
let easy = true;
easy = 42;
```

提示:

你也可以开启工作目录,或应用程序范围的类型检查, 通过在工作区设置, 或用户设置中添加

```json
"javascript.implicitProjectConfig.checkJs": true
```

还可以使用`// @ts-nocheck`忽略文件, `// @ts-ignore`明确忽略某些行.
请查看VS Code中的`JavaScript`文档以了解更多.

### 其他

按`` Ctrl+` ``(倒引号)打开`集成终端`.
然后通过查看[终端文档](https://code.visualstudio.com/docs/editor/integrated-terminal) 来了解它的功能.

按`Ctrl+Shift+G G`来使用`版本控制`.
通过查看[版本控制文档](https://code.visualstudio.com/docs/editor/versioncontrol) 了解如何`添加更改`, `提交`, `更改分支`和`查看差异`等.

通过按`Ctrl+Shift+X`在我们的`集成库`中浏览成千上万的`扩展`.
[拓展文档](https://code.visualstudio.com/docs/editor/extension-gallery)将告诉你如何查看最受欢迎的扩展,禁用已安装的扩展等等.

### 命令板

访问基于目前内容可使用的所有命令:  `Ctrl+Shift+P`

#### 默认快捷键

所有的命令都在`命令板`中,并有相关的键绑定(如果存在的话).
如果你忘记了一个键盘快捷键,可以用`命令板`来帮助你.`Ctrl+Shift+P`

### 快捷键清单

根据平台特性,不同平台的快捷键可能有所不同.下载你所在平台的键盘快捷键参考表.

[macOS键盘](https://code.visualstudio.com/shortcuts/keyboard-shortcuts-macos.pdf)
[Windows键盘](https://code.visualstudio.com/shortcuts/keyboard-shortcuts-windows.pdf)
[Linux键盘](https://code.visualstudio.com/shortcuts/keyboard-shortcuts-linux.pdf)

### 快速打开

快速打开文件, 键盘快捷键: `Ctrl+P`.

提示: 按下`Ctrl+P`,再输入`?`, 可以查看帮助建议.

#### 最近文件导航

重复按下快速打开的快捷键,比如`Ctrl+P` 可以在最近打开的文件中循环跳转.

#### 快开多文件

你可以通过按 `右箭头` 键从 `快速打开`中一次性打开多个文件.
将在后台打开当前选择的文件,你可以继续从 `快速打开` 选择文件.

#### 命令行

`VS Code`有一个强大的命令行界面(CLI),允许你自定义编辑器的启动方式, 以应对各种情况.

请确保`VS Code`二进制文件在你的路径上,这样你就可以简单地输入`code`来启动`VS Code`.
请参见平台特定的设置主题 , 查看`VS Code`在安装过程中, 如何被添加到`环境路径`中.

[Running VS Code on Linux](https://code.visualstudio.com/docs/setup/linux),
[macOS setup](https://code.visualstudio.com/docs/setup/mac),
[Windows setup](https://code.visualstudio.com/docs/setup/windows)

```bash
# open code with current directory
code .

# open the current directory in the most recently used code window
code -r .

# create a new window
code -n

# change the language
code --locale=es

# open diff editor
code --diff <file1> <file2>

# open file at specific line and column <file:line[:character]>
code --goto package.json:10:5

# see help options
code --help

# disable all extensions
code --disable-extensions .
```

### vscode 配置文件夹

工作区的特定文件在根部的`.vscode`文件夹中.
例如,`tasks.json`用于`Task Runner`,`launch.json`用于调试器.

### Status Bar 状态栏

#### 浏览错误

键盘快捷键:  `Ctrl+Shift+M`; 快速跳到项目中的错误和警告.用`F8`或`Shift+F8`循环浏览错误.

你可以通过输入`('errors', 'warnings')` 或文本匹配来过滤 bug.

#### 改变语言模式

键盘快捷键: `Ctrl+K M`; 如果你想为该文件类型持续使用新的语言模式,
你可以使用`Configure File Association for`命令将当前文件扩展名与已安装的语言相关联.

### 个性化 vscode

你可以做很多事情来定制`VS Code`.

+ 改变主题
+ 改变键盘快捷方式
+ 调整设置
+ 添加`JSON验证`
+ 创建`代码段`(snippets)
+ 安装扩展

#### 换主题

键盘快捷键: `Ctrl+K Ctrl+T`.

你可以从VS Code`扩展商店`中安装更多的主题.此外,你可以安装和改变你的文件图标主题.

#### 键盘映射

你是否习惯了其他编辑器的键盘快捷键?你可以安装一个`Keymap`扩展,把你喜欢的编辑器的键盘快捷键带到`VS Code`.
进入 `Preferences` > `Keymap Extensions`,可以看到`Marketplace`上的当前列表.一些比较流行的有:

+ Vim
+ Sublime Text Keymap
+ Emacs Keymap
+ Atom Keymap
+ Eclipse Keymap

#### 自定义快捷键

键盘快捷键: `Ctrl+K Ctrl+S`; 你可以搜索快捷键, 并在`keybindings.json`文件中添加你自己的键位.

详见: [Key Bindings for Visual Studio Code](https://code.visualstudio.com/docs/getstarted/keybindings).

#### 调教设置

默认情况下,VS Code会显示`Settings`编辑器,你可以在`搜索栏`搜索设置项, 在下面浏览列出的设置.

但你仍然可以通过使用`Open Settings (JSON)`命令来编辑底层`settings.json`文件,
或者更改`json`中的`workbench.settings.editor`, 来选择你的默认`setting`编辑器.

打开用户设置`settings.json`; 键盘快捷键: `Ctrl+,`.

##### 粘贴时自动格式化

```json
"editor.formatOnPaste": true
```

##### 改变UI字体样式

```json
// Main editor
"editor.fontSize": 18,
// Terminal panel
"terminal.integrated.fontSize": 14,
// Output panel
"[Log]": {
    "editor.fontSize": 15
}
```

##### 调整界面放大倍数

```json
"window.zoomLevel": 5
```

##### Font ligatures 合字

```json
"editor.fontFamily": "Fira Code",
"editor.fontLigatures": true
```

提示: 你需要安装一种支持`连字`的字体.[FiraCode](https://github.com/tonsky/FiraCode) 是VS Code团队中的一种流行字体.

##### 自动保存

```json
"files.autoSave": "afterDelay"
```

也可以在顶层菜单中打开 `Auto Save`,  `File > Auto Save`.

#### 保存时自动格式化

```json
"editor.formatOnSave": true
```

##### 调整tab大小

```json
"editor.tabSize": 4
```

##### 用空格还是制表符

```json
"editor.insertSpaces": true
```

##### 显示空白字符

```json
"editor.renderWhitespace": "all"
```

##### 忽略文件/文件夹

从编辑窗口中忽略

```json
"files.exclude": {
    "somefolder/": true,
    "somefile": true
}
```

从搜索结果中忽略

```json
"search.exclude": {
    "someFolder/": true,
    "somefile": true
}
```

And many, [many other customizations](https://code.visualstudio.com/docs/getstarted/settings).

##### 为特定语言设置

对于你只想对`特定语言`进行的`设置`,你可以通过`语言标识符`来确定设置的范围.
你可以在[语言标识符](https://code.visualstudio.com/docs/languages/identifiers)参考中找到常用的语言标识符列表.

```json
"[languageid]": {

}
```

提示: 你也可以用 `Configure Language Specific Settings` 命令创建特定语言设置.

#### 添加JSON验证

对许多文件类型默认启用.在`settings.json`中创建你自己的模式(schemas)和验证.

```json
"json.schemas": [
    {
        "fileMatch": [
            "/bower.json"
        ],
        "url": "http://json.schemastore.org/bower"
    }
]
```

或者在特定工作目录下定义的模式,

```json
"json.schemas": [
    {
        "fileMatch": [
            "/foo.json"
        ],
        "url": "./myschema.json"
    }
]
```

or a custom schema

```json
"json.schemas": [
    {
        "fileMatch": [
            "/.myconfig"
        ],
        "schema": {
            "type": "object",
            "properties": {
                "name" : {
                    "type": "string",
                    "description": "The name of the entry"
                }
            }
        }
    },

```

See more in the [JSON documentation][(https://code.visualstudio.com/docs/languages/json).

### 拓展 Extensions

快捷键 : `Ctrl+Shift+X`

#### 寻找拓展

+ In the [VS Code Marketplace](https://marketplace.visualstudio.com/vscode) .
+ Search inside VS Code in the Extensions view.
+ View extension recommendations
+ Community curated extension lists, such as awesome-vscode.

#### 安装拓展

In the `Extensions` view, you can search via the search bar or click the `More Actions` (...) button to filter and sort by install count.

#### 拓展推荐

In the `Extensions` view, click `Show Recommended Extensions` in the `More Actions` (...) button menu.

#### 自建拓展

你对创建你自己的扩展感兴趣吗?
你可以在 [扩展API文档](https://code.visualstudio.com/api) 中了解创建拓展.
特别是查看 [documentation on contribution points](https://code.visualstudio.com/api/references/contribution-points).

+ 配置,configuration
+ 命令,commands
+ 按键绑定,keybindings
+ 语言,languages
+ 调试器,debuggers
+ 语法,grammars
+ 主题,themes
+ 片段,snippets
+ json 验证,jsonValidation

### 文件和文件夹

#### 内部集成终端

快捷键: ``Ctrl+` ``

[Integrated Terminal documentation](https://code.visualstudio.com/docs/editor/integrated-terminal)
[Mastering VS Code's Terminal](https://www.growingwiththeweb.com/2017/03/mastering-vscodes-terminal.html)

#### 自动保存 Auto Save

Open User Settings `settings.json` with `Ctrl+,`

```json
"files.autoSave": "afterDelay"
```

你也可以通过顶层菜单的 `File > Auto Save` 来切换自动保存.

#### 切换侧栏 Toggle Sidebar

快捷键 : `Ctrl+B`

#### 禅模式/佛系模式 Zen mode

快捷键 : `Ctrl+K Z`

进入心无旁骛的`禅模式`.
按 `Esc` 键两次,退出`禅模式`.

#### 并列编辑 Side by side editing

快捷键 : `Ctrl+\`. 你也可以通过拖放`编辑器`来创建新的`编辑组`,并在组之间移动`编辑器`.

#### 编辑器间切换Switch between editors

快捷键 : `Ctrl+1`, `Ctrl+2`, `Ctrl+3`

#### 去往浏览器窗口 Move to Explorer window

快捷键 : `Ctrl+Shift+E`

#### 创建或者打开文件 Create or open a file

快捷键 : `Ctrl+click (Cmd+click on macOS)`

你可以通过将光标移动到`文件链接`处, 并使用`Ctrl+点击` 来快速打开`文件`或`图像`或创建`新文件`.

#### 关闭打开的文件夹Close the currently opened folder

快捷键 : `Ctrl+F4`

#### 编辑历史导航 Navigation history

浏览整个历史: `Ctrl+Tab`.
向后浏览: `Alt+Left`.
向前浏览: `Alt+Right`.

#### 文件拓展名关联 File associations

为没有正确检测的文件创建`语言关联`.例如,许多带有自定义文件扩展名的配置文件实际上是`JSON`.

```json
"files.associations": {
    ".database": "json"
}
```

#### 阻止瞎几把保存 Preventing dirty writes

当磁盘上的文件发生变化,而你试图保存在`VS Code`中的修改时,`VS Code` 会显示一个错误信息.
`VS Code`阻止保存文件,以防止覆盖在其他编辑器中所做的修改.

为了解决保存冲突,点击错误信息中的 `比较` 动作,打开`差异编辑器`,显示磁盘上的文件内容(在左边)和`VS Code`中的内容(在右边)的对比.

使用`编辑器工具栏`中的`动作`来处理`保存冲突`.
你可以`接受`code中的修改, 从而覆盖磁盘上的任何修改,或者 `恢复` 到磁盘上的版本. 恢复意味着你的修改将被丢失.

**注意:** 文件将保持`dirty`状态,无法被保存, 直到你选择这两个行动之一来解决冲突.

### 编辑的魔法 Editing hacks

下面是一些编辑代码的常用功能.如果这些键盘快捷键对你来说并不舒服.
可以考虑为你的旧编辑器安装一个[keymap extension](https://marketplace.visualstudio.com/search?target=VSCode&category=Keymaps&sortBy=Downloads).

提示: 你可以在 `扩展` 视图中用 `Ctrl+K Ctrl+M` 看到推荐的键盘映射扩展,它将搜索过滤设置为`@recommended:keymaps`.

#### 多光标编辑

+ 要在任意位置添加光标,用鼠标选择一个位置并使用`Alt+Click`(在`macOS`上使用`Option+click`).
+ 要在当前位置`上方`或`下方`设置光标,请使用键盘快捷键: `Ctrl+Alt+Up`或`Ctrl+Alt+Down`.
+ 你可以用`Ctrl+Shift+L`为当前选择的所有实例添加额外的光标.

注意: 你也可以通过更改`json`设置文件中的`editor.multiCursorModifier`, 将应用多个光标的`modifier`改为`Ctrl/Commd`.
详见:
[vscode settings](https://code.visualstudio.com/docs/getstarted/settings)
[Multi-cursor Modifier](https://code.visualstudio.com/docs/editor/codebasics#_multicursor-modifier)

如果你不想一次选择所有`实例`, 你可以使用`Ctrl+D`.这只会选择下一个`实例`, 所以你可以逐个添加选择.

#### 列(框)选择

你可以在拖动鼠标的同时按住`Shift+Alt`(在macOS上为`Shift+Option`)来选择文本块.每个被选中的行的末尾都会增加一个单独的光标.
你也可以使用`键盘快捷键`来触发`列选择`.

[键盘快捷键](https://code.visualstudio.com/docs/editor/codebasics#_column-box-selection)

#### 快速滚动 Fast scrolling

按 `Alt` 键可以在编辑器和探索器中`快速滚动`.
默认情况下,`快速滚动`使用`5`倍的速度倍数,但你可以用`Editor: Fast Scroll Sensitivity`设置 (`editor.fastScrollSensitivity`).

#### 向上/向下复制行 Copy line up/down

快捷键 : `Shift+Alt+Up` or `Shift+Alt+Down`

命令 `Copy Line Up/Down` 在 `Linux` 上没有被绑定,因为`VS Code`默认的按键绑定会与`Ubuntu`的按键绑定相冲突.
见[问题#509][(https://github.com/Microsoft/vscode/issues/509)
你仍然可以将命令`editor.action.copyLinesUpAction`和`editor.action.copyLinesDownAction`设置为你自己喜欢的快捷键.

#### 向上/向下移动行 Move line up and down

快捷键 : `Alt+Up or Alt+Down`

#### 缩小/扩大 选择 Shrink / expand selection

快捷键 : `Shift+Alt+Left` or `Shift+Alt+Right`

You can learn more in the [Basic Editing documentation](https://code.visualstudio.com/docs/editor/codebasics#_shrinkexpand-selection).

#### 查找符号 Go to Symbol in File

快捷键 : `Ctrl+Shift+O`

You can group the symbols by kind by adding a colon, `@:`.

#### 跳到指定行 Navigate to a specific line

快捷键 : `Ctrl+G`

#### 撤销光标移动 Undo cursor position

快捷键 : `Ctrl+U`

#### 删除行末空白 Trim trailing whitespace

快捷键 : `Ctrl+K Ctrl+X`

#### 代码格式化 Code formatting

Currently selected source code: `Ctrl+K Ctrl+F`

Whole document format: `Shift+Alt+F`

#### 代码折叠 Code folding

快捷键 : `Ctrl+Shift+[` and `Ctrl+Shift+]`

#### 选中当前行 Select current line

快捷键 : `Ctrl+L`

#### 跳转到文件首尾 Navigate to beginning and end of file

快捷键 : `Ctrl+Home` and `Ctrl+End`

#### 打开markdown预览 Open Markdown preview

在`Markdown`文件中,使用快捷键 : `Ctrl+Shift+V`

#### markdown同步预览 Side by side Markdown edit and preview

在`Markdown`文件中,使用快捷键 : `Ctrl+K V`.
`预览`和`编辑`将与你在两个视图中的滚动`同步`.

### 智能补全 IntelliSense

`Ctrl+Space`来触发`建议`小部件(widget).你可以查看可用的`方法`,`参数提示`,简短的`文档`等.

#### 瞄下定义 Peek

选择一个符号,然后输入`Alt+F12`.或者,你也可以使用上下文菜单.

#### 转到符号定义位置 Go to Definition

选择一个`符号`,然后输入`F12`.或者,你可以使用`上下文菜单`或`Ctrl+点击`(在`macOS`上为`Cmd+点击`).
你可以使用`Go > Back`命令或`Alt+Left`回到你以前的位置.

如果你将鼠标悬停在`type`上时按下`Ctrl`(macOS为`Cmd`),你也可以看到`类型定义`.

#### 转到符号引用 Go to References

选择一个符号,然后输入`Shift+F12`.或者,你也可以使用`上下文菜单`.

#### 打开所有引用视图 Find All References view

选择一个`符号`,然后输入 `Shift+Alt+F12`,打开 `References ` 视图,在一个专门的视图中显示所有文件的符号.

#### 重命名符号 Rename Symbol

选择一个符号然后输入`F2`.或者,你也可以使用上下文菜单.

#### 检索并修改 Search and modify

除了`搜索`和`替换`表达式,你还可以使用带有`捕获组`的`正则表达式`来`搜索`和重用被匹配的部分内容.

按下`Ctrl+F`, 在搜索框中, 通过点击`Use Regular Expression`(`.*`按钮, `Alt+R`)启用正则表达式,
然后编写正则表达式并使用括号`()`, 来定义`捕获组`.

然后在`Replace`字段中, 你可以通过使用`$1`,`$2` 等来重复使用每个`捕获组`匹配到的内容, 相当于临时寄存器.

#### .eslintrc.json --啥??-

Install the [ESLint extension](https://marketplace.visualstudio.com/items?itemName=dbaeumer.vscode-eslint).
Configure your linter however you'd like.
Consult the [ESLint specification](https://eslint.org/docs/user-guide/configuring) for details on its linting rules and options.

Here is configuration to use ES6.

```json
{
  "env": {
    "browser": true,
    "commonjs": true,
    "es6": true,
    "node": true
  },
  "parserOptions": {
    "ecmaVersion": 6,
    "sourceType": "module",
    "ecmaFeatures": {
      "jsx": true,
      "classes": true,
      "defaultParams": true
    }
  },
  "rules": {
    "no-const-assign": 1,
    "no-extra-semi": 0,
    "semi": 0,
    "no-fallthrough": 0,
    "no-empty": 0,
    "no-mixed-spaces-and-tabs": 0,
    "no-redeclare": 0,
    "no-this-before-super": 1,
    "no-undef": 1,
    "no-unreachable": 1,
    "no-use-before-define": 0,
    "constructor-super": 1,
    "curly": 0,
    "eqeqeq": 0,
    "func-names": 0,
    "valid-typeof": 1
  }
}
```

`Lint` : 在计算机科学中, `lint` 是一种工具程序的名称, 它用来标记源代码中, 某些可疑的, 不具结构性(可能造成`bug`)的段落. 它是一种静态进程分析工具, 最早适用于`C`语言, 在`UNIX`平台上开发出来. 后来它成为通用术语, 可用于描述在任何一种电脑编程语言中, 用来标记源代码中有疑义段落的工具.

#### package.json --啥??-

See `IntelliSense` for your `package.json` file.

#### 代码魔法 Emmet syntax

[Support for Emmet syntax](https://code.visualstudio.com/docs/editor/emmet).

### 代码片段生成 Snippets

#### 自定义片段 Create custom snippets

`File > Preferences > User Snippets` (`Code > Preferences > User Snippets` on macOS), select the language, and create a snippet.

```json
"create component": {
    "prefix": "component",
    "body": [
        "class $1 extends React.Component {",
        "",
        "\trender() {",
        "\t\treturn ($2);",
        "\t}",
        "",
        "}"
    ]
},
```

See more details in [Creating your own Snippets](https://code.visualstudio.com/docs/editor/userdefinedsnippets).

### 集成的Git;Git integration

快捷键 : `Ctrl+Shift+G`

Git集成在 `VS Code`中, 是`开箱即用`的(out-of-the-box).你可以从扩展市场上安装其他`SCM`(源代码版本控制)工具.
本节介绍了`Git集成`,但大部分的用户界面和手势对其他`SCM`也适用.

#### 查看更改;Diffs

在 `源代码控制` 视图中(`Ctrl+Shift+G`),选择要比较的文件.

+ `并排对比差别`; 默认为并排对比差别.
+ `内联视图`; 行内对比差别

通过点击右上方的`More Actions(...)` 按钮,选择 `Switch to Inline View` 来切换内联视图.
如果你喜欢`内联视图`,你可以设置 `setting.json` 为`"diffEditor.renderSideBySide": false`.

#### 检查修改;review pane

Navigate through diffs with `F7` and `Shift+F7`. This will present them in a unified patch format.
Lines can be navigated with arrow keys and pressing `Enter` will jump back in the diff editor and the selected line.

#### 编辑待定提交; Edit pending changes

You can make edits directly in the pending changes of the diff view.

#### 分支; Branches

Easily switch between Git branches via the Status Bar.

#### 提交修改;Staging

+ 提交所有; Hover over the number of files and click the plus button.
+ 提交选择范围 ; Stage a portion of a file by selecting that file (using the arrows) and then choosing `Stage Selected Ranges` from the `Command Palette`.

#### 撤销最新提交 Undo last commit

`More Actions > Undo last commit`

#### 查看 Git 输出 See Git output

使用 `VS Code` 可以很容易地看到,实际运行的 `Git` 命令.
这在学习`Git`, 或调试困难的源代码控制问题时很有帮助.

使用`切换输出`命令(`Ctrl+Shift+U`),在下拉菜单中选择`Git`.
或者按下`` Ctrl+` ``, 切换到`OUTPUT`标签页, 在下拉菜单中选择`Git`.

#### 侧边框指示器 Gutter indicators

在`编辑器`中查看`改动`的提示.

See [gutter documentation](https://code.visualstudio.com/docs/editor/versioncontrol#_gutter-indicators) for more details.

#### 解决合并冲突 Resolve merge conflicts

在`合并`过程中,进入 `源代码控制`视图(`Ctrl+Shift+G`),直接点击相应文件名, 在`差异视图`中进行修改, 会显示与上个提交的差异.

#### 将VS Code 设为默认合并工具 Set VS Code as default merge tool

修改`git config`, 全局文件在`~/.gitconfig`.

```bash
git config --global merge.tool code
```

### 调试 Debugging

#### 配置调试器 Configure debugger

打开 `命令板`(`Ctrl+Shift+P`)并选择 `Debug: Open launch.json`.

它将提示你选择与你的项目相匹配的环境(`Node.js`,`Python`,`C++`,等等).这将生成一个`launch.json`文件.
`Node.js`支持是内置的,其他环境需要安装相应的语言扩展.

更多细节见[调试文档](https://code.visualstudio.com/docs/editor/debugging).

### python 配置

`open launch.json` 打开调试文件

有两种标准配置,或者在`code`的集成终端中运行,或者在外部终端运行: 

```json
{
    "name": "Python: Current File (Integrated Terminal)",
    "type": "python",
    "request": "launch",
    "program": "${file}",
    "console": "integratedTerminal"
},
{
    "name": "Python: Current File (External Terminal)",
    "type": "python",
    "request": "launch",
    "program": "${file}",
    "console": "externalTerminal"
}
```

还可以添加其他设置如`args`,但它不属于标准配置的一部分.  比如,你经常运行 `startup.py`,并使用参数 `--port 1593`, 则可以添加如下配置: 

```bash
 {
     "name": "Python: startup.py",
     "type": "python",
     "request": "launch",
     "program": "${workspaceFolder}/startup.py",
     "args" : ["--port", "1593"]
 },
```

+ `name` ;  `vscode` 下拉列表中的名字
+ `type`;  要使用的调试器类型;对于 `Python`代码,将此设置为 `python`.
+ `request` ;  指定调试开始的`模式`.
+ `launch`;  在`program`中指定的文件上启动调试器.
+ `attach`;  将调试器附加到一个已经运行的进程.请看`Remote debugging`的例子.
+ `program`: 程序的路径. `${file}`,当前激活的编辑器,可以是绝对路径,也可以是相对路径,如: `"program": "${workspaceFolder}/pokemongo_bot/event_handlers/__init__.py"`
+ `python`: 用来debug的python 解释器的全路径. 如果不指定,使用`python.pythonPath`,等价于`${config:python.pythonPath}`,
也可以使用环境变量. 还可以向解释器传递参数,`"python": ["<path>", "<arg>",...]`.
+ `args` ; 传递给 python 程序的参数. 如`"args": ["--quiet", "--norepeat", "--port", "1593"]`
+ `stopOnEntry` ; 当设置为`true`时,在地一行停下. 默认忽略,在第一个间断点停下.
+ `console` ;  指定程序如何输出结果,可以设置成`"internalConsole"`,`"externalTerminal"`,`"integratedTerminal" (default)`
+ `cwd` 指定当前工作目录,默认为`${workspaceFolder}` (打开`vscode`的目录)
+ `redirectOutput` ; 是否重定向debug输出. 选择`XXterminal`时,默认关闭. (不在VS code debug window中输出)
+ `justMyCode` ;  `true`或忽略,只调试用户写的代码. `false`也调试标准库函数.
+ `django` ;  当设置为 `true` 时,会激活 `Django` 网络框架特有的调试功能. 
+ `sudo` ; 设置为`true`,且调试窗口选择为`externalTerminal`时,可以提升权限
+ `pyramid` ;  当设置为 `true`时,确保用必要的`pserve`命令启动一个`Pyramid`应用程序.
+ `env` ; 设置可选的环境变量, 为 `debugger` 进程, 除了系统变量之外. `值`必须为字符串.
+ `envFile` ; 包含`环境变量`定义的`文件`的可选路径.参见 `Configuring Python environments - environment variable definitions file`.
+ `gevent`;  如果设置为 `true`,可以对 `gevent monkey-patched` 的代码进行调试.

#### 断点和步进 Breakpoints and stepping through

在`行号`旁边放置`断点`.用`Debug`部件(widget)向前导航.

#### 数据检查 Data inspection

在 `Debug`  面板和控制台中检查变量.

#### 行内变量值预览 Inline values

你可以设置`"debug.inlineValues": true`在调试器中`inline`地看到变量值.
这个功能可能很昂贵,可能会拖慢步进速度,所以默认是禁用的.

### 任务执行器 task runner

#### 自动侦测任务 Auto detect tasks

从顶层菜单中选择`Terminal`,运行`Configure Tasks`命令.
然后选择你想运行的`任务类型`.这将产生一个`tasks.json`文件,内容如下.

```JSON
{
  // See https://go.microsoft.com/fwlink/?LinkId=733558
  // for the documentation about the tasks.json format
  "version": "2.0.0",
  "tasks": [
    {
      "type": "npm",
      "script": "install",
      "group": {
        "kind": "build",
        "isDefault": true
      }
    }
  ]
}
```

`自动探测`偶尔出问题.
请查看[任务文档](https://code.visualstudio.com/docs/editor/tasks), 以使程序正常工作.

#### 从终端菜单运行任务 Run tasks from the Terminal menu

从顶层菜单中选择 `终端`,运行 `Run Task`命令,并选择你要运行的任务.通过运行`Terminate Task`命令来终止正在运行的任务.

定义`任务`的键盘快捷键; 你可以为任何`任务`定义一个键盘快捷键.
从 `命令板`(`Ctrl+Shift+P`),选择`Preferences: Open 快捷键 s File`,
将所需的快捷键绑定到 `workbench.action.tasks.runTask`命令,并将`Task`定义为`args`.

例如,要将 `Ctrl+H`绑定到 `Run tests`任务,添加以下内容:

```json
{
  "key": "ctrl+h",
  "command": "workbench.action.tasks.runTask",
  "args": "Run tests"
}
```

#### 从浏览器运行npm脚本作为任务 Run npm scripts as tasks from the explorer

通过设置`npm.enableScriptExplorer`,你可以启用一个资源管理器, 显示工作空间中定义的脚本.

+ 在资源管理器中,你可以在编辑器中打开一个脚本,将其作为一个任务运行,并通过`node`调试器启动它(当脚本定义了一个调试选项如`--inspect-brk`).
+ 点击时的默认动作是`编辑`脚本.要想设置成`单击运行`脚本,请将`npm.scriptExplorerAction`设置为 `运行`.
+ 使用`package.json`文件中的设置`npm.exclude`, 来排除包含在特定文件夹中的的脚本.
+ 通过设置`npm.enableRunFromFolder`,你可以在文件资源管理器, 某个文件夹的上下文菜单中, 运行`npm`脚本.
当选择一个文件夹时,该设置会启用`Run NPM Script in Folder...`命令.该命令显示该文件夹中包含的`npm`脚本的快速选择列表,你可以选择要脚本作为任务执行.

### 便携模式

VS Code有一个[便携式模式](https://code.visualstudio.com/docs/editor/portable)
它可以让你把`设置`和`数据`保存在与你的安装相同的位置,例如,在一个`USB`驱动器上.

### 内部版本

`Visual Studio Code`团队使用`Insiders`版本来测试`VS Code`的最新功能和错误修正.
你也可以通过[这个链接](https://code.visualstudio.com/insiders/)来使用`Insiders`版本.

+ 对于早期采用者 - Insiders有最新的代码变化,供用户和扩展作者试用.
+ 频繁的构建 - 每天都有新的构建,包括最新的错误修复和功能.
+ 并排安装 - Insiders安装在稳定版的旁边,允许你独立使用这两个版本.
