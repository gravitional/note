# learn.vscode.md

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

toggle tab key moves focus: 用 tab 控制焦点切换, 会影响终端补全.

## gitlens blame 显示

文件上方会有小字标示更改`blame`, 这个是`gitlen`插件的功能, 在`gitlen`设置中关闭即可

## 常见概念

***
预览模式

When you `single-click` or select a file in the Explorer, it is shown in a `preview mode` and reuses an existing `Tab`. This is useful if you are quickly browsing files and don't want every visited file to have its own Tab. When you start editing the file or use `double-click` to open the file from the Explorer, a new Tab is dedicated to that file.

*预览模式的文件名是斜体字*

***
分屏

Pro Tip:
If you `press` and hold the `Alt` key while hovering over the **toolbar action** to split an editor, it will offer to split to the **other orientation**. This is a fast way to split either to the right or to the bottom.

***
小技巧

official document: [Visual Tricks][]

[Visual Tricks]: https://code.visualstudio.com/docs/getstarted/tips-and-tricks

### 练习场地

In the bottom right of the Welcome page, there is a link to the **Interactive playground** where you can interactively try out VS Code's features.
`Help > Interactive Playground`.

***
多光标编辑

1. `C+A+S+ up down left right`, or `S+A+mouse drag`
2. `C+A+Up`, `C+A+down`
3. `A+click`
4. `C+A+L` select all occurrence

***
智能补全

`C+space`

***
行操作

Since it's very common to work with the entire text in a line we provide a set of useful shortcuts to help with this.

1. Copy a line and insert it above or below the current position
`S+A+down or S+A+up`
1. Move an entire line or selection of lines up or down with `A+up` `A+down` respectively.
1. Delete the entire line with `C+S+k`

***
批量重命名

It's easy to rename a symbol such as a function name or variable name.
press `F2`
this will occur across all files in a project.
You can also see refactoring in the right-click context menu.

***
自动缩进

Keeping your code looking great is hard without a good formatter.

either for the entire document with `S+A+F`

or for the current selection with `C+K C+F` .

Both of these options are also available through the right-click context menu.

***
代码折叠

In a large file it can often be useful to collapse sections of code to increase readability.

To do this, you can simply press `C+S+[` `C+S+]`, to fold or unfold

Folding can also be done with the +/- icons in the left gutter.

To fold all sections use `C+K C+0`  or to unfold all use `C+K C+J`.

***
错误提示

Errors and warnings are highlighted with squiggles as you edit your code .

In the sample below you can see a number of syntax errors.

By pressing `F8` you can navigate across them in sequence and see the detailed error message.

***
代码片段生成

You can greatly accelerate your editing through the use of snippets.

Simply start typing `try` and select `trycatch` from the suggestion list and press `tab` to create a `try->catch` block.

Your cursor will be placed on the text `error` for easy editing. If more than one parameter exists then press `tab` to jump to it.

***
代码片段生成咒语

`Emmet` takes the snippets idea to a whole new level:

you can type CSS-like expressions that can be dynamically parsed, and produce output depending on what you type in the abbreviation.

Try it by selecting `Emmet: Expand Abbreviation` from the `Edit` menu with the cursor at the end of a valid Emmet abbreviation or snippet and the expansion will occur.

`ul>li.item$*5`

***
JavaScript类型检查

Sometimes type checking your JavaScript code can help you spot mistakes you might have not caught otherwise.
You can run the TypeScript type checker against your existing JavaScript code by simply adding a `// @ts-check` comment to the top of your file.

```JavaScript
// @ts-nocheck

let easy = true;
easy = 42;
```

Tip:
You can also enable the checks workspace or application wide by adding `"javascript.implicitProjectConfig.checkJs"`: true
to your workspace or user settings
and explicitly ignoring files or lines using `// @ts-nocheck` and `// @ts-ignore`.
Check out the docs on JavaScript in VS Code to learn more.

***
其他

Open the Integrated Terminal by pressing `C+`,
then see what's possible by reviewing the [terminal documentation][]

Work with version control by pressing `C+S+G G`.
Understand how to stage, commit, change branches, and view diffs and more by reviewing the [version control documentation][]

Browse thousands of extensions in our integrated gallery by pressing `C+S+X`.
 The [documentation][] will show you how to see the most popular extensions, disable installed ones and more.

[terminal documentation]: https://code.visualstudio.com/docs/editor/integrated-terminal

[version control documentation]: https://code.visualstudio.com/docs/editor/versioncontrol

[documentation]: https://code.visualstudio.com/docs/editor/extension-gallery

### 命令板

Access all available commands based on your current context.

Keyboard Shortcut: `Ctrl+Shift+P`

### 默认快捷键

All of the commands are in the Command Palette with the associated key binding (if it exists).
If you forget a keyboard shortcut, use the Command Palette to help you out.

### 快捷键清单

Download the keyboard shortcut reference sheet for your platform ([macOS Keyboard][], [Windows Keyboard][], [Linux Keyboard][]).

[Windows Keyboard]: https://code.visualstudio.com/shortcuts/keyboard-shortcuts-windows.pdf

[macOS Keyboard]: https://code.visualstudio.com/shortcuts/keyboard-shortcuts-macos.pdf

[Linux Keyboard]: https://code.visualstudio.com/shortcuts/keyboard-shortcuts-linux.pdf

### 快速打开

Quickly open files.
Keyboard Shortcut: `Ctrl+P`

Tip: Type `?` to view help suggestions.

***
最近文件导航

Repeat the `Quick Open` keyboard shortcut to cycle quickly between recently opened files.

***
快开多文件

You can open multiple files from `Quick Open` by pressing the `Right arrow` key. This will open the currently selected file in the background and you can continue selecting files from `Quick Open`.

***
命令行

VS Code has a powerful command line interface (CLI) which allows you to customize how the editor is launched to support various scenarios.

Make sure the VS Code binary is on your path so you can simply type '`code`' to launch VS Code. See the platform specific setup topics if VS Code is added to your environment path during installation ([Running VS Code on Linux][], [macOS setup][], [Windows setup][]).

[Windows setup]: https://code.visualstudio.com/docs/setup/windows

[Running VS Code on Linux]: https://code.visualstudio.com/docs/setup/linux

[macOS setup]: https://code.visualstudio.com/docs/setup/mac

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

Workspace specific files are in a `.vscode` folder at the root.
For example, `tasks.json` for the Task Runner and `launch.json` for the debugger.

### Status Bar 状态栏

***
浏览错误

Keyboard Shortcut: `Ctrl+Shift+M`

Quickly jump to errors and warnings in the project.
Cycle through errors with `F8` or `Shift+F8`

You can filter problems either by type `('errors', 'warnings')` or text matching.

***
改变语言模式

Keyboard Shortcut: `Ctrl+K M`

If you want to persist the new language mode for that file type, you can use the `Configure File Association for` command to associate the current file extension with an installed language.

### 个性化 vscode

There are many things you can do to customize VS Code.

+ Change your theme
+ Change your keyboard shortcuts
+ Tune your settings
+ Add JSON validation
+ Create snippets
+ Install extensions

***
换主题

Keyboard Shortcut: `Ctrl+K Ctrl+T`
You can install more themes from the VS Code extension `Marketplace`.

Additionally, you can install and change your File Icon themes.

***
键盘映射

Are you used to keyboard shortcuts from another editor?
You can install a Keymap extension that brings the keyboard shortcuts from your favorite editor to VS Code. Go to `Preferences` > `Keymap Extensions` to see the current list on the Marketplace. Some of the more popular ones:

+ Vim
+ Sublime Text Keymap
+ Emacs Keymap
+ Atom Keymap
+ Eclipse Keymap

***
自定义快捷键

Keyboard Shortcut: `Ctrl+K Ctrl+S`
You can search for shortcuts and add your own keybindings to the `keybindings.json` file.

See more in [Key Bindings for Visual Studio Code][].

[Key Bindings for Visual Studio Code]: https://code.visualstudio.com/docs/getstarted/keybindings

***
调教设置

By default VS Code shows the Settings editor, you can find settings listed below in a search bar, but you can still edit the underlying `settings.json` file by using the `Open Settings (JSON)` command or by changing your default settings editor with the `workbench.settings.editor` setting.

Open User Settings `settings.json`
Keyboard Shortcut: `Ctrl+,`

***
粘贴时自动格式化

```json
"editor.formatOnPaste": true
```

***
改变UI字体样式

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

Change the zoom level --调整界面放大倍数

```json
"window.zoomLevel": 5
```

***
Font ligatures 合字

```json
"editor.fontFamily": "Fira Code",
"editor.fontLigatures": true
```

Tip: You will need to have a font installed that supports font ligatures.
[FiraCode][] is a popular font on the VS Code team.

[FiraCode]: https://github.com/tonsky/FiraCode

***
自动保存

```json
"files.autoSave": "afterDelay"
```

You can also toggle Auto Save from the top-level menu with the `File > Auto Save`.

***
保存时自动格式化

```json
"editor.formatOnSave": true
```

***
调整`tab`大小

```json
"editor.tabSize": 4
```

***
用`空格`还是`制表符`

```json
"editor.insertSpaces": true
```

***
显示空白字符

```json
"editor.renderWhitespace": "all"
```

***
忽略文件/文件夹

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

And many, [many other customizations][].

[many other customizations]: https://code.visualstudio.com/docs/getstarted/settings

***
为特定语言设置

For the settings, which you only want for specific languages, you can scope the settings by the language identifier. You can find a list of commonly used language ids in the [Language Identifiers][] reference.

```json
"[languageid]": {

}
```

Tip: You can also create language specific settings with the `Configure Language Specific Settings` command.

[Language Identifiers]: https://code.visualstudio.com/docs/languages/identifiers

***
添加JSON验证

Enabled by default for many file types. Create your own schema and validation in `settings.json`
schemas : 模式, 纲要

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

or for a schema defined in your workspace

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

See more in the [JSON documentation][] .

[JSON documentation]: https://code.visualstudio.com/docs/languages/json

### Extensions 拓展

Keyboard Shortcut: `Ctrl+Shift+X`

***
寻找拓展

+ In the [VS Code Marketplace][] .
+ Search inside VS Code in the Extensions view.
+ View extension recommendations
+ Community curated extension lists, such as awesome-vscode.

[VS Code Marketplace]: https://marketplace.visualstudio.com/vscode

***
安装拓展

In the `Extensions` view, you can search via the search bar or click the `More Actions` (...) button to filter and sort by install count.

***
拓展推荐

In the `Extensions` view, click `Show Recommended Extensions` in the `More Actions` (...) button menu.

***
自建拓展

Are you interested in creating your own extension?
You can learn how to do this in the [Extension API documentation][], specifically check out the [documentation on contribution points][].

+ configuration
+ commands
+ keybindings
+ languages
+ debuggers
+ grammars
+ themes
+ snippets
+ jsonValidation

[Extension API documentation]: https://code.visualstudio.com/api

[documentation on contribution points]: https://code.visualstudio.com/api/references/contribution-points

### Files and folders --文件和文件夹

#### Integrated Terminal --自带的终端

Keyboard Shortcut: ``Ctrl+` ``

Further reading:

[Integrated Terminal documentation][]
[Mastering VS Code's Terminal article][]

[Integrated Terminal documentation]: https://code.visualstudio.com/docs/editor/integrated-terminal

[Mastering VS Code's Terminal article]: https://www.growingwiththeweb.com/2017/03/mastering-vscodes-terminal.html

#### Auto Save --自动保存

Open User Settings `settings.json` with `Ctrl+,`

```json
"files.autoSave": "afterDelay"
```

You can also toggle Auto Save from the top-level menu with the `File > Auto Save`.

#### Toggle Sidebar --切换侧栏

Keyboard Shortcut: `Ctrl+B`

#### Zen mode --禅模式/佛系模式

Keyboard Shortcut: `Ctrl+K Z`

Enter distraction free Zen mode.

Press `Esc` twice to exit Zen Mode.

#### Side by side editing --并列编辑

Keyboard Shortcut: `Ctrl+\`

You can also drag and drop editors to create new editor groups and move editors between groups.

#### Switch between editors --编辑器间切换

Keyboard Shortcut: `Ctrl+1`, `Ctrl+2`, `Ctrl+3`

### Move to Explorer window --去往浏览器窗口

Keyboard Shortcut: `Ctrl+Shift+E`

#### Create or open a file --创建或者打开文件

Keyboard Shortcut: `Ctrl+click (Cmd+click on macOS)`

You can quickly open a file or image or create a new file by moving the cursor to the file link and using `Ctrl+click`.

#### Close the currently opened folder --关闭打开的文件夹

Keyboard Shortcut: `Ctrl+F4`

#### Navigation history --编辑历史导航

Navigate entire history: `Ctrl+Tab`

Navigate back: `Alt+Left`

Navigate forward: `Alt+Right`

#### File associations --文件拓展名关联

Create language associations for files that aren't detected correctly. For example, many configuration files with custom file extensions are actually JSON.

```json
"files.associations": {
    ".database": "json"
}
```

#### Preventing dirty writes --阻止瞎几把保存

VS Code will show you an error message when you try to save a file that cannot be saved because it has changed on disk. VS Code blocks saving the file to prevent overwriting changes that have been made outside of the editor.

In order to resolve the save conflict, click the `Compare` action in the error message to open a diff editor that will show you the contents of the file on disk (to the left) compared to the contents in VS Code (on the right):

Use the actions in the editor toolbar to resolve the save conflict. You can either `Accept` your changes and thereby overwriting any changes on disk, or `Revert` to the version on disk. Reverting means that your changes will be lost.

**Note:** The file will remain dirty and cannot be saved until you pick one of the two actions to resolve the conflict.

### Editing hacks --编辑的魔法

Here is a selection of common features for editing code.
If the keyboard shortcuts aren't comfortable for you, consider installing a [keymap extension][] for your old editor.

Tip: You can see recommended keymap extensions in the `Extensions` view with `Ctrl+K Ctrl+M` which filters the search to `@recommended:keymaps`.

[keymap extension]: https://marketplace.visualstudio.com/search?target=VSCode&category=Keymaps&sortBy=Downloads

#### Multi cursor selection --多光标编辑

To add cursors at arbitrary positions, select a position with your mouse and use `Alt+Click` (`Option+click` on macOS).

To set cursors above or below the current position use:

Keyboard Shortcut: `Ctrl+Alt+Up` or `Ctrl+Alt+Down`

You can add additional cursors to all occurrences of the current selection with `Ctrl+Shift+L`.

Note: You can also change the modifier to `Ctrl/Cmd` for applying multiple cursors with the `editor.multiCursorModifier` [vscode settings][] . See [Multi-cursor Modifier][] for details.

If you do not want to add all occurrences of the current selection, you can use `Ctrl+D` instead. This only selects the next occurrence after the one you selected so you can add selections one by one.

add cursor to next occurrences of current selection one by one

[vscode settings]: https://code.visualstudio.com/docs/getstarted/settings

[Multi-cursor Modifier]: https://code.visualstudio.com/docs/editor/codebasics#_multicursor-modifier

Column (box) selection

You can select blocks of text by holding `Shift+Alt` (`Shift+Option` on macOS) while you drag your mouse. A separate cursor will be added to the end of each selected line.

You can also use [keyboard shortcuts] to trigger column selection.

[keyboard shortcuts]: https://code.visualstudio.com/docs/editor/codebasics#_column-box-selection

#### Fast scrolling --快速滚动

Pressing the `Alt` key enables fast scrolling in the editor and Explorers. By default, fast scrolling uses a 5X speed multiplier but you can control the multiplier with the **Editor: Fast Scroll Sensitivity** (`editor.fastScrollSensitivity`) setting.

#### Copy line up / down --向上/向下复制行

Keyboard Shortcut: `Shift+Alt+Up` or `Shift+Alt+Down`

The commands `Copy Line Up/Down` are unbound on Linux because the VS Code default keybindings would conflict with Ubuntu keybindings, see [Issue #509][]. You can still set the commands `editor.action.copyLinesUpAction` and `editor.action.copyLinesDownAction` to your own preferred keyboard shortcuts.

[Issue #509]: https://github.com/Microsoft/vscode/issues/509

#### Move line up and down --向上/向下移动行

Keyboard Shortcut: `Alt+Up or Alt+Down`

#### Shrink / expand selection --缩小/扩大 选择

Keyboard Shortcut: `Shift+Alt+Left` or `Shift+Alt+Right`

You can learn more in the [Basic Editing documentation][].

[Basic Editing documentation]: https://code.visualstudio.com/docs/editor/codebasics#_shrinkexpand-selection

#### Go to Symbol in File --查找符号

Keyboard Shortcut: `Ctrl+Shift+O`

You can group the symbols by kind by adding a colon, `@:`.

#### Navigate to a specific line --跳到指定行

Keyboard Shortcut: `Ctrl+G`

#### Undo cursor position --撤销光标操作

Keyboard Shortcut: `Ctrl+U`

#### Trim trailing whitespace --修剪多余空白

Keyboard Shortcut: `Ctrl+K Ctrl+X`

#### Code formatting --代码格式化

Currently selected source code: `Ctrl+K Ctrl+F`

Whole document format: `Shift+Alt+F`

#### Code folding --代码折叠

Keyboard Shortcut: `Ctrl+Shift+[` and `Ctrl+Shift+]`

#### Select current line --选中当前行

Keyboard Shortcut: `Ctrl+L`

#### Navigate to beginning and end of file --跳转到文件首尾

Keyboard Shortcut: `Ctrl+Home` and `Ctrl+End`

#### Open Markdown preview --打开markdown预览

In a Markdown file, use
Keyboard Shortcut: `Ctrl+Shift+V`

#### Side by side Markdown edit and preview --markdown同步预览

In a Markdown file, use
Keyboard Shortcut: `Ctrl+K V`

The preview and editor will synchronize with your scrolling in either view.

### IntelliSense --智能补全

`Ctrl+Space` to trigger the Suggestions widget.

You can view available methods, parameter hints, short documentation, etc.

#### Peek --偷窥下定义

Select a symbol then type `Alt+F12`. Alternatively, you can use the context menu.

#### Go to Definition --转到符号定义位置

Select a symbol then type `F12`. Alternatively, you can use the `context menu` or `Ctrl+click` (`Cmd+click` on macOS).

You can go back to your previous location with the `Go > Back` command or `Alt+Left`.

You can also see the type definition if you press `Ctrl` (`Cmd` on macOS) when you are hovering over the type.

#### Go to References --转到符号参考

Select a symbol then type `Shift+F12`. Alternatively, you can use the context menu.

#### Find All References view --打开所有参考视图

Select a symbol then type `Shift+Alt+F12` to open the References view showing all your file's symbols in a dedicated view.

#### Rename Symbol --重命名符号

Select a symbol then type `F2`. Alternatively, you can use the context menu.

#### Search and modify --检索并修改

Besides searching and replacing expressions, you can also search and reuse parts of what was matched, using regular expressions with capturing groups.
Enable regular expressions in the search box by clicking the **Use Regular Expression** `.*` button (`Alt+R`) and then write a regular expression and use parenthesis to define groups.
You can then reuse the content matched in each group by using `$1`, `$2`, etc. in the Replace field.

#### .eslintrc.json --啥??-

Install the [ESLint extension][]. Configure your linter however you'd like. Consult the [ESLint specification][] for details on its linting rules and options.

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

[ESLint extension]: [ESLint extension](https://marketplace.visualstudio.com/items?itemName=dbaeumer.vscode-eslint)

[ESLint specification]: https://eslint.org/docs/user-guide/configuring

#### package.json --啥??-

See IntelliSense for your `package.json` file.

#### Emmet syntax --代码魔法

[Support for Emmet syntax][].

[Support for Emmet syntax]: https://code.visualstudio.com/docs/editor/emmet

### Snippets --代码片段生成-

#### Create custom snippets 自定义片段

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

See more details in [Creating your own Snippets][].

[Creating your own Snippets]: https://code.visualstudio.com/docs/editor/userdefinedsnippets

### Git integration --自带的Git

Keyboard Shortcut: `Ctrl+Shift+G`

Git integration comes with VS Code "out-of-the-box". You can install other SCM providers from the extension Marketplace.
This section describes the Git integration but much of the UI and gestures are shared by other SCM providers.

#### Diffs --对比差别

From the `Source Control` view(`Ctrl+Shift+G`), select the file to diff.

Side by side 并排对比差别

Default is side by side diff.

Inline view 行内对比差别

Toggle inline view by clicking the `More Actions` (...) button in the top right and selecting `Switch to Inline View`.

If you prefer the inline view, you can set `"diffEditor.renderSideBySide": false`.

#### Review pane --检查修改

Navigate through diffs with `F7` and `Shift+F7`. This will present them in a unified patch format.
Lines can be navigated with arrow keys and pressing `Enter` will jump back in the diff editor and the selected line.

#### Edit pending changes --编辑待定改动

You can make edits directly in the pending changes of the diff view.

#### Branches --分支

Easily switch between Git branches via the Status Bar.

#### Staging --提交修改

**Stage all**
Hover over the number of files and click the plus button.

**Stage selected**
Stage a portion of a file by selecting that file (using the arrows) and then choosing `Stage Selected Ranges` from the `Command Palette`.

#### Undo last commit --撤销最新提交

`More Actions > Undo last commit`

#### See Git output --查看 Git 输出

VS Code makes it easy to see what Git commands are actually running.
This is helpful when learning Git or debugging a difficult source control issue.

Use the **Toggle Output** command (`Ctrl+Shift+U`) and select `Git` in the drop-down.

#### Gutter indicators --侧边框 指示器

View diff decorations in editor. See [gutter documentation][] for more details.

[gutter documentation]: https://code.visualstudio.com/docs/editor/
versioncontrol#_gutter-indicators

#### Resolve merge conflicts --解决合并冲突

During a merge, go to the `Source Control` view (`Ctrl+Shift+G`) and make changes in the diff view.

#### Set VS Code as default merge tool --将VS Code 设为默认合并工具

```bash
git config --global merge.tool code
```

### Debugging --调试

#### Configure debugger --配置调试器

Open the **Command Palette** (`Ctrl+Shift+P`) and select `Debug: Open launch.json`, which will prompt you to select the environment that matches your project (Node.js, Python, C++, etc). This will generate a `launch.json` file. Node.js support is built-in and other environments require installing the appropriate language extensions. See the [debugging documentation][] for more details.

[debugging documentation]: https://code.visualstudio.com/docs/editor/debugging

#### Breakpoints and stepping through --断点和步进

Place breakpoints next to the line number. Navigate forward with the Debug widget.

#### Data inspection --数据筛查

Inspect variables in the `Debug` panels and in the console.

#### Inline values --行内变量值预览

You can set `"debug.inlineValues": true` to see variable values inline in the debugger. This feature can be expensive and may slow down stepping, so it is disabled by default.

### task runner --任务执行器

#### Auto detect tasks --自动侦测任务

Select `Terminal` from the top-level menu, run the command `Configure Tasks`, then select the type of task you'd like to run. This will generate a `tasks.json` file with content like the following. See the [Tasks documentation][] for more details.

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

There are occasionally issues with auto generation. Check out the documentation for getting things to work properly.

[Tasks documentation]: https://code.visualstudio.com/docs/editor/tasks

#### Run tasks from the Terminal menu --从终端菜单运行任务

Select `Terminal` from the top-level menu, run the command `Run Task`, and select the task you want to run. Terminate the running task by running the command `Terminate Task`

Define keyboard shortcuts for tasks

You can define a keyboard shortcut for any task. From the `Command Palette` (`Ctrl+Shift+P`), select `Preferences: Open Keyboard Shortcuts File`, bind the desired shortcut to the `workbench.action.tasks.runTask` command, and define the `Task` as `args`.

For example, to bind `Ctrl+H` to the `Run tests` task, add the following:

```json
{
  "key": "ctrl+h",
  "command": "workbench.action.tasks.runTask",
  "args": "Run tests"
}
```

#### Run npm scripts as tasks from the explorer --从浏览器运行npm脚本作为任务

With the setting `npm.enableScriptExplorer`, you can enable an explorer that shows the scripts defined in your workspace.

Filter problems

From the explorer you can open a script in the editor, run it as a task, and launch it with the node debugger (when the script defines a debug option like `--inspect-brk`).
The default action on click is to open the script. To run a script on a single click, set `npm.scriptExplorerAction` to "run".
Use the setting `npm.exclude` to exclude scripts in `package.json` files contained in particular folders.

With the setting `npm.enableRunFromFolder`, you can enable to run npm scripts from the File Explorer's context menu for a folder.
The setting enables the command `Run NPM Script in Folder...` when a folder is selected. The command shows a Quick Pick list of the npm scripts contained in this folder and you can select the script to be executed as a task.

### Portable mode --VS Code 便携模式

VS Code has a [Portable mode][] which lets you keep settings and data in the same location as your installation, for example, on a USB drive.

[Portable mode]: https://code.visualstudio.com/docs/editor/portable

### Insiders builds --VS Code 内部版本

The Visual Studio Code team uses the Insiders version to test the latest features and bug fixes of VS Code. You can also use the Insiders version by [downloading it here][].

+ For Early Adopters - Insiders has the most recent code changes for users and extension authors to try out.
+ Frequent Builds - New builds every day with the latest bug fixes and features.
+ Side-by-side install - Insiders installs next to the Stable build allowing you to use either independently.

[downloading it here]: https://code.visualstudio.com/insiders/
