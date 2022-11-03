# vs 插件 VAssistX

## 快捷键

+ 寻找符号; `S+A+S`
+ 寻找引用; `S+A+F`
+ Goto Implementation; `Alt+G`
+ Goto Related; `S+A+G`
+ 列出所有 Method; `Alt+M`
+ 打开对应的 `.h`/`.cpp`; `Alt+O`
+ 打开解决方案中的文件; `S+A+O`

[Documentation for Visual Assist](https://docs.wholetomato.com/default.asp)
[Introduction to VA Snippets](https://docs.wholetomato.com/default.asp?W171)

## 通过菜单和快捷方式调用 Snippets

下面的步骤演示了通过菜单访问调用 `VA Snippet` 来插入 `for-loop`,
然后是通过快捷方式调用同一 `VA Snippet` 的步骤.

1. 打开一个C/C++或C#文件
1. 将光标移至适合循环的位置

![insert](https://docs.wholetomato.com/default.asp?pg=pgDownload&pgType=pgWikiAttachment&ixAttachment=30148&sFileName=examplePositionCaret.png)

1. 从菜单栏中选择 `VAssistX | Insert VA Snippet...`

![snippet](https://docs.wholetomato.com/default.asp?pg=pgDownload&pgType=pgWikiAttachment&ixAttachment=45360&sFileName=exampleVAssistX.png)

1. 从 `VA Snippets with shortcuts` 子菜单中, 选择 `for loop forward`.

![for loop](https://docs.wholetomato.com/default.asp?pg=pgDownload&pgType=pgWikiAttachment&ixAttachment=45397&sFileName=exampleMenuForr.png)

1. 因为 `VA Snippet` 包含用户提示的占位符, 所以文本编辑器中会出现一个行内提示.
输入 `index` 和 `length` 的值, 使用 `tab` 在占位符之间移动.

![holder](https://docs.wholetomato.com/default.asp?pg=pgDownload&pgType=pgWikiAttachment&ixAttachment=45365&sFileName=snippetsIntroInline.png)

1. 按Enter键完成VA Snippet的扩展.

![enter](https://docs.wholetomato.com/default.asp?pg=pgDownload&pgType=pgWikiAttachment&ixAttachment=30152&sFileName=exampleExpanded.png)

### 通过快捷方式调用同一个VA Snippet

步骤如下.

1. 在Visual Assist的 `选项` 对话框中, 确认VA Snippets被包含在建议列表框中

![option](https://docs.wholetomato.com/default.asp?pg=pgDownload&pgType=pgWikiAttachment&ixAttachment=46257&sFileName=includeVASnippets.png)

2. 将光标移至适合循环的位置

![for](https://docs.wholetomato.com/default.asp?pg=pgDownload&pgType=pgWikiAttachment&ixAttachment=30148&sFileName=examplePositionCaret.png)

3. 输入 "forr", 如有必要, 将选择移动到 "for loop forward", 这是与VA Snippet相关的标题.
(如果你有一个内置代码片段的IDE, 你的列表框可能包括微软的对应代码: forr).

![forr](https://docs.wholetomato.com/default.asp?pg=pgDownload&pgType=pgWikiAttachment&ixAttachment=30154&sFileName=example2suggestionList.png)

选择 "for loop forward "可以看到行内提示和VA Snippet的完整扩展.

![snippet](https://docs.wholetomato.com/default.asp?pg=pgDownload&pgType=pgWikiAttachment&ixAttachment=30152&sFileName=exampleExpanded.png)

### 私有与共享的VA Snippets

Visual Assist支持私有的VA Snippets和共享的VA Snippets,
前者在单个开发者的所有IDE中共享,
后者存储在一个公共位置并在Visual Assist的多个用户中共享.
默认情况下, Visual Assist只安装和使用私有VA Snippets.
调整注册表以[在用户之间共享VA Snippets](https://docs.wholetomato.com/default.asp?W433).

## 在用户之间共享VA片段

Visual Assist支持私有的VA片段, 这些片段在单个开发人员的所有IDE中共享,
以及共享的VA片段, 这些片段存储在一个公共位置, 在Visual Assist的多个用户中共享.
`私人VA片段` 是在安装Visual Assist时创建的;它们被存储在本地的 `%APPDATA%` 中.
共享的VA片段可以从单个开发人员的私有VA片段的副本中创建.

### 优先级

带有快捷方式的 `共享片段` 优先于具有相同快捷方式的 `私有片段`.
如果存在一个带有快捷方式 "if "的共享VA片段,
它将出现在具有相同 "if "快捷方式的私有片段之前的列表框中.

如果存在用于 `refactoring` 和 suggestions-for-type 的私有VA片段, 它们将优先于共享版本.
目前, 让共享的重构和建议类型优先的唯一方法是配置共享, 然后删除私人版本的片段;
只有在这种情况下, Visual Assist不会自动重新创建私人版本.

如果一个给定的编程语言不存在共享的VA片段, Visual Assist只使用私有VA片段.
因此, 没有必要为所有编程语言创建共享的 VA Snippets.

### 注册表设置

要在用户之间共享一组VA Snippets,
请为Visual Assist的每个用户在注册表中创建以下字符串值(`REG_SZ`),
并将该条目的值设置为 `共享VA Snippets` 所在的 `目录`.
该目录可以是本地的, 也可以是在映射的网络驱动器上, 或在网络共享上.

```powershell
HKCU\Software\Whole Tomato\Visual Assist X\<IDE spec>\SharedSnippetsDir = <shared directory>
```

### 创建共享的VA片段

创建一套共享VA片段的简单方法是.

+ 备份您的私人VA Snippets, 用于您想共享的编程语言的片段.
你可以在 `.tpl` 文件中找到VA Snippets.

```powershell
%APPDATA%\VisualAssist\Autotext\
```

+ 使用Visual Assist中的 `VA Snippet` 编辑器来创建/编辑您的私人片段, 以便它们适用于多个用户.
请确保删除您不想共享的VA片段, 因为共享片段优先于私人版本.
+ 将修改后的 VA Snippets 复制到您的共享目录.
+ 恢复您的私有VA Snippets.

### 权限

将包含共享VA Snippets的文件的权限设置为 `只读`, 这样用户就无法编辑它们.
在只读的情况下, 用户可以复制共享的VA Snippets来制作私人版本,
尽管带有快捷方式的 `共享VA Snippets` 优先于共享版本.

如果共享的VA Snippets所在的目录是可写的,
请为未使用的编程语言创建空的, 只读的tpl文件, 以防止用户无意中创建共享版本.

## VA Snippet 保留字符串

即具有特殊含义的字符串，可以自动替换
