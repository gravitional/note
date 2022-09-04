# vim-x.md

## 概念

`buffer`是内存中的文本文件.
`window`是`buffer`的`viewport`.
`tab`是`windows`的集合.

***
Vim 寄存器

[Vim 寄存器](https://www.cnblogs.com/RichardLee/articles/2479376.html)

## 设置字体

[gvim 字体(font)+风格(style)+ 大小(size)设置][]

[gvim 字体(font)+风格(style)+ 大小(size)设置]: https://blog.csdn.net/sinat_34647203/article/details/90107444

编辑配置文件, 设置格式如下所示:

+ windows:  `guifont=字体:风格:大小`
+ linux:  `guifont=字体\ 风格\ 大小`

三个部分每个部分中有空格的话,  linux中在空格前面加个反斜线\, 每个部分之间的分割线也是`\+空格`,  windows中用`下划线`代替空格. 以上图为例, 它们的设置如下:

+ windows:   `guifont=Courier_New:b:h16`
+ centos(linux): `guifont=Courier\ 10\ Pitch\ Bold\ 12`

我是严格按照图中的顺序设置的, 即字体, 风格, 大小, 若不这样的话, 在linux中似乎有些问题.

友情提示:
常规风格可省略, windows中的各种风格分别为: 斜体(`i`), 粗体(`b`), 粗斜体(两个都要写上, 即 字体`:b:i:大小`), 大小加上`h`;
linux风格和字体直接写完整单词, 大小是数字, 写在最后即可.

修正1: 由于这: 是设置`gui`界面的, 因此这两句放在`if has("gui_running") `后面比较好, 免得在文字界面运行时有一条出错信息.

## 编辑常用

[学习vim命令 :w !sudo tee %](https://www.cnblogs.com/jkmiao/p/5553837.html)

`<c-f>` or `q:` 查看历史命令, 注意`:q`是退出.
标记书签`m[mark]`, 查看书签 `'[mark]`
`Sexplorer` or `Sex` 打开文件所在的目录.
`e .`打开当前工作目录
`sp .` 在新窗口打开 CWD 目录
`bro ol` `browse old`:浏览之前打开过的文件目录
`ctrl-W t`: 把当前 window 转为 tab
`terminal`: 进入命令行

+ `:w !sudo tee %`: 用 `sudo` 权限保存文件.

`:[range]w[rite] !{cmd}`: 将 `[range]` 内的行作为标准输入, 执行`{cmd}`命令.
`%`是`vim`当中一个只读寄存器的名字, 总保存着当前编辑的`文件路径`.

查阅vim的文档(输入`:help :w`), 会提到命令`:w!{cmd}`:
 让`vim`执行一个外部命令`{cmd}`, 然后把当前缓冲区的内容从`stdin`传入.
`:w !sudo tee %` 的含义: 
是把 `当前编辑的内容` 当做标准输入, 传入到命令 `sudo tee 文件路径`.

### 补全

`Vim` 能自动补全插入的单词. 你键入一个单词的开头部分, 按 `CTRL-P`, `Vim` 就会为你猜测余下的部分. 往下搜索, 用 `CTRL-N`

### 补全特定文本

如果你知道你要找什么, 那么你可以用这些命令来补全某种类型的文本:

- `CTRL-X CTRL-F`           文件名
- `CTRL-X CTRL-L`           整行
- `CTRL-X CTRL-D`           宏定义 (包括包含文件里的)
- `CTRL-X CTRL-I`           当前文件以及所包含的文件
- `CTRL-X CTRL-K`           字典文件内的单词
- `CTRL-X CTRL-T`           同义词词典文件内的单词
- `CTRL-X CTRL-]`           标签
- `CTRL-X CTRL-V`           `Vim` 命令行

`:r filename` : 插入文件内容
`:r! shell_command`: 插入命令的标准输出

### 修改

可使用`daw`, `diw`, `di{`, `da[` 等等, 删除特定结构的文本. 如`di[`是删除`[]`内的文本

## vim跳转到上/下一个修改的位置; 回到之前的位置; 上次编辑位置

当你编辑一个很大的文件时, 经常在某处进行修改, 然后跳到另外一处.
如果你想跳回之前修改的地方, 使用命令: `Ctrl+o`来回到之前修改的地方
类似的: `Ctrl+i` 会回到跳跃之前的位置

### 文档内查找

[vim 查找替换](https://www.cnblogs.com/ltang/articles/2034291.html)

- `*` 向后查找光标当前所在单词
- `#` 向前查找光标当前所在单词
- `/<search>` 向后查找指定字符串
- `?<search>` 向前查找指定字符串
- `n` 继续查找下一个 `next`
- `N` 继续查找上一个

- `r<X>` 将当前字符替换为 `X`, `replace`
- `gu<X>` 将指定的文本转换为小写, `这个实在不知道是啥的缩写,go to u ? ? `
- `gU<X>` 将指定的文本转换为大写
- `:%s/<search>/<replace>/` 查找 `search` 内容并替换为 `replace` 内容,

### 范围指定

`vi/vim` 中可以使用 `:s` 命令来替换字符串

`:s/vivian/sky/` 替换当前行第一个 `vivian` 为 `sky`
`:s/vivian/sky/g` 替换当前行所有 `vivian` 为 `sky`
`:n,$s/vivian/sky/` 替换第 `n` 行开始到最后一行中每一行的第一个 `vivian` 为 `sky`
`:n,$s/vivian/sky/g` 替换第 n 行开始到最后一行中每一行所有 `vivian` 为 `sky`
`n` 为数字, 若 `n` 为 `.`, 表示从当前行开始到最后一行
`:%s/vivian/sky/`(等同于` :g/vivian/s//sky/`) 替换每一行的第一个 `vivian` 为 `sky`
`:%s/vivian/sky/g`(等同于 `:g/vivian/s//sky/g`) 替换每一行中所有 `vivian` 为 `sky`

***
可以使用 `#` 作为分隔符, 此时中间出现的 `/` 不会作为分隔符
`:s#vivian/#sky/#` 替换当前行第一个 `vivian/` 为 `sky/`
`:%s+/oradata/apras/+/user01/apras1+ `(使用`+`来替换`/` ): ` /oradata/apras/`替换成`/user01/apras1/`

***
`:s/vivian/sky/ `替换当前行第一个 `vivian` 为 `sky`
`:s/vivian/sky/g` 替换当前行所有 `vivian` 为 `sky`

***

`:n,$s/vivian/sky/` 替换第 `n` 行开始到最后一行中每一行的第一个 `vivian` 为 `sky`
`:n,$s/vivian/sky/g` 替换第 `n` 行开始到最后一行中每一行所有 `vivian` 为 `sky`

(`n` 为数字, 若 `n` 为 `.`, 表示从当前行开始到最后一行)

***
`:%s/vivian/sky/`(等同于 `:g/vivian/s//sky/`) 替换每一行的第一个 `vivian` 为 `sky`
`:%s/vivian/sky/g`(等同于 `:g/vivian/s//sky/g`) 替换每一行中所有 `vivian` 为 `sky`

***

可以使用 `#` 作为分隔符, 此时中间出现的 `/` 不会作为分隔符

`:s#vivian/#sky/#` 替换当前行第一个 `vivian/` 为 `sky/`

### 替换的细节

[Search and replace](https://vim.fandom.com/wiki/Search_and_replace)

当搜索时, 也就是左侧:

`.`, `*`, `\`, `[`, `^`, and `$`  是元字符.
`+`, `?`, `|`, `&`, `{`, `(`, and `)`必须转义才能使用其特殊功能.
`\/` is `/`(使用反斜杠+正斜杠搜索正斜杠)
`\t`是制表符, `\s`是空格(空格或制表符)
`\n`是换行符, `\r`是CR(回车= `Ctrl-M` = `^M`)
在`[...]`之前的内容被指定为一个集合.  字符范围可以用`-`表示;  例如, 字母`a`, `b`, `c` or 数字`1`可以与`[1a-c]`匹配.
排除集合用`[^..]`表示,  例如`[^ 1a-c]`匹配除`a`, `b`, `c`或`1`以外的任何字符.
`\{#\}`用于重复.  `/foo.\{2\}`将匹配foo和后面的两个字符.  结束符`\}`也可以写成`}`, 因此`/foo.\{2}`效果相同.
`\(foo\)`对foo进行反向引用. 普通的没有转义的括号进行字面匹配. 在这里, `\`是结束`\)`所必需的.

当替换时, 也就是右侧:

+ `\r`是换行符, `\n`是空字节(`0x00`).
+ `\&`是`&`符(`&`是与搜索模式匹配的文本).
+ `\0`插入与整个模式匹配的文本
+ `\1`插入第一个反向引用的文本.  `\2`插入第二个反向引用, 依此类推.

在 `vscode` 中使用`$1`, `$2`代替.

## 恢复到最后一次保存

```vim
:e!
```

## plugin manger

[junegunn/vim-plug/wiki/tutorial][]

What is a Vim plugin and why would I need a plugin manger?

A Vim plugin is a set of Vimscript files that are laid out in a certain directory structure.
Before, Users would manually download the file and extract it in a single directory called `~/.vim`, and Vim would load the files under the directory during startup.

### Setting up

vim-plug is distributed as a single Vimscript file. All you have to do is to download the file in a directory so that Vim can load it.

### Installing plugins

With `vim-plug`, you declare the list of plugins you want to use in your Vim configuration file. It's `~/.vimrc` for ordinary Vim, and `~/.config/nvim/init.vim` for Neovim. The list should start with `call plug#begin(PLUGIN_DIRECTORY)` and end with `call plug#end()`.

After adding the above to the top of your Vim configuration file,
reload it (`:source ~/.vimrc`) or restart Vim.
Now run `:PlugInstall` to install the plugins.

### Updating plugins

Run `:PlugUpdate` to update the plugins. After the update is finished, you can review the changes by pressing `D` in the window. Or you can do it later by running `:PlugDiff`.

### Reviewing the changes

Updated plugins may have new bugs and no longer work correctly. With `:PlugDiff` command you can review the changes from the last `:PlugUpdate` and roll each plugin back to the previous state before the update by pressing `X` on each paragraph.

#### Removing plugins

Delete or comment out Plug commands for the plugins you want to remove.
Reload vimrc (`:source ~/.vimrc`) or restart Vim
Run `:PlugClean`. It will detect and remove undeclared plugins.

[junegunn/vim-plug/wiki/tutorial]: https://github.com/junegunn/vim-plug/wiki/tutorial

## 分屏

### 分屏方式

- `:split` 缩写 `:sp` or `Ctrl-w s` 上下分屏
- `:vsplit` 缩写 `:vs` or `Ctrl-w v` 左右分屏
- `:diffsplit` 缩写 `:diffs` diff 模式打开一个分屏, 后面可以加上 {filename}

#### 窗口跳转

- `Ctrl-w w` 激活下一个窗口
- `Ctrl-w j` 激活下方窗口
- `Ctrl-w k` 激活上方窗口
- `Ctrl-w h` 激活左侧窗口
- `Ctrl-w l` 激活右侧窗口
- `Ctrl-w c` 关闭当前窗口

#### 移动分屏

- `Ctrl-w L` 移动到最右侧
- `Ctrl-w H` 移动到最左侧
- `Ctrl-w K` 移动到顶部
- `Ctrl-w J` 移动到底部

注意: 区分大小写. 另外, 可以将底部的屏幕移动到右侧, 实现上下分屏到左右分屏的转换.

## 标签页

### 创建标签页

- `:tabnew` or `:tabedit` 缩写 `:tabe` 打开新标签页
- `Ctrl-w` or `gf` 在新标签页中打开当前光标所在位置的文件名

注意:

`:tabnew` 和 `:tabedit` 后面都可以跟一个 `<空格><文件名>` 用以在新标签页中
打开指定文件, 还可以在 `:` 后面加一个数字, 指出新标签页在列表中的位置(从 `0` 开始).

### 切换标签页

- `gt` or `:tabnext`; 缩写 `:tabn` 下一个标签页(最后一个会循环到第一个),
- `gT` or `:tabprevious` 缩写 `:tabp` 上一个标签页(第一个会循环到最后一个)
- `:tabrewind` 缩写 `:tabr` or `:tabfirst` 缩写 `:tabfir` 到第一个
- `:tablast` 缩写 `:tabl` 到最后一个标签页

可以加上页码数字.

+ `:tabp[revious] {count}`, `:tabN[ext]{count}{count}`
+ `{count}<Ctrl-上翻页>`, `{count}gT` 往前翻`{count}`标签页.  第一页接着最后一页.
注意, `{count}` 的使用与 `:tabnext` 不同, 在后者中它被用作标签页的页码.

#### 关闭标签页

- `:tabclose` 缩写 `:tabc` 关闭当前标签页
- `:-tabc` 关闭上一个标签页
- `:+tabc` 关闭下一个标签页
- `:tabonly` 缩写 `:tabo` 关闭其他标签页

## Vim 中的宏

按 `qa` 开启宏录制, 前方高能, 连续按 `I<单引号><Esc>A<单引号><逗号><Esc>jq7@a`

- `q` 是开启录制宏, `a` 是给这次宏的录制过程一个存储位置, 可以是 `0-9` 或 `a-z`;
- 然后 `I<单引号><Esc>A<单引号><逗号><Esc>j` 是你这次录制的整个宏的操作过程, 意思就是行首
插入单引号, 行尾插入单引号和逗号, 跳到下一行;
- 接下来的 `q` 是结束本次宏的录制;
- `@` 是唤起宏, `a` 是要唤起的宏的名字(存储位置), 前面的 `7` 你应该明白吧, 就是执行 `7` 次.

**Tips:** `@@` 再次唤起最后一次执行的宏.

## 使用剪贴板

假如你已经从其它程序中拷贝了一些文字到剪贴板, 你还是可以用普通的 `y` (`yank`) 和 `p` (`put`) 命令, 但在前面必须加上 `"*` (一个双引号加一个星号). 例如, 要拷贝一行到剪贴板中:

```vim
"*yy
```

要粘贴回来:

```vim
"*p
```

这仅在支持剪贴板的 `Vim` 版本中才能工作.

更多的寄存器名称:

| 类型 | 标识  | 读写者 | 是否为只读 | 包含的字符来源 |
| ------- | ------- | ------- | ------ | ------ |
|   `Numbered`    |  `0`至`9`    |  `vim`     | 否 |  ``寄存器 0: 最近一次复制. 寄存器 1: 最近一次删除. 寄存器 2: 倒数第二次删除, 以此类推. 对于寄存器 1 至 9, 他们其实是只读的最多包含 9 个元素的队列. 这里的队列即为数据类型 queue``
|   `Named`   |  `a`至`z`, `A`至`Z`    |  用户   |  否    |   `如果你通过复制操作存储文本至寄存器 a, 那么 a 中的文本就会被完全覆盖. 如果你存储至 A, 那么会将文本添加给寄存器 a, 不会覆盖之前已有的文本`   |

### ubuntu vim复制内容至系统剪切板

[ubuntu vim复制内容至系统剪切板][]

[ubuntu vim复制内容至系统剪切板]: https://blog.csdn.net/u012604810/article/details/79431698

在VIM中编辑的程序有时需要复制到网页, gedit, 或者这window系统中(如果ubuntu是虚拟机),
那么用原先的复制y(yank)和p(paste)就不行了.

#### vim寄存器

为何用`y``,`p`可以在vim之间复制内容, 却不能将内容复制到其他软件中呢?
因为`y`是将内容复制到vim的一个寄存器, 而这个寄存器并不是系统的剪切板.
`vim`中也有系统剪切板的寄存器, 需要用其他命令进行操作.

```vim
:help registers
```

可以在vim使用上述命令, 查看vim支持的所有寄存器.

```vim
There are ten types of registers:
1. The unnamed register ""
...
8. The selection and drop registers "*, "+ and "~
```

第8个寄存器就对应系统的剪切板.
只有当`vim`的`xterm_clipboard`存在时, 方可以使用系统的剪切板.
那么如何看vim的xterm_clipboard是否存在呢?

```vim
$vim --version | grep clipboard
```

查看`xterm_clipboard`前是`+`还是`-`,`+`表示可用, `-`表示不可用.

如果`xterm_clipboard`不可用, 需要按照vim的插件

```bash
$sudo apt-get install vim vim-scripts vim-gtk vim-gnome
```

安装之后, 可以再次查看, 发现`xterm_clipboard`从`-`变成了`+`.

#### 操作方法

可以`shift+v`, `ctrl+v`进行`visual`模式进行选择.

复制: `"+y`
粘贴: `"+gp`

但是显然复制和粘贴的命令都比较复杂, 可以将其绑定成其他更方便的快捷键. 进行`.vimrc`文件添加下列命令

```vim
let mapleader = ","
"set shortcut for copy to clipboard of system
nmap <leader>c "+y
nmap <leader>v "+gp
```

就将`"+y` 绑定为 `,c`, 将`"+p` 绑定为 `,v`

## 重新载入文件

[VIM使用小技巧](https://blog.csdn.net/race604/article/details/7314082)

有时候要使用VIM打开了一些文件, 但是在其他地方把次文件改动了, 例如使用git进行checkout等操作, 需要重新载入此文件.

+ 重新载入当前文件:
    + `:e`
    + `:e!` #放弃当前修改, 强制重新载入

+ 重新载入所有打开的文件:
    `:bufdo e` 或者 `:bufdo :e!`
    `:bufdo`命令表示把后面的命令应用到所有`buffer`中的文件.

## 输入 unicode 字符

[enter non-ascii characters](https://unix.stackexchange.com/questions/61118/how-to-enter-non-ascii-characters-using-hex-or-octal-codes-in-vi)

```vim
:help i_CTRL-V_digit
```

在插入模式下, 键入 `Ctrl+V`, 然后是

+ 十进制数字(0-255)
+ `o`, 然后是一个八进制数字(o0-o377, 即255是最大值)
+ `x`, 然后是一个十六进制数字(x00-xFF, 即255是最大值).
+ `u`, 然后是 `4-hexchar` Unicode序列
+ `U`, 然后是 `8-hexchar` 的Unicode序列

十进制和八进制的数字限制在三位数.
小于100的十进制数字可能包括前导零, 这将被忽略.
小于100oct(即64)的八进制数字可以包括前导零, 但不需要.
大于或等于100oct的八进制数可能不包括前导零(但如果你想, 你可以键入一个前导o).

你可以通过输入一个不是该小数的有效数字的字符来终止一个数字.  比如说

```vim
Ctrl+V 0 6 5 -> A
Ctrl+V 6 5 B -> Ab
Ctrl+V o 0 4 1 -> !
Ctrl+V o 4 1 9 -> !9
```

普通的(一个八位数)十六进制数字被限制在两位数.
和上面一样, 对于十六进制代码指定的字符, 你可以重复 radix 字符(例如, Ctrl+V u u 0 0 4 1 → A),
`o` 和 `x` 不区分大小写.
