# vim 快捷键, leader key

[vim leader的使用](https://blog.csdn.net/qq_32949893/article/details/113339607)

## vim leader的使用

vim leader可以看做是vim快捷键的触发.
通过 `leader+其他按键` 达到快捷键的作用.
例如保存文件通常使用 `:wq`,如果我们设置leader的快捷方式我们可以用更少的按键达到这效果.

+ 例子

假设我们配置了如下设置:

```viml
nmap <leader>wq :wq<CR>
```

解释: `nmap` 可以理解为关键字映射.
映射了什么呢?
把 `<leader>wq` 映射为 `:wq<CR>`

`:wq<CR>` 是不是很熟悉?
`:wq` 就是我们常用的保存, `<CR>`是回车的字符表示.

也就是说我们配置了 `nmap <leader>wq :wq<CR>` 之后,
通过三个按键(`leader` 是一个按键)达到了多个键的功能.

+ 如何配置; 在 `~/.vimrc` 添加相关的映射.

```viml
syntax on
nmap <C-n> <Esc>:tabnext<CR>
nmap <F6> <Esc>:Tlist<CR>
nmap <leader>wq :wq<CR>
```

`vim` 默认的 `leader` 是 `\`, 如果想改其他的键可以在此文件中追加如下:

```viml
let mapleader=" "
```

此时空格键成为了leader, 之后想要保存文件退出,
只需要在normal模式下依次按下: `空格, w, q` 三个键就可以了.

## vim的寄存器和系统剪贴板

[vim的复制粘贴(包括系统剪贴板)](https://www.cnblogs.com/jpfss/p/9040561.html)

### 寄存器简介

vim强大之处之一就是它自带一堆寄存器, 每个寄存器独立使用,
你可以在不同寄存器中存储不同数据, 命令,
你可以把这个想象成一个加强版的剪贴板, 当然它的功能不止剪贴板这么简单.
如果你想看vim的官方文档有关部分:

```viml
: help  registers
```

根据官方手册: vim有9种寄存器

1. 无名(unnamed)寄存器: "", 缓存最后一次操作内容;
2. 数字(numbered)寄存器: "0~"9, 缓存最近操作内容, 复制与删除有别,
"0寄存器缓存最近一次复制的内容, "1-"9缓存最近9次删除内容
3. 行内删除(small delete)寄存器: "-, 缓存行内删除内容;
4. 具名(named)寄存器: "a ～ "z或"A - "0Z, 指定时可用;
5. 只读(read-only)寄存器: ":,".,"%,"#,
分别缓存最近命令, 最近插入文本, 当前文件名, 当前交替文件名;
6. 表达式(expression)寄存器: "=, 只读, 用于执行表达式命令;
7. 选择及拖拽(selection and drop)寄存器: `"*`,`"+`,`"~`,
存取GUI选择文本, 可用于与外部应用交互, 使用前提为系统剪切板(clipboard)可用;
8. 黑洞(black hole)寄存器: "_, 不缓存操作内容(干净删除);
9. 模式寄存器(last search pattern): "/, 缓存最近的搜索模式.

至于每个寄存器更加具体的使用方法, 我希望大家能查询手册,

## 基本操作

现在输入命令, 你就可以查询一下目前寄存器的情况

```viml
:reg
```

寄存器的使用方法也很简单: 通过`"` 加上 `寄存器名`, 就可以访问特定的寄存器:

```viml
"ap      粘贴字母a寄存器内容
"1y      把选取内容复制到数字寄存器1
```

### 系统剪贴板

选择及拖拽寄存器 就是系统的剪贴板,
我们通常使用的ctrl+c  ctrl+v 的内容就保存在这个寄存器中,
所以你要把需要复制的内容放在+寄存器中, 就可以在gui界面中用粘贴或ctrl+v粘贴了,
同理, 粘贴在vim中也一样

```viml
"+y    复制到系统剪贴板
"+p    粘贴
"+gp  粘贴并且移动光标到粘贴内容后
```

但是光是输入命令 `"+p` 就已经让人觉得很麻烦了,
这时候, vim的map功能就又可以大显神通了,
我们只需要把 `"+y` 和 `"+gp` map到你喜欢的快捷键上即可:

首先打开 `vimrc`(如果没有, 请创建一个)

```bash
vim ~/.vimrc
```

然后在其中输入:

```viml
noremap <leader>v "+gp
noremap <leader>c "+y
```

至于 `<leader>` 是什么按键, 默认是 `\`,
可以按照你的喜好来设置, 例如 `,`

```viml
let mapleader=","
```

### map,namp,noremap

[vim中为什么有那么多map?nnoremap, vnoremap](https://www.zhihu.com/question/20741941)

+ `noremap` 是不会递归的映射 (大概是no recursive),例如

```viml
noremap Y y
noremap y Y
```

不会出现问题.

+ 前缀代表生效范围
`inoremap` 就只在插入(`insert`)模式下生效
`vnoremap` 只在 `visual` 模式下生效
`nnoremap` 就在 `normal` 模式下(狂按esc后的模式)生效

这样可以减少快捷键所用到的键位组合的个数,
一个组合可以有多种用途,就不用费劲心思思考,该映射哪个没被绑定过的键了

## vim 快捷键符号

[Vim魔法堂: 认识快捷键绑定](https://developer.aliyun.com/article/32185)

绑定快捷键

`:map` 命令就是将一连串操作绑定到指定的快捷键, 语法格式:

```viml
:map <快捷键> <命令组>
```

`<快捷键>`, 各功能键和组合键均可作为快捷键,
并且均可通过VIM内置的键盘映射来指定(如上面提到的<ESC>则是Escape键的映射,
<CR>则是回车键的映射, <C-Esc>代表Ctrl-Esc和<S-F1>表示Shift-F1等).
具体 的键盘映射信息可通过在命令模式下执行 :h key-notation 查看.

`<命令组>`, 按下<快捷键>后依次执行的命令组.
其中 `%` 占位符用于表示当前操作的文件全称, 而 `%<` 占位符表示当前操作的文件名称(不带扩展名).
