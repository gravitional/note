# Emacs

[GNU Emacs](https://www.gnu.org/software/emacs/)
[GNU Emacs Lisp Package Archive](https://elpa.gnu.org/) : `ELPA`
[emacs 自带的简单入门教程](https://zhuanlan.zhihu.com/p/27299756)
[一年成为Emacs高手](https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/guide-zh.org)

`Linux`安装:

```bash
sudo apt-get install emacs
```

`emacs` 命令行选项;

+ `-nw, --no-window-system`; 告诉Emacs不要创建一个图形框架.  如果你在从`xterm(1)`窗口调用`Emacs`时使用这个开关, 那么将显示命令行界面.

随`portacle` 安装的`emacs`编辑器,  按下`Alt+x`, 输入`help-with-tutorial-spec-languag`, 输入过程可以使用`tab`补全, 
也可以使用左右箭头选择命令, 按下回车`RET`选定`Point`附近的命令, `Point`就是一闪一闪的那个光标.
如果选错了, 或者不想执行命令了, 可以多按几次`Ctrl+g`取消.

## emacs 快捷键

[Emacs快捷键](https://www.cnblogs.com/suzhou/p/3638975.html).

我们约定常用快捷键表示: `C`=`Ctrl`, `M`=`Meta`=`Alt`.  `Del` = `Backspace`. 
`C`的操作大部分跟单个字符有关, `M`的操作大部分跟单词/句子有关.

+ 命令集扩展, EXTENDING THE COMMAND SET

`Emacs` 的命令就像天上的星星, 数也数不清. 把它们都对应到 `CONTROL` 和 `META` 的单发组合键上显然是不可能的. 
`Emacs` 用扩展(eXtend)命令来解决这个问题, 用过`VSCode`的同学一定不陌生. 
扩展命令有两种风格: 

+ `C-x` ; 字符扩展.   `C-x` 之后输入单个字符或者组合键. 
+ `M-x` ; 命令名扩展. `M-x` 之后输入一长串命令名. 

很多扩展命令都相当有用, 虽然与你已经学过的命令比起来, 他们可能不那么常用. 
我们早已经见过一些扩展命令了, 比如用 `C-x C-f` 寻找文件和用 `C-x C-s`保存文件; 
退出 `Emacs` 用的 `C-x C-c` 也是扩展命令. (不用担心退出 Emacs 会给你带来什么损失, Emacs 会在退出之前提醒你存盘的). 

### 基本快捷键(Basic) 

+ `C-x C-f` ; `find`文件, 即在缓冲区打开或新建一个文件 
+ `C-x C-s` ; 保存文件. 如果不幸被冻结, 可以输入`C-q`
+ `C-x C-w` ; 使用其他文件名另存为文件 
+ `C-x C-v` ; 关闭当前缓冲区文件并打开新文件 
+ `C-x i` ; 在当前光标处插入文件 
+ `C-x b` ; 新建或切换缓冲区 
+ `C-x C-b` ; 显示缓冲区列表 
+ `C-x k` ; 关闭当前缓冲区 
+ `C-z` ; 挂起 `emacs `
+ `C-x C-c` ; 关闭 `emacs `
+ `C-l`; 移动到屏幕中间.

重要快捷键(Important) 

+ `C-g` ; 停止当前运行/输入的命令 
+ `C-x u` ; 撤销前一个命令 
+ `M-x revert-buffer 回车` ; (照着这个输入)撤销上次存盘后所有改动 
+ `M-x recover-file 回车` ; 从自动存盘文件恢复 
+ `M-x recover-session 回车` ; 如果你编辑了多个文件, 用这个恢复 

### 光标移动基本快捷键(Basic Movement) 

+ `C-f` ; 后一个字符 
+ `C-b` ; 前一个字符 
+ `C-p` ; 上一行 
+ `C-n` ; 下一行 
+ `M-f` ; 后一个单词 
+ `M-b` ; 前一个单词 
+ `C-a` ; 行首 
+ `C-e` ; 行尾 
+ `C-v` ; 向下翻一页 
+ `M-v` ; 向上翻一页 
+ `M-<` ; 到文件开头 
+ `M->` ; 到文件末尾 
    
+ `C-M-u,d,p,n` ; 移动到外层/内层, 前一组, 后一组括号
+ `C-M-b,f`; 移动到下一个左括号, 右括号

### 编辑(Editint) 

+ `M-n` ; 重复执行后一个命令`n`次 
+ `C-u` ; 重复执行后一个命令`4`次 
+ `C-u n` ; 重复执行后一个命令`n`次 
+ `C-d` ; 删除(delete)后一个字符 
+ `M-d` ; 删除后一个单词 
+ `Del` ; 删除前一个字符
+ `M-Del` ; 删除前一个单词 
+ `C-k` ; 移除(kill)一行 
      
+ `C-Space` ; 设置开始标记 (例如标记区域) 
+ `C-@` ; 功能同上, 用于`C-Space`被操作系统拦截的情况 
+ `C-w` ; 移除(`kill`)标记区域的内容 
+ `M-w` ; 复制标记区域的内容 
+ `C-y` ; 召回(`yank`)复制/移除的区域/行 
+ `M-y` ; 召回更早的内容 (在`kill`缓冲区内循环) 
+ `C-x C-x ` ;交换光标和标记 
      
+ `C-t` ; 交换两个字符的位置 
+ `M-t` ; 交换两个单词的位置 
+ `C-x C-t` ; 交换两行的位置 
+ `M-u` ; 使从光标位置到单词结尾处的字母变成大写 
+ `M-l` ; 与`M-u`相反 
+ `M-c` ; 使从光标位置开始的单词的首字母变为大写 

### undo,redo   

[Emacs的undo与redo](https://www.cnblogs.com/wendellyi/archive/2013/08/29/3290366.html)

`redo`=`C-x u`=`C-/`=`C-_`. 
`Emacs`的`redo`与通常编辑器中的有点区别. 两次`undo`就相当于`redo`. 通过`C-g`切换`undo`,`redo`的方向.

每个缓冲区都有一个`undo`记录, 每次更改缓冲区都会放入这个`undo`记录中, 我们可以通过连续的`c-_`进行`undo`, 
如果在连续的`undo`命令序列中间, 插入其他命令比如文档中提到的`C-f`或者是`C-g`, 
那么前面的连续的撤销操作会被打断, 这个序列会被视为单独的修改放入`undo`记录中, 从而可以逆转这个撤销序列.

可以安装一个`undo-tree`插件. 方便地查看`undo`序列的树形结构.

### 在线帮助(Online-Help) 

+ `C-h c` ; 显示快捷键绑定的命令 
+ `C-h k` ; 显示快捷键绑定的命令和它的作用 
+ `C-h l` ; 显示最后`100`个键入的内容 
+ `C-h w` ; 显示命令被绑定到哪些快捷键上 
+ `C-h f` ; 显示函数的功能 
+ `C-h v` ; 显示变量的含义和值 
+ `C-h b` ; 显示当前缓冲区所有可用的快捷键 
+ `C-h t` ; 打开`emacs`教程 
+ `C-h i` ; 打开`info`阅读器 
+ `C-h C-f` ;显示`emacs FAQ` 
+ `C-h p` ; 显示本机`Elisp`包的信息 .

### 搜索/替换(Seach/Replace) 

+ `C-s` ; 向后搜索 
+ `C-r` ; 向前搜索 
+ `C-g` ; 回到搜索开始前的位置(如果你仍然在搜索模式中) 
+ `M-%` ; 询问并替换(query replace) 
  + `Space` 或 `y` 替换当前匹配 
  + `Del` 或 `n` 不要替换当前匹配 
  + `.` ; 仅仅替换当前匹配并退出(替换) 
  + `,` ; 替换并暂停(按`Space`或`y`继续) 
  + `!`;  替换以下所有匹配 
  + `^` ; 回到上一个匹配位置 
  + `RETURN` 或 `q` 退出替换 

使用正则表达式(Regular expression)搜索/替换 . 可在正则表达式中使用的符号: 

+ `^` 行首 
+ `$` 行尾 
+ `.` 单个字符 
+ `.*` 任意多个(包括没有)字符 
+ `\<` 单词开头 
+ `\>` 单词结尾 
+ `[]` 括号中的任意一个字符(例如`[a-z]`表示所有的小写字母) 
      
+ `M C-s RETURN` ; 使用正则表达式向后搜索 
+ `M C-r RETURN` ; 使用正则表达式向前搜索 
+ `C-s` ; 增量搜索 
+ `C-s` ; 重复增量搜索 
+ `C-r` ; 向前增量搜索 
+ `C-r` ; 重复向前增量搜索 
+ `M-x query-replace-regexp` ; 使用正则表达式搜索并替换 
      
### 窗口命令(Window Commands) 

+ `C-x 2` ; 水平分割窗格 
+ `C-x 3` ; 垂直分割窗格 
+ `C-x o` ; 切换至其他窗格 
+ `C-x 0` ; 关闭窗格 
+ `C-x 1` ; 关闭除了光标所在窗格外所有窗格 
+ `C-x ^` ; 扩大窗格 
+ `M-x shrink-window` ; 缩小窗格 
+ `M C-v` ; 滚动其他窗格内容 
+ `C-x 4 f` ; 在其他窗格中打开文件 
+ `C-x 4 0` ; 关闭当前缓冲区和窗格 
+ `C-x 5 2` ; 新建窗口(frame) 
+ `C-x 5 f` ; 在新窗口中打开文件 
+ `C-x 5 o` ; 切换至其他窗口 
+ `C-x 5 0` ; 关闭当前窗口 
      
### 书签命令(Bookmark commands) 

+ `C-x r m` ; 在光标当前位置创建书签 
+ `C-x r b` ; 转到书签 
+ `M-x bookmark-rename` ; 重命名书签 
+ `M-x bookmark-delete` ; 删除书签 
+ `M-x bookmark-save` ; 保存书签 
+ `C-x r l` ; 列出书签清单 
  + `d` ; 标记等待删除 
  + `Del` ; 取消删除标记 
  + `x` ; 删除被标记的书签 
  + `r` ; 重命名 
  + `s` ; 保存列表内所有书签 
  + `f` ; 转到当前书签指向的位置 
  + `m` ; 标记在多窗口中打开 
  + `v` ; 显示被标记的书签(或者光标当前位置的书签) 
  + `t` ; 切换是否显示路径列表 
  + `w` ; 显示当前文件路径 
  + `q` ; 退出书签列表 
      
+ `M-x bookmark-write` ; 将所有书签导出至指定文件 
+ `M-x bookmark-load` ; 从指定文件导入书签 
      
Shell 

+ `M-x shell` ; 打开shell模式 
+ `C-c C-c` ; 类似unix里的`C-c` ;(停止正在运行的程序) 
+ `C-d` ; 删除光标后一个字符 
+ `C-c C-d` ; 发送`EOF `
+ `C-c C-z` ; 挂起程序(unix下的`C-z`) 
+ `M-p` ; 显示前一条命令 
+ `M-n` ; 显示后一条命令 
      
### DIRectory EDitor (dired) 

+ `C-x d` ; 打开 `dired `
+ `C` ;(大写C) 复制 
+ `d` ; 标记等待删除 
+ `D` ; 立即删除 
+ `e`或`f` ; 打开文件或目录 
+ `g` ; 刷新当前目录 
+ `G` ; 改变文件所属组(chgrp) 
+ `k` ; 从屏幕上的列表里删除一行(不是真的删除) 
+ `m` ; 用`*`标记 
+ `n` ; 光标移动到下一行 
+ `o` ; 在另一个窗格打开文件并移动光标 
+ `C-o` ; 在另一个窗格打开文件但不移动光标 
+ `P` ; 打印文件 
+ `q` ; 退出`dired `
+ `Q` ; 在标记的文件中替换 
+ `R` ; 重命名文件 
+ `u` ; 移除标记 
+ `v` ; 显示文件内容 
+ `x` ; 删除有`D`标记的文件 
+ `Z` ; 压缩/解压缩文件 
+ `M-Del`; 移除标记(默认为所有类型的标记) 
+ `~` ; 标记备份文件(文件名有`~`的文件)等待删除 
+ `#` ; 标记自动保存文件(文件名形如`#name#`)等待删除 
+ `*/` ; 用`*`标记所有文件夹(用`C-u */n`移除标记) 
+ `=` ; 将当前文件和标记文件(使用`C-@`标记而不是`dired`的`m`标记)比较 
+ `M-=` 将当前文件和它的备份比较 
+ `!` ; 对当前文件应用`shell`命令 
+ `M-}` ; 移动光标至下一个用`*`或`D`标记的文件 
+ `M-{` ; 移动光标至上一个用`*`或`D`标记的文件 
+ `% d` 使用正则表达式标记文件等待删除 
+ `% m` 使用正则表达式标记文件为`*` 
+ `+` ; 新建文件夹 
+ `>` ; 移动光标至后一个文件夹 
+ `<` ; 移动光标至前一个文件夹 
+ `s` ; 切换排序模式(按文件名/日期) 
      
或许把这个命令归入这一类也很合适: 

+ `M-x speedbar` ; 打开一个独立的目录显示窗口 
      
Telnet 

+ `M-x telnet` ; 打开`telnet`模式 
+ `C-d` ; 删除后一个字符或发送`EOF `
+ `C-c C-c` ; 停止正在运行的程序(和unix下的`C-c`类似) 
+ `C-c C-d` ; 发送`EOF `
+ `C-c C-o` ; 清除最后一个命令的输出 
+ `C-c C-z` ; 挂起正在运行的命令 
+ `C-c C-u` ; 移除前一行 
+ `M-p` ; 显示前一条命令 
      
Text : 只能在text模式里使用 

+ `M-s` ; 使当前行居中 
+ `M-S ` ; 使当前段落居中 
+ `M-x center-region` ; 使被选中的区域居中 
      
### 宏命令(Macro-commands) 

+ `C-x (` ; 开始定义宏 
+ `C-x )` ; 结束定义宏 
+ `C-x e` ; 运行最近定义的宏 
+ `M-n C-x e` ; 运行最近定义的宏`n`次 
+ `M-x name-last-kbd-macro` ; 给最近定义的宏命名(用来保存) 
+ `M-x insert-kbd-macro` ; 将已命名的宏保存到文件 
+ `M-x load-file` ; 载入宏 
      
编程(Programming) 

+ `M C-\` ; 自动缩进光标和标记间的区域 
+ `M-m` ; 移动光标到行首第一个(非空格)字符 
+ `M-^` ; 将当前行接到上一行末尾处 
+ `M-;` ; 添加缩进并格式化的注释 

### C, C++和Java模式 

+ `M-a` ; 移动光标到声明的开始处 
+ `M-e` ; 移动光标到声明的结尾处 
+ `M C-a` ; 移动光标到函数的开始处 
+ `M C-e` ; 移动光标到函数的结尾处 
+ `C-c RETURN` ; 将光标移动到函数的开始处并标记到结尾处 
+ `C-c C-q` ; 根据缩进风格缩进整个函数 
+ `C-c C-a` ; 切换自动换行功能 
+ `C-c C-d` ; 一次性删除光标后的一串空格(greedy delete) 
      
为了实现下面的一些技术, 你需要在保存源代码的目录里运行`etags  *.c *.h *.cpp`(或者源代码的其他的扩展名) 

+ `M-.(点)` ; 搜索标签 
+ `M-x tags-search ENTER` ; 在所有标签里搜索(使用正则表达式) 
+ `M-,(逗号)` ; 在`tags-search`里跳至下一个匹配处 
+ `M-x tags-query-replace` ; 在设置过标签的所有文件里替换文本 
      
### GDB(调试器) 
    
+ `M-x gdb` ; 在另一个的窗格中打开`gdb `

版本控制(Version Control) 

+ `C-x v d` ; 显示当前目录下所有注册过的文件(show all registered files in this dir) 
+ `C-x v =` ; 比较不同版本间的差异(show diff between versions) 
+ `C-x v u` ; 移除上次提交之后的更改(remove all changes since last checkin) 
+ `C-x v ~` ; 在不同窗格中显示某个版本(show certain version in different window) 
+ `C-x v l` ; 打印日志(print log) 
+ `C-x v i` ; 标记文件等待添加版本控制(mark file for version control add) 
+ `C-x v h` ; 给文件添加版本控制文件头(insert version control header into file) 
+ `C-x v r` ; 获取命名过的快照(check out named snapshot) 
+ `C-x v s` ; 创建命名的快照(create named snapshot) 
+ `C-x v a` ; 创建gnu风格的更改日志(create changelog file in gnu-style)

## 问题

### 括号平衡无法删除

[Disable parenthesis matching in emacs](https://stackoverflow.com/questions/28055176/disable-parenthesis-matching-in-emacs)

`Emacs`默认不会这样做, 可能是`ParEdit`. 你也许能从安装说明中得出如何摆脱它. 
