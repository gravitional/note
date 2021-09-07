# lisp

[http://buildyourownlisp.com/](https://www.abnerchou.me/BuildYourOwnLispCn/)

## Getting Started

[Getting Started](https://lisp-lang.org/learn/getting-started/)
[为什么Lisp语言如此先进](http://www.ruanyifeng.com/blog/2010/10/why_lisp_is_superior.html)

### 便携环境

[portacle](https://portacle.github.io/)

`Portacle` 是一个完整的通用语言集成开发环境, 你可以用`U`盘携带. 
它是多平台的, 可以在 `Windows`, `OS X`和`Linux`上运行. 由于它不需要任何复杂的安装过程, 它可以在短时间内完成设置并运行. 

它很适合`Lisp`的需要好起点的初学者, 也适合想尽量减少准备一切的时间的高级用户. 
以下是`Portacle`中包含的软件包. 

+ `Emacs`; Emacs是一个非常灵活和可扩展的编辑器, 已经存在了很长时间. 由于它的历史和巨大的社区, 它能够覆盖几乎所有的编程领域, 甚至更多. 
与Emacs捆绑在一起的是一些非常有用的软件包, 其中主要的是`Slime`, `Magit`和`Company`. 
+ `SBCL` ; SBCL是当今最快和最有能力的Lisp实现之一.  [Steel Bank Common Lisp](http://www.sbcl.org/)
+ `Quicklisp` ; `Quicklisp`是标准的`Lisp`软件包管理器, 它允许你通过寥寥数键来检索和管理一千多个库. 
+ `Git`; `Git` 可能是当今使用最广泛的版本控制系统. 它允许你快速有效地管理各种大小项目中的代码变化.

根据平台下载安装之后, 打开运行.

窗口应该被划分为两个区域, 每个区域都显示一个叫做`缓冲区`(buffer)的东西. 
显示`CL-USER>`的那个, 是`Lisp`命令行. 另一个是`scratch `(草稿)缓冲区, 应该包含一些关于如何查看参考指南的提示. 
作为测试, 在`Lisp`命令行缓冲区内点击, 并输入以下代码. 

```lisp
 (progn (format T "Hey, what's your name?~%")
                (format T "Hello, ~a" (read-line)))
```

你会注意到它会自动为你插入结尾的圆括号和引号. 要运行它, 通过点击或方向键将光标移到末尾, 然后按下回车键. 
祝贺你 你刚刚在`Portacle`中运行了你的第一个程序. 

你可能还应该配置一下`Portacle`的默认设置.  按下`Alt+X`(Mac上为`⌘+X`), 然后输入`portacle-configure`, 按下回车. 
然后会在窗口的底部看到提示, 在一个叫做 `minibuffer` 的区域. 
回答所有的问题, `Portacle`应该就设置好了.
请确保阅读帮助文件. 它应该解释所有关于如何使用编辑器的基本术语和命令. 要访问它, 请按`Ctrl+H H`. 

### 安装SBCL 

在`Linux`和`OS X`上, 我们将使用`SBCL`作为通用语言的实现. 

要在`Ubuntu/Debian`上安装`SBCL`, 只需运行. 

```bash
sudo apt-get install sbcl
```

+ `Arch Linux`

由于`SBCL`可以从官方软件库中获得, 你可以用以下方式安装它. 

```bash
sudo pacman -S sbcl
```

+ `OS X`

要在`OS X`上安装`SBCL`, 只需进行以下操作. 

```bash
brew install sbcl
```

#### 安装Quicklisp

接下来, 我们设置`Quicklisp`, 即软件包管理器, 同样很简单. 

```bash
curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
       --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
       --eval '(ql:add-to-init-file)' \
       --quit
```

这将把 `Quicklisp` 安装到 `~/.quicklisp/` 目录中. 

### 安装 Emacs and SLIME

[SLIME: The Superior Lisp Interaction Mode for Emacs](https://common-lisp.net/project/slime/)

`SLIME`是一个用于`Common Lisp`开发的`Emacs mode `, 即建立在 `Emacs` 上的`Common Lisp`IDE(集成开发环境).
受现有系统的启发, 如`Emacs Lisp`和`ILISP`, 我们正在努力创造一个环境, 以便在其中集成`Common Lisp`. 

+ `slime-mode` ; 一个`Emacs`的`minor`模式, 用来增强`lisp-mode`的功能. 
  + 代码运行, 编译和宏展开. 
  + 在线文档(`describe`, `apropos`, `hyperspec`). 
  + 定义查找(又名`Meta-Point`, 或`M-`). 
  + 符号和包名补全. 
  + 自动宏缩进, 基于`&body`. 
  + 交叉引用界面(`WHO-CALLS`, 等等). 
+ `SLDB`: Common Lisp debugger , 具有基于`Emacs`的用户接口. 
+ `REPL`: ` Read-Eval-Print Loop`(top-leve)是用`Emacs Lisp`写的, 以便与`Emacs`更紧密地结合. 
`REPL`也有类似`McCLIM `监听器的内置 `快捷键`命令. 
+ `Compilation notes`. `SLIME`能够接受编译器信息, 并将其直接注释到源代码缓冲区. 
+ `Inspector`. `Emacs` 缓冲区中的交互式对象检查器. 

你可以用`Quicklisp`来安装它. 

```bash
sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit
# 删除请使用：
sbcl --eval '(ql:uninstall :quicklisp-slime-helper)' --quit
```

然后, 在你的 `~/.emacs.d/init.el` 中添加这个内容. 

```lisp
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
```

[Emacs编辑器](https://www.jianshu.com/p/732157b02ecc) 安装:

```bash
sudo apt-get install emacs
```

+ `-nw, --no-window-system`; 告诉Emacs不要创建一个图形框架.  如果你在从`xterm(1)`窗口调用`Emacs`时使用这个开关, 那么将显示命令行界面.

[emacs 自带的简单入门教程](http://jixiuf.github.io/blog/emacs-%E8%87%AA%E5%B8%A6%E7%9A%84%E7%AE%80%E5%8D%95%E5%85%A5%E9%97%A8%E6%95%99%E7%A8%8B(tutorial)/#sec-1)
[emacs 自带的简单入门教程](https://zhuanlan.zhihu.com/p/27299756)
[一年成为Emacs高手](https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/guide-zh.org)

常用快捷键: `C`表示`Ctrl`, `M`(Meta)表示`Alt`
`C`的操作大部分跟单个字符有关, `M`按键大部分跟单词的操作有关.

+ `C-g` ; 放弃命令
+ `C-x C-c` ; 退出
+ `C-v` ; 向下移动屏幕
+ `M-v` ; 向上移动屏幕
+ `C-l` ; 定位到屏幕中间
+ `C-p,n,f,b` ;  上下左右导航按键.
+ `C-a` ; 移动到行首
+ `C-e` ; 移动到行尾
+ `M-a` ; 移回句首
+ `M-e` ; 移到句尾
+ `M-<,M->` ; 移动到文件头和末尾
+ `C-u 8 C-f` ; 重复`8`次某个命令
+ `C-space` ; 可视化选取, `C-w`删除选中的部分
+ `C-y`; 粘贴寄存器中保留的删除段落. 
+ `M-y` ; 循环粘贴之前删掉的. 
+ `C-x u` ; 取消之前的命令. 
+ `c-x c-f` ; 打开文件.
+ `C-x C-s` ; 保存文件.  如果不幸被冻结, 可以输入`C-q`

#### 自动补全插件

安装完`Emacs`之后, 还需要安装一个自动补全(可以自动补全关键字, 文本等)的扩展, 名叫[company-mode](http://company-mode.github.io/)官网: 

`Emacs`里有个安装扩展的列表, 有点类似 Debian系列的 `apt-get` 和 Redhat系列的 `yum` 这样的包管理工具, 所以我们只需要按步骤安装: 

+ 按下 `M-x` 来调出命令输入 (在Windows下就是Alt + x 键)
+ 输入 `list-packages`, 回车
+ 在出现的列表里, 点击 `company`
+ 在右边出现的新缓冲区里点击 `Install`

### vscode 环境搭建

`code` 中安装插件, [ailisp:Common Lisp](https://marketplace.visualstudio.com/items?itemName=ailisp.commonlisp-vscode)
然后按照插件作者的教程配置. 

先安装`roswell` : [https://github.com/roswell/roswell/wiki/Installation#building-from-source](https://github.com/roswell/roswell/wiki/Installation#linux)

`Roswell`是一个 `Lisp` 实现的安装/管理程序, 启动程序, 以及更多的东西.
Roswell最初是一个命令行工具, 目的是使安装和管理`Common Lisp`变得非常简单和容易. 

现在, `Roswell`已经发展成为一个用于普通`Lisp`开发的全栈环境, 并具有许多功能, 使测试, 共享和分发你的Lisp应用程序变得容易. 
Roswell目前在类`Unix`的平台上运行良好, 如`Linux`, `Mac OS X`和`FreeBSD`.

如果你对缺少的东西感兴趣, 请查看问题列表. 

安装`roswell`后, 不要错过推荐的设置. 以便用`Slime`安装软件包时在`Roswell`的上下文中进行. 
注意：这可能是你第一次使用`Lisp`的经验, 你可能已经尝试了你在互联网上找到的其他方法/教程(没有一个对你有用). 
或者, 你可能已经设法使它工作, 但没有信心, 只是后来你发现了`roswell`.
我们的建议是, 你最好删除你以前做的任何事情, 否则你最终会有两个重复的`lisp`实现/`quicklisp`/`asdf`, 这使问题更加复杂. 

在`ubuntu`下需要进行手动编译. 安装为`system-wide`

```bash
sudo apt-get -y install git build-essential automake libcurl4-openssl-dev
git clone -b release https://github.com/roswell/roswell.git
cd roswell
sh bootstrap
./configure
make
sudo make install
ros setup
```

如果不想安装为`系统应用`, 安装到用户目录:

```bash
git clone -b release https://github.com/roswell/roswell.git
cd roswell
sh bootstrap
./configure --prefix=$HOME/.local
make
make install
# 添加到系统变量, 也可以手动添加
echo 'PATH=$HOME/.local/bin:$PATH' >> ~/.profile 
PATH=$HOME/.local/bin:$PATH ros setup
```

安装完成后, 使用`ros`安装`cl-lsp`, 还需要另外两个, 因为原始包在`prepl`中没有`readline`支持. 

```bash
ros install ailisp/linedit
ros install ailisp/prepl
ros install ailisp/cl-lsp
```

+ (推荐)安装 [`strict-paredit-vscode`](https://github.com/ailisp/strict-paredit-vscode), 它提供了接近 `Emacs` 的最佳括号编辑体验. 
在`vscode`中按下`Ctrl+P`(VS Code Quick Open ), 输入

```code
ext install ailisp.strict-paredit
```

+ (推荐)在 `roswell` 中使用 `sbcl` 而不是 `sbcl_bin`, 这样可以跳转到 `common-lisp` 包中的符号定义. 

```bash
ros install sbcl
ros use sbcl
```

## lisp语法

[practical common lisp 中文文档](https://binghe.github.io/pcl-cn/chap06/lexical-variables-and-closures.html)

`Lisp`的语法非常简单: 需要记住的规则很少. 

语法是由`S-表达式`组成的, Symbol-expression. `S-表达式`要么是一个`原子`(atom), 要么是一个`列表`(list). 

原子可以是`数字`, 如`10`, `3.14`, 或`符号`, 如`t`(真值常数), `+`, `my-variable`. 
还有一种特殊的符号叫做关键字, 它是以冒号为前缀的符号, 如 `:thing` 或 `:keyword`. 关键词运算为自身: 你可以把它们理解成是枚举. 

### 注释

```lisp
;; 单行注释用; 开头，可以从一行的任意位置开始
#|
多行注释
  #|
    多行注释可以嵌套
  |#
|#
```

### 格式化 Lisp 代码

[Formatting Lisp Code](https://binghe.github.io/pcl-cn/chap04/formatting-lisp-code.html)

严格说起来, 代码格式化既不是句法层面也不是语法层面上的事情, 好的格式化对于阅读和编写流利而又地道的代码而言非常重要. 
格式化 `Lisp` 代码的关键在于正确缩进它. 这一缩进应当反映出代码结构, 这样就不需要通过数括号来查看代码究竟写到哪儿了. 
一般而言, 每一个新的嵌套层次都需要多缩进一点儿, 并且如果折行是必需的, 位于同一个`嵌套层次`的项应当按行对齐. 这样, 一个需要跨越多行的函数调用可能会被写成这样: 

```lisp
(some-function arg-with-a-long-name
               another-arg-with-an-even-longer-name)
```

那些实现控制结构的宏和特殊形式在缩进上稍有不同: `body`相对于整个`form`的开放括号缩进`两个空格`. 就像这样: 

```lisp
(defun print-list (list)
  (dolist (i list)
    (format t "item: ~a~%" i)))
```

尽管如此, 但也不需要太担心这些规则, 因为一个像 `SLIME` 这样的优秀 `Lisp` 环境将会帮你做到这点. 
事实上, Lisp 正则语法的优势之一就在于, 它可以让诸如编辑器这样的软件相对容易地知道应当如何缩进. 
由于缩进的本意是反映代码的结构, 而结构是由括号来标记的, 因此很容易让编辑器帮你缩进代码. 

有经验的 `Lisp` 程序员们倾向于依赖的编辑器来自动处理缩进, 不但可以确保代码美观, 还可以检测笔误: 
一旦熟悉了代码该如何缩进, 那么一个错误放置的括号就将立即由于编辑器所给出的奇怪缩进而被发现. 例如, 假设要编写一个如下所示函数: 

```lisp
(defun foo ()
  (if (test)
    (do-one-thing)
    (do-another-thing)))
```

现在假设你不小心忘记了 `test` 后面的闭合括号. 如果不去数括号的话, 那么很可能会在 `DEFUN` 形式的结尾处添加一个额外的括号. 
如果一直都在每行的开始处按 Tab 来缩进的话, 你将得到如下的代码: 

```lisp
(defun foo ()
  (if (test
       (do-one-thing)
       (do-another-thing))))
```

看到 `then` 和 `else` 子句被缩进到了条件语句的位置, 而不是仅仅相对于`IF`稍微缩进了一点, 你将立即看出有错误发生. 

另一个重要的格式化规则是, 闭合的括号总是位于与它们所闭合的列表最后一个元素相同的行. 也就是说, 不要写成这样: 

```lisp
(defun foo ()
  (dotimes (i 10)
    (format t "~d. hello~%" i)
  )
)
```

而一定要写成这样: 

```lisp
(defun foo ()
  (dotimes (i 10)
    (format t "~d. hello~%" i)))
```

结尾处的 `)))` 可能看起来令人生畏, 但是一旦代码缩进正确, 那么括号的意义就不存在了——没有必要通过将它们分散在多行来加以突出. 

最后, 注释应该根据其适用范围被前置一到四个分号, 如同下面所说明的: 

```lisp
;;;; 四个分号用于文件头注释.

;;; 一个带有三个分号的注释将通常作为段落注释应用到接下来的一大段代码上
(defun foo (x)
  (dotimes (i x)
    ;; 两个分号说明该注释应用于接下来的代码上. 
    ;; 注意到该注释处在与其所应用的代码相同的缩进上. 
    (some-function-call)
    (another i)              ; 本注释仅用于此行
    (and-another)            ; 本注释仅用于此行
    (baz)))
```
