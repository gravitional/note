# git-basic

reference: [廖雪峰git教程](https://www.liaoxuefeng.com/wiki/896043488029600)
and [git-scm-book](https://git-scm.com/book/zh/v2/Git-%E5%9F%BA%E7%A1%80-Git-%E5%88%AB%E5%90%8D)

## SSH配置

第1步: 创建`SSH Key`.
在用户主目录下, 看看有没有`.ssh`目录, 如果有, 再看看这个目录下有没有`id_rsa`和`id_rsa.pub`这两个文件, 如果已经有了, 可直接跳到下一步.如果没有, 打开`Shell`( `Windows`下打开`Git Bash`), 创建`SSH Key`:

```bash
$ ssh-keygen -t rsa -C "youremail@example.com"
"nothing"
```

你需要把邮件地址换成你自己的邮件地址, 然后一路回车, 
使用默认值即可, 由于这个`Key`也不是用于军事目的, 所以也无需设置密码.
如果一切顺利的话, 可以在用户主目录里找到`.ssh`目录, 
里面有`id_rsa`和`id_rsa.pub`两个文件, 
这两个就是`SSH Key`的秘钥对, `id_rsa`是私钥, 不能泄露出去, `id_rsa.pub`是公钥, 可以放心地告诉任何人.

第2步: 登陆`GitHub`, 打开"`Account settings`", "`SSH Keys`"页面:

然后, 点"`Add SSH Key`", 填上任意`Title`, 在`Key`文本框里粘贴`id_rsa.pub`文件的内容:

### SSH警告

当你第一次使用`Git`的`clone`或者`push`命令连接`GitHub`时, 会得到一个警告:

```bash
The authenticity of host 'github.com (xx.xx.xx.xx)' can't be established.
RSA key fingerprint is xx.xx.xx.xx.xx.
Are you sure you want to continue connecting (yes/no)?
```

这是因为`Git`使用`SSH`连接, 而`SSH`连接在第一次验证`GitHub`服务器的`Key`时, 需要你确认`GitHub`的`Key`的指纹信息是否真的来自`GitHub`的服务器, 输入`yes`回车即可.
`Git`会输出一个警告, 告诉你已经把`GitHub`的`Key`添加到本机的一个信任列表里了:

```bash
Warning: Permanently added 'github.com' (RSA) to the list of known hosts.
```

这个警告只会出现一次, 后面的操作就不会有任何警告了.
如果你实在担心有人冒充`GitHub`服务器, 输入`yes`前可以对照`GitHub`的`RSA Key`的指纹信息是否与`SSH`连接给出的一致.

## 配置文件

配置文件放哪了? 每个仓库的`Git`配置文件都放在`.git/config`文件中:
而当前用户的`Git`配置文件放在用户主目录下的一个隐藏文件`.gitconfig`中:
别名就在[alias]后面, 要删除别名, 直接把对应的行删掉即可.
配置别名也可以直接修改这个文件, 如果改错了, 可以删掉文件重新通过命令配置.

## alias (别名)

### git status git st

`git config --global alias.st status`

### git unstage

例如, 为了解决取消暂存文件的易用性问题, 可以向`Git`中添加你自己的取消暂存别名:
`$ git config --global alias.unstage 'reset HEAD --'`
这会使下面的两个命令等价:
`$ git unstage fileA`
`$ git reset HEAD -- fileA`
这样看起来更清楚一些.

### git last

通常也会添加一个 `last` 命令, 像这样:
`$ git config --global alias.last 'log -1 HEAD'`
这样, 可以轻松地看到最后一次提交:

### logpretty

甚至还有人丧心病狂地把`lg`配置成了:

`git config --global alias.logpretty "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"`

## basics

### 初始化一个git仓库

`git init`

初始化一个`git`仓库

### 添加文件

`git add`

`git add -A` 和 `git add .`   `git add -u`
在功能上看似很相近, 但还是存在一点差别

`git add .` : 他会监控工作区的状态树, 使用它会把工作时的所有变化提交到`stage`, 包括文件内容修改(`modified`)以及新文件(`new`), 但不包括被删除的文件.

`git add -u` : 他仅监控已经被`add`的文件(即`tracked file`), 他会将被修改的文件提交到`stage`.`add -u` 不会提交新文件(`untracked file`).(`git add --update`的缩写)

`git add -A` : 是上面两个功能的合集(`git add --all`的缩写)

[原文链接](https://blog.csdn.net/caseywei/article/details/90945295)

### 提交更改

`git commit -m [comment message]`

### 修改注释

[作者: 筱湮](https://www.jianshu.com/p/098d85a58bf1)

### 修改最后一次注释

如果你只想修改最后一次注释(就是最新的一次提交),

`git commit --amend`

出现有注释的界面(你的注释应该显示在第一行), 输入`i`进入修改模式, 修改好注释后, 按`Esc`键退出编辑模式, 输入`:wq`保存并退出.`ok`, 修改完成.

### 修改之前的某次注释

[原文地址](https://www.jianshu.com/p/098d85a58bf1)

1. 输入:
`git rebase -i HEAD~2` 最后的数字`2`指的是显示到倒数第几次 比如这个输入的`2`就会显示倒数的两次注释(最上面两行)
1. 你想修改哪条注释 就把哪条注释前面的`pick`换成`edit`.
方法就是上面说的编辑方式: `i---`编辑, 把`pick`换成`edit`---`Esc`---`:wq`
1. 然后: (接下来的步骤Terminal会提示)`git commit --amend`
1. 修改注释, 保存并退出后, 输入: `git rebase --continue`

其实这个原理我的理解就是先版本回退到你想修改的某次版本, 然后修改当前的`commit`注释, 然后再回到本地最新的版本

### 修改之前的某几次注释

修改多次的注释其实步骤和上面的一样, 不同点在于:

1. 你可以将多个想修改的`commit`注释前面的`pick`换成`edit`
2. 依次修改你的注释(顺序是从旧到新), `Terminal`基本都会提示你接下来的操作, 每修改一个注释都要重复上面的`3`和`4`步, 直到修改完你所选择的所有注释

### 已经将代码push到远程仓库

首先, 你把最新的版本从远程仓库先`pull`下来, 修改的方法都如上, 最后修改完成后, 强制`push`到远程仓库:

`git push --force origin master`

注: 很重要的一点是, 你最好保证在你强制`push`之前没有人提交代码, 如果在你`push`之前有人提交了新的代码到远程仓库, 然后你又强制`push`, 那么会被你的强制更新覆盖! ! !

### git status

要随时掌握工作区的状态

### 比较差别 git diff

如果`git status`告诉你有文件被修改过, 用`git diff`可以查看修改内容.
查看和上一版本的具体变动内容 显示内容如下:

```bash
diff --git a/test.txt b/test.txt
index 629d9c8..3d98a7f 100644
--- a/test.txt
+++ b/test.txt
@@ -4,8 +4,9 @@
test line3.
test line4.
test line5.
test line6.
-Git is a version control system.
+Git is a distributed version control system.
Git is free software.
+Very Good!
test line7.
test line8.
test line9.
```

### git diff 输出说明

`diff --git a/test.txt b/test.txt`
 -- 对比两个文件, 其中`a`改动前, `b`是改动后, 以`git`的`diff`格式显示;

`index 629d9c8..3d98a7f 100644`
 -- 两个版本的`git`哈希值, `index`区域(`add`之后)的`629d9c8`对象和工作区域的`3d98a7f`对象,
`100`表示普通文件, `644`表示权限控制;

`--- a/test.txt`
`+++ b/test.txt`
 -- 减号表示变动前, 加号表示变动后;

```bash
@@ -4,8 +4,9 @@ test line3.
test line4.  test line5.  test line6.
```

` -- @@`表示文件变动描述合并显示的开始和结束, 一般在变动前后多显示3行,
其中`-+`表示变动前后, 逗号前是起始行位置, 逗号后为从起始行往后几行.
合起来就是变动前后都是从第`4`行开始, 变动前文件往后数`8`行对应变动后文件往后数`9`行.
变动内容 `+`表示增加了这一行, `-`表示删除了这一行, 没符号表示此行没有变动.

### git diff 用法

`git-diff` - 显示`commits`之间的变化, `commit`和`working tree`之间的差异等.

SYNOPSIS

```git
git diff [<options>] [<commit>] [--] [<path>... ]
git diff [<options>] --cached [<commit>] [--] [<path>... ]
git diff [<options>] <commit> <commit> [--] [<path>... ]
git diff [<options>] <blob> <blob>
git diff [<options>] --no-index [--] <path> <path>
```

DESCRIPTION

Show:

+ changes between the `working tree` and the `index` or a `tree`,
+ changes between the `index` and `a tree`,
+ changes between two trees,
+ changes between two `blob` objects,
+ changes between two `files` on disk.

use as:

| SYNOPSIS | explanation |
| ---- | ---- |
| `git diff [<options>] [--] [<path>... ]` | `working tree` and the `index` |
| `git diff [<options>] --no-index [--] <path> <path>` | two paths on the filesystem |
| `git diff [<options>] --cached [<commit>] [--] [<path>... ]` | changes you staged relative to the named `<commit>` |
| `git diff [<options>] <commit> [--] [<path>... ]` | changes in your working tree relative to the named `<commit>` |
| `git diff [<options>] <commit> <commit> [--] [<path>... ]` | changes between two arbitrary `<commit>` |
| `git diff [<options>] <commit>..<commit> [--] [<path>... ]` | This is synonymous to the previous form |

### the details

+ `git diff [<options>] <blob> <blob>` ; 这种形式是为了查看两个`blob`对象的原始内容之间的差异.
+ `git diff [<options>] [--] [<path>... ]`; 这个形式是用来查看你相对于`index`(下次提交的暂存区)所做的修改.
换句话说, 这些差异是你可以告诉`Git`进一步添加到stage中的, 但还没有实际添加. 你可以通过使用`git-add`对这些修改进行添加.

+ `git diff [<options>] --no-index [--] <path> <path>` ; 这种形式是为了比较文件系统上给定的两个路径.
当在一个由Git控制的工作树中运行该命令, 并且至少有一个路径指向该工作树之外时, 可以省略`--no-index`选项. 或在`Git`控制的工作树之外运行该命令.

+ `git diff [<options>] --cached [<commit>] [--] [<path>... ]`; 这种形式是用来查看你为下一次`commit`所做的相对于`<commit>`的修改.
通常情况下, 你希望与最新的提交进行比较, 所以如果你没有给出`<commit>`, 它默认为`HEAD`.
如果`HEAD`不存在(例如未出生的分支), 并且没有给出`<commit>`, 它将显示所有已提交的修改. `--staged`是`--cached`的同义词.

+ `git diff [<options>] <commit> [--] [<path>... ]`;  这种形式是用来查看你的工作区中相对于命名为`<commit>`的修改.
你可以用`HEAD`与最新的提交进行比较, 或者用`branch name`与不同分支的`tip`进行比较.

+ `git diff [<options>] <commit> <commit> [--] [<path>... ]` ; 用来查看两个任意的`<commit>`之间的变化.
+ `git diff [<options>] <commit>..<commit> [--] [<path>... ]`; 上一条的同义形式. 如果省略了一边的`<commit>`, 将使用`HEAD`.
+ `git diff [<options>] <commit>...<commit> [--] [<path>... ]` ; 这种形式是用来查看
    + 当前分支上的修改
    + 包含并直到第二个 `<commit>`
    + 起点是两个`<commit>`的共同祖先.
  `git diff A...B`等同于`git diff $(git merge-base A B) B`. 你可以省略`<commit>`中的任何一个, 相当于使用`HEAD`.

注意:

+ 为了防止你做一些奇怪的事情(exotic), 提醒一下, 上述描述中的所有`<commit>`, 除了最后两种使用`..`的形式, 也可以是任何`<tree>`.
+ 关于`<commit>`更完整的拼写方式, 请参见`gitrevisions`中的 `SPECIFYING REVISIONS`部分.
然而, `diff`是关于比较两个 `端点`, 而不是 `范围`, 范围符号(`<commit>...<commit>`和`<commit>...<commit>`)
并不能按照`gitrevisions`中 `SPECIFYING RANGES`的定义理解.

### log 查看提交历史

`git log` ; 穿梭时光前, 用`git log`可以查看提交历史, 以便确定要回退到哪个版本

+ `-<number>,-n <number>,--max-count=<number>`; 限制输出的提交数量.
+ `--skip=<number>`; 在开始显示提交输出之前, 跳过提交的数量.
+ `--since=<date>, --after=<date>`; 显示比特定日期更近的提交.
+ `--until=<date>, --before=<date>`; 显示比特定日期更早的提交.

+ `--author=<pattern>, --committer=<pattern>`;
将提交文件的输出限制在, `作者`/`提交`人标题行符合指定模式(正则表达式)的提交文件.
如果有多个 `--author=<pattern>`, 则会选择那些作者与任意模式相匹配的提交(多个` --committer=<pattern>` 的情况也是如此).

+ `--grep-reflog=<pattern>`;
将提交结果限制在有符合指定模式(正则表达式)的reflog条目的提交.
如果有多个 `--grep-reflog`, 则会选择那些 `reflog` 信息符合任何模式的提交. 除非使用 `--walk-reflogs`, 否则使用此选项是错误的.

+ `--grep=<pattern>`;
将提交结果限制在, 日志信息与指定模式(正则表达式)相匹配.
如果有多个 `--grep=<pattern>`, 则选择信息与任何指定模式相匹配的提交(另外参见 `--all-match`).
当`--show-notes`生效时, 笔记中的信息会被匹配, 就像它是日志信息的一部分.

+ `--all-match`; 将提交的输出限制在与所有给定的 `--grep` 匹配的内容上, 而不是至少匹配一个的内容.
+ `--invert-grep`; 将提交的内容限制在与 `--grep=<pattern>` 指定的模式不匹配的日志信息中.
+ `-i,--regexp-ignore-case`; 匹配限制模式的正则表达式, 不考虑字母大小写.
+ `--basic-regexp`; 将限制模式视为基本正则表达式; 这是默认的.

### 查看本地+远程所有分支的全部提交以及关系

[git查看本地+远程所有分支的全部提交以及关系](https://blog.csdn.net/wq6ylg08/article/details/89052225)

当我们深入学习Git后, 我们不仅在本地仓库有超多的分支,
还在远程仓库有超多的分支, 如果我们只使用`git log`和`gitk`命令, 我们会发现这两个命令只能显示当前所处分支的全部提交记录, 并不能查看本地+远程所有分支的全部提交记录.

解决方案是我们采用git log和gitk命令的升级版

+ `git log --graph --all`
+ `gitk --all`

`gitk --all`是真实的画出 `本地`+ `远程` 所有分支的全部提交的树状结构, 看起来更全面.
强烈推荐以后查看整个项目的所有分支情况, 使用这个命令`gitk --all`
