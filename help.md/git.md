# learn.git.md

reference: [廖雪峰git教程](https://www.liaoxuefeng.com/wiki/896043488029600)
and [git-scm-book](https://git-scm.com/book/zh/v2/Git-%E5%9F%BA%E7%A1%80-Git-%E5%88%AB%E5%90%8D)

## SSH配置

第1步：创建`SSH Key`.
在用户主目录下, 看看有没有`.ssh`目录, 如果有, 再看看这个目录下有没有`id_rsa`和`id_rsa.pub`这两个文件, 如果已经有了, 可直接跳到下一步.如果没有, 打开`Shell`( `Windows`下打开`Git Bash`), 创建`SSH Key`：

```bash
$ ssh-keygen -t rsa -C "youremail@example.com"
"nothing"
```

你需要把邮件地址换成你自己的邮件地址, 然后一路回车, 使用默认值即可, 由于这个`Key`也不是用于军事目的, 所以也无需设置密码.
如果一切顺利的话, 可以在用户主目录里找到`.ssh`目录, 里面有`id_rsa`和`id_rsa.pub`两个文件, 这两个就是`SSH Key`的秘钥对, `id_rsa`是私钥, 不能泄露出去, `id_rsa.pub`是公钥, 可以放心地告诉任何人.

第2步：登陆`GitHub`, 打开“`Account settings`”, “`SSH Keys`”页面：

然后, 点“`Add SSH Key`”, 填上任意`Title`, 在`Key`文本框里粘贴`id_rsa.pub`文件的内容：

### SSH警告

当你第一次使用`Git`的`clone`或者`push`命令连接`GitHub`时, 会得到一个警告：

```bash
The authenticity of host 'github.com (xx.xx.xx.xx)' can't be established.
RSA key fingerprint is xx.xx.xx.xx.xx.
Are you sure you want to continue connecting (yes/no)?
```

这是因为`Git`使用`SSH`连接, 而`SSH`连接在第一次验证`GitHub`服务器的`Key`时, 需要你确认`GitHub`的`Key`的指纹信息是否真的来自`GitHub`的服务器, 输入`yes`回车即可.
`Git`会输出一个警告, 告诉你已经把`GitHub`的`Key`添加到本机的一个信任列表里了：

```bash
Warning: Permanently added 'github.com' (RSA) to the list of known hosts.
```

这个警告只会出现一次, 后面的操作就不会有任何警告了.
如果你实在担心有人冒充`GitHub`服务器, 输入`yes`前可以对照`GitHub`的`RSA Key`的指纹信息是否与`SSH`连接给出的一致.

## 配置文件

配置文件放哪了？每个仓库的`Git`配置文件都放在`.git/config`文件中：
而当前用户的`Git`配置文件放在用户主目录下的一个隐藏文件`.gitconfig`中：
别名就在[alias]后面, 要删除别名, 直接把对应的行删掉即可.
配置别名也可以直接修改这个文件, 如果改错了, 可以删掉文件重新通过命令配置.

## alias (别名)

### git status git st

`git config --global alias.st status`

### git unstage

例如, 为了解决取消暂存文件的易用性问题, 可以向`Git`中添加你自己的取消暂存别名：
`$ git config --global alias.unstage 'reset HEAD --'`
这会使下面的两个命令等价：
`$ git unstage fileA`
`$ git reset HEAD -- fileA`
这样看起来更清楚一些.

### git last

通常也会添加一个 `last` 命令, 像这样：
`$ git config --global alias.last 'log -1 HEAD'`
这样, 可以轻松地看到最后一次提交：

### logpretty

甚至还有人丧心病狂地把`lg`配置成了：

`git config --global alias.logpretty "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"`

## basics

### 初始化一个git仓库

`git init`

初始化一个`git`仓库

### 添加文件

`git add`

`git add -A` 和 `git add .`   `git add -u`
在功能上看似很相近, 但还是存在一点差别

`git add .` ：他会监控工作区的状态树, 使用它会把工作时的所有变化提交到`stage`, 包括文件内容修改(`modified`)以及新文件(`new`), 但不包括被删除的文件.

`git add -u` ：他仅监控已经被`add`的文件(即`tracked file`), 他会将被修改的文件提交到`stage`.`add -u` 不会提交新文件(`untracked file`).(`git add --update`的缩写)

`git add -A` ：是上面两个功能的合集(`git add --all`的缩写)

[原文链接](https://blog.csdn.net/caseywei/article/details/90945295)

### 提交更改

`git commit -m [comment message]`

### 修改注释

[作者：筱湮](https://www.jianshu.com/p/098d85a58bf1)

### 修改最后一次注释

如果你只想修改最后一次注释(就是最新的一次提交),

`git commit --amend`

出现有注释的界面(你的注释应该显示在第一行), 输入`i`进入修改模式, 修改好注释后, 按`Esc`键退出编辑模式, 输入`:wq`保存并退出.`ok`, 修改完成.

### 修改之前的某次注释

[原文地址](https://www.jianshu.com/p/098d85a58bf1)

1. 输入：
`git rebase -i HEAD~2` 最后的数字`2`指的是显示到倒数第几次 比如这个输入的`2`就会显示倒数的两次注释(最上面两行)
1. 你想修改哪条注释 就把哪条注释前面的`pick`换成`edit`.
方法就是上面说的编辑方式：`i---`编辑, 把`pick`换成`edit`---`Esc`---`:wq`
1. 然后：(接下来的步骤Terminal会提示)`git commit --amend`
1. 修改注释, 保存并退出后, 输入：`git rebase --continue`

其实这个原理我的理解就是先版本回退到你想修改的某次版本, 然后修改当前的`commit`注释, 然后再回到本地最新的版本

### 修改之前的某几次注释

修改多次的注释其实步骤和上面的一样, 不同点在于：

1. 你可以将多个想修改的`commit`注释前面的`pick`换成`edit`
2. 依次修改你的注释(顺序是从旧到新), `Terminal`基本都会提示你接下来的操作, 每修改一个注释都要重复上面的`3`和`4`步, 直到修改完你所选择的所有注释

### 已经将代码push到远程仓库

首先, 你把最新的版本从远程仓库先`pull`下来, 修改的方法都如上, 最后修改完成后, 强制`push`到远程仓库：

`git push --force origin master`

注：很重要的一点是, 你最好保证在你强制`push`之前没有人提交代码, 如果在你`push`之前有人提交了新的代码到远程仓库, 然后你又强制`push`, 那么会被你的强制更新覆盖！！！

### git status

要随时掌握工作区的状态

### 比较差别 git diff

如果`git status`告诉你有文件被修改过, 用`git diff`可以查看修改内容.
查看和上一版本的具体变动内容 显示内容如下：

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
——对比两个文件, 其中`a`改动前, `b`是改动后, 以`git`的`diff`格式显示；

`index 629d9c8..3d98a7f 100644`
——两个版本的`git`哈希值, `index`区域(`add`之后)的`629d9c8`对象和工作区域的`3d98a7f`对象,
`100`表示普通文件, `644`表示权限控制；

`--- a/test.txt`
`+++ b/test.txt`
——减号表示变动前, 加号表示变动后；

```bash
@@ -4,8 +4,9 @@ test line3.
test line4.  test line5.  test line6.
```

`——@@`表示文件变动描述合并显示的开始和结束, 一般在变动前后多显示3行,
其中`-+`表示变动前后, 逗号前是起始行位置, 逗号后为从起始行往后几行.
合起来就是变动前后都是从第`4`行开始, 变动前文件往后数`8`行对应变动后文件往后数`9`行.
变动内容 `+`表示增加了这一行, `-`表示删除了这一行, 没符号表示此行没有变动.

### git diff 用法

`git-diff` - 显示`commits`之间的变化, `commit`和`working tree`之间的差异等.

SYNOPSIS

```git
git diff [<options>] [<commit>] [--] [<path>…​]
git diff [<options>] --cached [<commit>] [--] [<path>…​]
git diff [<options>] <commit> <commit> [--] [<path>…​]
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
| `git diff [<options>] [--] [<path>…​]` | `working tree` and the `index` |
| `git diff [<options>] --no-index [--] <path> <path>` | two paths on the filesystem |
| `git diff [<options>] --cached [<commit>] [--] [<path>…​]` | changes you staged relative to the named `<commit>` |
| `git diff [<options>] <commit> [--] [<path>…​]` | changes in your working tree relative to the named `<commit>` |
| `git diff [<options>] <commit> <commit> [--] [<path>…​]` | changes between two arbitrary `<commit>` |
| `git diff [<options>] <commit>..<commit> [--] [<path>…​]` | This is synonymous to the previous form |

### the details

+ `git diff [<options>] <blob> <blob>` ; 这种形式是为了查看两个`blob`对象的原始内容之间的差异.
+ `git diff [<options>] [--] [<path>…​]`; 这个形式是用来查看你相对于`index`(下次提交的暂存区)所做的修改.
换句话说, 这些差异是你可以告诉`Git`进一步添加到索引中的, 但还没有实际添加. 你可以通过使用`git-add`对这些修改进行添加.

+ `git diff [<options>] --no-index [--] <path> <path>` ; 这种形式是为了比较文件系统上给定的两个路径.
当在一个由Git控制的工作树中运行该命令, 并且至少有一个路径指向该工作树之外时, 可以省略`--no-index`选项. 或在`Git`控制的工作树之外运行该命令.

+ `git diff [<options>] --cached [<commit>] [--] [<path>…​]`; 这种形式是用来查看你为下一次`commit`所做的相对于`<commit>`的修改.
通常情况下, 你希望与最新的提交进行比较, 所以如果你没有给出`<commit>`, 它默认为`HEAD`.
如果`HEAD`不存在(例如未出生的分支), 并且没有给出`<commit>`, 它将显示所有已提交的修改. `--staged`是`--cached`的同义词.

+ `git diff [<options>] <commit> [--] [<path>…​]`;  这种形式是用来查看你的工作区中相对于命名为`<commit>`的修改.
你可以用`HEAD`与最新的提交进行比较, 或者用`branch name`与不同分支的`tip`进行比较.

+ `git diff [<options>] <commit> <commit> [--] [<path>…​]` ; 用来查看两个任意的`<commit>`之间的变化.
+ `git diff [<options>] <commit>..<commit> [--] [<path>…​]`; 上一条的同义形式. 如果省略了一边的`<commit>`, 将使用`HEAD`.
+ `git diff [<options>] <commit>...<commit> [--] [<path>…​]` ; 这种形式是用来查看
  + 当前分支上的修改
  + 包含并直到第二个 `<commit>`
  + 起点是两个`<commit>`的共同祖先.
  `git diff A...B`等同于`git diff $(git merge-base A B) B`. 你可以省略`<commit>`中的任何一个, 相当于使用`HEAD`.

注意：

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
+ `--basic-regexp`; 将限制模式视为基本正则表达式；这是默认的.

### 查看本地+远程所有分支的全部提交以及关系

[git查看本地+远程所有分支的全部提交以及关系](https://blog.csdn.net/wq6ylg08/article/details/89052225)

当我们深入学习Git后, 我们不仅在本地仓库有超多的分支,
还在远程仓库有超多的分支, 如果我们只使用`git log`和`gitk`命令, 我们会发现这两个命令只能显示当前所处分支的全部提交记录, 并不能查看本地+远程所有分支的全部提交记录.

解决方案是我们采用git log和gitk命令的升级版

+ `git log --graph --all`
+ `gitk --all`

`gitk --all`是真实的画出本地+远程所有分支的全部提交的树状结构, 看起来更全面.
强烈推荐以后查看整个项目的所有分支情况, 使用这个命令`gitk --all`

## git 文件管理

`git reflog`

要重返未来, 用`git reflog`查看命令历史, 以便确定要回到未来的哪个版本.

### git reset

***
`git reset --hard <commit>` or 别名 `grhh <commit>`

`--hard` 会清空`working tree`和`index`的改动.
彻底回退版本, 连本地文件都会被回退到上个版本的内容

***
`git reset --soft xxxx` or 别名 `grh --soft <commit>`

保留`working tree`和`index`, 并合并到`index`中.
只回退`commit`, 如果你想再次提交直接`git commit`即可.

`reset --soft` 会在重置 `HEAD` 和 `branch` 时, 保留`working tree`和`index`中的内容,
并把重置 `HEAD` 所带来的新的差异放进`index`.

***
`reset 不加参数(--mixed)` or 别名 `grh <commit>`

清空`index`,`mix`到`working tree`中

`reset` 如果不加参数, 那么默认使用 `--mixed` 参数.它的行为是：保留`working tree`, 并且清空`index`.
也就是说, `working tree`的修改, `index`的内容以及由 `reset` 所导致的新的文件差异, 都会被放进`working tree`.
简而言之, 就是把所有差异都混合(`mixed`)放在`working tree`中`.

***
同理, `reset --hard` 不仅可以撤销提交, 还可以用来把 `HEAD` 和 `branch` 移动到其他的任何地方.

```bash
git reset --hard branch2
```

把 `HEAD` 和 `branch`移动到`branch2`指向的提交.

[Git Reset 三种模式](https://www.jianshu.com/p/c2ec5f06cf1a)
[git reset --hard xxx, git reset --soft 及git revert 的区别](https://www.jianshu.com/p/8be0cc35e672)

### git revert

`git-revert` - 回复某些已经存在的提交

SYNOPSIS

+ `git revert [--[no-]edit] [-n] [-m parent-number] [-s] [-S[<keyid>]] <commit>...`
+ `git revert --continue`
+ `git revert --quit`
+ `git revert --abort`

DESCRIPTION

给出一个或者多个提交, 逆转相关的`patches` 引入的更改.并生成新的提交来记录这个操作.
前提是你的 `working tree` 是 `clean` 的(no modifications from the HEAD commit).

`git revert `用来记录撤销提交的操作(通常是错误的提交).
如果只是想丢弃工作区的修改, 可以使用`git-reset --hard`, 或者用`git checkout <commit>  -- <filename>`从别的提交中提取文件(覆盖当前版本), 不同于`git revert`, 这些操作都会导致工作区未提交的更改丢失.

相比`git reset`, 它不会改变现在的提交历史.因此, `git revert`可以用在公共分支上, `git reset`应该用在私有分支上.

[git reset --hard xxx, git reset --soft 及git revert 的区别]: https://www.jianshu.com/p/8be0cc35e672

[Git Reset 三种模式]: https://www.jianshu.com/p/c2ec5f06cf1a

### 恢复EXAMPLES

To restore all files in the current directory

```bash
git restore .
```

The following sequence switches to the master branch, reverts the Makefile to two revisions back, deletes `hello.c` by mistake, and gets it back from the index.

```bash
git switch master
git restore --source master~2 Makefile  (1)
rm -f hello.c
git restore hello.c                     (2)
```

1. take a file out of another commit
2. restore hello.c from the index

If you want to restore all `C` source files to match the version in the index, you can say

```bash
git restore '*.c'
```

Note the quotes around `*.c` The file `hello.c` will also be restored, even though it is no longer in the working tree, because the file globbing is used to match entries in the index (not in the working tree by the shell).

### 删除一个文件

`git rm`

命令`git rm`用于删除一个文件.
如果一个文件已经被提交到版本库, 那么你永远不用担心误删, 但是要小心, 你只能恢复文件到最新版本, 你会丢失最近一次提交后你修改的内容.

### checkout还原文件

```bash
git checkout [<tree-ish>] [--] <pathspec>…​
```

用 **index**或者`<tree-ish>`(通常是一个`commit`)里面的内容替换`working tree`里面的 paths.

当给出一个`<tree-ish>`的时候, the **paths** that match the `<pathspec>`会在**index** and in the **working tree**里面都更新.

`index `中可能包含有之前合并失败的`entries`.默认情况下, 如果你想checkout 一个这样的`entries`, 会失败, 什么都不会发生.
使用`-f`选项忽略未合并的`entries`.

可以选择`merge`的特定一方的内容, 使用选项`--ours` or `--theirs`.

使用`-m`选项, 可以抛弃对`working tree`的更改, 恢复到 the original conflicted merge result

## 分支管理

那么, `Git`又是怎么知道当前在哪一个分支上呢？ 也很简单, 它有一个名为`HEAD`的特殊指针.请注意它和许多其它版本控制系统(如`Subversion`或`CVS`)里的 `HEAD` 概念完全不同. 在`Git`中, 它是一个指针, 指向当前所在的本地分支(译注：将 `HEAD` 想象为当前分支的别名).在本例中, 你仍然在`master`分支上. 因为`git branch`命令仅仅创建一个新分支, 并不会自动切换到新分支中去.

由于`Git`的分支实质上仅是包含所指对象校验和(长度为`40`的`SHA-1`值字符串)的文件, 所以它的创建和销毁都异常高效.创建一个新分支就相当于往一个文件中写入`41`个字节(`40`个字符和`1`个换行符), 如此的简单能不快吗？

这与过去大多数版本控制系统形成了鲜明的对比, 它们在创建分支时, 将所有的项目文件都复制一遍, 并保存到一个特定的目录.完成这样繁琐的过程通常需要好几秒钟, 有时甚至需要好几分钟.所需时间的长短, 完全取决于项目的规模.而在 `Git` 中, 任何规模的项目都能在瞬间创建新分支.同时, 由于每次提交都会记录父对象, 所以寻找恰当的合并基础(译注：即共同祖先)也是同样的简单和高效. 这些高效的特性使得 `Git` 鼓励开发人员频繁地创建和使用分支.

### 各种本地分支命令

`Git`鼓励大量使用分支：

查看分支：`git branch`

`git br -a`
`git br -vv`
`git br -avv`

创建分支：`git branch name`

切换分支： `git switch name`

新建+切换 到新分支： `git checkout -b branchname`

合并某分支到当前分支：`git merge name`

删除分支：`git branch -d name`

### 创建分支

```bash
git branch [--track | --no-track] [-f] <branchname> [<start-point>]
```

命令的上述形式创建了名为 `<branchname>` 的新分支头, 它指向当前的`HEAD`, 或者如果给定了 `<start-point>`.

`<start-point>`有一个特殊形式: 如果`A`和`B`恰好有一个合并基, 您可以使用`A...B`作为指定这个合并基的快捷方式.最多可以省略`A`和`B`中的一个, 它默认为`HEAD`.

也可以用checkout 的特殊语法

`git checkout -b`
`git checkout -b "branchname " "startpoint"`

### 删除远程分支

可以运行带有`--delete`选项的`git push`命令

```bash
$ git push origin --delete serverfix
To https://github.com/schacon/simplegit
- [deleted]         serverfix
```

```bash
git push [远程仓库] --delete [branchname]
```

或者用

```bash
git push origin :分支名
```

例如删除远程分支`dev`, 命令：`git push origin :dev`, 注意`:`前的空格

### 查看分支详情

`git branch`
`-v`
`-vv`
`-avv`
`--verbose`

If --list is given, or if there are no non-option arguments, existing branches are listed;
it is in list mode, show sha1 and commit subject line for each head, along with relationship to upstream branch (if any).
If given twice, print the path of the linked worktree (if any) and the name of the upstream branch, as well (see also git remote show `remote`).
Note that the current worktree’s `HEAD` will not have its path printed (it will always be your current directory).

### 创建并切换到分支

`git checkout -b`
`git checkout -b "branchname " "startpoint"`

The new branch **head**  will point to **this commit**. It may be given as a **branch name**, a **commit-id**, or **a tag**.
If this option is omitted, the **current HEAD** will be used instead.

`git branch [--track | --no-track] [-f] <branchname> [<start-point>]`
`-t`
`--track`

当创建一个新分支的时候, 设置 `branch.<name>.remote` and `branch.<name>.merge`项目来标记新分支的“上游”.
这个设置会告诉git如何在 `git status` and `git branch -v`显示两个分支的关系
此外, 切换到新分支时, 不带参数的`git pull`将从上游拉取更新

When creating a new branch, set up `branch.<name>.remote` and `branch.<name>.merge` configuration entries to mark the start-point branch as "upstream" from the new branch. This configuration will tell git to show the relationship between the two branches in `git status` and `git branch -v`.
Furthermore, it directs git pull without arguments to pull from the upstream when the new branch is checked out.

This behavior is the default when the start point is a remote-tracking branch.
Set the `branch.autoSetupMerge` configuration variable to `false` if you want `git switch`, `git checkout` and `git branch` to always behave as if `--no-track` were given.
Set it to `always` if you want this behavior when the start-point is either a local or remote-tracking branch.

### 重命名git分支名称

1. `git branch -m local_oldbranch local_newbranch`(修改本地分支)
2. `git push origin :remote_branch`(删除远程分支)
3. `git push origin local_newbranch:remote_branch`(push到远程分支)
4. `git branch -u  origin/remote_branch`绑定远程分支

### merge-file 合并文件

`git-merge-file` - Run a three-way file merge

### 修改提交历史

reset是用来修改提交历史的, 想象这种情况, 如果你在`2`天前提交了一个东西, 突然发现这次提交是有问题的.

这个时候你有两个选择, 要么使用`git revert`(推荐), 要么使用git reset.

[git的reset和checkout的区别](https://segmentfault.com/a/1190000006185954)

### 借用其他分支的文件

经常被问到如何从一个分支合并特定的文件到另一个分支.
其实, 只合并你需要的那些commits, 不需要的commits就不合并进去了.

#### 合并单个commit

合并某个分支上的单个commit

首先, 用`git log`或`sourcetree`工具查看一下你想选择哪些`commits`进行合并, 例如：

比如`feature` 分支上的`commit 82ecb31` 非常重要, 它含有一个`bug`的修改, 或其他人想访问的内容.
无论什么原因, 你现在只需要将`82ecb31` 合并到`master`, 而不合并`feature`上的其他`commits`, 所以我们用`git cherry-pick`命令来做：

```bash
git checkout master
git cherry-pick 82ecb31
```

这样就好啦.现在`82ecb31`就被合并到`master`分支, 并在`master`中添加了`commit`(作为一个新的`commit`).
`cherry-pick` 和`merge`比较类似, 如果git不能合并代码改动(比如遇到合并冲突), git需要你自己来解决冲突并手动添加commit.

这里`git cherry-pick`每次合并过来会显示文件冲突(其实并没有冲突代码部分, 只需手动解决既可)

#### 合并一系列commits

合并某个分支上的一系列commits

在一些特性情况下, 合并单个commit并不够, 你需要合并一系列相连的commits.这种情况下就不要选择`cherry-pick`了, `rebase` 更适合.
还以上例为例, 假设你需要合并`feature`分支的`commit 76cada ~62ecb3` 到`master`分支.

首先需要基于`feature`创建一个新的分支, 并指明新分支的最后一个`commit`：

```bash
git checkout featuregit
git checkout -b newbranch 62ecb3
```

然后, rebase这个新分支的`commit`到`master`(`--ontomaster`).
`76cada^` 指明你想从哪个特定的commit开始.

```bash
git rebase --ontomaster 76cada^
```

得到的结果就是`feature`分支的`commit 76cada ~62ecb3` 都被合并到了master分支.

#### 合并某个文件

另外如果只想将master分支的某个文件`f.txt`合并到feature分支上.

```bash
1: git checkout feature
2: git checkout --patch master f.txt
```

第一个命令： 切换到feature分支；
第二个命令：合并master分支上`f`文件到`feature`分支上,
将`master`分支上 `f` 文件追加补丁到`feature`分支上的`f`文件.
你可以接受或者拒绝补丁内容.

如果只是简单的将`feature`分支的文件`f.txt` copy到`master`分支上；

```bash
git checkout master
git checkout feature f.txt
```

[Git合并指定文件到另一个分支](https://www.cnblogs.com/yanglang/p/11436304.html)

## 远程分支

远程跟踪分支是远程分支状态的引用. 它们是你不能移动的本地引用, 当你做任何网络通信操作时, 它们会自动移动. 远程跟踪分支像是你上次连接到远程仓库时, 那些分支所处状态的书签.

+ 克隆远程

`git clone`

要克隆一个仓库, 首先必须知道仓库的地址, 然后使用`git clone`命令克隆.

+ 添加远程

`git remote add`

要关联一个远程库, 使用命令
`git remote add origin git@server-name:path/repo-name.git`
`origin` 是远程仓库的名字

+ 查看某个远程仓库

`git remote show [remote-name]` 命令.
`remote-name` 如 `origin`

+ 从远程获取更新

`git fetch [remote-name]`

这个命令会访问远程仓库, 从中拉取所有你还没有的数据. 执行完成后, 你将会拥有那个远程仓库中所有分支的引用, 可以随时合并或查看

`git fetch` 命令会将数据拉取到你的本地仓库——它并不会自动合并或修改你当前的工作. 当准备好时你必须手动将其合并入你的工作

现在 `Paul` 的 `master` 分支可以在本地通过 `pb/master` 访问到——你可以将它合并到自己的某个分支中,

+ 删除远程分支

可以运行带有`--delete`选项的`git push`命令

```bash
$ git push origin --delete serverfix
To https://github.com/schacon/simplegit
- [deleted]         serverfix
```

```bash
git push [远程仓库] --delete [branchname]
```

或者用

```bash
git push origin :分支名
```

例如删除远程分支`dev`, 命令：`git push origin :dev`, 注意`:`前的空格

### 设置跟踪/上游

`--set-upstream` **过时命令**

As this option had confusing syntax, it is no longer supported. Please use `--track` or `--set-upstream-to` instead.

`branch -u`

```bash
git branch (--set-upstream-to=<upstream> | -u <upstream>) [<branchname>]
```

`-u <upstream>`
`--set-upstream-to=<upstream>`

`<branchname>` 指的是想要设置上游的本地branchname

Set up `<branchname>`'s tracking information so `<upstream>` is considered `<branchname>`'s upstream branch.
If no `<branchname>`  is specified, then it defaults to the current branch.

你可以在任意时间使用`-u`或`--set-upstream-to`选项运行`git branch`来显式地设置

例

```bash
$ git branch -u origin/serverfix
Branch serverfix set up to track remote branch serverfix from origin.
```

### 移除upstream/上游

syntax:  `branch --unset-upstream [<branchname>]`

删除`<branchname>`的上游信息. 如果没有指定分支, 则默认为当前分支.

### checkout -- track

当你克隆一个`repository`时, 它通常会自动创建一个跟踪`origin/master`的主分支.
不过, 如果你愿意, 也可以设置其他的跟踪分支--跟踪其他远程的分支, 或者不跟踪`master`分支.
简单的例子是, 运行

```bash
git checkout -b <branch> <remote>/<branch>
```

这是一个很常见的操作, 所以`Git`提供了`--track`的缩写

```bash
git checkout --track origin/serverfix
```

事实上, 这种情况非常普遍, 甚至有一个快捷方式来处理这个快捷方式. 如果你要检出的分支名称

+ 不存在, 并且
+ 只匹配一个远程仓库的分支

`Git` 会为你创建一个跟踪分支.

```bash
git checkout serverfix
```

要使本地分支的名字和远程不同, 你可以很容易地使用第一个版本的命令, 使用不同的本地分支名称`sf`

```bash
git checkout -b sf origin/serverfix
```

现在, 你的本地分支`sf`会自动从 `origin/serverfix` 拉取更新.

### branch -u 跟踪上游

如果你已经有一个本地分支, 并想让它跟踪你刚拉下来的远程分支, 或者想改变你所追踪的上游分支,
你可以在任何时候使用 `git branch` 的`-u`或`-set-upstream-to`选项来明确设置.

```bash
$ git branch -u origin/serverfix
```

如果想要查看设置的所有跟踪分支, 可以使用`git branch -vv`.

### push -u 推到远程

当你想分享你的项目时, 必须将其推送到上游. 这个命令很简单:

```bash
git push [remote-name] [branch-name]
git push -u origin master
```

+ `-u, --set-upstream`等效.

`git push -u origin master`第一次推送`master`分支的所有内容
加上了`-u`参数, `Git`不但会把本地的`master`分支内容推送的远程新的`master`分支, 还会把本地的`master`分支和远程的`master`分支关联起来, 在以后的推送或者拉取时就可以简化命令
此后, 每次本地提交后, 就可以使用命令`git push origin master`推送最新修改

只有当你有所克隆服务器的写入权限, 并且之前没有人推送过时, 这条命令才能生效.
当你和其他人在同一时间克隆, 他们先推送到上游然后你再推送到上游, 你的推送就会毫无疑问地被拒绝.
你必须先将他们的工作拉取下来并将其合并进你的工作后才能推送

syntex:

`git push [-u | --set-upstream] [<repository> [<refspec>… ]]`

对于每一个最新的或成功推送的分支, 添加上游(跟踪)引用.
被无参数的 `git-pull`和其他命令使用. 更多信息, 请参阅 `git-config[1]` 中的 `branch.<name>.merge`.

### push 详细语法

```bash
git push
[--all | --mirror | --tags] [--follow-tags] [--atomic]
[-n | --dry-run] [--receive-pack=<git-receive-pack>]
[--repo=<repository>] [-f | --force] [-d | --delete] [--prune]
[-v | --verbose]
[-u | --set-upstream] [-o <string> | --push-option=<string>]
[--[no-]signed|--signed=(true|false|if-asked)]
[--force-with-lease[=<refname>[:<expect>]]]
[--no-verify] [<repository> [<refspec>… ]]
```

the **current branch** is pushed to the corresponding **upstream branch**, but as a safety measure,
the push is aborted if the upstream branch does not have **the same name** as the local one.

Specify what destination ref to update with what source object.
The format of a `<refspec>` parameter is an optional plus `+`,
followed by the source object `<src>`, followed by a colon `:`,
followed by the destination ref `<dst>`.

i.e. `git push origin <src>:<dst>`

The `<src>` is often the name of the branch you would want to push, but it can be any arbitrary "SHA-1 expression", such as `master~4` or `HEAD`
The `<dst>` tells which ref on the remote side is updated with this push.
Arbitrary expressions **cannot** be used here, an actual ref must be named.

If `git push [<repository>]`
without any `<refspec>` argument
is set to update some ref at the destination
with `<src>`
with `remote.<repository>.push` configuration variable,
`:<dst>` part can be omitted
—such a push will update a ref that `<src>` normally updates
without any `<refspec>` on the command line.

Otherwise, missing `:<dst>` means to update the same ref as the `<src>`.

`git push origin :`

Push "matching" branches to origin. See `<refspec>`  in the OPTIONS section above for a
description of "matching" branches.

`git push origin master:refs/heads/experimental`

Create **the branch experimental in the origin** repository by copying the **current master branch**. This form is only needed to create a new branch or tag in the remote repository when the local name and the remote name are different; otherwise, the ref name on its own will work.

`git push origin master`

Find a ref that matches master in the source repository (most likely, it would find refs/heads/master),
and update the same ref (e.g. refs/heads/master) in origin repository with it. If master did not exist remotely, it would be created.

`git push origin HEAD`

A handy way to push the current branch to the same name on the remote.

`git push mothership master:satellite/master dev:satellite/dev`

Use the source ref that matches master (e.g. refs/heads/master) to update the ref that matches satellite/master (most probably refs/remotes/satellite/master) in the mothership repository;
do the same for dev and satellite/dev.

See the section describing `<refspec>`... above for a discussion of the matching semantics.

### push flag

A single character indicating the status of the ref:

+ `(space)` : for a successfully pushed fast-forward;
+ `+` : for a successful forced update;
+ `-` : for a successfully deleted ref;
+ `*` : for a successfully pushed new ref;
+ `!` : for a ref that was rejected or failed to push; and
+ `=` : for a ref that was up to date and did not need pushing.

### 强制推送

```bash
git push -f origin master
```

`-f`
`--force`

Usually, the command refuses to update a remote ref that is not an ancestor of the local ref used to overwrite it.
Also, when `--force-with-lease` option is used, the command refuses to update a remote ref whose current value does not match what is expected.

This flag disables these checks, and can cause the remote repository to lose commits; use it with care.

### 拉取远程仓库

`git pull [<options>] [<repository> [<refspec>…]]`

`<repository>` should be the name of a remote repository as passed to `git-fetch`.
`<refspec>` can name an arbitrary remote ref (for example, the name of a tag) or even a collection of refs with corresponding remote-tracking branches (e.g., `refs/heads/*:refs/remotes/origin/*`),
**but usually it is the name of a branch in the remote repository.**

More precisely,`git pull` runs git fetch with the given parameters and calls git merge to merge the retrieved branch heads into **the current branch**.

Default values for `<repository>` and `<branch>` are read from
the "remote" and "merge" configuration
for the current branch
as set by `git-branch --track`

`--all` : Fetch all remotes.

### 远程仓库的移除与重命名

`git remote rename`

如果想要重命名引用的名字可以运行 `git remote rename` 去修改一个远程仓库的简写名.
例如, 想要将 `pb` 重命名为`paul`, 可以用`git remote rename`这样做：

```bash
$ git remote rename pb paul
"no output"
$ git remote
origin
paul
```

值得注意的是这同样也会修改你的远程分支名字.那些过去引用`pb/master` 的现在会引用`paul/master`,
如果因为一些原因想要移除一个远程仓库——你已经从服务器上搬走了或不再想使用某一个特定的镜像了, 又或者某一个贡献者不再贡献了——可以使用 `git remote rm` ：

```bash
$ git remote rm paul
"no output"
$ git remote
origin
```

### 清理无效远程追踪

如果在远程版本库上删除了某一分支, 该命令并不会删除本地的远程追踪分支, 这时候, 有另一个命令

```bash
git remote prune
```

该命令可以删除本地版本库上那些失效的远程追踪分支, 具体用法是, 假如你的远程版本库名是`origin`,则使用如下命令先查看哪些分支需要清理：

```bash
git remote prune origin --dry-run
```

然后执行

```bash
git remote prune origin
```

这样, 就完成了无效的远程追踪分支的清理工作.
需要注意, 这里远程追踪分支批位于

```bash
.git/refs/remote/origin
```

下的分支, 如果有本地分支作为下游存在的话, 还需要手动清理

## git 删除历史中的大文件

[仓库体积过大, 如何减小？ ](https://gitee.com/help/articles/4232#article-header2)
[Git清理删除历史提交文件](https://www.jianshu.com/p/7ace3767986a)

常见的`Git`清理方式有两种, 一种是使用`BFG`工具,
另外一种是使用`git filter-branch`手动处理.

注意：无论使用哪种方式, 都涉及破坏性操作, 使用时应严格谨慎.在开始操作之前, 请使用`--mirror`参数克隆备份你的`Git`仓库.

使用`BFG`的方式, 简单易操作, 使用方法可参考`BFG Repo-Cleaner` .
本文主要介绍的是使用 `git filter-branch` 的方式进行瘦身操作.

先进行垃圾回收, 并压缩一些文件

```bash
git gc --prune=now
```

`Git` 最初向磁盘中存储对象使用松散的格式, 后续会将多个对象打包为一个二进制的包文件(packfile), 以节省磁盘空间

+ `.pack`文件存储了对象的内容
+ `.idx`文件存储了包文件的偏移信息, 用于索引具体的对象

打包对象时, 查找命名和大小相近的文件, 保留文件不同版本之间的差异(最新一版保存完整内容, 访问频率最高)

### 查找大文件

使用`git rev-list --objects --all`显示所有`commit`及其所关联的所有对象：

```bash
git rev-list --objects --all | grep -E "$(git verify-pack -v .git/objects/pack/*.idx | sort -k 3 -n | tail -10 | awk '{print$1}')"
```

命令说明:

+ `git-rev-list` ; 列出可以从给定的`提交`中通过`父链接`到达的`提交`, 但不包括可以通过前缀`^`到达的提交.
默认情况下, 输出结果按反时间顺序排列.
  + `--objects`;  打印列出的提交所引用的任何对象的`ID`. `--objects foo ^bar`的意思是："如果我已经有了提交对象`bar`, 但没有`foo`, 请把我需要下载的所有对象`ID`发给我".
  + `--all` ; 假设 `refs/`中的所有 `refs`, 连同 `HEAD` 都被当作`<commit>`, 列在命令行中.

***

+ `git verify-pack -v *.idx`：查看压缩包内容.
  `git verify-pack` 读取给出的`idx`文件指定的, 用`git pack-objects`命令创建的`Git`打包档案, 并验证`idx`文件和相应的打包文件.
  + 当指定选项`-v` 时, 对没有`deltified`的对象, 使用的格式是:

    SHA-1, 类型, 体积, packfile中的体积, packfile中的偏移量

  + 对`deltified`的对象使用的格式为：

    SHA-1, 类型, 体积, packfile中的体积, packfile中的偏移量, depth, base-SHA-1

+ `sort -k, --key=KEYDEF` ;  通过`key`排序, `KEYDEF`给出 gives location and type
+ `sort  -n, --numeric-sort` ; compare according to string numerical value

### 删除指定的大文件

```bash
git filter-branch --force --index-filter "git rm -rf --cached --ignore-unmatch <dir/filename>" --prune-empty --tag-name-filter cat -- --all
```

SYNOPSIS :

```bash
git filter-branch [--setup <command>] [--subdirectory-filter <directory>]
... [--] [<rev-list options>...]
```

+ `git-filter-branch`; 重写分支, 通过一个`filter`来重写历史提交, 这个`filter`针对指定的所有分支(`rev-list`)运行.
+ `--index-filter`：过滤`Git`仓库的`index`, 该过滤命令作用于`git rm -rf --cached --ignore-unmatch  <dir/filename>`.
它不会`checkout`到`working directory`, 只修改`index`的文件, 速度快.
+ `--cached`会删除`index`中的文件
+ `--ignore-unmatch`：如果没匹配到文件, 不会报错, 会继续执行命令
+ 最后一个参数`<dir/filename>`是要被删除的文件的名字.
+ `--prune-empty`：指示`git filter-branch` 完全删除所有的空`commit`.
+ `-–tag-name-filter`：将每个`tag`指向重写后的`commit`.
+ `cat`命令会在收到`tag`时返回`tag`名称
+ `--`用来分割 `rev-list` 和 `filter-branch` 选项
+ `--all`参数告诉`Git`我们需要重写所有分支或引用.

注意：`git rm` 这一行命令使用双引号`"git rm -rf --cached --ignore-unmatch <dir/filename>"`

默认会警告：`git-filter-branch`有大量的问题, 会产生错误的历史记录重写.
在继续进行之前按`Ctrl-C`中止, 然后使用另一个替代的过滤工具, 如 `git filter-repo` (https://github.com/newren/git-filter-repo/)来代替.
参见 `filter-branch` 手册页了解更多细节；要消除这个警告. 设置 `FILTER_BRANCH_SQUELCH_WARNING=1`.

### 删除缓存

你的历史中将不再包含对那个文件的引用.
不过, 你的`引用日志`和你在 `.git/refs/original` 通过 `filter-branch` 选项添加的新引用中还存有对这个文件的引用,
所以你必须移除它们然后重新打包数据库. 在重新打包前需要移除任何包含指向那些旧提交的指针的文件.

移除本地仓库中指向旧`commit`的剩余`refs`:
`git for-each-ref` 会打印仓库中匹配`refs/original`的所有`refs`, 并使用`delete`作为前缀,
此命令通过管道传送到 `git update-ref` 命令, 该命令会移除所有指向旧`commit`的引用.

```bash
rm -Rf .git/refs/original ;rm -Rf .git/logs/ ; git gc
# 或者
git for-each-ref --format='delete %(refname)' refs/original | git update-ref --stdin
```

以下命令会使`reflog`到期, 因为它依然包含着对旧`commit`的引用. 使用 `--expire=now` 参数, 确保它在目前为止到期了. 如果没有该参数, 只会移除超过`90`天的`reflog`.

```bash
git reflog expire --expire=now --all
```

现在本地仓库依然包含着所有旧`commit`的对象, 但已经没有引用指向它们了, 这些对象需要被删除掉.
此时可以使用 `git gc` 命令, `Git`的垃圾回收器会删除这些没有引用指向的对象.
`git-gc`使用 `--prune` 参数来清理特定时期的对象, 默认情况下为`2`周, 指定`now`将删除所有这些对象而没有时期限制.

```bash
git gc --prune=now
```

让我们看看你省了多少空间.

```bash
git count-objects -v
```

可以从 `size` 的值看出, 这个大文件还在你的松散对象中, 并没有消失；但是它不会在推送或接下来的克隆中出现, 这才是最重要的.
如果真的想要删除它, 可以通过有 `--expire` 选项的 `git prune` 命令来完全地移除那个对象：

```bash
git prune --expire now
git count-objects -v
```

用`du -sh`查看文件夹的体积

```bash
du -sh .git
```

### 提交重写的历史到远程

如果确认所做的删除大文件操作没有问题, 就可以提交到远程仓库了, 一旦提交, 再也没有办法恢复到原来的状态.
一定要小心谨慎！一定要小心谨慎！一定要小心谨慎！

先进行备份工作, 以免出现问题：

```bash
cd ~/Desktop/
mkdir gitthin_mirror && cd gitthin_mirror
git push --mirror git@github.com:gravitional/note.git
```

`git@github.com:gravitional/note.git`是你的项目在`gitee`上的地址.

再回到刚才做的已经瘦身的`Git`仓库

```bash
$ cd ~/Desktop/gitthin/gitthin
```

把已瘦身的仓库同步到远程仓库, 使用`--mirror`参数：

```bash
git push --mirror git@github.com:gravitional/note.git
```

为了确保都已同步, 再执行以下命令：

```bash
git push --all --force
git push --tags --force
```

+ `git push --mirror`

不用一个一个`ref`的指定, 直接推送所有`refs/`下的所有 `ref` 到远程仓库, 也就是`镜像`.
包括但不限于 `refs/heads/`, `refs/remotes/` 和 `refs/tags/`.
新创建的本地 `refs` 将被推送到远程端, 本地更新的 `refs` 将被强制更新到远程端, 而删除的 `refs` 将从远程端删除.
如果配置选项 `remote.<remote>.mirror` 被设置, 这将是默认行为.

### 更新其他的clone

在过滤存储库, 并重写提交历史后, 将更改强制推送到远程服务器之后.
现在要更新该存储库的每一份`clone`, 仅靠常用的`pull`是无法做到这一点的.

从远程服务器获取存储库, 再使用 `git reset` 将`HEAD`移动到 `origin/master`.

```bash
git fetch origin # git 会自动检测到远程服务器进行了 force update
git reset --hard origin/master
```

和上面的一样, 需要删除旧提交, 清理本地仓库

```bash
git for-each-ref --format='delete %(refname)' refs/origin | git update-ref --stdin
git reflog expire --expire=now --all
git gc --prune=now
```

### 总结

查看存储库中的大文件

```bash
git rev-list --objects --all | grep -E $(git verify-pack -v .git/objects/pack/*.idx | sort -k 3 -n | tail -10 | awk '{print$1}' | sed ':a;N;$!ba;s/\n/|/g')
```

或

```bash
git rev-list --objects --all | grep "$(git verify-pack -v .git/objects/pack/*.idx | sort -k 3 -n | tail -15 | awk '{print$1}')"
```

改写历史, 去除大文件

注意：下方命令中的 `path/to/large/files` 是大文件所在的路径, 千万不要弄错！

```bash
git filter-branch --tree-filter 'rm -f path/to/large/files' --tag-name-filter cat -- --all
git push origin --tags --force
git push origin --all --force
```

如果在 `git filter-branch` 操作过程中遇到如下提示, 需要在 `git filter-branch` 后面加上参数 `-f`

```bash
Cannot create a new backup.
A previous backup already exists in refs/original/
Force overwriting the backup with -f
```

并告知所有组员, `push`代码前需要 `pull rebase`, 而不是 `merge`, 否则会从该组员的本地仓库再次引入到远程库中.

## 解决冲突

解决冲突就是把`Git`合并失败的文件手动编辑为我们希望的内容, 再提交.用 `git log --graph` 命令可以看到分支合并图.

冲突的位置`git`会提醒并作标记, 需要手动修改, 然后提交.
注意, `git` 只会标出冲突的位置, 并不能帮你解决冲突, 也不能判断你是否正确解决了冲突,
所以下一次的提交, 就会被视为冲突已经解决的提交--无论你的修改是否正确.当然由于可以恢复, 这也算不了什么问题.

也可以

```bash
git reset --hard commit
```

还原本地工作, 然后`git pull`

### 合并前检查

在应用外部更改之前, 您应该使自己的工作空间保持良好状态并在本地提交, 这样在发生冲突时不会毁坏文件.
另请参见`git-stash`. 当本地未提交的更改与`git pull / git merge`可能需要更新的文件重叠时, `git pull`和`git merge`将停止而不进行任何操作.
为了避免在在合并提交时, 记录下不相关的更改, 如果相对于`HEAD`在`index`记录了任何更改, 则`git pull`和`git merge`也将中止. (取决于所使用的合并策略, 但通常索引必须与`HEAD`匹配.)
如果所有已命名的提交都已经是`HEAD`的祖先, 则`git merge`会提前退出, 并显示消息`已经更新`.

***
git-stash

当您想记录工作目录和索引的当前状态, 但又想回到干净的工作目录时, 请使用`git stash`.该命令将您的本地修改保存下来, 并还原工作目录以匹配`HEAD`commit.

可以使用`git stash list`列出存储的修改, 可以使用`git stash show`进行检查, 还可以使用`git stash apply`恢复(可能应用到其他commit).
不带任何参数调用`git stash`等效于`git stash push`.默认情况下, 一个 stash 显示为`WIP on branchname ...`, 但是在创建存储项时, 可以在命令行上提供更具描述性的消息.

您创建的最新`stash`存储在`refs/stash`中.在此引用的`reflog`中可以找到较旧的`stashes`,
并且可以使用通常的`reflog`语法进行调用.(例如`stash@{0}`是最近创建的stash, `stash@{1}`是之前创建的stash,`stash@{2.hours.ago}`也可以).
还可以通过仅指定`stash index`来引用存储(例如, 整数`n`等于`stash@{n}`).

### 快进式合并

当前分支`head`通常是要合并的提交的祖先.这是最常见的情况, 尤其是使用`git pull`时：您正在跟踪上游 repository, 尚未提交任何本地更改, 现在想更新到较新的上游修订版.
在这种情况下, 不需要新的 commit 来存储合并的历史记录； 相反, 将`HEAD`(以及`index`)更新到新的commit即可, 而不创建额外的`merge commit`.

这个行为可以通过`--no-ff`选项来抑制.

### 真正的合并

除了`fast-forward merge`之外, 要合并的分支必须有一个共同的父节点.在进行真正的合并操作时, 将会提交一个合并的版本,
协调所有要合并的分支中的更改, 并且将`HEAD`, `index`, and `working tree` 更新为该版本.
只要修改不重叠, 工作树中可以有修改, 合并操作将保留这些修改.

如果不清楚如何协调修改, 则会发生以下情况：

+ `HEAD`指针保持不变.
+ `MERGE_HEAD` 引用被设置为指向要合并进来的另一个分支头.
+ 对于不矛盾的合并文件, 将会在`index`中和工作树中都更新.
+ 对于冲突的文件/路径, `index`将记录三个版本：`stage 1`存储共同祖先的版本, `stage 2`存储` HEAD`的修改,
`stage 3`存储`MERGE_HEAD`的修改(可以使用`git ls-files -u`检查这些`stage`).工作区中包含合并的结果:即使用熟悉的冲突标记` <<< === >>>`进行三方合并的结果.
+ 不进行其他更改.特别是, 在开始合并之前进行的本地修改将保持不变, 指向它们的`index`条目也保持不变, 即匹配`HEAD`.

如果合并产生了复杂的冲突, 则可以使用git`merge --abort`恢复到合并之前.

### 合并冲突的表示

在合并期间, 将更新工作区以反映合并结果.
在对共同祖先版本所做的更改中, 不重叠的更改被原封不动地合并到最终结果中(即你更改了文件的某区域, 而其他人没有修改这个地方, 反之亦然).
但是, 当双方对同一区域进行更改时, `Git`不能随意选择一边, 会将两边的修改都列出来, 让用户选择.

默认情况下, Git使用与`RCS`套件中`合并`程序相同的样式来呈现这种冲突, 如下所示：

```git
这些行相对于共同祖先版本不变, 或者只有一方进行修改.下面的<<<===>>>将形成两个区域.
<<<<<<< yours:sample.txt
Conflict resolution is hard;
let's go shopping.
=======
Git makes conflict resolution easy.
>>>>>>> theirs:sample.txt
这是另一行解决好的或者未修改的行.
```

发生冲突的更改的区域用`<<<<<<<`, `=======`, and `>>>>>>>`标记.在`========`之前的部分通常是你的修改, 而在其后的部分通常是他人的修改.

默认格式不显示原文件在冲突区域中的内容. 您无法判断有多少行被删除或替换.
唯一可以确定的是, 你想表达这事儿很难, 你更喜欢去购物, 而另一方则想声称这很容易.
通过将`merge.conflictStyle`配置变量设置为`diff3`, 可以使用其他样式.

### 合并策略

合并机制(`git merge`和`git pull`命令)允许使用`-s`选项选择后端具体的合并策略.
一些策略还具有自己的二级选项, 可以通过为`git merge`和/或`git pull`提供`-X <option>`参数来传递它们.

***
`resolve`:使用三方合并算法, 只能处理两个`head`的情况(即当前分支和要pull的分支). 它试图仔细检测纠缠的部分, 并且通常被认为是安全且快速的.

***
`recursive`: 使用三方合并算法, 只能处理两个`head`的情况. 当有一个以上的共同祖先可用于三方合并时, 它将创建一个共同祖先的合并树, 并将其用作三方合并的参考树.
据纪录, 在`Linux 2.6`内核开发历史中, 这样做能减少合并冲突, 以及合并错误. 此外, 还可以检测和处理涉及重命名的合并, 但是当前还无法使用检测到的副本.
这是`pulling`或`merging`分支时的默认合并策略.`recursive`策略可以采用以下选项：

+ `ours`: 将冲突通过采用`ours`的版本解决.与另一棵树不冲突的变化也会反映在合并结果中.对于二进制文件, 全部内容都将来自我们这边.
`git merge -s recursive -Xours`与`git merge -s ours`合并策略并不相同, `merge -s ours`根本不看另一棵树的内容.它丢弃另一棵树所做的所有操作, 只使用我们的提交记录.
+ `theirs`: 与`ours`相反.不过, 并没有`git merge -s theirs`合并策略, 所以不会混淆.
+ `patience`: 使用此选项, `merge-recursive`会花费一些额外的时间来避免有时由于不重要的匹配行(例如来自不同函数的花括号)而导致的合并错误.
当要合并的分支出现巨大分歧时, 请使用此选项.另请参见`git-diff[1] --patience`.
+ `diff-algorithm=[patience|minimal|histogram|myers]`: 告诉`merge-recursive`使用不同的`diff`算法, 这可以帮助避免由于不重要的匹配行(例如, 来自不同函数的花括号)而导致的合并错误.另请参见`git-diff[1] --diff-algorithm`.
+ `ignore-space-change`
+ `ignore-all-space`
+ `ignore-space-at-eol`
+ `ignore-cr-at-eol`: 在进行三方合并时, 将具有指示的空白类型更改的行视为未更改.空格更改与行的其他更改混合在一起将不被忽略.
另请参见`git-diff[1] -b, -w, --ignore-space-at-eol`, and `--ignore-cr-at-eol`.
如果`their`版本的改动都是空白内容, 则使用`our`版本；如果`our`版本引入了空白更改, 但`their`版本包含实质性更改, 则使用`their`版本；否则, 合并将以通常的方式进行.
+ `renormalize`:进行三方合并时, 这将对文件的所有三个`stage`进行虚拟`check-out` and `check-in`.当合并具有不同`clean filters`或`end-of-line normalization`规则的分支时, 应使用此选项.有关详细信息, 请参见`gitattributes[5]`中的`合并具有不同checkin/checkout属性的分支`.
+ `no-renormalize`: 禁用`renormalize`选项.这将覆盖`merge.renormalize`配置变量.
+ `no-renames`: 关闭重命名检测.这将覆盖`merge.renames`配置变量.另请参见`git-diff [1] --no-renames`.
+ `  find-renames[=<n>]`:打开重命名检测, 可以选择设置相似性阈值.这是默认值.这将覆盖merge.renames配置变量.另请参见`git-diff[1] --find-renames`.
+ `rename-threshold=<n>`:`find-renames=<n>`的已弃用同义词.
+ `subtree[=<path>]`:此选项是`subtree`策略的一种更高级形式, `subtree`策略会猜测如何平移目录, 才能让两棵树在合并时相互匹配.
此选项可以将特定的路径添加到前缀(或从开头删除)以使两棵树的形状匹配.

***
`octopus`:这样可以解决具有两个`heads`的情况, 但是拒绝执行需要手动解决的复杂合并.它主要用于将`topic`分支头捆绑在一起.当`pulling`或`merging`多个分支时, 这是默认的合并策略.

***
`ours`:该策略可以处理任意数量的`head`, 但是合并的结果树始终是当前分支`head`的树, 忽略所有其他分支的所有更改.它旨在取代`side branches`的旧开发历史.
请注意, 它与`recursive`合并策略的`-Xours`选项不同.

***
`subtree`:    这是一种修改的`递归`策略.合并树`A`和`B`时, 如果`B`对应于`A`的子树, 则首先调整`B`以匹配`A`的树结构, 而不是读取相同级别的树.对多个共同祖先树也进行此调整.

***
对于使用三方合并的策略(包括默认的`recursive`), 如果在两个分支上都进行了更改, 但随后又在其中一个分支上进行了还原, 则该更改将出现在合并结果中.
有些人觉得这种行为令人困惑.发生这种情况的原因是, 执行合并时仅考虑`heads`和`merge base`, 而不考虑单独的提交.因此, 合并算法将还原后的更改视为完全没有更改, 转而使用更改过的版本.

### 如何解决冲突

看到冲突后, 您可以做两件事：

+ 决定不合并. 您唯一需要进行的清理工作就是将`index`文件重置为`HEAD` 提交以撤消`2`, 并清理`2`和`3`所做的工作树更改.运行`git merge --abort`即可.
+ 解决冲突. `Git`将在工作树中标记冲突, 将文件修改好之后将它们添加到`index`中.使用`git commit`或`git merge --continue`完成封印.
后一个命令在调用`git commit`之前将检查是否有一个(中断的)合并正在进行中.

您可以使用多种工具来解决冲突：

+ 使用合并工具. `git mergetool`启动一个图形化的合并工具, 它将帮助您完成合并.
+ 查看差异. `git diff`将显示三方差异, 并突出显示`HEAD`和`MERGE_HEAD`版本的变化.
+ 查看每个分支的修改. `git log --merge -p <path>`将首先显示`HEAD`版本的差异, 然后显示`MERGE_HEAD`版本.
+ 看一下原件. `git show :1:filename`显示共同祖先, `git show :2:filename`显示`HEAD`版本, `git show :3:filename`显示`MERGE_HEAD`版本.

## 分支管理策略

`Git`分支十分强大, 在团队开发中应该充分应用.
合并 **临时分支**到 **feature分支** 后(并删除 **临时分支** ),
如果加上了 `--no-ff` 参数就可以用普通模式合并, 合并后的 **log** 有分支, 能看出来曾经做过合并,
而默认的 `fast forward` 合并就看不出来曾经做过合并.

## git tag

`git tag`  列出已有的标签

`git tag -l 'v1.8.5*'` 查找  `'v1.8.5*'`

### 创建附注标签

即完整标签

```bash
git tag -a v1.4 -m "my version 1.4"
```

`-m` 选项指定了一条将会存储在标签中的信息

```bash
git show v1.4
```

`git show`命令可以看到标签信息与对应的提交信息

### 轻量标签

```bash
git tag v1.4-lw
```

轻量标签本质上是提交`校验和`, 将其存储到一个文件中——没有保存任何其他信息.
创建轻量标签, 不需要使用 `-a, -s` 或 `-m` 选项, 只需要提供标签名字

### 后期打标签

`git tag -a v1.2 9fceb02`

在命令的末尾指定提交的校验和(或部分校验和)

### 推送标签

`git push origin v1.5`
`git push origin [tagname]`
`git push origin --tags`

默认情况下, `git push`命令并不会传送标签到远程仓库服务器上.
在创建完标签后你必须显式地推送标签到共享服务器上,这个过程就像共享`远程分支`一样

如果想要一次性推送很多标签, 也可以使用带有`--tags`选项的`git push`命令. 这将会把所有不在远程仓库服务器上的标签全部传送到那里.

```bash
$ git push origin --tags
Counting objects: 1, done.
Writing objects: 100% (1/1), 160 bytes | 0 bytes/s, done.
Total 1 (delta 0), reused 0 (delta 0)
To git@github.com:schacon/simplegit.git
 * [new tag]         v1.4 -> v1.4
 * [new tag]         v1.4-lw -> v1.4-lw
```

现在, 当其他人从仓库中克隆或拉取, 他们也能得到你的那些标签.

### 删除标签

```bash
git tag -d <tagname>
```

for example `$ git tag -d v1.4-lw`
Deleted tag 'v1.4-lw' (was e7d5add)

```bash
git push <remote> :refs/tags/<tagname>
```

你必须使用 `git push <remote> :refs/tags/<tagname>` 来更新你的远程仓库：
`$ git push origin :refs/tags/v1.4-lw`
`To /git@github.com:schacon/simplegit.git`
`- [deleted]         v1.4-lw`

### checkout 到某个 标签

如果你想查看某个标签所指向的文件版本, 可以使用`git checkout`命令, 虽然说这会使你的仓库处于“分离头指针(`detacthed HEAD`)”状态——这个状态有些不好的副作用：

```bash
$ git checkout 2.0.0
Note: checking out '2.0.0'.
```

比如说你正在修复旧版本的错误——这通常需要创建一个新分支：

`$ git checkout -b version2 v2.0.0`
`Switched to a new branch 'version2'`

## 储藏 stash

+ `stash` ：储存工作现场
+ `stash list`: 查看工作现场列表
+ `git stash apply`: 恢复, 但是恢复后, `stash`内容并不删除
+ `stash drop`: 删除
+ `stash pop`: 恢复的同时把`stash`内容也删了

When no `<stash>` is given, stash@{0} is assumed, otherwise `<stash>` must be a reference of the form `stash@{<revision>}`.

当工作只进行到一半, 还没法提交, 预计完成还需1天时间.但是, 必须在两个小时内修复该bug, 怎么办？
幸好, `Git`还提供了一个`stash`功能, 可以把g当前工作现场“储藏”起来, 等以后恢复现场后继续工作：

```bash
$ git stash
Saved working directory and index state WIP on dev: f52c633 add merge
```

现在, 用`git status`查看工作区, 就是干净的(除非有没有被`Git`管理的文件), 因此可以放心地创建分支来修复bug.

太棒了, 原计划两个小时的bug修复只花了5分钟！现在, 是时候接着回到`dev`分支干活了！

```bash
$ git checkout dev
Switched to branch 'dev'

$ git status
On branch dev
nothing to commit, working tree clean
```

工作区是干净的, 刚才的工作现场存到哪去了？

用`git stash list`命令看看：

```bash
$ git stash list
stash@{0}: WIP on dev: f52c633 add merge
```

工作现场还在, `Git`把`stash`内容存在某个地方了, 但是需要恢复一下, 有两个办法：

一是用`git stash apply`恢复, 但是恢复后, `stash`内容并不删除, 你需要用`git stash drop`来删除；

另一种方式是用`git stash pop`, 恢复的同时把`stash`内容也删了

Use git stash when you want to record the current state of the working directory and the index, but want to go
       back to a clean working directory. The command saves your local modifications away and reverts the working
       directory to match the HEAD commit.

       The modifications stashed away by this command can be listed with git stash list, inspected with git stash
       show, and restored (potentially on top of a different commit) with git stash apply. Calling git stash without
       any arguments is equivalent to git stash push. A stash is by default listed as "WIP on branchname ...", but
       you can give a more descriptive message on the command line when you create one.

       The latest stash you created is stored in refs/stash; older stashes are found in the reflog of this reference
       and can be named using the usual reflog syntax (e.g. stash@{0} is the most recently created stash, stash@{1}
       is the one before it, stash@{2.hours.ago} is also possible). Stashes may also be referenced by specifying just
       the stash index (e.g. the integer n is equivalent to stash@{n}).

## rebase 变基

在`Git`中整合来自不同分支的修改主要有两种方法：`merge`以及`rebase`

reference: [scm tutorial](https://git-scm.com/book/zh/v2/Git-%E5%88%86%E6%94%AF-%E5%8F%98%E5%9F%BA#rrbdiag_g)

### 变基的风险

呃, 奇妙的变基也并非完美无缺, 要用它得遵守一条准则：不要对在你的仓库外有副本的分支执行变基.

如果你遵循这条金科玉律, 就不会出差错. 否则, 人民群众会仇恨你, 你的朋友和家人也会嘲笑你, 唾弃你.

变基操作的实质是丢弃一些现有的提交, 然后相应地新建一些内容一样但实际上不同的提交. 如果你已经将提交推送至某个仓库, 而其他人也已经从该仓库拉取提交并进行了后续工作, 此时, 如果你用 `git rebase`命令重新整理了提交并再次推送, 你的同伴因此将不得不再次将他们手头的工作与你的提交进行整合, 如果接下来你还要拉取并整合他们修改过的提交, 事情就会变得一团糟.

我自己理解的变基就是：

1. 先寻找一个命令参数中提到的 **提交** 使用的共同 **父节点**,
2. 计算所有在 **父节点** 之后各次 **提交** 进行的所有更改, 然后按照 `git rebase` 命令的安排,
    把它们依次播放/运用/apply,
3. 然后重新生成各次提交, 新生成的提交就按照我们希望的那样排列了

### scm-book 例子 1

在上面这个例子中, 运行：

```bash
$ git checkout experiment
"no output"
$ git rebase master
First, rewinding head to replay your work on top of it...
Applying: added staged command
```

它的原理是首先找到这两个分支(即当前分支 `experiment`, 变基操作的目标基底分支 `master`)的最近共同祖先 `C2`, 然后对比当前分支相对于该祖先的历次提交, 提取相应的修改并存为临时文件, 然后将**当前分支**指向目标基底 `C3`, 最后以此将之前另存为临时文件的修改依序应用

### scm-book 例子 2

假设你希望将 `client` 中的修改合并到主分支并发布, 但暂时并不想合并 `server` 中的修改, 因为它们还需要经过更全面的测试. 这时, 你就可以使用 `git rebase` 命令的 `--onto` 选项, 选中在 `client` 分支里但不在 `server` 分支里的修改(即 `C8` 和 `C9`), 将它们在 `master` 分支上重放：

```bash
git rebase --onto master server client
```

以上命令的意思是：
“取出 `client` 分支,
找出处于 `client` 分支和 `server`  分支的共同祖先之后的修改,
然后把它们在 `master` 分支上重放一遍”.
这理解起来有一点复杂, 不过效果非常酷.

即

```bash
git rebase --onto master[被施加重放的分支] server[父节点/修改起始点 参考分支] client[提取重放内容的分支]
```

## 自定义 git

### gitignore

忽略某些文件时, 需要编写`.gitignore`；
`.gitignore`文件本身要放到版本库里, 并且可以对`.gitignore`做版本管理！

[git设置忽略文件和目录](https://www.cnblogs.com/wtil/p/11676092.html)

1. 创建.gitignore
2. 修改文件, 添加忽略正则

***

+ `.idea` //忽略`.idea`文件夹及文件夹下文件
+ `*.iml` //忽略以`.iml`结尾的文件

#### 例子

```git
# 忽略*.o和*.a文件
*.[oa]

# 忽略*.b和*.B文件, my.b除外
*.[bB]
!my.b

# 忽略dbg文件和dbg目录
dbg

# 只忽略dbg目录, 不忽略dbg文件
dbg/

# 只忽略dbg文件, 不忽略dbg目录
dbg
!dbg/

# 只忽略当前目录下的dbg文件和目录, 子目录的dbg不在忽略范围内
/dbg
```

以`#`开始的行, 被视为注释.

+ `?`：代表任意的一个字符
+ `*`：代表任意数目的字符
+ `{!ab}`：必须不是此类型
+ `{ab,bb,cx}`：代表`ab`,`bb`,`cx`中任一类型即可
+ `[abc]`：代表`a`,`b`,`c`中任一字符即可
+ `[ ^abc]`：代表必须不是`a`,`b`,`c`中任一字符

***
添加忽略之后, 已经提交到版本库中的文件是无法忽略的.
只能clone到本地, 删除后, 再进行忽略.

`.gitignore`只能忽略那些原来没有被track的文件,
如果某些文件已经被纳入了版本管理中, 则修改`.gitignore`是无效的.

正确的做法是在每个clone下来的仓库中手动设置不要检查特定文件的更改情况.
`git update-index --assume-unchanged PATH` 在PATH处输入要忽略的文件.
另外 git 还提供了另一种 `exclude` 的方式来做同样的事情,
不同的是 `.gitignore` 这个文件本身会提交到版本库中去, 用来保存的是公共的需要排除的文件.
而 `.git/info/exclude` 这里设置的则是你自己本地需要排除的文件, 它不会影响到其他人, 也不会提交到版本库中去

Python我一般添加这个三个

```git
.idea
*.iml
__pycache__
```

## git 术语

### index

[whats-the-deal-with-the-git-index](https://gitguys.com/topics/whats-the-deal-with-the-git-index/)

The git “index” is where you place files you want committed to the git repository.

Before you “commit” (checkin) files to the git repository, you need to first place the files in the git “index”.

The git index goes by many names. But they all refer to the same thing. Some of the names you may have heard:

+ Index
+ Cache
+ Directory cache
+ Current directory cache
+ Staging area
+ Staged files

The Index Isn’t The Working Directory.

## Git 对象

Git Objects

Git is a content-addressable filesystem. Great. What does that mean?
It means that at the core of Git is a simple `key-value` data store.
What this means is that you can insert any kind of content into a Git repository, for which Git will hand you back a unique key you can use later to retrieve that content.

As a demonstration, let’s look at the plumbing command((建筑物的)管路系统,自来水管道)
`git hash-object`, which takes some data, stores it in your `.git/objects` directory (the object database), and gives you back the unique key that now refers to that data object.

First, you initialize a new Git repository and verify that there is (predictably) nothing in the objects directory:

```git
$ git init test
Initialized empty Git repository in /tmp/test/.git/
$ cd test
null
$ find .git/objects
.git/objects
.git/objects/info
.git/objects/pack
$ find .git/objects -type f
null
```

Git has initialized the objects directory and created pack and info subdirectories in it, but there are no regular files. Now, let’s use `git hash-object` to create a new data object and manually store it in your new Git database:

```git
$ echo 'test content' | git hash-object -w --stdin
d670460b4b4aece5915caf5c68d12f560a9fe3e4
```

In its simplest form, `git hash-object` would take the content you handed to it and merely return the unique key that would be used to store it in your Git database.
The `-w` option then tells the command to not simply return the key, but to write that object to the database.
Finally, the --stdin option tells `git hash-object` to get the content to be processed from stdin; otherwise, the command would expect a filename argument at the end of the command containing the content to be used.

The output from the above command is a `40-character checksum hash`. This is the SHA-1 hash — a checksum of the content you’re storing plus a header, which you’ll learn about in a bit. Now you can see how Git has stored your data:

```git
$ find .git/objects -type f
.git/objects/d6/70460b4b4aece5915caf5c68d12f560a9fe3e4
```

If you again examine your objects directory, you can see that it now contains a file for that new content.
This is how Git stores the content initially — as a single file per piece of content, named with the SHA-1 checksum of the content and its header. The subdirectory is named with the first 2 characters of the SHA-1, and the filename is the remaining 38 characters.

Once you have content in your object database, you can examine that content with the `git cat-file` command. This command is sort of a Swiss army knife for inspecting Git objects. Passing `-p` to `cat-file` instructs the command to first figure out the type of content, then display it appropriately:

```git
$ git cat-file -p d670460b4b4aece5915caf5c68d12f560a9fe3e4
test content
```

### Tree 对象

`tree`, 解决了git中储存文件名的问题, 并且允许你把一组文件存在一起.

git 存储文件的方式类似于`Unix`系统, 但是有点简化.所有的内容被存储为`tree`and`blob`, `tree`对应UNIX 文件夹,
`blob`对应`inodes`or `file contents`

一个`tree`包含一个或多个条目, 每一个条目是一个`SHA-1 hash`of a blob or `subtree`with its associated mode, type, and filename. 比如, 项目中最新的一个tree看起来大概像：

```git
$ git cat-file -p master^{tree}
100644 blob a906cb2a4a904a152e80877d4088654daad0c859      README
100644 blob 8f94139338f9404f26296befa88755fc2598c289      Rakefile
040000 tree 99f1a6d12cb4b6f19c8655fca46c3ecf317074e0      lib
```

The `master^{tree}` syntax 指定了master分支的最新提交.
注意the `lib` subdirectory isn't a `blob` but a `pointer` to another `tree`:

```git
$ git cat-file -p 99f1a6d12cb4b6f19c8655fca46c3ecf317074e0
100644 blob 47c6340d6459e05787f644c2447d2595f5d3a54b      simplegit.rb
```

注意, 取决于你用的shell, 当你使用`master^{tree}` syntax时, 可能会遇到一些错误

In CMD on Windows, the `^` character is used for escaping, so you have to double it to avoid this: `git cat-file -p master^^{tree}`.
When using PowerShell, parameters using `{}` characters have to be quoted to avoid the parameter being parsed incorrectly: `git cat-file -p 'master^{tree}'`.

If you’re using `ZSH`, the `^` character is used for globbing, so you have to enclose the whole expression in quotes: `git cat-file -p "master^{tree}"`.

*** 你可以相当容易的创建你自己的tree

git 通常用 `staging` area ( `index`)中的state创建`tree`, 然后写入一系列tree对象.所以首先来`staging`一些文件

先创建一个`index`with a single entry--the first version of your `test.txt file`, 使用管道命令`git update-index`, 使用这个命令, 手动添加earlier version of the `test.txt`to a new staging area.

你需要用`--add` option, 因为这个文件在your staging area中还不存在,
还有`--cacheinfo`, because the file you're adding isn't in your directory but is in your database.

Then, you specify the `mode`, `SHA-1`, and `filename`:

```git
git update-index --add --cacheinfo 100644 \
83baae61804e65cc73a7201a7252750c76066a30 test.txt
```

在本例中, 你指定的模式是`100644`, 代表它是正常文件.其他选项有`100755`, 代表可执行文件；
还有`120000`, 代表符号链接.

这些模式来自于 普通Unix mode, 但是比较简化--这是对files(blobs)合法的三个mode.

现在, 用 `git write-tree` 把`stage`写入到一个`tree` object 中.
不用加上`-w` option--这个命令会自动创建一个tree from the state of the index , 如果它还不存在.

```git
$ git write-tree
d8329fc1cc938780ffdd9f94e0d364e0ea74f579
$ git cat-file -p d8329fc1cc938780ffdd9f94e0d364e0ea74f579
100644 blob 83baae61804e65cc73a7201a7252750c76066a30      test.txt
```

你可以验证, 它的确是一个tree object, using `git cat-file -t` command you saw earlier:

```git
$ git cat-file -t d8329fc1cc938780ffdd9f94e0d364e0ea74f579
tree
```

你可以创建一个新tree, 包含 `test.txt`的新版本和一个新文件：

```git
$ git read-tree --prefix=bak d8329fc1cc938780ffdd9f94e0d364e0ea74f579
null
$ git write-tree
3c4e9cd789d88d8d89c1073707c3585e41b0e614
$ git cat-file -p 3c4e9cd789d88d8d89c1073707c3585e41b0e614
040000 tree d8329fc1cc938780ffdd9f94e0d364e0ea74f579      bak
100644 blob fa49b077972391ad58037050f2a75f74e3671e92      new.txt
100644 blob 1f7a7a472abf3dd9643fd615f6da379c4acb3e3a      test.txt
```

### Commit 对象

要创建一个提交对象, 你需要调用`commit-tree`, 并指定一棵树的`SHA-1`, 把想要提交的对象直接放在树的前面, 如果有的话. 从第一个树开始:

```git
$ echo 'first commit' | git commit-tree d8329f
fdf4fc3344e67ab068f836878b6c4951e3b15f3d
```

由于创建时间和作者数据不同, 你会得到不同的哈希值.
在本章的后面, 用你自己的`checksums`替换提交和标签的哈希值. 现在你可以用 `git cat-file` 来查看你的新提交对象了.

```git
git cat-file -p fdf4fc3
tree d8329fc1cc938780ffdd9f94e0d364e0ea74f579
author Scott Chacon <schacon@gmail.com> 1243040974 -0700
committer Scott Chacon <schacon@gmail.com> 1243040974 -0700

first commit
```

提交对象的格式很简单:
它指定了当时项目快照的顶层树.
父级提交(如果有的话)；作者/提交人信息(使用您的用户名和电子邮件配置以及一个时间戳).
一个空行, 然后是提交信息.

接下来, 你要提交另外两个对象, 每个对象都引用在它之前的提交.

```bash
echo 'second commit' | git commit-tree 0155eb -p fdf4fc3
cac0cab538b970a37ea1e769cbbde608743bc96d
echo 'third commit'  | git commit-tree 3c4e9c -p cac0cab
1a410efbd13591db07496601ebc7a059dd55cfe9
```

这三个提交对象分别指向你创建的三个快照树中的一个.
奇怪的是, 你现在有了一个真正的 `Git` 历史, 如果你使用上次提交的 `SHA-1` , 就可以用 `git log` 命令查看：

```bash
git log --stat 1a410e
commit 1a410efbd13591db07496601ebc7a059dd55cfe9
Author: Scott Chacon <schacon@gmail.com>
Date:   Fri May 22 18:15:24 2009 -0700
...
```

真了不起. 你刚刚完成了建立 `Git` 历史的底层操作, 而没有使用任何前端命令.
这就是`Git`在运行`git add`和 `git commit` 命令时所做的事情--它为发生变化的文件存储`blob`, 更新索引, 写出树, 写入提交对象, 引用顶层树, 以及最邻近的前一个提交.

这三个主要的Git对象--`blob`, `树`和`提交`--最初是作为单独的文件存储在你的`.git/objects`目录下. 下面是示例目录中的所有对象, 并注释了它们的存储内容.

```git
$ find .git/objects -type f
.git/objects/01/55eb4229851634a0f03eb265b69f5a2d56f341 # tree 2
.git/objects/1a/410efbd13591db07496601ebc7a059dd55cfe9 # commit 3
...
```

### Tree-ish

`Tree-ish` 是一个术语, 指的是一个标识符, 最终指引到一个(子)目录树(Git 把目录成为"tree" and "tree objects")

如果想指定文件夹`foo`, 正确的方法是使用`git`的"tree-ish"语法:

```bash
HEAD:README, :README, master:./README
```

`:`(冒号)前面是tree-ish object, 冒号后面是具体的路径, 也就是说`master:foo`是正确的语法, 而不能写成`master/foo`.
注：`master`是分支的名字, `foo`是具体的路径.

### refspec

应该是`Reference Specification`的缩写, 字面意思就是具体的引用.
它其实是一种格式, `git`通过这种格式的判断来获取不同引用下的数据.

你可以具体参考：[Reference Specification](http://git-scm.com/book/zh/ch9-5.html)

`Refspec` 的格式是一个可选的 `+` 号, 接着是 `<src>:<dst>` 的格式, 这里 `<src>` 是远端上的引用格式, `<dst>` 是将要记录在本地的引用格式.

可选的 `+` 号告诉 `Git` 在即使不能"Fast Forward"的情况下, 也去强制更新它.
缺省情况下 `refspec` 会被 `git remote add` 命令所自动生成,
`Git` 会获取远端上 `refs/heads/` 下面的所有引用, 并将它写入到本地的 `refs/remotes/origin/`. 所以, 如果远端上有一个 `master` 分支, 你在本地可以通过下面这种方式来访问它的历史记录：

```bash
$ git log origin/master
$ git log remotes/origin/master
$ git log refs/remotes/origin/master
```

它们全是等价的, 因为 Git 把它们都扩展成 `refs/remotes/origin/master`.
如果你想让 Git 每次只拉取远程的 `master` 分支, 而不是远程的所有分支, 你可以把 `fetch` 这一行修改成这样：
`fetch = +refs/heads/master:refs/remotes/origin/master`

[git中的refspec是什么意思?](http://www.imooc.com/wenda/detail/503063)

### 路径指定

`pathspec`

在`git`命令中用来限制路径的模式

一般就和`linux`中常用的表示方法一样.
可以使用通配符, `*` and `?` 也可以匹配目录分隔符`\`.

比如 `Documentation/*.jpg`, 也会匹配`Documentation/chapter_1/figure_1.jpg`.

以冒号`:`开始的路径有特殊的含义.只有`:`表示"there is no pathspec".应该和其他路径指定结合使用.
`:`形式分为short form and long form

+ shortform `:a:`
+ long form `:(a,b,c...)`

可以使用下面这些`magic words`

+ top: 咒语(magic signature`/`), 表示从工作树的根目录开始匹配, 即使在一个子目录运行命令.
+ literal: 通配符如`*` and `?`被当成普通字符.
+ icase: 忽略大小写

+ glob

`Git` 将模式视为`shell`模式, 适合由带有 `FNM_PATHNAME` 标志的 `fnmatch(3)` 解析. 模式中的通配符不会匹配路径名中的`/`.
例如, `Documentation/*.html`匹配 `Documentation/git.html`, 但不匹配 `Documentation/ppc/ppc.html` 或 `tools/perf/Documentation/perf.html`.

在与全路径名匹配的模式中, 两个连续的星号`**`可能有特殊含义.

+ `**/`这种形式表示在所有目录中进行匹配.
例如, `**/foo`匹配任何地方的文件或目录`foo`, 与模式`foo`相同.
`**/foo/bar`匹配任何地方的, 直接在`foo`目录下的`bar`, 无论`bar`是文件还是目录.

+ 斜线后面的`**`, 也就是`/**`匹配目录下的所有内容.
例如, `abc/**`匹配`abc`目录下的所有文件, 相对于`.gitignore`文件的位置, 不限制深度.

+ `/**/`这种形式匹配零个或多个目录. 例如, `a/**/b`匹配 `a/b`, `a/x/b`, `a/x/y/b` 等等.

+ 其他连续的星号被认为是无效的.
+ `Glob` 魔法和 `原文`魔法 不兼容.

#### attr

`attr`后面是一个以空格分隔的`属性要求`的列表. 所有这些要求都必须按顺序满足, 才匹配某个路径.
这是在通常的`non-magic`路径模式匹配之外的. 见`gitattributes[5]`.

对路径的每个属性要求是下列形式之一:

+ `ATTR`; 要求属性`ATTR`被设置.
+ `-ATTR`要求属性`ATTR`不被设置.
+ `ATTR=VALUE`要求将属性`ATTR`设置为字符串`VALUE`.
+ `!ATTR`要求属性`ATTR`是未指定的.

请注意, 当与树对象匹配时, 属性仍然是从`working tree`中获得, 而不是从给定的树对象中获得.

+ exclude 排除指定的路径, 也可以使用`!` or `^`

### 包文件

[Git 内部原理 - 包文件](https://git-scm.com/book/en/v2/Git-Internals-Packfiles)

Git 使用`zlib`压缩文件的内容.

Git 最初向磁盘中存储对象时所使用的格式被称为`松散(loose)`对象格式.
但是, Git 会时不时地将多个这些对象打包成一个称为`包文件(packfile)`的二进制文件, 以节省空间和提高效率.
当版本库中有太多的松散对象, 或者你手动执行 `git gc` 命令, 或者你向远程服务器执行推送时, `Git` 都会这样做.
要看到打包过程, 你可以手动执行 `git gc` 命令让 `Git` 对对象进行打包：

```bash
git gc
```

这个时候再查看`objects`目录, 你会发现大部分的对象都不见了, 与此同时出现了一对新文件：

```bash
$ find .git/objects -type f
...
.git/objects/pack/pack-978e03944f5c581011e6998cd0e9e30000905586.idx
.git/objects/pack/pack-978e03944f5c581011e6998cd0e9e30000905586.pack
```

它们分别是包文件和一个索引.
包文件包含了刚才从文件系统中移除的所有对象的内容.
索引文件包含了包文件的偏移信息, 我们通过索引文件就可以快速定位任意一个指定对象.
有意思的是运行`gc`命令前磁盘上的对象大小约为`15K`, 而这个新生成的包文件大小仅有`7K`.
通过打包对象减少了一半的磁盘占用空间.

Git 是如何做到这点的？ Git 打包对象时, 会查找命名及大小相近的文件, 并只保存文件不同版本之间的差异内容.
 你可以查看包文件, 观察它是如何节省空间的. `git verify-pack` 这个底层命令可以让你查看已打包的内容：

```bash
git verify-pack -v .git/objects/pack/pack-978e03944f5c581011e6998cd0e9e30000905586.idx
```

你会看到类似类似下面的格式

```git
033b4... blob   9 20 7262 1 \
  b042a...
b042a... blob   22054 5799 1463
```

`033b4` 这个数据对象引用了数据对象 `b042a`.
命令输出内容的第三列显示的是各个对象在包文件中的大小, 可以看到 `b042a` 占用了 `22K` 空间, 而 `033b4` 仅占用 `9` 字节.
同样有趣的地方在于, 第二个版本完整保存了文件内容, 而原始的版本反而是以差异方式保存的——这是因为大部分情况下需要快速访问文件的最新版本.

最妙之处是你可以随时重新打包. `Git` 时常会自动对仓库进行重新打包以节省空间.当然你也可以随时手动执行 `git gc` 命令来这么做.

## revision 的写法

A revision parameter  `<rev>`一般是`commit`, 它使用what is called an extended `SHA-1` syntax

### sha1

`<sha1>`, e.g. `dae86e1950b1277e545cee180551750029cfe735`, `dae86e`

The full SHA-1 object name (40-byte hexadecimal string), or a leading substring that is  within the repository

***
`<describeOutput>`, e.g. `v1.7.4.2-679-g3bee7fb`

Output from git describe;
i.e. a closest tag,  optionally followed by a dash and a number of commits,  followed by a dash, a g, and an abbreviated object name.

### refname

`<refname>`, e.g. `master`, `heads/master`, `refs/heads/master`

A symbolic `ref` name. E.g.  `master` typically means the commit object referenced by `refs/heads/master`.
If you happen to have both `heads/master` and `tags/master`,you can explicitly say heads/master to tell Git which one you mean.

### @

`@`

`@` alone is a shortcut for `HEAD`.

***
`<refname>@{<date>}`, e.g. `master@{yesterday}`, `HEAD@{5 minutes ago}`

A ref followed by the suffix `@` with a 日期包围在大括号中 (e.g.  `{yesterday}`, `{1 month 2 weeks 3 days 1 hour 1 second ago}` or `{1979-02-26 18:30:00}`) specifies the value of the ref at a prior point in time.

这个后缀只能用在 `ref name` 后面.它会寻找给定时间内的状态, 比如上星期,
如果你想寻找时间段内的, 用`--since` and `--until`.

***
`<refname>@{<n>}, e.g. master@{1}`

A `ref` followed by the suffix `@` with an 大括号中的顺序(e.g.  `{1}`, `{15}`) specifies the `n-th` prior value of that `ref`.

For example `master@{1}` is the immediate prior value of `master` while `master@{5}` is the 5th prior value of master.

This suffix may only be used immediately following a ref name and the ref must have an existing log (`$GIT_DIR/logs/<refname>`).

`@{<n>}`, e.g. `@{1}`

如果省略前面的`ref`指定的话, 默认指的是当前分支

For example, if you are on branch blabla then @{1} means the same as blabla@{1}.

***
`@{-<n>}, e.g. @{-1}`
The construct `@{-<n>}` means the `<n>th branch/commit` checked out before the current one.

***
`<branchname>@{upstream}`, e.g. `master@{upstream}`, `@{u}`
`<branchname>@{push}`, e.g. `master@{push}`, `@{push}`

上游分支, 推送分支,

Here’s an example to make it more clear:

```git
$ git config push.default current # 配置默认
$ git config remote.pushdefault myfork #默认远程push 分支
$ git checkout -b mybranch origin/master #创建并切换到新分支 mybranch, 上游是`origin/master`

$ git rev-parse --symbolic-full-name @{upstream} # 给出 上游分支
refs/remotes/origin/master

$ git rev-parse --symbolic-full-name @{push} #给出默认push分支
refs/remotes/myfork/mybranch
```

在这个例子中, 我们建立了一个triangular workflow(三角形工作流), 从一个位置pull然后push到另一个位置, 如果是一个普通的工作流, 那么`@{push}` is the same as `@{upstream}`.

后缀`@{push}` or `@{upstream}`大小写不敏感

### ^ caret and ~ tilde

`<rev>^`, e.g. `HEAD^`, `v1.5.1^0`

`<rev>^`等价于`<rev>^1`, `^<n>`意思是当前`ref`的第`n`个父节点, 指的是同一个`level`上的, 也就是水平方向的.

As a special rule,` <rev>^0` 指向自身, 可以用`tag`(tag object)指向提交(commit object)

***
`<rev>~<n>`, e.g. `master~3`

A suffix `~<n>` to a revision parameter 之的是第`n`个首位父节点,
I.e.  `<rev>~3`等价于`<rev>^^^`, 等价于 `<rev>^1^1^1`.

参见下面的图示

***
`<rev>^{<type>}`, e.g. `v0.99.8^{commit}`

A suffix `^` followed by 大括号中的类型名 means dereference the object at `<rev>`  recursively until an object of type `<type>` is found or the object cannot be dereferenced anymore (in which case, barf).

For example, if `<rev>` is a commit-ish, `<rev>^{commit}` describes the corresponding commit object.
Similarly, if `<rev>` is a tree-ish, `<rev>^{tree}` describes the corresponding tree object.
`<rev>^0` is a short-hand for `<rev>^{commit}`.

`rev^{object}` can be used to make sure `rev` names an object that exists,

without requiring `rev` to be a tag, and without dereferencing rev;  because a tag is already an object, it does not have to be dereferenced even once to get to an object.

`rev^{tag}` can be used to ensure that `rev` identifies an existing tag object.

***
`<rev>^{}`, e.g. `v0.99.8^{}`

`<rev>^{}` 意思是这个object可能是个tag, and  the tag recursively until a non-tag object is found.

***
`<rev>^{/<text>}`, e.g. `HEAD^{/fix nasty bug}`

这个形式等价于下面的`:/fix nasty bug` , 除了它返回  the youngest matching commit which is reachable from the `<rev>` before `^`.

### : colon

`:/<text>`, e.g. `:/fix nasty bug`

引用一个commit, 它的提交信息匹配特性的正则表达式. 正则表达式可以匹配commit message的任意部分.

匹配某些字符开头, 用`:/^foo`, 序列`/!`有特殊含义, `:/!-foo` 反相匹配, `:/!!foo`匹配`!foo`本身,

Any other sequence beginning with :`/!`  is reserved for now.

***
`<rev>:<path>`, e.g. `HEAD:README`, `:README`, `master:./README`

给出tree-ish object `<rev>`下的`path`对应的文件( blob or tree),

`:path` (`:`前面没有指定`rev`) 表示的是`index`中的内容,  A path starting with `./` or `../` is relative to the current working directory.

给出的路径会被转换成相对于 working tree的根目录, 对于引用跟当前working tree 有相同目录结构的commit or tree是很有用的.

***
`:<n>:<path>`, e.g. `:0:README`, `:README`

冒号后面的数字可以取`0` to `3`, 引用相应`index`中的`blob` object, 缺省数字的话相当于`0`,
在`merge`的时候, `stage 1`(也就是index)指代common ancestor, stage 2是目标分支(一般是当前分支)
`stage 3`是被合并过来的分支

***
这里有一个图示, by Jon Loeliger.

nodes B and C 是 A 的父节点, 父亲的顺序从左到右.

```graph
G         H   I       J
 \        /       \    /
  D    E          F
   \     |       /     \
    \    |     /        |
     \   |  /           |
        B             C
        \            /
            \      /
                A
```

```git
A =      = A^0
B = A^   = A^1     = A~1
C = A^2  = A^2
D = A^^  = A^1^1   = A~2
E = B^2  = A^^2
F = B^3  = A^^3
G = A^^^ = A^1^1^1 = A~3
H = D^2  = B^^2    = A^^^2  = A~2^2
I = F^   = B^3^    = A^^3^
J = F^2  = B^3^2   = A^^3^2
```

## 比较二进制文件

你可以使用 `Git 属性`来有效地比较两个二进制文件.
秘诀在于, 告诉 Git 怎么把你的二进制文件转化为文本格式, 从而能够使用普通的 diff 方式进行对比.

比如：对 Microsoft Word 文档进行版本控制. 大家都知道, Microsoft Word 几乎是世上最难缠的编辑器, 尽管如此, 大家还是在用它. 如果想对 Word 文档进行版本控制, 你可以把文件加入到 Git 库中, 每次修改后提交即可.
 把下面这行文本加到你的 `.gitattributes` 文件中：

```bash
*.docx diff=word
```

这告诉 Git 当你尝试查看包含变更的比较结果时, 所有匹配 `.docx` 模式的文件都应该使用`word`过滤器.
`word`过滤器是什么？ 我们现在就来设置它.
我们会对 Git 进行配置, 令其能够借助 `docx2txt` 程序将 Word 文档转为可读文本文件, 这样不同的文件间就能够正确比较了.

首先, 你需要安装 `docx2txt`；
它可以从 [https://sourceforge.net/projects/docx2txt](https://sourceforge.net/projects/docx2txt) 下载. 按照 `INSTALL` 文件的说明, 把它放到你的可执行路径下.
接下来, 你还需要写一个脚本把输出结果包装成 `Git` 支持的格式. 在你的可执行路径下创建一个叫 `docx2txt` 文件, 添加这些内容：

```bash
#!/bin/bash
docx2txt.pl "$1" -
```

别忘了用 `chmod a+x` 给这个文件加上可执行权限. 最后, 你需要配置 `Git` 来使用这个脚本：

`$ git config diff.word.textconv docx2txt`

现在如果在两个快照之间进行比较, Git 就会对那些以 .docx 结尾的文件应用`word`过滤器, 即 `docx2txt`.
这样你的 Word 文件就能被高效地转换成文本文件并进行比较了.

你还能用这个方法比较图像文件.
其中一个办法是, 在比较时对图像文件运用一个过滤器, 提炼出 `EXIF` 信息——这是在大部分图像格式中都有记录的一种元数据.
如果你下载并安装了 `exiftool` 程序, 可以利用它将图像转换为关于元数据的文本信息, 这样比较时至少能以文本的形式显示发生过的变动： 将以下内容放到你的 `.gitattributes` 文件中：

`*.png diff=exif`

配置 Git 以使用此工具：

`$ git config diff.exif.textconv exiftool`
