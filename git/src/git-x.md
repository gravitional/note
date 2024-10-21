# git.x.md

## git 分支的目录结构

`.git`文件夹下, 有`/refs/`文件夹, `/refs/`结构如下:

```bash
/heads/
   branch1
   branch2
   ...
/remotes/
   origin
       reomote_branch1
       HEAD
   origin2
   ...
/tags/
   tag1
   ...
```

```bash
git log origin/master
git log remotes/origin/master
git log refs/remotes/origin/master
```

## git 用户端配置

[初次运行 Git 前的配置](https://gitee.com/help/articles/4107)

在新的系统上, 我们一般都需要先配置下自己的 `Git` 工作环境.
配置工作只需一次, 以后升级时还会沿用现在的配置. 当然, 如果需要, 你随时可以用相同的命令修改已有的配置.

Git 提供了一个叫做 `git config` 的工具, 专门用来配置或读取相应的工作环境变量.
>译注: 实际是 `git-config` 命令, 只不过可以通过 `git` 加一个名字来呼叫此命令.
而正是由这些环境变量, 决定了 Git 在各个环节的具体工作方式和行为. 这些变量可以存放在以下三个不同的地方:

+ `/etc/gitconfig` 文件: 系统中对所有用户都普遍适用的配置. 若使用 `git config` 时用 `--system` 选项, 读写的就是这个文件.
+ `~/.gitconfig` 文件: 用户目录下的配置文件只适用于该用户. 若使用 `git config` 时用 `--global` 选项, 读写的就是这个文件.
+ 当前仓库的 `Git` 目录中的配置文件(也就是工作目录中的 `.git/config` 文件): 这里的配置仅仅针对当前仓库有效.
每一个级别的配置都会覆盖上层的相同配置, 所以 `.git/config` 里的配置会覆盖 `/etc/gitconfig` 中的同名变量.
此外, Git 还会尝试找寻 `/etc/gitconfig` 文件, 只不过看当初 Git 装在什么目录, 就以此作为根目录来定位.

+ 用户信息配置

第一个要配置的是你个人的用户名称和电子邮件地址,
说明是谁提交了更新, 会随更新内容一起被永久纳入历史记录:

```bash
git config --global user.name "John Doe"
git config --global user.email johndoe@example.com
```

+ 文本编辑器配置

接下来要设置的是默认使用的文本编辑器.
`Git` 需要你输入一些额外消息的时候, 会自动调用一个外部文本编辑器给你用.
如果你有其他偏好, 比如 `Emacs` 的话, 可以重新设置:

```bash
$ git config --global core.editor emacs
```

+ 差异分析工具

```bash
$ git config --global merge.tool vimdiff
```

+ 查看配置信息 : `git config --list`

`Zsh`, 以及一些专门为它打造的完整框架, 比如 `on-my-zsh`,
包含强大的 `Git Tab` 补全功能, 并且提示符主题可以展示版本控制数据.

### git 配置文件写法

以`# ` or ` ;` 开头的将被看成是注释, 到行末.
大部分空格将被忽略, 空行也被忽略

文件包含`Sections` 和 `variables` , 类似下面这种格式, `[section]`, 还可以有`[section "subsection"]`
所有名字都是大小写敏感的. 其他的行被看成是设置变量值.

```gitconfig
[core]
   repositoryformatversion = 0
   filemode = true
[branch "master"]
   remote = origin
   merge = refs/heads/master
```

配置文件的路径为 `$GIT_DIR/config`, 指定远程的格式如下:

```gitconfig
[remote "<name>"]
   url = <url>
   pushurl = <pushurl>
   push = <refspec>
   fetch = <refspec>
```

`pushurl`之用来推送, 默认和 `<url>` 相同

### windows 图形界面

[git scm book](https://git-scm.com/book/zh/v2/),
附录A: 在其他环境中使用 Git - 图形界面

两个常用的图形工具 `gitk ` 和 `git-gui`,
安装 Github 客户端, 会提供`git-gui`

`GitHub`客户端将许多操作整合成一个功能, 比如点击同步的时候, 实际上会执行

+ `git pull --rebase`; 如果上述命令由于存在合并冲突而失败, 则会退而执行 `git pull --no-rebase`.
+ `git push`

### 在 PowerShell 中使用 Git

[在 Windows 终端中设置 Powerline](https://docs.microsoft.com/en-us/windows/terminal/tutorials/custom-prompt-setup)
[PowerShell 图标 module](https://github.com/devblackops/Terminal-Icons)

适用于 `PowerShell` , 也适用于 Linux or macOS 上运行的 `PowerShell Core`,
只需安装名为 `Posh-Git` 的拓展包.
link: [Posh-Git](https://github.com/dahlbyk/posh-git)

#### 安装前提需求(仅限 Windows)

在可以运行 PowerShell 脚本之前, 你需要将本地的 `ExecutionPolicy` 设置为 `RemoteSigned`
(可以说是允许除了 `Undefined` 和 `Restricted` 之外的任何内容).
如果你选择了 `AllSigned` 而非 `RemoteSigned` , 那么你的本地脚本还需要数字签名后才能执行.

如果设置为 `RemoteSigned` ,  那么只有 `ZoneIdentifier` 设置为 `Internet`,
即从 `Web` 上下载的脚本才需要签名, 其它则不需要.
如果你是管理员, 想要为本机上的所有用户设置它, 请使用 `-Scope LocalMachine` .
如果你是没有管理权限的普通用户, 可使用 `-Scope CurrentUser` 来只给自己设置.

```powershell
Set-ExecutionPolicy -Scope LocalMachine -ExecutionPolicy RemoteSigned -Force
```

在 `PowerShell 5` 以上, 使用包管理器来安装 `posh-git`.
`Posh-Git` 将 `Git`状态信息添加到提示, 并为 `Git` 命令, 参数, 远程和分支名称添加 `tab` 自动补全.
`Oh-My-Posh` 为 `PowerShell` 提示符提供主题功能

```powershell
Install-Module posh-git -Scope CurrentUser -Force
# Install-Module posh-git -Scope CurrentUser -AllowPrerelease -Force
# 带有 PowerShell Core 支持的更新的 beta 版
Install-Module oh-my-posh -Scope CurrentUser # 安装 oh-my-posh 插件
```

如果使用的是 `PowerShell Core`, 请安装 `PSReadline`:

```powershell
#PSReadline 允许在 PowerShell 中自定义命令行编辑环境.
Install-Module -Name PSReadLine -Scope CurrentUser -Force -SkipPublisherCheck
```

如果你想为所有的用户安装 `posh-git`,
请使用 `-Scope AllUsers` 并在管理员权限启动的 `PowerShell` 控制台中执行.
如果第二条命令执行失败并出现类似

    Module 'PowerShellGet' was not installed by using Install-Module

这样的错误, 那么你需要先运行另一条命令:

```powershell
Install-Module PowerShellGet -Force -SkipPublisherCheck
```

之后你可以再试一遍.
出现这个错误的原因是 `Windows PowerShell `搭载的模块是以不同的发布证书签名的.

+ 要使这些操作生效, 即在你的提示符中包含 Git 信息, 需要导入 `Posh-Git` 模块.

要让 `PowerShell` 在每次启动时都导入 `Posh-Git`, 请执行 `Add-PoshGitToProfile` 命令,
它会在你的 `$profile` 脚本中添加导入语句.
此脚本会在每次打开新的 `PowerShell` 终端时执行. 注意, `$profile` 脚本可能有多个.
例如其中一个是控制台的, 另一个则属于 `ISE`.

```powershell
> Import-Module posh-git
> Add-PoshGitToProfile -AllHosts
```

或者使用 `notepad $PROFILE` 打开 PowerShell 配置文件,
该脚本在每次启动 PowerShell 时运行. 在 `PowerShell` 配置文件中, 将以下内容添加到文件的末尾:

```powershell
Import-Module posh-git
Import-Module oh-my-posh
Set-Theme Paradox
```

现在, 每个新实例启动时都会导入 `Posh-Git` 和 `Oh-My-Posh`,
然后从 `Oh-My-Posh` 设置 `Paradox` 主题.  `Oh-My-Posh` 附带了若干内置主题.

You need to close all powershell instances and then run

```powershell
<path-to-pwsh-executable> -noprofile -command "Install-Module PSReadLine -Force -SkipPublisherCheck -AllowPrerelease"
```

Unloading the module doesn't unload the assembly from the PSReadLine module
(by design in .NET), hence you need to close all instances.

#### 设置字体

首先从 github 上安装 `Cascadia Code PL`字体,
然后从`Windows 终端`下拉菜单中选择`设置`(`Ctrl+, `)来打开 `settings.json` 文件中的配置文件设置.
找到 Windows PowerShell 配置文件, 添加`"fontFace": "Cascadia Code PL"`到配置中,
添加完的配置文件 `settings.json` 应如下所示:

```json
{
    // Make changes here to the powershell.exe profile.
    "guid": "{61c54bbd-c2c6-5271-96e7-009a87ff44bf}",
    "name": "Windows PowerShell",
    "commandline": "powershell.exe",
    "fontFace": "Cascadia Code PL",
    "hidden": false
},
```

或者添加到所有终端的默认配置 `"defaults"` 中,

```json
"profiles":
{
"defaults":
{
"fontFace": "Cascadia Code PL",
其他内容
}
}
```

添加到上面的位置, 注意如果不是列表中最后一个, 要在括号后面加逗号.
这样就将 `Cascadia Code PL` 指定为字体. 这样就会显示很好看的 `Cascadia Code Powerline` 字形.
在编辑器中选择`保存`后, 终端应会即刻显示出变化.

### 服务器上的 Git

如果需要搭建本地`Git` 服务器, 可以参考 [4.8 服务器上的 Git - GitLab](https://git-scm.com/book/zh/v2/) .

虽然 `GitWeb` 相当简单.  但如果你正在寻找一个更现代, 功能更全的 Git 服务器,
这里有几个开源的解决方案可供你选择安装.
因为 `GitLab` 是其中最出名的一个, 我们将它作为示例并讨论它的安装和使用.
这比 `GitWeb` 要复杂的多并且需要更多的维护, 但它的确是一个功能更全的选择.

### git-restore, 还原文件,恢复

[git-restore - Restore working tree files](https://git-scm.com/docs/git-restore/en)

还原工作区的文件, 可以用 `--source` 指定例如 `HEAD` .

```git
git restore [<options>] [--source=<tree>] [--staged] [--worktree] [--] <pathspec>...
git restore [<options>] [--source=<tree>] [--staged] [--worktree] --pathspec-from-file=<file> [--pathspec-file-nul]
git restore (-p|--patch) [<options>] [--source=<tree>] [--staged] [--worktree] [--] [<pathspec>...]
```

+ 使用`source`中的某些内容, 还原`工作树`中的`<pathspec>`.
+ 如果某个`路径`已被`git`追踪, 但在`source`中它不存在, 则会删除它以匹配`source`的状态.
+ 还可以使用 `--staged` 选项, 将此命令用于还原`index`中的内容,
+ 或通过 `--staged --worktree` 同时还原`working tree`和`index`.
+ 使用给定`树对象`中的内容, 还原`working tree`文件.

    ```git
    -s <tree>, --source=<tree>
    ```

+ 可以通过与之关联的 `commit` , `branch`或 `tag` 来指定 `source tree` .
+ 如果未指定, 则 `working tree` 的默认还原源为 `index` , 而 `index`的默认还原源为 `HEAD` .
+ 当同时指定了 `--staged` 和 `--worktree` 时, 则必须指定 `--source` .
+ 指定要还原的对象: 如果未给出, 默认还原 `working tree` .

    ```git
    -W, --worktree ; 工作区
    -S, --staged ; 暂存区
    ```

+ 指定 `--staged` 则只还原 `index` , 也可以同时指定两个, 并且都还原. 例子:

    ```bash
    git restore --source master~2 Makefile
    # or
    git restore --source=9ea00d1 parton.note.1.nb
    ```

### git restore 例子

如果你想恢复所有的 `C` 源文件, 使之与 `index` 中的版本一致, 你可以输入

```bash
git restore '*.c'
```

注意 `*.c` 周围的引号.
文件 `hello.c` 也会被还原, 尽管它已经不在工作树中了,
因为文件 `globbing` 是用来匹配索引中的条目的(不是shell中的工作树).

要恢复当前目录下的所有文件

```bash
git restore .
```

或者用 `top pathspec` 魔法恢复所有工作树上的文件,
见[gitglossary 7](https://git-scm.com/docs/gitglossary).

```bash
git restore :/
```

要恢复 `index` 中的文件以匹配 `HEAD` 中的版本,
这与使用 [git-reset 1](https://git-scm.com/docs/git-reset)相同

```bash
git restore --staged hello.c
```

或者您可以同时恢复索引和工作树(这与使用 git-checkout[1]相同)

```bash
git restore --source=HEAD --staged --worktree hello.c
```

或者用更实用但不容易读懂的简短形式.

```bash
git restore --source=HEAD --staged --worktree hello.c
```

## git 查看所有提交的 commit

[Get a list of all Git commits, including the 'lost' ones](https://stackoverflow.com/questions/4786972/get-a-list-of-all-git-commits-including-the-lost-ones)

Try:

```bash
git log --reflog
gitk --reflog # gitk 也支持
```

会列出所有 git 提交, 方法是假装所有 reflog(git reflog)
提到的对象都在命令行中以 `<commit>` 列出. 

## git-checkout

切换分支, 或者恢复 `working tree` 中的文件

```bash
git checkout [-q] [-f] [-m] [<branch>]
git checkout [-q] [-f] [-m] --detach [<branch>]
git checkout [-q] [-f] [-m] [--detach] <commit>
git checkout [-q] [-f] [-m] [[-b|-B|--orphan] <new_branch>] [<start_point>]
git checkout [-f|--ours|--theirs|-m|--conflict=<style>] [<tree-ish>] [--] <pathspec>...
git checkout [-f|--ours|--theirs|-m|--conflict=<style>] [<tree-ish>] --pathspec-from-file=<file> [--pathspec-file-nul]
git checkout (-p|--patch) [<tree-ish>] [--] [<pathspec>...]
```

+ 用 `index`或者 `<tree-ish>` (通常是一个 `commit` )里面的内容替换 `working tree` 里面的 `<pathspec>` (可以有多个指定).
+ 当给出一个 `<tree-ish>` 的时候, 匹配 `<pathspec>` 的文件路径(path), 会在 `index ` 和 `working tree` 里面都更新.
+ `index`中可能包含有之前合并失败的条目 . 如果你想 `checkout ` 这样的条目, 默认情况下会失败, 什么都不会发生.
    使用 `-f` 选项忽略未合并的项目.
+ 通过使用`--ours`或`--theirs`, 可以从`index` 中检查出, 合并的特定一方的内容.
+ 使用`-m`, 对`工作树`文件所做的修改可以被丢弃, 以重新创建原先产生冲突(conflicted)的合并结果.

### 描述

更新 `工作树` 中的文件, 使其与 `index` 或指定树中的版本一致.
如果没有给出 `pathspec`, `git checkout` 也会更新 `HEAD`, 将指定的 `分支` 设为 `当前分支`.

#### `git checkout [<branch>]`

+ 要准备在 `<branch>` 上工作, 需要更新 索引 和 工作树 中的文件,
并将 HEAD 指向该分支, 从而切换到该分支.
对 工作树 中文件的本地修改将被保留, 以便提交到 `<branch>`.

+ 如果找不到 `<branch>`, 但正好有一个远端(就叫它 `<remote>`)存在名称匹配的 跟踪分支(tracking branch),
且未指定 `--no-guess`, 则处理方式等同于

```bash
git checkout -b <branch> --track <remote>/<branch>
```

+ 另一方面, 您可以省略 `<branch>`, 在这种情况下, 命令会退化为 `签出当前分支`,
这是费力不讨好地显示当前分支的 `跟踪信息`(如果存在的话).

#### `git checkout -b|-B <new_branch> [<start point>]`

指定 `-b` 会创建一个新分支, 就像调用 `git-branch(1)` 然后 检出 一样.
在这种情况下, 你可以使用 `--track` 或 `--no-track` 选项, 它们将被传递给 `git branch`.
为方便起见, `--track`(不含 `-b`)意味着 创建新分支;参见下文对 `--track` 的描述.

如果给定了 `-B`, `<new_branch>` 如果不存在, 就会被创建;否则, 就会被重置.
这相当于

```bash
git branch -f <branch> [<start point>]
git checkout <branch>
```

也就是说, 除非 `git checkout` 成功, 否则不会重置/创建分支.

#### `git checkout --detach [<branch>]`

`git checkout [--detach] <commit>`

准备在 `<commit>` 上工作, 在其上分离 HEAD(参见 `DETACHED HEAD` 部分), 并更新索引和工作树中的文件.
对 工作树 中文件的本地修改将被保留, 因此生成的 工作树 将是 `commit`的状态加上 本地修改.

当 `<commit>` 参数是 `branch` 名时, 可以使用 `--detach` 选项来 将 `HEAD` 与 `分支顶端` 分离.
(`git checkout <branch>` 会检出该分支, 但不分离 `HEAD`).
省略 `<branch>` 则会在当前分支的顶端分离 `HEAD`.

#### detached

[DETACHED HEAD](https://git-scm.com/docs/git-checkout#_detached_head)

如果用 `git checkout <commit>` 切换到某次提交,
那么 `HEAD`不是指向某个指针的(如 `master`, `dev` ), 所以是游离的 -- `detached`,

如果这个时候进行更改 并提交, 相当于创建了匿名分支, 所作的提交日后无法再索引到.
它们将被 `git` 的默认回收机制所回收. 解决方法是创建新分支:

```bash
git checkout -b new
```

重要的是要意识到, 此时没有任何东西会指向提交 `f`.
提交 f(以及延伸的提交 e)最终会被 Git 的例行垃圾回收程序删除, 除非我们在这之前创建一个引用.
如果我们还没有离开提交 `f`, 那么下面的任一命令都可以创建对它的引用.

```bash
$ git checkout -b foo
$ git branch foo
$ git tag foo
```

创建一个新的分支 foo, 它引用了提交的 f, 然后更新 HEAD 以引用分支 foo.
换句话说, 这条命令之后, 我们将不再处于分离的 HEAD 状态.
类似地, 创建一个新的分支 foo, 它指向提交的 f, 但让 HEAD 脱离.
创建一个新的标签 foo, 它引用了提交的 f, 但 HEAD 被分离.

如果我们已经离开了提交 f, 那么我们必须首先恢复它的对象名称(通常是通过使用 git reflog), 然后我们可以为它创建一个引用.
例如, 要查看 `HEAD` 最后提到的两个提交, 我们可以使用以下任一命令.

```bash
$ git reflog -2 HEAD # 或
$ git log -g -2 HEAD
```

### git-reset

```bash
git reset [-q] [<tree-ish>] [--] <pathspec>...
git reset [-q] [--pathspec-from-file=<file> [--pathspec-file-nul]] [<tree-ish>]
git reset (--patch | -p) [<tree-ish>] [--] [<pathspec>...]
git reset [--soft | --mixed [-N] | --hard | --merge | --keep] [-q] [<commit>]
```

+ 在前三种形式中, 将 `entries` 从 `<tree-ish>` 复制到 `index` .
+ 在最后一种形式中, 将当前分支头( `HEAD` )设置为 `<commit>` ,
可以选择修改 `index` 和 `working tree` 以使其匹配.
` <tree-ish>` / `<commit>`在所有形式中的默认值都是 `HEAD`.

+ `git reset --hard <commit>` or 别名 `grhh <commit>`

`--hard` 会清空 `working tree` 和 `index` 的改动.
彻底回退版本, 连本地文件都会被回退到上个版本的内容

+ `git reset --soft xxxx` or 别名 `grh --soft <commit>`

保留 `working tree` 和 `index` , 并合并到 `index` 中.
只回退 `commit` , 如果你想再次提交直接 `git commit` 即可.

`reset --soft` 会在重置 `HEAD` 和 `branch` 时, 保留 `working tree` 和 `index` 中的内容,
并把重置 `HEAD` 所带来的新的差异放进 `index` .

+ `reset 不加参数(--mixed)` or 别名 `grh <commit>`

清空 `index` , `mix` 到 `working tree` 中

`reset` 如果不加参数, 那么默认使用 `--mixed` 参数.
它的行为是: 保留 `working tree` , 并且清空 `index` .
也就是说, `working tree`的修改, `index`的内容以及由 `reset` 所导致的新的文件差异,
都会被放进 `working tree` .
简而言之, 就是把所有差异都混合( `mixed` )放在 `working tree` 中.

+ 同理, `reset --hard` 不仅可以撤销提交, 还可以用来把 `HEAD` 和 `branch` 移动到其他的任何地方.

```bash
git reset --hard branch2
```

把 `HEAD` 和 `branch`移动到 `branch2` 指向的提交.

### 三者的区别

有关这三个命令之间的差异, 见["Reset, restore and revert" in git(1)][].
有三个名称相似的命令: `git reset`, `git restore` 和 `git revert`.

+ [git-revert (1)][]; 将产生新的 `commit` , 新 `commit` 将还原旧 `commit` 所做的更改.

+ [git-restore (1)][]; 用于从 `index` 或某个 `commit` 还原 `working tree` 中的文件.
此命令不会更新您的 `分支`. 该命令还可用于从某个 `commit` 还原 `index` 中的文件.

+ [git-reset (1)][]; 用于 `更新` 某个分支, 移动 分支头(`tip`),
以添加或删除 `commit`s. 此操作将更改 `commit` 历史.

+ `git reset`也可以用来还原 `index`, 与 `git restore` 功能重叠.

["Reset, restore and revert" in git(1)]: https://git-scm.com/docs/git#_reset_restore_and_revert
[git-revert (1)]: https://git-scm.com/docs/git-revert
[git-restore (1)]: https://git-scm.com/docs/git-restore
[git-reset (1)]: https://git-scm.com/docs/git-reset

## 修改最后一次注释

如果你只想修改最后一次注释(就是最新的一次提交),

`git commit --amend`

## git 撤销提交

详见 git-x-reset.md.

## git重命名文件夹

不用先在本地修改文件夹名称

文件夹名称: `game` 文件夹修改后名称: `gamesdk`

+ `git mv game gamesdk`
+ `git commit -m 'rename dir game to gamesdk'`
+ `git push origin dev`  ; 推送到 `dev` 分支

ref: [git重命名文件夹](https://www.jianshu.com/p/e886fde18ba0)

## 远程仓库

### 添加ssh 公匙

[SSH 公钥设置](https://gitee.com/help/articles/4191#article-header0)

用如下命令来生成 `sshkey`:

```bash
ssh-keygen -t rsa -C "abc@def.com"
# Generating public/private rsa key pair...
```

复制生成后的ssh key, 一般在 `~/.ssh/id_rsa.pub`, 在仓库主页`管理`页面中, 添加生成的 public key 添加到仓库中.

添加后, 在终端中输入下面的命令, 来检测是否能成功连接

```bash
ssh -T git@gitee.com # -T  禁止分配伪终端
```

首次使用需要确认并添加主机到本机SSH可信列表. 若返回 `Hi XXX! You've successfully authenticated,....` 内容, 则证明添加成功.
添加成功后, 就可以使用SSH协议对仓库进行操作了.

***
ssh-keygen 选项说明;

+ `-C comment`; 提供一个新注释. 上面的 `abc@def.com` 只是生成的 `sshkey` 的名称, 并不约束或要求具体命名为某个邮箱, 邮箱只是为了便于识别
+ `-t dsa | ecdsa | ecdsa-sk | ed25519 | ed25519-sk | rsa`; 指定要创建的密钥的类型.  可能的值是 "dsa", "ecdsa", "ecdsa-sk", "ed25519", "ed25519-sk", 或 "rsa".
当使用RSA CA密钥签署证书时, 该标志也可用于指定所需的签名类型.  可用的RSA签名变体是 "ssh-rsa"(SHA1签名, 不推荐), "rsa-sha2-256 "和 "rsa-sha2-512"(默认).

***
ssh 选项说明;

+ `-T` ; 禁用伪终端分配.
+ `-t` ; 强制进行伪终端分配.  这可以用来在远程机器上执行任意的, 基于屏幕的程序, 这可能非常有用, 例如在实现菜单服务时.  多个`-t`选项强制分配 `tty`, 即使 `ssh` 没有本地 `tty`.

### 远程仓库操作

远程仓库可以在你的本地主机上

+ `git remote -v` ; 查看远程仓库
+ `git remote add <shortname> <url>` ; 添加远程仓库
+ `git fetch <remote>` ; 从远程仓库抓取
+ `git push origin master` ; 推送到远程仓库
+ `git remote show origin` ; 查看远程仓库`origin`
+ `git remote rename pb paul` ; 将远程仓库 `pb` 重命名为`paul`
+ `git remote remove paul` ; 删除失效的远程仓库
+ `git remote set-url origin` ; 修改远程仓库 `origin` 对应的地址

```bash
git remote set-url [--push] <远程仓库名> <newurl> [<oldurl>]
git remote set-url --add [--push] <远程仓库名> <newurl>
git remote set-url --delete [--push] <远程仓库名> <url>
```

+ `--add` ;    添加地址, 而不是修改
+ `--delete` ; 删除所有匹配的地址
+ `--push` ;   操作 `push` 地址而不是`fetch` URLs

### 设置多个远程

[git 本地仓库同时推送到多个远程仓库](https://blog.csdn.net/fox9916/article/details/79386169)

先准备两个空的远程仓库, 如果远程仓库里有 `readme` 这样的文件, 先 `pull` 一下.
如果 `pull` 的时候失败, 提示: `fatal: refusing to merge unrelated histories`,
这是由于本地仓库和远程仓库的历史没有联系, git 拒绝合并, 我们添加选项让它强行合并,

```bash
git pull origin master --allow-unrelated-histories
```

有两种方法:

+ git remote add 命令:

将本地仓库与远程仓库关联起来, 再查看一下远程仓库情况

```bash
# 添加第一个仓库
git remote add origin git@xxxx1
# 再添加另一个远程仓库
git remote add gitee git@xxxx2
# 查看远程仓库状态
git remote -v
```

查看远程仓库的情况, 可以看到已经有两个远程仓库了.
然后再使用相应的命令 `push` 到对应的仓库就行了. 这种方法的缺点是每次要 `push` 两次.

```bash
git  push origin master:master
git  push gitee master:master
```

+ git remote set-url 命令

另一种方法是使用 git remote set-url 添加多个仓库地址

```bash
#删除方法一的 gitee 远程仓库.
git  remote rm gitee
#使用如下命令添加远程仓库.
git remote set-url --add  origin git@xxxx2
```

```zsh
grset --add  origin git@xxxx2
```

查看远程仓库情况. 可以看到 `origin` 远程仓库有两个 `push` 地址.
这种方法的好处是每次只需要 `push` 一次就行了.

```bash
git remote -v
git push origin master:master
```

#### 修改 config

另外手动更改本地仓库`.git/config`文件也是可以的, 改成如下格式:

```bash
[remote "origin"]
   url = git@github.com:xxx
   url = git@gitee.com:xxx
   fetch = +refs/heads/*:refs/remotes/github/*
```

+ 命令语法:

```bash
git remote get-url [--push] [--all] <name>
git remote set-url [--push] <name> <newurl> [<oldurl>]
git remote set-url --add [--push] <name> <newurl>
git remote set-url --delete [--push] <name> <url>
```

为远程仓库设置新的链接,改变远程 `<name>` 的链接, 可以通过给出 ` <oldurl>` 进一步确认.

`--push `, 设置push URLs 而不是 fetch URLs
`--add`, 不改变已经存在的 URLs, 添加新 URL
`--delete`, 不改变已经存在的 URLs, 删除 `<name>` 上匹配正则 `<url>`的 地址. 试图删除所有 non-push URLs 将导致错误.

### 远程分支

```bash
git push [--all | --mirror | --tags] [--follow-tags] [--atomic] [-n | --dry-run] [--receive-pack=<git-receive-pack>]
                  [--repo=<repository>] [-f | --force] [-d | --delete] [--prune] [-v | --verbose]
                  [-u | --set-upstream] [-o <string> | --push-option=<string>]
                  [--[no-]signed|--signed=(true|false|if-asked)]
                  [--force-with-lease[=<refname>[:<expect>]]]
                  [--no-verify] [<repository> [<refspec>...]]
```

+ `<refspec>...`: 指定用哪个`源对象`, 来更新哪个`目标 ref`.
+ `<refspec>` 参数的格式是: 可选的加号`+`,  后面跟上源对象 `<src>`, 后面跟冒号`:`, 后面是目标 ref `<dst>`.
+ `<src>` 通常是你想推送的`分支`的名称, 但它可以是任何任意的 `SHA-1表达式`, 如 `master~4` 或 `HEAD`(见gitrevisions(7)).

+ `<dst>` 指明`远程`的哪个`ref`会随着这次推送被更新.
这里不能使用任意的表达式, 必须指定一个实际的 `ref` 名称.
在配置了 `remote.<repository>.push`的情况下,
可以使用 `git push [<repository>] <src>` 或者 `git push [<repository>] `, 来更新 `<src>` 默认对应的远程分支.
如果未配置上述变量, 缺少 `:<dst>` 意味着要更新与   `<src>` 同名的远程 `ref`.

+ 如果 `<dst>` 不是以 `refs/` 开头(例如 `refs/heads/master`), 我们将尝试推断它在远程 `<repository> refs/*` 中的位置 .
这取决于被推送的 `<src>` 的类型, 和 `<dst>` 是否有歧义:

- 如果 `<dst>` 明确地指向远程 `<repository>` 的某个 `ref`, 那么就推送到那个 `ref`.
- 如果 `<src>` 被解析为以 `refs/heads/` 或 `refs/tags/` 开头的 `ref`, 则将完整路径添加到 `<dst>` 前面.
- 其他含糊不清的解决方法可能会在将来被添加, 但现在任何其他情况都会出错, 会有一个错误表明我们所作的尝试,
并根据 `advice.pushUnqualifiedRefname` 的配置(见 git-config(1)), 建议你可能想要推送至的,  `refs/` 命名空间.

+ `--follow-tags`
推送 (不使用该选项时要推送的) 所有`ref`, 同时也会推送 `ref/tags` 中的 注释标签(annotated tags),
这些标签还未推送到 remote, 但指向的提交与正在推送的 ref 是一致的(reachable).
这也可以通过配置变量 `push.followTags` 来指定.
详情请参阅 [git-config[1]](https://git-scm.com/docs/git-config) 中的 `push.followTags`.

#### 例子

如果你的当前分支设置了`跟踪远程分支`, 那么可以用 `git pull` 命令来自动抓取后合并该远程分支到当前分支

推送工作使用 `git push <remote> <branch>` , 比如 `git push origin serverfix`

这里有些工作被简化了.
Git 自动将 `serverfix` 分支名字展开为 `refs/heads/serverfix:refs/heads/serverfix`,
意味着, 推送本地的 `serverfix` 分支来更新远程仓库上的 `serverfix` 分支.

我们将会详细学习 Git 内部原理 的 `refs/heads/` 部分,  但是现在可以先把它放在儿.
你也可以运行 `git push origin serverfix:serverfix`,  它起同样的效果.
可以通过这种格式来推送本地分支到一个命名不相同的远程分支.

如果并不想让远程仓库上的分支叫做 `serverfix`,  可以运行 `git push origin serverfix:awesomebranch`,
来将本地的 `serverfix` 分支推送到远程仓库上的 `awesomebranch` 分支.

使用 `git checkout -b serverfix origin/serverfix`来从设置的远程仓库里创建新分支

这会给你一个用于工作的本地分支, 并且起点位于 `origin/serverfix`.

### 跟踪分支

从一个远程跟踪分支 `checkout` 一个本地分支会自动创建所谓的`跟踪分支`(它跟踪的分支叫做`上游分支`).  跟踪分支是与远程分支有直接关系的本地分支.
如果在一个跟踪分支上输入 `git pull`, Git 能自动地识别去哪个服务器上抓取, 合并到哪个分支.

当克隆一个仓库时, 它通常会自动地创建一个跟踪 `origin/master` 的 `master` 分支.
然而, 如果你愿意的话可以设置这个远程库的其他分支, 或是一个在其他远程仓库上的分支, 又或者不跟踪`master` 分支.

***
上游快捷方式

当设置好跟踪分支后, 可以通过简写 `@{upstream}` 或 `@{u}` 来引用它的上游分支.
所以在 `master` 分支时并且它正在跟踪 `origin/master` 时, 如果愿意的话可以使用 `git merge @{u}` 来取代 `git merge origin/master`.

## 删除远程分支

可以运行带有 `--delete` 选项的 `git push` 命令

```bash
$ git push origin --delete serverfix
To https://github.com/schacon/simplegit
- [deleted]         serverfix
```

```bash
git push [远程仓库] --delete [branchname]
```

## 创建新分支, git checkout -b

`git checkout -b|-B <new_branch> [<start point>]`

指定 `-b` 选项, 将会创建新分支, 如同调用`git-branch(1)`, 然后 `checkout` 一样.

在这种情况下, 你可以使用`--track ` or ` --no-track` options, 这些选项会传递给`git branch`
为方便起见, `--track ` without ` -b`意味着创建新分支; 见`--track` 的描述

### `git checkout`,`-t, --track`

当创建新分支的时候, 自动设置上游.
如果`-b` 选项没有给出, 本地分支的名字会从 远程跟踪分支(remote-tracking branch) 推导.
`git` 先查看本地中配置的远程信息 `refspec` , 然后去掉 `*` 之前的初始部分.
也就是说, 如果远程名字是`origin/hack`(or `remotes/origin/hack`, or `refs/remotes/origin/hack`),
新的本地分支就叫做 `hack` ,

如果查询到的名称中没有 `slash` (`/`), 或者上面的推测得到 空字符串, 那么猜测就会停止,
此时, 你可以用 `-b` 选项手动指定 名称.

### `git branch`,`-t`, `--track`

当创建新分支的时候, 设置 `branch.<name>.remote` 和 `branch.<name>.merge` 条目,
把 `start-point branch` 当作 `upstream` (上游分支).
这个配置会告诉 `git` , 在`git status ` and ` git branch -v`命令中显示两个分支的关系.
而且, 当切换到新分支的时候, 它指导不带参数的 `git pull` 从上游拉取更新.

如果 `start point` 是 `remote-tracking` 分支, 会默认进行上面的设置.
如果你想让`git checkout ` and ` git branch`默认行为是 `--no-track` ,
也就是不自动跟踪上游,可以配置变量 `branch.autoSetupMerge` 为`false` .
也可以设置成 `always` , 这样不管 `start-point` 是本地还是远程分支, 都会自动跟踪.

### 常见使用方法

+ 先运行 `git checkout -b` 命令创建新分支

```bash
git checkout -b branchname startpoint
```

+ 然后用 `git push -u` 命令推送到远程:

```bash
git push -u origin <refspec>
```

第一次推送 `source` 分支的所有内容, 并把本地的 `source` 分支和远程的 `destination` 分支关联起来

***
`git push` 语法说明;

+ `<refspec>...`; `<refspec>`指定用 `source` 对象更新哪个 `destination` ref.
    `<refspec> `的格式是: 可选的`+`号, 接着一个 `source object <src>` , 然后是`:`,
    然后是the `destination ref <dst>`, 就是`本地分支:远程分支`的格式,

    推送一个空的 `<src>` 相当于删除远程库中的 `<dst> ref` .
    特殊的refspec `:` (or `+:` to allow non-fast-forward updates) ,
    告诉Git推送匹配的分支: 如果远程库里存在和本地名字一样的分支, 就把本地分支推送过去.

+ `--all` ; 推送所有分支(i.e. `refs/heads/`下面的所有ref); 这时候不要再指定其他特定 `<refspec>` .

## git diff

`git-diff` - Show changes between commits, commit and working tree, etc

`commit` 可以用`HEAD~2`的格式,
`HEAD~2`最后的数字`2`指的是显示到倒数第几次, 比如`2`指定倒数第二次

### 语法

SYNOPSIS

```git
git diff [<options>] [<commit>] [--] [<path>... ]
git diff [<options>] --cached [<commit>] [--] [<path>... ]
git diff [<options>] <commit> <commit> [--] [<path>... ]
git diff [<options>] <blob> <blob>
git diff [<options>] --no-index [--] <path> <path>
```

+ `工作树` v.s. `暂存区`; `git diff [--options] [--] [<path>...]`;  默认相对于 `index` ( `stage` )的改动.

+ `path` v.s. `path`; `git diff --no-index [--options] [--] [<path>...]`;
    文件系统上的两个 `path` , 如果其中一个不是 `Git` 控制的 `working tree` , 可以不加`--no-index`

+ stage v.s. commit; `git diff [--options] --cached [<commit>] [--] [<path>...]`
    比较`staged ` and ` <commit>`, 默认commit 是 HEAD. `--staged ` is a synonym of ` --cached`.

+ `commit` v.s. `working tree`; `git diff [--options] <commit> [--] [<path>...]`
    比较 `working tree` 相对于 `<commit>` , commit可以是HEAD, 也可以是分支名字, 就是比较 分支的顶端.

+ `commit` v.s. `commit`; `git diff [--options] <commit> <commit> [--] [<path>...]`
    比较任意两个 `<commit>`, 前一个是base, 后一个是改动

+ `git diff [--options] <commit>..<commit> [--] [<path>...]`;
跟上一个相同, 如果有一边的 `<commit>` 省略, 则相当于`HEAD`

+ `git diff [--options] <commit>...<commit> [--] [<path>...]`
查看变化, 从A, B的共同祖先开始, 到B为止, "git diff A...B" 等价于`git diff $(git-merge-base A B) B`

You can omit any one of `<commit>`, which has the same effect as using HEAD instead.
为了避免你写的很奇怪, 注意所有的 `<commit>` , 除了最后两个使用 `..` 记号的, 都可以是任何`<tree>`
更完整的关于拼写 `<commit>` 的方法, 见"SPECIFYING REVISIONS" in gitrevisions(7)
然而, `diff`比较的是两个 endpoints, 而不是一个范围.
所以 `<commit>..<commit> `and ` <commit>...<commit>`在这里指的不是范围.

## git rebase

### rebase 删除某次提交

[git删除中间某次提交](https://www.cnblogs.com/qiqi715/p/11540999.html)

首先用`git log ` or ` gitk --all`获取 `commit` 信息
比如:

```git
commit 58211e7a5da5e74171e90d8b90b2f00881a48d3a
...

commit 0fb295fe0e0276f0c81df61c4fd853b7a000bb5c
...

commit 7753f40d892a8e0d14176a42f6e12ae0179a3210
...
```

假如要删除备注为 `commit` 为 `0fb295fe0e0` 的这次提交

+ 首先找到此次提交之前的一次提交的 `commit` `7753f40d89`
+ 执行如下命令

```bash
git rebase -i 7753f40
```

解释:
`git rebase -i  commit-id`
commit-id 为要删除的 `commit` 的前一次 `commit` 号
`-i` `--interactive`

将弹出一个编辑界面

+ 根据提示编辑文件, 将要删除的 `commit` 之前的单词改为`drop` , 然后按照提示保存退出

+ 这样就删除了指定的 `commit` , 可以使用 `git log` 查看下.
`git push  –f` 然后推送到远程仓库. 此时 `commit 0fb295fe0e`就被干掉了, 不影响后面的提交

说明:
`-f`, `--force`

通常, `git push`拒绝更新不是本地引用的祖先的远程引用, 从而避免覆盖它.
另外, 当使用 `--force-with-lease` 选项时, `git push`将拒绝更新其与当前值不匹配的远程引用.
`-f`将禁用这些检查, 并可能导致远程存储库丢失提交.  小心使用.

### 合并提交

[Git 压缩多个commit为单个commit](https://kinboyw.github.io/2019/04/09/Git-%E5%8E%8B%E7%BC%A9%E5%A4%9A%E4%B8%AAcommit%E4%B8%BA%E5%8D%95%E4%B8%AAcommit/)
[Squash commits into one with Git](https://www.internalpointers.com/post/squash-commits-into-one-git)

`git` 具有一个很棒的, 能将多次`修改`合并起来的方法, 尤其是在将他们共享出去之前.

你可以使用强大的 `interactive rebase`(交互式 rebase)将多次`提交`合并成`一次`.
这样可以把多个临时的小的`提交`合并成一次提交, 然后将整理好的代码 `push` 给远端.

#### 选择你的起始提交

首先让 `git` 开始交互式 `rebase` 会话:

```bash
git rebase --interactive HEAD~[N]
# 或者使用简写:
git rebase -i HEAD~[N]
```

这里的 `N` 就是你想要合并的提交的数量, 从最近的一次提交`往前数`.
下面是一个假想的从 git log 中拉取的提交列表, 我们以它为例, 假设当前我们正在修改 `feature Z`:

```log
1. 871adf OK, feature Z 完成              --- 最近的 commit
2. 0c3317 问题不大...
...
7. d94e78 准备实现 feature Z
8. 6394dc Feature Y                               --- 早先的 commit
```

然后这是我要做的事情:

```log
1. 871adf OK, feature Z 完成           --- 最近的 commit --┐
2. 0c3317 问题不大...                                                                    |
...                                                                                                      |
                                                                                                         |-- 合并成一个提交
                                                                                                         |
                                                                                                         |
7. d94e78 准备实现 feature Z     -----------------------------┘
8. 6394dc Feature Y                                --- 早先的 commit
```

要达到的效果:

```log
1. 84d1f8 Feature Z                               --- 新的 commit (rebase 的结果)
2. 6394dc Feature Y                               --- 早先的 commit
```

所以在这个案例中, 要执行的命令就是:

```bash
git rebase --interactive HEAD~[7]
```

例如我想将最后的 `7` 次提交合并为一次, 所以 `d94e78 准备实现 feature Z`  就是第 `7` 次提交.
那如果我有数不清的`提交`要压缩, 必须一个一个的数吗? dark 不必, 使用 `提交的哈希` 即可

```bash
git rebase --interactive [commit-hash]
```

这里的 `[commit-hash]` 是压缩范围起点的`前一次`提交的 `hash`.
`[commit-hash]` 将被作为压缩的`基`, 它需要比压缩的起点还要早一次, 或者你可以看自己的情况选择.
所以在示例中的命令就是:

```bash
git rebase --interactive 6394dc # 6394dc Feature Y
```

你可以将这个命令理解为: 对提交时间比 `[commit-hash]` 新的所有`提交`进行合并.

#### 选择与压缩

这时你的默认 `编辑器` 会有弹窗, 显示出你想要`合并`的`提交列表`, 就是我们在上一步选中的.
注意, 一开始可能会感觉有点看不明白, 因为是按 `反序` 排列的, `旧的提交` 显示在顶部.
我通过 `--- 早先的 commit` 和 `--- 新的 commit` 进行了说明, 在 `编辑器` 的窗口中不会显示这些说明, 但可以根据提交信息判断.

```log
pick d94e78 准备实现 feature Z     --- 早先的 commit
...
pick 0c3317 问题不大...
pick 871adf feature Z 完成     --- 新的 commit
[...]
```

在 `提交列表` 的底部有一个简短的 `注释`(示例中忽略了), 提示了所有的 `操作选项`.
你可以在交互式 `rebase` 中进行各种操作, 我们现在只进行一些基本的操作.
我们的任务是将所有的`提交`注释为 `squashable`, 除了第一个(最早的)提交: 它将被用作`基`.

把提交哈希前面的 `pick` 标记修改为 `squash` (或者简写为 `s` , 下面以 # 开头的行中有提示),
这样 `提交` 就被标记为可压缩的 . 最后的结果就是:

```log
pick d94e78 准备实现 feature Z       --- 早先的 commit
...
s 0c3317 问题不大...
s 871adf OK, feature Z 完成      --- 新的 commit
[...]
```

保存文件, 关闭`编辑器`.

#### 创建新的提交

你刚刚告诉了 `Git` 将全部的 `7` 次 `提交` 合并到列表的第一个 `提交` 中.
现在要给它添加 `注释`: 你的编辑器会再次弹出一个带有 `默认消息` 的窗口, 内容合并了被压缩的所有`提交 `的 ` 注释`.

你可以保留默认的 `提交注释`, 这样最终的提交信息将会是这些临时提交的 `注释列表`, 如下所示:

```log
准备实现 feature Z
...
问题不大...
feature Z 完成
```

通常我不喜欢保留这些信息, 所以我会清除默认消息, 使用一些自定义 `注释`, 例如只保留 `feature Z 完成`.

## git 目录操作

### 获取当前仓库的根目录

[Git 如何使用一条命令来获取Git根目录](https://geek-docs.com/git/git-questions/267_git_is_there_a_way_to_get_the_git_root_directory_in_one_command.html)

使用 `git rev-parse`命令获取Git根目录
Git提供了一个名为 `git rev-parse` 的命令, 可以用来解析, 处理Git对象以及获取与之相关的信息.
通过使用 `git rev-parse` 命令, 我们可以获取Git根目录的绝对路径.

要获取Git根目录的绝对路径, 可以在命令行中输入以下命令:

```bash
git rev-parse --show-toplevel
```

这将返回 `Git根目录` 的绝对路径.
例如, 假设我们的 `Git仓库` 位于 `/Users/username/git-repo` 目录中,
那么以上命令将返回 `/Users/username/git-repo`.
你可以在自己的 Git仓库 中尝试该命令, 看看它是否返回了正确的根目录路径.

## git submodule

+ 添加/引入 子仓库

```bash
git submodule add https://xxx/some.git 自定义目录
```

+ 克隆含有子仓库的项目

```bash
git clone --recurse-submodules https://github.com/项目地址
```

+ 克隆之后, 手动初始化子项目配置(`--init`)并检出子项目(update)

```bash
git submodule update --init
```

+ 递归 初始化 并 更新子模块

```bash
git submodule update --init --recursive
```

+ 更新子模块仓库 url, 然后递归更新子模块

```bash
# 将新的 URL 复制到本地配置中
$ git submodule sync --recursive
# 从新 URL 更新子模块
$ git submodule update --init --recursive
```

+ 合并子模块冲突

```bash
# ------ 查看主仓库更新
$ git diff
diff --cc DbConnector
index eb41d76,c771610..0000000
--- a/DbConnector
+++ b/DbConnector

# --------- 迁出子模块 远程更新
$ cd DbConnector
$ git rev-parse HEAD
eb41d764bccf88be77aced643c13a7fa86714135

$ git branch try-merge c771610
(DbConnector) $ git merge try-merge
Auto-merging src/main.c

# -------- 修改代码, 合并冲突, 在子模块提交  (1)
$ code .
$ git add src/main.c
$ git commit -am 'merged our changes'

# ------------- 回到主仓库, 查看子模块 commit 更新
$ cd .. (2)
$ git diff (3)
diff --cc DbConnector
index eb41d76,c771610..0000000
--- a/DbConnector
+++ b/DbConnector
@@@ -1,1 -1,1 +1,1 @@@
- Subproject commit eb41d764bccf88be77aced643c13a7fa86714135
 -Subproject commit c77161012afbbe1f58b5053316ead08f4b7e6d1d
++Subproject commit 9fd905e5d7f45a0d4cbc43d1ee550f16a30e825a

# -------------- 提交子模块 commit 更新
$ git add DbConnector (4)
$ git commit -m "Merge Tom's Changes" (5)
[master 10d2c60] Merge Tom's Changes
```

+ 递归`git`操作 子模块

```bash
# 递归 stash
$ git submodule foreach 'git stash'
# 递归 checkout
$ git submodule foreach 'git checkout -b featureA'
# 递归 生成 diff log
$ git diff; git submodule foreach 'git diff'
```

## git 合并冲突

git 拉去更新或者合并分支时, 形成的冲突的格式如下

```git
<<<<<<< HEAD:index.html
<div id="footer">contact : email.support@github.com</div>
=======
<div id="footer">
 please contact us at support@github.com
</div>
>>>>>>> iss53:index.html
```

`<<<<< xxx ======` 上半部分, 是本地的修改
`===== xxx >>>>>>` 下半部分, 是远程的修改

## git 用远程分支覆盖本地分支

[overwrite local files](https://stackoverflow.com/questions/1125968/how-do-i-force-git-pull-to-overwrite-local-files)

>警告:
>对已跟踪文件的任何未提交的本地变更都将丢失, 即使已暂存(`staged`).
>但任何未被 Git 跟踪的本地文件都不会受到影响.

首先, 将所有 `origin/<branch>` refs 更新到最新版本:

```bash
git fetch --all
```

备份当前分支(如 `master`):

```bash
git branch backup-master
```

跳转到 `origin/master` 上的最新提交, 并 checkout 这些文件:

```bash
git reset --hard origin/master
```

### 说明

+ `git fetch` 从远程下载最新提交, 而不会合并或重置任何内容.
+ `git reset` 会将主分支重置为刚获取的版本.
选项 `--hard` 会修改工作树中的所有文件, 使之与 `origin/master` 中的文件一致.

### 维护当前本地提交

值得注意的是, 在重置之前, 可以从 `master` 创建一个分支来保持当前的本地提交:

```bash
git checkout master
git branch new-branch-to-save-current-commits
git fetch --all
git reset --hard origin/master
```

之后, 所有旧提交都会保留在 `new-branch-to-save-current-commits` 中.

### 未提交的变更 Uncommitted changes

未提交的改动, 即使已暂存(使用 git add), 也会丢失.
请确保缓存或提交任何您需要的内容. 例如, 运行

```bash
git stash
```

稍后(在 `git reset` 后), 重新应用这些未提交的更改:

```bash
git stash pop
```

这可能会产生合并冲突.

## git 提交日志规范

[Git Commit Log的小型团队最佳实践](https://blog.csdn.net/weixin_34409703/article/details/88819727)

type: 提交 commit 的类型, 包括以下几种

+ `feat`: 新功能
+ `fix`: 修复问题
+ `docs`: 修改文档
+ `chore`: 构建工具, 日常事务
+ `style`: 修改代码格式, 不影响代码逻辑
+ `refactor`: 重构代码, 理论上不影响现有功能
+ `perf`: 提升性能
+ `test`: 增加修改测试用例
+ `revert`: 回退, 建议直接使用Github Desktop回退, 而不是使用命令

## git cherry-pick 注意事项

[Stop cherry-picking, start merging, Part 1: The merge conflicts](https://devblogs.microsoft.com/oldnewthing/20180312-00/?p=98215)

如果 在 `dev` 和 `feat` 分支之间使用 `cherry-pick` 功能,
一旦两个分支对 `cherry-pick` 到的 `commit` 中的文件都进行了改动,
那么下次合并的时候, 很容易出现问题.

如果想进行所谓的 `partial merge`

+ 所以要么 cherry-pick 之后完全不进行改动.
+ 要么新拉一个 `patch` 分支进行改动, 然后 同时合并到 `dev` 和 `feat` 分支上.

`ABA` 问题

## git push, git pull error, XML error: not well-formed (invalid token)

["no DAV locking support" errors while pushing to Bitbucket](https://confluence.atlassian.com/bitbucketserverkb/no-dav-locking-support-errors-while-pushing-to-bitbucket-1072486686.html)

在使用 git 命令行时, 报以下错误:

```bash
git push
XML error: not well-formed (invalid token)
error: no DAV locking support on '<https URL>'
fatal: git-http-push failed
error: failed to push some refs to '<https URL>'
```

重启 terminal 可能就好了.
