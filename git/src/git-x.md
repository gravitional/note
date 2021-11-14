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

[初次运行 Git 前的配置 ](https://gitee.com/help/articles/4107)

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

第一个要配置的是你个人的用户名称和电子邮件地址,说明是谁提交了更新, 会随更新内容一起被永久纳入历史记录:

```bash
$ git config --global user.name "John Doe"
$ git config --global user.email johndoe@example.com
```

+ 文本编辑器配置

接下来要设置的是默认使用的文本编辑器. `Git` 需要你输入一些额外消息的时候, 会自动调用一个外部文本编辑器给你用.
如果你有其他偏好, 比如 `Emacs` 的话, 可以重新设置:

```bash
$ git config --global core.editor emacs
```

+ 差异分析工具

```bash
$ git config --global merge.tool vimdiff
```

+ 查看配置信息 : `git config --list`

`Zsh`, 以及一些专门为它打造的完整框架, 比如 `on-my-zsh` , 包含强大的 `Git Tab` 补全功能, 并且提示符主题可以展示版本控制数据.

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

[git scm book](https://git-scm.com/book/zh/v2/), 附录A: 在其他环境中使用 Git - 图形界面

两个常用的图形工具 `gitk ` and `git-gui`, 安装 Github 客户端, 会提供`git-gui`

`GitHub`客户端将许多操作整合成一个功能, 比如点击同步的时候, 实际上会执行

+ `git pull --rebase`.  如果上述命令由于存在合并冲突而失败, 则会退而执行 `git pull --no-rebase`.
+ `git push`

### 在 PowerShell 中使用 Git

[在 Windows 终端中设置 Powerline](https://docs.microsoft.com/en-us/windows/terminal/tutorials/custom-prompt-setup)
[PowerShell 图标 module](https://github.com/devblackops/Terminal-Icons)

适用于 `PowerShell` , 也适用于 Linux or macOS 上运行的 `PowerShell Core` , 只需安装名为 `Posh-Git` 的拓展包.
link: [Posh-Git](https://github.com/dahlbyk/posh-git)

***
安装前提需求(仅限 Windows)

在可以运行 PowerShell 脚本之前, 你需要将本地的 `ExecutionPolicy` 设置为 `RemoteSigned` (可以说是允许除了 `Undefined` 和 `Restricted` 之外的任何内容).
如果你选择了 `AllSigned` 而非 `RemoteSigned` , 那么你的本地脚本还需要数字签名后才能执行.
如果设置为 `RemoteSigned` ,  那么只有 `ZoneIdentifier` 设置为 `Internet`, 即从 `Web` 上下载的脚本才需要签名, 其它则不需要.
如果你是管理员, 想要为本机上的所有用户设置它, 请使用 `-Scope LocalMachine` .  如果你是没有管理权限的普通用户, 可使用 `-Scope CurrentUser` 来只给自己设置.

```powershell
Set-ExecutionPolicy -Scope LocalMachine -ExecutionPolicy RemoteSigned -Force
```

在 `PowerShell 5` 以上, 使用包管理器来安装 `posh-git`.
`Posh-Git` 将 `Git`状态信息添加到提示, 并为 `Git` 命令, 参数, 远程和分支名称添加 `tab` 自动补全.  `Oh-My-Posh` 为 `PowerShell` 提示符提供主题功能

```powershell
Install-Module posh-git -Scope CurrentUser -Force
# Install-Module posh-git -Scope CurrentUser -AllowPrerelease -Force  # 带有 PowerShell Core 支持的更新的 beta 版
Install-Module oh-my-posh -Scope CurrentUser # 安装 oh-my-posh 插件
```

如果使用的是 PowerShell Core, 请安装 `PSReadline` :

```powershell
Install-Module -Name PSReadLine -Scope CurrentUser -Force -SkipPublisherCheck #PSReadline 允许在 PowerShell 中自定义命令行编辑环境.
```

如果你想为所有的用户安装 `posh-git`, 请使用 `-Scope AllUsers` 并在管理员权限启动的 `PowerShell` 控制台中执行.
如果第二条命令执行失败并出现类似 `Module 'PowerShellGet' was not installed by using Install-Module` 这样的错误,  那么你需要先运行另一条命令:

```powershell
Install-Module PowerShellGet -Force -SkipPublisherCheck
```

之后你可以再试一遍. 出现这个错误的原因是 `Windows PowerShell `搭载的模块是以不同的发布证书签名的.

***
要使这些操作生效, 即在你的提示符中包含 Git 信息, 需要导入 `Posh-Git` 模块.
要让 `PowerShell` 在每次启动时都导入 `Posh-Git`, 请执行 `Add-PoshGitToProfile` 命令,  它会在你的 `$profile` 脚本中添加导入语句.
此脚本会在每次打开新的 `PowerShell` 终端时执行.  注意, `$profile` 脚本可能有多个 . 例如, 其中一个是控制台的, 另一个则属于 `ISE`.

```powershell
> Import-Module posh-git
> Add-PoshGitToProfile -AllHosts
```

或者使用 `notepad $PROFILE` 打开 PowerShell 配置文件,该脚本在每次启动 PowerShell 时运行. 在 `PowerShell` 配置文件中, 将以下内容添加到文件的末尾:

```powershell
Import-Module posh-git
Import-Module oh-my-posh
Set-Theme Paradox
```

现在, 每个新实例启动时都会导入 `Posh-Git` 和 `Oh-My-Posh`, 然后从 `Oh-My-Posh` 设置 `Paradox` 主题.  `Oh-My-Posh` 附带了若干内置主题.

You need to close all powershell instances and then run

```powershell
<path-to-pwsh-executable> -noprofile -command "Install-Module PSReadLine -Force -SkipPublisherCheck -AllowPrerelease"
```

Unloading the module doesn't unload the assembly from the PSReadLine module (by design in .NET), hence you need to close all instances.

#### 设置字体

首先从 github 上安装 `Cascadia Code PL`字体, 然后从`Windows 终端`下拉菜单中选择`设置`(`Ctrl+, `)来打开 `settings.json` 文件中的配置文件设置.
找到 Windows PowerShell 配置文件, 添加`"fontFace": "Cascadia Code PL"`到配置中, 添加完的配置文件 `settings.json` 应如下所示:

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

添加到上面的位置, 注意如果不是列表中最后一个, 要加逗号. 这样就将 `Cascadia Code PL` 指定为字体.
这样就会显示很好看的 `Cascadia Code Powerline` 字形.  在编辑器中选择`保存`后, 终端应会即刻显示出变化.

### 服务器上的 Git

如果需要搭建本地`Git` 服务器, 可以参考 [4.8 服务器上的 Git - GitLab](https://git-scm.com/book/zh/v2/) .

虽然 `GitWeb` 相当简单.  但如果你正在寻找一个更现代, 功能更全的 Git 服务器, 这里有几个开源的解决方案可供你选择安装.
因为 `GitLab` 是其中最出名的一个, 我们将它作为示例并讨论它的安装和使用.  这比 `GitWeb` 要复杂的多并且需要更多的维护, 但它的确是一个功能更全的选择.

## 还原文件

### git-restore

还原工作区的文件, 可以用 `--source` 指定例如 `HEAD` .

```git
git restore [<options>] [--source=<tree>] [--staged] [--worktree] [--] <pathspec>...
git restore [<options>] [--source=<tree>] [--staged] [--worktree] --pathspec-from-file=<file> [--pathspec-file-nul]
git restore (-p|--patch) [<options>] [--source=<tree>] [--staged] [--worktree] [--] [<pathspec>...]
```

使用`source`中的某些内容, 还原`工作树`中的`<pathspec>`.
如果某个`路径`已被`git`追踪, 但在`source`中它不存在, 则会删除它以匹配`source`的状态.
还可以使用 `--staged` 选项, 将此命令用于还原`index`中的内容, 
或通过 `--staged --worktree` 同时还原`working tree`和`index`.

+ 使用给定`树对象`中的内容, 还原`working tree`文件.

```git
-s <tree>, --source=<tree>
```

可以通过与之关联的 `commit` , `branch`或 `tag` 来指定 `source tree` .
如果未指定, 则 `working tree` 的默认还原源为 `index` , 而 `index`的默认还原源为 `HEAD` .
当同时指定了 `--staged` 和 `--worktree` 时, 则必须指定 `--source` .

+ 指定要还原的对象: 如果未给出, 默认还原 `working tree` . 

```git
-W, --worktree ; 工作区
-S, --staged ; 暂存区
```

指定 `--staged` 则只还原 `index` , 也可以同时指定两个, 并且都还原. 例子:

```bash
git restore --source master~2 Makefile
# or 
git restore --source=9ea00d1 parton.note.1.nb
```

### git-checkout

切换分支或者恢复 `working tree` 中的文件

```bash
git checkout [<tree-ish>] [--] <pathspec>... ​
```

用 `index`或者 `<tree-ish>` (通常是一个 `commit` )里面的内容替换 `working tree` 里面的 `paths`.
当给出一个 `<tree-ish>` 的时候, 与 `<pathspec>` 匹配的路径会在 `index` 和`working tree` 里面都更新.

`index` 中可能包含有之前合并失败的 `entries` .默认情况下, 如果你想 `checkout` 一个这样的entries, 会失败, 什么都不会发生. 使用 `-f` 选项忽略未合并的项目.

当合并的时候, 特定来源方的内容可以通过使用 `--ours ` or ` --theirs`从 `index` 中取出.

使用 `-m` , 对 `working tree` 所做的更改将会被丢弃, 重新创建冲突的 `merge` 结果.

### git-reset

```bash
git reset [-q] [<tree-ish>] [--] <pathspec>...
git reset [-q] [--pathspec-from-file=<file> [--pathspec-file-nul]] [<tree-ish>]
git reset (--patch | -p) [<tree-ish>] [--] [<pathspec>...]
git reset [--soft | --mixed [-N] | --hard | --merge | --keep] [-q] [<commit>]
```

在前三种形式中, 将 `entries` 从 `<tree-ish>` 复制到 `index` .
在最后一种形式中, 将当前分支头( `HEAD` )设置为 `<commit>` , 可以选择修改 `index` 和 `working tree` 以使其匹配.
` <tree-ish>` / `<commit>`在所有形式中均默认为 `HEAD` .

***
`git reset --hard <commit>` or 别名 `grhh <commit>`

`--hard` 会清空 `working tree` 和 `index` 的改动.
彻底回退版本, 连本地文件都会被回退到上个版本的内容

***
`git reset --soft xxxx` or 别名 `grh --soft <commit>`

保留 `working tree` 和 `index` , 并合并到 `index` 中.
只回退 `commit` , 如果你想再次提交直接 `git commit` 即可.

`reset --soft` 会在重置 `HEAD` 和 `branch` 时, 保留 `working tree` 和 `index` 中的内容,
并把重置 `HEAD` 所带来的新的差异放进 `index` .

***
`reset 不加参数(--mixed)` or 别名 `grh <commit>`

清空 `index` , `mix` 到 `working tree` 中

`reset` 如果不加参数, 那么默认使用 `--mixed` 参数. 它的行为是: 保留 `working tree` , 并且清空 `index` .
也就是说, `working tree`的修改, `index`的内容以及由 `reset` 所导致的新的文件差异, 都会被放进 `working tree` .
简而言之, 就是把所有差异都混合( `mixed` )放在 `working tree` 中`.

***
同理, `reset --hard` 不仅可以撤销提交, 还可以用来把 `HEAD` 和 `branch` 移动到其他的任何地方.

```bash
git reset --hard branch2
```

把 `HEAD` 和 `branch`移动到 `branch2` 指向的提交.

### 三者的区别

有关这三个命令之间的差异, 见["Reset, restore and revert" in git(1)](https://git-scm.com/docs/git#_reset_restore_and_revert). 
有三个名称相似的命令: `git reset`, `git restore`和 `git revert` .

+ [git-revert (1)][]; 将产生新的 `commit` , 新 `commit` 将还原旧 `commit` 所做的更改.
+ [git-restore (1)][]; 用于从 `index` 或某个 `commit` 还原 `working tree` 中的文件.
此命令不会更新您的`分支`.  该命令还可用于从某个 `commit` 还原 `index` 中的文件.
+ [git-reset (1)][]; 用于`更新`某个分支, 移动 分支头(`tip`), 以添加或删除 `commit`s .  此操作将更改 `commit` 历史.

+ `git reset`也可以用来还原 `index` , 与 `git restore` 功能重叠.

[git-revert (1)]: https://git-scm.com/docs/git-revert
[git-restore (1)]: https://git-scm.com/docs/git-restore
[git-reset (1)]: https://git-scm.com/docs/git-reset
There are three commands with similar names: git reset, git restore and git revert.

## git重命名文件夹

不用先在本地修改文件夹名称

文件夹名称: `game`   文件夹修改后名称: `gamesdk`

+ `git mv game gamesdk`
+ `git commit -m 'rename dir game to gamesdk'`
+ `git push origin dev`  ; 推送到 `dev` 分支

ref: [git重命名文件夹](https://www.jianshu.com/p/e886fde18ba0)

## 修改最后一次注释

如果你只想修改最后一次注释(就是最新的一次提交),

`git commit --amend`

## 远程仓库

### 添加ssh 公匙

[SSH 公钥设置 ](https://gitee.com/help/articles/4191#article-header0)

用如下命令来生成 `sshkey`:

```bash
ssh-keygen -t rsa -C "xxxxx@xxxxx.com"
# Generating public/private rsa key pair...
```

注意: 这里的 `xxxxx@xxxxx.com` 只是生成的 `sshkey` 的名称, 并不约束或要求具体命名为某个邮箱.

现网的大部分教程均讲解的使用邮箱生成, 其一开始的初衷仅仅是为了便于辨识所以使用了邮箱
复制生成后的 `ssh key`, 在仓库主页`管理`页面中, 添加生成的 public key 添加到仓库中.

添加后, 在终端中输入下面的命令, 来检测是否能成功连接

```bash
ssh -T git@gitee.com
# -T      Disable pseudo-terminal allocation.
```

首次使用需要确认并添加主机到本机SSH可信列表. 若返回 `Hi XXX! You've successfully authenticated,....` 内容, 则证明添加成功.

添加成功后, 就可以使用SSH协议对仓库进行操作了.

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

先准备两个空的远程仓库, 如果远程仓库里有 `readme` 这样的文件, 先 `pull` 一下, 如果 `pull` 的时候失败, 提示: `fatal: refusing to merge unrelated histories`

那么在进行 `git pull` 时, 添加一个可选项
`git pull origin master --allow-unrelated-histories`

有两种方法

#### git remote add 命令

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

#### git remote set-url 命令

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

查看远程仓库情况. 可以看到 `origin` 远程仓库有两个 `push` 地址. 这种方法的好处是每次只需要 `push` 一次就行了.

```bash
git remote -v
git push origin master:master
```

另外手动更改本地仓库`/.git/config`文件也是可以的, 改成如下格式

```bash
[remote "origin"]
   url = git@github.com:xxx
   url = git@gitee.com:xxx
   fetch = +refs/heads/*:refs/remotes/github/*
```

下面介绍命令语法
***

```bash
git remote get-url [--push] [--all] <name>
git remote set-url [--push] <name> <newurl> [<oldurl>]
git remote set-url --add [--push] <name> <newurl>
git remote set-url --delete [--push] <name> <url>
```

为远程仓库设置新的链接,改变远程 `<name>` 的链接, 可以通过给出 ` <oldurl>` 进一步确认.

`--push `, 设置push URLs 而不是 fetch URLs
`--add`, 不改变已经存在的 URLs, 添加新 URL
`--delete`, 不改变已经存在的 URLs, 删除 `<name>` 上匹配 regex `<url>`的URLs.Trying to delete all non-push URLs is an error.

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

#### 例子

如果你的当前分支设置了`跟踪远程分支`, 那么可以用 `git pull` 命令来自动抓取后合并该远程分支到当前分支

推送工作使用 `git push <remote> <branch>` , 比如`$ git push origin serverfix`

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

## 创建新分支

`git checkout -b|-B <new_branch> [<start point>]`

指定 `-b` 选项, 将会创建新分支, 如同调用`git-branch(1)`, 然后 `checkout` 一样.

在这种情况下, 你可以使用`--track ` or ` --no-track` options, 这些选项会传递给`git branch`
为方便起见, `--track ` without ` -b`意味着创建新分支; 见`--track` 的描述

***
`git checkout`:
`-t, --track`

当创建新分支的时候, 自动设置上游. 如果`-b` 选项没有给出, 本地分支的名字会从 `remote-tracking branch` 推导. `git` 先查看本地中远程的 `refspec` , 然后把前面的初始部分去掉.
也就是说, 如果远程名字是`origin/hack` (or `remotes/origin/hack`, 或者是`refs/remotes/origin/hack`),
新的本地分支就叫做 `hack` , 如果查询到的名称中没有 `slash` (`/`), 或者上面的猜测得到一个空字符串, 那么猜测就会停止,
你可以用 `-b` 选项手动指定一个名字.

***
`git branch`:
`-t`, `--track`

当创建新分支的时候, 设置 `branch.<name>.remote` 和 `branch.<name>.merge` 条目,把 `start-point branch` 当作 `upstream` (上游分支).
这个配置会告诉 `git` , 在`git status ` and ` git branch -v`命令中显示两个分支的关系.而且, 当切换到新分支的时候, 它指导不带参数的 `git pull` 从上游拉取更新.

如果 `start point` 是 `remote-tracking` 分支, 会默认进行上面的设置.
如果你想让`git checkout ` and ` git branch`默认行为是 `--no-track` , 也就是不自动跟踪上游,可以配置变量 `branch.autoSetupMerge` 为`false` .
也可以设置成 `always` , 这样不管 `start-point` 是本地还是远程分支, 都会自动跟踪.

### 常见使用方法

先运行 `checkout -b` 命令创建新分支

`git checkout -b branchname startpoint`

然后用 `push -u` 命令推送到远程

`git push -u origin <refspec>`

第一次推送 `source` 分支的所有内容, 并把本地的 `source` 分支和远程的 `destination` 分支关联起来

***
`git push`:

`<refspec>...`

`<refspec>`指定用 `source object` 更新哪一个 `destination ref` .
`<refspec> `的格式是: 可选的`+`号, 接着一个 `source object <src>` , 然后是`:`,
然后是the `destination ref <dst>`,就是`本地分支:远程分支`的格式,

推送一个空的 `<src>` 相当于删除远程库中的 `<dst> ref` .
特殊的refspec `:` (or `+:` to allow non-fast-forward updates) ,
告诉Git推送匹配的分支: 如果远程库里存在和本地名字一样的分支, 就把本地分支推送过去.

`--all`
推送所有分支(i.e. `refs/heads/`下面的所有ref); 这时候不要再指定其他特定 `<refspec>` .

## git diff

`git-diff` - Show changes between commits, commit and working tree, etc

`commit` 可以用`HEAD~2`的格式,
`HEAD~2`最后的数字`2`指的是显示到倒数第几次, 比如`2`指定倒数第二次

### 语法

SYNOPSIS

```git
git diff [<options>] [<commit>] [--] [<path>... ​]
git diff [<options>] --cached [<commit>] [--] [<path>... ​]
git diff [<options>] <commit> <commit> [--] [<path>... ​]
git diff [<options>] <blob> <blob>
git diff [<options>] --no-index [--] <path> <path>
```

***
working tree v.s. stage
`git diff [--options] [--] [<path>...]`

默认相对于 `index` ( `stage` )的改动.

***
path v.s. path
`git diff --no-index [--options] [--] [<path>...]`

文件系统上的两个 `path` , 如果其中一个不是 `Git` 控制的 `working tree` , 可以不加`--no-index`

***
stage v.s. commit
`git diff [--options] --cached [<commit>] [--] [<path>...]`

比较`staged ` and ` <commit>`, 默认commit 是 HEAD. `--staged ` is a synonym of ` --cached`.

***
commit v.s. working tree
`git diff [--options] <commit> [--] [<path>...]`

比较 `working tree` 相对于 `<commit>` , commit可以是HEAD, 也可以是分支名字, 就是比较 分支的顶端.

***
commit v.s. commit
`git diff [--options] <commit> <commit> [--] [<path>...]`

比较任意两个 `<commit>`, 前一个是base, 后一个是改动

***
`git diff [--options] <commit>..<commit> [--] [<path>...]`

跟上一个相同, 如果有一边的 `<commit>` 省略, 则相当于`HEAD`

***
`git diff [--options] <commit>...<commit> [--] [<path>...]`

查看变化, 从A, B的共同祖先开始, 到B为止, "git diff A...B" 等价于`git diff $(git-merge-base A B) B`

You can omit any one of `<commit>`, which has the same effect as using HEAD instead.

为了避免你写的很奇怪, 注意所有的 `<commit>` , 除了最后两个使用 `..` 记号的, 都可以是任何`<tree>`

更完整的关于拼写 `<commit>` 的方法, 见"SPECIFYING REVISIONS" in gitrevisions(7)
然而, `diff`比较的是两个 endpoints, 而不是一个范围.
所以 `<commit>..<commit> `and ` <commit>...<commit>`在这里指的不是范围.

## 文件恢复

### checkout 还原文件

```bash
git checkout [-q] [-f] [-m] [<branch>]
git checkout [-q] [-f] [-m] --detach [<branch>]
git checkout [-q] [-f] [-m] [--detach] <commit>
git checkout [-q] [-f] [-m] [[-b|-B|--orphan] <new_branch>] [<start_point>]
git checkout [-f|--ours|--theirs|-m|--conflict=<style>] [<tree-ish>] [--] <pathspec>...
git checkout [-f|--ours|--theirs|-m|--conflict=<style>] [<tree-ish>] --pathspec-from-file=<file> [--pathspec-file-nul]
git checkout (-p|--patch) [<tree-ish>] [--] [<pathspec>...]
```

用 `index`或者 `<tree-ish>` (通常是一个 `commit` )里面的内容替换 `working tree` 里面的 `pathspec` (可以有多个指定).
当给出一个 `<tree-ish>` 的时候, the`paths ` that match the ` <pathspec>`会在`index ` and in the ` working tree`里面都更新.

`index`中可能包含有之前合并失败的 `entries` . 默认情况下, 如果你想 `checkout ` 一个这样的entries, 会失败, 什么都不会发生.
使用 `-f` 选项忽略未合并的entries.

The contents from a specific side of the merge can be checked out of the `index ` by using ` --ours ` or ` --theirs`.

With `-m`, changes made to the working tree file can be discarded to re-create the original conflicted merge result.

### detached

如果用 `git checkout <commit>` 切换到某次提交, 那么 `HEAD`不是指向某个指针的(如 `master`, `dev` ), 所以是游离的 -- `detached` ,

如果这个时候进行更改, 并提交, 相当于创建了匿名分支, 所作的提交日后无法再索引到.
它们将被 `git` 的默认回收机制所回收.解决方法是创建新分支:

```bash
git checkout -b new
```

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

## 清理大文件, filter-repo

[Reduce repository size](http://code.ihep.ac.cn/help/user/project/repository/reducing_the_repo_size_using_git.md)

`Git` 仓库会随着时间的推移变得越来越大. 当`大文件`被添加到 `Git` 仓库时:

+ 获取 `repository` 的速度会变慢, 因为大家必须下载这些文件.
+ 它们在服务器上占用了大量的存储空间.
+ `Git` 仓库的存储空间会达到极限.

这样的问题可以用 [git-sizer](https://github.com/github/git-sizer#getting-started) 来检测.
`重写仓库`(rewrite)可以删除不需要的`历史`, 使仓库变小.
我们推荐 [git filter-repo](https://github.com/newren/git-filter-repo/blob/main/README.md), 而不是 `git filter-branch` 和 `BFG`.

>警告. 重写`库历史`是一个破坏性的操作.
>开始之前, 确保备份你的 `repository` . 备份repository的最好方法是[导出项目](http://code.ihep.ac.cn/help/user/project/settings/import_export.md#export-a-project-and-its-data).

### 清除repository历史中的文件

要减少 `GitLab` 仓库的大小, 首先必须从`分支`, `标签`和其他由 `GitLab` 自动创建的`内部引用`(refs)中删除对`大文件`的`引用`.
其他由 `GitLab` 自动创建的`内部引用`(refs). 这些 `refs` 包括

+ `refs/merge-requests/*` ; 用于 `merge ` 请求.
+ `refs/pipelines/*` ; 用于[管道](http://code.ihep.ac.cn/help/ci/troubleshooting.md#fatal-reference-is-not-a-tree-error).
+ `refs/environments/*` ; 用于环境.
+ `refs/keep-around/*` 被创建为`隐藏的 refs`, 以防止`数据库`中引用的`提交`被删除.

这些 `refs` 不会被自动下载, 隐藏 `refs` 也不会被公布, 但我们可以通过`项目导出`来删除这些 `refs`.
要从 `GitLab` 仓库中清除文件:

+ 使用包管理器, [安装git filter-repo](https://github.com/newren/git-filter-repo/blob/main/INSTALL.md)
或从`源代码`安装.
+ [从项目生成一个新的导出](http://code.ihep.ac.cn/help/user/project/settings/import_export.html#export-a-project-and-its-data), 并下载它.
这个项目导出包含了你的`仓库`和 `refs` 的备份, 我们可以用它来清除 `repository` 中的`文件`.
    + 完整的项目 `export` 功能仅限于项目`维护者`和`所有者`. 你可以通过`项目设置`来配置这种功能. 要导出一个项目及其数据, 请遵循以下步骤.
    + 转到你的项目主页.
    + 点击侧边栏的设置.
    + 向下滚动, 找到导出项目的按钮.
    + 文件可用后, 页面将显示下载导出按钮.
+ 使用 `tar` 解压备份.

   ```bash
   tar xzf project-backup.tar.gz
   ```

解压结果会包含一个 `project.bundle`文件, 它是由 [git bundle](https://git-scm.com/docs/git-bundle) 创建的.

+ 新建一个目录例, 从 `bundle` 克隆一个新鲜的 `repository ` .

   ```bash
   mkdir repofilter && cd repofilter # 新建目录
   git clone <project.bundle的路径>
   ```

+ 使用 `git filter-repo`, 清除 `repository` 历史中的`任何文件`.
因为我们正在试图删除`内部 refs`, 我们依靠每次运行产生的 `commit-map` (提交图)来告诉我们哪些`内部 refs`需要删除.
   > 注意. `git filter-repo` 每次运行都会创建一个新的 `commit-map` 文件, 并覆盖上一次运行产生的 `commit-map`.
   >  You need this file from every run. 请在每次运行 `git filter-repo` 时都要做下一步.

+ 要清除所有大于 `10M` 的文件, 可以使用 `--strip-blobs-bigger-than` 选项.

   ```bash
   git filter-repo --strip-blobs-bigger-than 10M
   ```

+ 要清除`特定路径`的大文件, 需要结合使用` --path` 和 `--invert-paths` 选项, `--invert-paths` 表示`反向选择`:

   ```bash
   git filter-repo --invert-paths --path 大文件路径.m4v
   ```

   参见[git filter-repo 文档](https://htmlpreview.github.io/?https://github.com/newren/git-filter-repo/blob/docs/html/git-filter-repo.html#EXAMPLES), 获取更多的例子和完整的文档.
+ 因为从 `bundle` 文件克隆会将 `origin remote` 设置为本地 `bundle` 文件, 所以删除这个 `origin remote` , 并将其设置为你的仓库的 `URL`.

   ```bash
   git remote show  # 查看现有的远程仓库
   git remote remove origin # 移除现有的远程仓库
   git remote add origin https://gitlab.example.com/<命名空间>/<项目名称>.git
   ```

+ 强制推送你的改动以覆盖 `GitLab` 上的所有分支.

   ```bash
   git push origin --force 'refs/heads/*'
   ```

   [受保护的分支](http://code.ihep.ac.cn/help/user/project/protected_branches.md)会导致推送失败.
   要继续, 您必须`移除`分支保护, `推送`, 然后再重新启用`受保护的分支`.
   在侧边栏: `settings->Repository->Protected branches, Protected tags `, 展开之后选择 `unprotect` .

   从 `tagged releases` 中删除`大文件`, 请强制推送您的修改到 `GitLab` 上的所有 `tags ` .

   ```bash
   git push origin --force 'refs/tags/*'
   ```

   [受保护的标签](http://code.ihep.ac.cn/help/user/project/protected_tags.md)会导致推送失败.
   要继续, 你必须`移除`标签保护, 然后`推送`, 再重新启用`受保护的标签`.

   为了防止产生指向不存在`提交`的 `dead links` , 推送由 `git filter-repo` 创建的 `refs/replace`.

   ```bash
   git push origin --force 'refs/replace/*'
   ```

关于如何操作, 请参考 [Git replace 文档](https://git-scm.com/book/en/v2/Git-Tools-Replace).

+ 强制推送这些操作合并起来就是:

```bash
git push origin --force 'refs/heads/*'
git push origin --force 'refs/tags/*'
git push origin --force 'refs/replace/*'
```

### 更新其他的clone

在过滤存储库, 并重写提交历史后, 将更改`强制推送`到远程服务器之后.
现在要更新该`存储库`的每一份 `clone` , 仅靠常用的 `pull` 是无法做到这一点的.

从远程服务器获取存储库, 再使用 `git reset` 将 `HEAD` 移动到 `origin/master`.

```bash
# git 会自动检测到远程服务器进行了 force update
git fetch origin
git reset --hard origin/master
# 和上面的一样, 需要删除旧提交, 清理本地仓库
git for-each-ref --format='delete %(refname)' refs/origin | git update-ref --stdin
git reflog expire --expire=now --all
git gc --prune=now
```

+ 运行[仓库清理](http://code.ihep.ac.cn/help/user/project/repository/reducing_the_repo_size_using_git.md#repository-cleanup).

### 仓库清理

引入于 `GitLab 11.6`.

`仓库清理`允许你上传一个包含`对象列表`的文本文件, `GitLab` 将删除 内部 Git 对这些对象的`引用`.
你可以使用 `git filter-repo` 来生成一个`对象列表`(在一个commit-map文件), 可以用来进行仓库清理.

引入于GitLab 13.6.  安全地清理仓库, 需要在操作期间将其变成`只读`.
这将自动进行,  提交清理请求时, 但如果有任何写操作正在进行, `清理请求`就会失败,
所以在继续之前要取消任何未完成的 `git push` 操作.
要清理一个 `repository`.

+ 转到该 `repository` 的项目.
+ 导航到 `settings > repository` .
+ 上传`对象列表`. 例如, 一个由 `git filter-repo` 创建的 `commit-map` 文件, 它位于 `filter-repo` 目录.
+ 如果你的 `commit-map` 文件大于 `250KB` 或 `3000 行`, 可以将该文件拆分并逐块上传.

   ```bash
   split -l 3000 filter-repo/commit-map filter-repo/commit-map-
   ```

+ 点击`开始清理`.

这样.

+ 删除任何内部 `Git` 对`旧提交`的`引用`.
+ 对仓库运行 `git gc --prune=30.minutes.against`, 以`删除未引用的对象`.
+ 暂时重新打包你的仓库, 导致你的仓库的大小显著增加, 因为旧的打包文件不会被`删除`, 直到新的包文件被创建之前, 旧的包文件不会被删除.
+ 解除连接到你的项目的任何未使用的 `LFS` 对象的`链接`, 释放出存储空间. 重新计算磁盘上存储库的大小.

清理工作完成后, `GitLab` 会发送一封电子邮件通知, 告知重新计算的仓库大小.

如果仓库的大小没有减少, 这可能是由于`松散的对象`被保留下来, 因为它们被引用了.
也就是在过去30分钟内发生的 `Git` 操作中, 引用了这些`松散的对象`
试着在仓库休眠至少30分钟后重新运行这些步骤. 当使用仓库清理时, 注意.

+ 项目的统计数据是`缓存的`. 你可能需要等待 `5-10` 分钟才能看到存储利用率的降低.
+ 清理工作会删去超过`30分钟的松散对象`. 这意味着在过去30分钟内添加或引用的对象不会被立即删除.
如果你有机会进入Gitaly服务器, 你可以忽略这个延迟, 运行 `git gc --prune=now` 来立即修剪所有松散的对象.
+ 这个过程会从`GitLab的缓存`和`数据库`中删除一些重写的提交的副本.
但在覆盖范围上仍有许多空白, 有些副本可能会无限期地存在.
`清除实例缓存`可能有助于消除其中一些副本, 但为了安全起见, 不应依赖这种方法.

## git-filter-repo

      git filter-repo --analyze
      git filter-repo [<path_filtering_options>]
      [<content_filtering_options>]
           [<ref_renaming_options>]
           [<commit_message_filtering_options>]
           [<name_or_email_filtering_options>]
           [<parent_rewriting_options>]
           [<generic_callback_options>] [<miscellaneous_options>]

### 描述

使用用户指定的 `filters` 快速重写整个`版本库`历史.
这是一个破坏性的操作, 不能轻易使用; 它写入新的`提交`, `树`, `标签 `和 ` blobs`, 对应于版本库中的原始对象, 但经过`过滤`, 然后删除`原始历史`, 只留下新的.
关于使用这个工具的更多细节, 请参见 `[DISCUSSION]` . 可以进行多种不同类型的`历史重写`; 例子包括(但不限于).

+ 剥离大文件(或大目录或大扩展名).
+ 按`路径`剥离(stripping)不需要的文件
+ `提取`想要的路径和它们的历史(剥离其他的东西)
+ 重组`文件布局`(比如将所有文件移到一个子目录中, 准备与另一个 `repo` 合并, 使一个子目录成为新的顶层目录, 或者将两个有独立文件名的目录合并到一个目录中).
+ 重命名 `tags` (通常也是为了准备与另一个 `repo` 合并).
+ 替换或删除敏感文本, 如密码
+ 永久性的重写`用户名`或`电子邮件`的`mailmap`
+ 永久性的`移植`或替换`refs`
+ 重写`提交`信息

此外, 有几个问题会被`自动处理`(其中许多选项可以被`覆盖`, 但它们都是`默认开启`的).

+ 重写`提交`信息中的`哈希值`(可能是缩写的), 指向`重写后`的`新提交`的哈希值
+ `修剪`(pruning)`空提交`, 产生自使用`过滤器`(也处理边缘情况, 如修剪之后变成`退化`和`空`的`合并提交`).
+ 为旧的`commit 哈希`创建 `replace-refs` , 见 [git-replace(1)](https://htmlpreview.github.io/?https://raw.githubusercontent.com/newren/git-filter-repo/docs/html/git-replace.html).
如果手动 `pushed` 和 `fetched` , 将允许用户继续使用(非缩写的)`旧的提交ID`来引用`新的提交`
+ `剥离`(stripping)`原历史`, 以避免新旧历史的混合
+ `rewrite`后, `重新打包`版本库, 为用户缩小版本库的体积

另外, 值得注意的是, 有一个重要的`安全机制`.

+ 如果从不是新鲜克隆(fresh clone)的 `repo` 中运行, 则`中止`(abort, 以防止, 因改写不存在于其他地方的`本地历史`, 而意外丢失数据). 见`[FRESHCLONE]`.
+ 对于那些知道他们的`历史`中有大量不需要的东西, 并希望帮助找到它的人,
也提供了一个`选项`来 `analyze` 版本库并生成报告, 这对决定过滤什么很有用(或者决定一个单独的过滤命令是否成功).

参见 `[VERSATILITY]`, `[DISCUSSION]`, `[EXAMPLES]`, 和 `[INTERNALS]`.

### 选项

`--analyze` ;  分析 `repository` 历史, 并创建一份报告, 对于确定在后续运行中过滤什么可能很有用, 或确定之前的过滤命令是否达到了你想要的效果. 不会修改你的 `repo`.

基于`路径`的过滤(参见-filename-callback)

+ `--invert-paths ` ; 反向选择下面指定的`--path-{match,glob,regex}`选项中的文件, 即只选择与这些`选项`都`不匹配`的文件.
+ `--path-match   <dir_or_file> <dir_or_file>.`
+ `--path <dir_or_file> `; 确切的路径(文件或目录), 将被包括在`过滤历史`中. 可以指定多个 `--path` 选项来获得一个路径的并集(union).
+ `--path-glob <glob>` ; 要包含在`过滤的历史`的`glob 路径`(也就是 shell 展开). 可以指定多个`--path-glob` 选项来获得路径的并集.
+ `--path-regex <regex>`; 包含在`过滤的历史 `的 ` Regex` 路径.可以指定多个 `--path-regex` 选项来获得路径的联合.
+ `--use-base-name ` ; 匹配文件基本名称(basename), 而不是从 `repo` 顶部开始的全路径. 与 `--path-rename` 不兼容, 与即于`目录名`的匹配不兼容.

### 例子

只保留 `README.md` 文件, 以及 `guards` 和 `tools/releases/` 目录.

```bash
git filter-repo --path README.md --path guides/ --path tools/releases
```

目录名带不带`尾部斜杠`都可以, 所有文件名都相对于 `repo` 的顶层. 要进行反向选择, 保留这些`路径以外`的所有文件, 只需添加 `--invert-paths`.

```bash
git filter-repo --path README.md --path guides/ --path tools/releases --invert-paths
```

如果你想同时指定一个 `inclusion` 过滤器和一个 `exclusion` 过滤器, 只需多次运行 `filter-repo`.
例如, 要保留 `src/main` 子目录, 但排除 `src/main` 下名为 `data` 的文件, 运行

```bash
git filter-repo --path src/main/
git filter-repo --path-glob 'src/*/data' --invert-paths
```

注意, 星号(`*`)会匹配多层`目录`, 所以第二条命令会`删除`例如 `src/main/org/whatever/data`.
另外, 第二条命令本身也会删除例如 `src/not-main/foo/data`, 但由于 `src/not-main/` 已经被第一条命令`删除`, 所以这不是问题.
另外, 在`星号`周围使用`引号`有时很重要, 可以避免 `shell` 进行 `glob展开`.

你也可以通过`正则表达式`来选择路径, 见[python3-regex](https://docs.python.org/3/library/re.html#regular-expression-syntax).
例如, 只包括 `repo` 中名称为 `YYYY-MM-DD.txt`, 并且至少在两层深度的, 子目录中的文件.

```bash
git filter-repo --path-regex '^.*/.*/[0-9]{4}-[0-9]{2}-[0-9]{2}.txt$'
```

如果你想对两个`目录`进行`重命名`(如果两个目录被`重命名`到相同的`位置`, 也许会`合并`),
使用 `--path-rename`; 例如, 将 `cmds/` 和 `src/scripts/` 都重命名为 `tools/`.

```bash
git filter-repo --path-rename cmds:tools --path-rename src/scripts/:tools/
```

与 `--path` 一样, 在 `--path-rename` 中指定的目录写不写`尾部斜线`都可以.

如果你对使用`--path-rename` 重命名到已存在的文件, 它将被静默地`覆盖`.
然而, 如果你试图将多个`文件`重命名到`同一位置`(例如 `src/scripts/run_release.sh` 和 `cmds/run_release.sh` 都存在, 但其中内容不相同),
那么你会得到一个`错误`. 如果遇到这样的情况, 你可能想添加另一条`重命名`命令, 把其中一个`路径`移到其他地方, 使其不会发生冲突.

```bash
git filter-repo --path-rename cmds/run_release.sh:tools/do_release.sh \
                --path-rename cmds/:tools/ \
                --path-rename src/scripts/:tools/
```

另外, `--path-rename` 带来了`排序问题`; 所有的`路径参数`都是按`顺序`应用的. 因此, 类似于

```bash
git filter-repo --path-rename sources/:src/main/ --path src/main/
```

这样的命令是有意义的, 但将两个`参数`颠倒过来就没有意义了( `src/main/` 是由`重命名`创建的, 所以将这两个`参数`颠倒过来会给你一个空 `repo`).
另外, 请注意, 几个例子中 `cmds/run_release.sh` 的重命名是在其他重命名之前完成的.

注意, `路径重命名`并不做`路径过滤`, 因此以下命令

```bash
git filter-repo --path src/main/ --path-rename tools/:scripts/
```

将不会选中 `tools` 或 `scripts` 目录, 让它们出现在结果中, 因为单个`过滤器`只选择了 `src/main/`. 你想运行的其实是,

```bash
git filter-repo --path src/main/ --path tools/ --path-rename tools/:scripts/
```

如果你喜欢只根据 `basename` 来过滤, 可以使用 `--use-base-name` 标志(flag, 不过这与 `--path-rename` 不兼容).
例如, 只选中在任何目录下的 `README.md` 和 `Makefile` 文件.

```bash
git filter-repo --use-base-name --path README.md --path Makefile
```

如果您想删除任何目录下的所有 `.DS_Store` 文件, 您可以使用:

```bash
git filter-repo --invert-paths --path '.DS_Store' --use-base-name # 或
git filter-repo --invert-paths --path-glob '*/.DS_Store' --path '.DS_Store'
```

`--path-glob`本身并不足够, 因为它可能会漏掉顶层的 `.DS_Store`文件;
此外, 虽然像 `--path-glob '*.DS_Store'`这样的写法可以解决这个问题, 但它也会抓取名为 `foo.DS_Store` 或 `bar/baz.DS_Store` 的文件)

最后, 也可以参考`[CALLBACKS]`中的 `--filename-callback`.

### 过滤多个文件

如果你有一长串要过滤的`文件`, `目录`, `globs` 或 `正则表达式`, 你可以把它们放在一个`文件`里, 然后使用 `--paths-from-file`;
例如, 有一个名为 `stuff-i-want.txt` 的文件, 内容为

```bash
# 空白行和注释行被忽略.
# 类似于 --path: 的例子.
README.md
guides/
tools/releases

# 类似于 --path-glob 的例子.
glob:*.py

# 类似于 --path-regex 的例子.
regex:^.*/.*/[0-9]{4}-[0-9]{2}-[0-9]{2}.txt$

# 重命名路径的例子
tools/==>scripts/

# 使用 regex 来重命名路径的例子
regex:(.*)/([^/]*)/([^/]*)\.text$==>\2/\1/\3.txt
```

那么你可以运行

```bash
git filter-repo --paths-from-file stuff-i-want.txt
```

来得到一个只包含顶层 `README.md` 文件, `guidelines/` 和 `tools/releases/` 目录, 所有 `python` 文件,
名称为 `YYY-MM-DD.txt` 且目录深度至少两层的文件的 `repo`, 并将 `tools/` 重命名为 `scripts/`, 将 `foo/bar/baz.text` 等文件重命名为 `bar/foo/baz.txt`.
注意特殊行前缀 `glob: `和 ` regex:`, 以及表示`重命名`的特殊字符串 `==>`.

有时, 你可以轻松地生你想保留的文件的列表.
例如, 如果您知道当前`追踪`(tracked)的文件中没有任何`换行符`或`特殊字符`(参见 `git config --help` 中的 `core.quotePath`),
那么 `git ls-files` 就会`逐行`打印所有文件, 并且您知道您只想保留当前追踪的文件,
即从`历史`的所有`提交`中删除, 任何只出现在其他`分支`或只出现在`较早提交`中的文件,
那么您可以使用一对命令, 如

```bash
git ls-files >../paths-i-want.txt
git filter-repo --paths-from-file ../paths-i-want.txt
```

同样地, 您可以使用 `--paths-from-file` 来删除大量文件.
例如, 你可以运行 `git filter-repo --analyze`来获得报告, 在其中查找, 比如 `.git/filter-repo/analysis/path-deleted-sizes.txt`,
并将所有的文件名复制到一个文件, 比如 `/tmp/files-i-dont-want-anymore.txt`, 然后运行

```bash
git filter-repo --invert-paths --paths-from-file /tmp/files-i-dont-want-anymore.txt
```

### 与目录相关的shortcuts

假设你有一个像下面这样的`目录结构`.

```conf
module/
   foo.c
   bar.c
otherDir/
   blah.config
   stuff.txt
zebra.jpg
```

如果你只想要`module/`目录, 并且你想让它成为新的`根目录`(root), 这样你的新目录结构看起来像

```conf
foo.c
bar.c
```

那么你可以运行

```bash
git filter-repo --subdirectory-filter module/
```

如果你想要原来 `repo` 中的所有文件, 但想把所有文件移到一个名为`my-module/`的子目录下, 这样你的`新目录结构`看起来就像

```conf
my-module/
   module/
      foo.c
      bar.c
   otherDir/
      blah.config
      stuff.txt
   zebra.jpg
```

那么你可以改用运行

```bash
git filter-repo --to-subdirectory-filter my-module/
```

### fresh clone 安全检查和 --force

由于 `filter-repo` 会对历史进行`不可逆`的重写, 因此必须避免对用户`没有良好备份 `的 ` repo` 进行修改.
主要的`防御机制`是简单地教导用户, 并依靠他们成为数据的好管家; 因此在文档中对 `filter repo` 如何重写历史有几个警告.

然而, 作为对用户的一项服务, 我们想在`文档`之外提供额外的`安全检查`.
很难找到好方法来检查用户是否具有良好的备份, 但我们可以问一个`相关的问题`, 这是一个不完美但相当合理的代理: `这个版本库是一个新的克隆吗?`
不幸的是, 这也是一个我们无法得到`完美答案`的问题; `git` 没有提供回答这个问题的方法.
然而, 我发现大约有`一打东西`, 对于全新的克隆, 似乎总是成立的 (假设它们要么是`远程仓库`的克隆, 要么是用 `--no-local `标志制作的), 我检查所有这些东西.

这些检查可能有`假阳性`和`假阴性`之分. 有人可能有一个很好的备份, 但实际上它并不是一个`新的克隆`--但 `filter-repo`没有办法知道这点.
相反, 有人可以查看 `filter-repo` 在`安全检查`中, 检查的所有东西, 然后`调整`他们`没有备份`的版本库以满足这些条件.
(尽管这需要相当大的努力, 而且一个不是`新鲜克隆`的`版本库`, 随机地符合`所有标准`的可能性是非常小的).
在实践中,  `filter-repo` 使用的安全检查, 似乎很好地避免了人们意外地在不适用的版本库上运行 ` filter-repo`.
甚至有一次, 我确实想在错误的目录运行 `filter-repo`, 它阻止了我.

简而言之, 只要你同意 `filter-repo 不可逆地`改写当前版本库的内容, 使用 `--force` 来覆盖`安全检查`是完全可以的.
养成总是指定 `--force` 的习惯是个坏主意; 如果你这样做了, 有一天你会像我一样在`错误的目录下`运行你的某个命令, 你就不会再有`安全检查`来救你了.

此外, 在论坛, Q&A 网站或给其他用户的电子邮件中推荐 `--force` , 而不首先仔细解释 `--force` 意味着将你的存储库数据置于危险之中, 这是绝对不行的.
我对那些在明显不需要的情况下, 建议使用该标志的人感到特别困扰; 他们毫无必要地将其他人的数据置于危险之中.

## zsh 定义的 git别名

***
`--depth <depth>`:

创建一个浅表克隆, 其历史记录将被截断为指定的提交数. 暗示使用了 `--single-branch` , 除非给出 `--no-single-branc` 来获取所有分支的tip附近的历史记录.
如果要浅层克隆 `--no-single-branc` , 则还要传递 `--shallow-submodules` .

***
`-C <path>`:
将 `git` 的起始目录设置成 `<paht>` .  给定多个 `-C` 选项时,  后面每个不是绝对路径的指定, 将和前面的连接起来.
如果 `<path>` 存在但为空, 例如 `-C` , 则当前工作目录保持不变.

### short

+ `gcl`='git clone --recurse-submodules', `git clone  <仓库> [本地目录]`, 也就是后面可以跟上本地目录位置.

`git clone -b <name>`; 克隆之后, 指向 `<name>` 分支, 如 `release` .

```bash
gst='git status'
gaa='git add --all'
gcam='git commit -a -m'

gco='git checkout'
gb='git branch'
gcb='git checkout -b'

gp='git push'
gpd='git push --dry-run'
gpoat='git push origin --all && git push origin --tags'
ggpull='git pull origin "$(git_current_branch)"'
gf='git fetch'
gl='git pull'

gd='git diff'
gdw='git diff --word-diff'
```

### 查看状态

`git status -s` ; -s forshort

+ `gss`='git status -s'
+ `gst`='git status'

### branch

+ `gb`='git branch'
+ `gbD`='git branch -D'
+ `gba`='git branch -a'
+ `gbd`='git branch -d'
+ `gbr`='git branch --remote'

选项:

+ `-D`: 与 `--delete --force`相同.
+ `-d, --delete` ;删除分支. 该分支必须完全被合并到上游, 如果没有使用 `--track` 或 `--set-upstream-to` 设置上游, or in `HEAD`.
+ `-f, --force`: 将 `<branchname>` 重置为 `<startpoint>`, 即使 `<branchname>` 已经存在.  如果没有 `-f`, `git branch` 将拒绝更改现有分支.
结合 `-d` (或 `--delete` ), 允许删除分支, 而不管其合并状态如何.
结合 `-m` (或 `--move` ), 即使新分支名称已经存在, 也允许重命名分支, 同样适用于 `-c` (或 `--copy` ).
+ `-m` `-M`: 对分支进行重命名, 并且把 `reflog` 出现的分支名字一并更改. 如果新分支已经存在, 使用 `-M` 强迫重命名
+ `-r`, ` --remotes` 列出或删除(与 `-d` 一起使用)远程跟踪分支.

### add

+ `ga`='git add'
+ `gaa`='git add --all'
+ `gapa`='git add --patch'
+ `gau`='git add --update'
+ `gav`='git add --verbose'

选项:

+ `-p`, `--patch`: 交互式地选择更新的内容. 能够使用户在增加文件前查看与 `index` 的不同.
+ `-u`, ` --update` : 更新 `index` 中匹配 `working tree`的文件. 移除相比 `working tree` 多余的, 但是不会增加新的文件. 如果没有给出具体的 `<pathspec>` , `working tree`中所有被追踪的文件都会被更新, 下同.
+ `-A`, `--all` , `--no-ignore-removal` : 添加, 修改, 删除 `index entries` , 使之完全匹配 `working tree` .

### commit

+ `gc`='git commit -v'
+ `'gc!'`='git commit -v --amend'
+ `gca`='git commit -v -a'
+ `'gca!'`='git commit -v -a --amend'
+ `gcam`='git commit -a -m'

+ `gcs`='git commit -S'
+ `gcsm`='git commit -s -m'

选项:

+ `-a`, `--all`: 自动 `stage` 所有被修改或删除的文件, 但是还没有被 `Git` 追踪的文件不受影响. 也就是跳过使用 `index` .
+ `-v`, `--verbose`: 在提交信息的尾部, 展示 `HEAD` 和将要提交 `commit` 的 `diff` . 这个 `diff` 的输出行没有前置的`#`. 并且不是提交信息的一部分. See the commit.verbose configuration variable in git-config(1).
如果使用两次,i.e. `-vv` , 则额外展示 `working tree` 和 `next commit` 的区别
+ `--amend`: 创造一个新的 `commit` , 代替当前分支的 `tip` . 提交信息基于上次的 `commit` .
+ `-m`: 添加提交信息, 可以给出多个 `-m` , 会被当作多个段落被合并.
+ `-s`, `-S` :签名相关

### checkout cherry-pick

+ `gcb`='git checkout -b'
+ `gco`='git checkout'
+ `gcp`='git cherry-pick'
+ `gcpa`='git cherry-pick --abort'
+ `gcpc`='git cherry-pick --continue'

选项:

+ `git checkout -b|-B <new_branch> [<start point>]`:
指定 `-b` 选项会创建新分支, 如同调用了 `git branch` 一样, 然后check out到新分支一样.
可以使用`--track ` or ` --no-track`选项, 它们会被传递给 `git branch` .
为了方便起见, `--track ` without ` -b`意味着创建新分支.
如果给的是 `-B` , 新分支会被创建, 或者直接 `reset` 已存在的分支,
相当于`git branch -f <branch> [<start point>] ; git checkout <branch>`

`git-cherry-pick` :从已经存在的一系列 `commits` 中应用改变

给出一个或者多个已经存在的 `commits` , 然后 `apply` 每个的 `change` , 对于每个改变生成一个 `commit` .
需要 `working tree` 是 `clean` 的.  (从 HEAD commit 之后没有修改过).

选项:

`--abort`: 取消操作, 回复到pre-sequence 状态.
`--continue`: 继续操作, 利用`.git/sequencer.`中的信息. 可以在`cherry-pick ` or ` revert`失败, 解决冲突之后使用.

### gitk & log

***

1. `git-shortlog` - 总结 `git log` 的输出.

选项:

`-n`, `--numbered`:对输出结果进行排序, 按照每个提交者的提交数量, 而不是字母顺序.
`-s`, `--summary`: 压缩 `commit` 描述, 只总结 `commit` 数量.

***
` gitk [<options>] [<revision range>] [--] [<path>...]`

2. `gk`='\gitk --all --branches'
3. `gke`='\gitk --all $(git log -g --pretty=%h)'

`--all`:把`refs/`下的所有条目, 包括 `HEAD` 都用 `<commit>`的形式列出
`--branches[=<pattern>]`: 类似 `--all` , 但是要匹配 `shell glob` 模式, `?`, `*`, or `[`, `/*`
`--tags[=<pattern>]`: 类似`--branches`

`gitk`可以查看单个文件的提交历史, 使用`gitk filepath`

### restore

+ `grs`='git restore'
+ `grss`='git restore --source'
+ `grst`='git restore --staged'

### remote

`git remote add 远程仓库简称 <url>`

+ `gr`='git remote'
+ `gra`='git remote add'
+ `grmv`='git remote rename'
+ `grrm`='git remote remove'
+ `grset`='git remote set-url'
+ `grup`='git remote update'
+ `grv`='git remote -v'

### rebase

假如我们现在在 `topic` 分支上:

```diagram
       A---B---C topic
      /
 D---E---F---G master
```

运行下面任意一个指令

```bash
git rebase master
git rebase master topic
```

将会变成

```diagram
              A'--B'--C' topic
             /
D---E---F---G master
```

***
--onto 用法
假设 `topic` 基于`next`

```diagram
 o---o---o---o---o  master
      \
       o---o---o---o---o  next
                        \
                         o---o---o  topic
```

我们想把 `topic` 移动到 `master` 分支上, 最后想得到下面这个图

```diagram
o---o---o---o---o  master
    |            \
    |             o'--o'--o'  topic
     \
      o---o---o---o---o  next
```

可以使用下面的命令

```bash
git rebase --onto master next topic
```

也就是说, rebase 这个运算是向左进行的, `topic - next`, 然后应用到 `master` 上.

### push

+ `gp`='git push'
+ `gpd`='git push --dry-run'
+ `gpoat`='git push origin --all && git push origin --tags'
+ `'gpf!'`='git push --force'
+ `gpf`='git push --force-with-lease'

选项:

+ `-n`, `--dry-run`: 模拟运行所有步骤, 但不实际发送更新.
+ `--all`: Push all branches (i.e. refs under `refs/heads/`); cannot be used with other `<refspec>`.
+ ` --prune`: 删除远程分支, 如果它没有local对应.
+ `--force-with-lease` 单独使用,不指定细节, 将会保护所有远程分支, 如果远程分支的名字和remote-tracking branch 一样才更新.
+ `-f`, `--force`: 通常, 远程分支是本地分支祖先的时候, 才会更新, 并且名字需要和期望的一样. `-f`选项禁用这些检查, 可能会使远程库丢失 `commit` , 小心使用.

### fetch

+ `gf`='git fetch'
+ `gfa`='git fetch --all --prune'
+ `gfo`='git fetch origin'

选项:

`--all`: Fetch 所有`remote`
`--prune`: Before fetching, remove any remote-tracking references, 如果它们在远程上已经不存在.

### pull

+ `ggpull`='git pull origin "$(git_current_branch)"'
+ `gl`='git pull'
+ `gup`='git pull --rebase'

选项:

1. `--all`: fetch all remotes.
2. `-r`,` --rebase[=false|true|preserve|interactive]`:
当设置为 `true` 时, `rebase`当前分支on top of the upstream branch after fetching.
如果某一 `remote-tracking branch` 对应的 `upstream` 在上次 `fetch` 之后 `rebase` 过, `rebase`使用那些信息避免 `rebase` 非本地的改变.

### diff

+ `gd`='git diff'
+ `gdca`='git diff --cached'
+ `gdcw`='git diff --cached --word-diff'
+ `gds`='git diff --staged'
+ `gdw`='git diff --word-diff'

选项:

1. `--color[=<when>]`: 展示着色的diff.
` --color` (i.e. `without =<when> `) is the same as ` -color=always`.
` <when> ` can be one of ` always`, `never`, or `auto`

2. `--word-diff[=<mode>]`: Show a word diff, 使用 `<mode>` 定界改变的 `words` .
默认的定界符是 `whitespace` ,参见下面的 `--word-diff-regex` .
`<mode>`默认是 `plain` , 可以是以下之一:

+ `color`: Highlight changed words using only colors. Implies --color.
+ `plain `: Show words as ` [-removed-] ` and ` {+added+}`. 不尝试 `escape` 定界符号, 如果它们出现在input中, 所以可能有歧义.
+ `porcelain`: 使用一种特殊的line-based格式for script consumption.
Added/removed/unchanged runs are printed in the usual unified diff format,
starting with a `+/-/ ` character at the beginning of the line and extending to the end of the line. Newlines in the input are represented by a tilde ` ~` on a line of its own.
+ `none`: Disable word diff again.

注意: 不管使用哪个模式, 都会使用颜色标示改变, 如果可用的话.

### others

+ `gpsup`='git push --set-upstream origin $(git_current_branch)'
+ `gpu`='git push upstream'
+ `gpv`='git push -v'
+ `ggpush`='git push origin "$(git_current_branch)"'
+ `gupa`='git pull --rebase --autostash'
+ `gupav`='git pull --rebase --autostash -v'
+ `gupv`='git pull --rebase -v'

+ `gm`='git merge'
+ `gma`='git merge --abort'
+ `gmom`='git merge origin/$(git_main_branch)'
+ `gmt`='git mergetool --no-prompt'
+ `gmtvim`='git mergetool --no-prompt --tool=vimdiff'
+ `gmum`='git merge upstream/$(git_main_branch)'

+ `gpristine`='git reset --hard && git clean -dffx'
+ `grh`='git reset'
+ `grhh`='git reset --hard'
+ `groh`='git reset origin/$(git_current_branch) --hard'
+ `grev`='git revert'

+ `grb`='git rebase'
+ `grba`='git rebase --abort'
+ `grbc`='git rebase --continue'
+ `grbd`='git rebase develop'
+ `grbi`='git rebase -i'
+ `grbm`='git rebase $(git_main_branch)'
+ `grbs`='git rebase --skip'

+ `gr`='git remote'
+ `gra`='git remote add'
+ `grmv`='git remote rename'
+ `grrm`='git remote remove'

+ `gsta`='git stash push'
+ `gstaa`='git stash apply'
+ `gstall`='git stash --all'
+ `gstc`='git stash clear'
+ `gstd`='git stash drop'
+ `gstl`='git stash list'
+ `gstp`='git stash pop'
+ `gsts`='git stash show --text'
+ `gstu`='git stash --include-untracked'

+ `grm`='git rm'
+ `grmc`='git rm --cached'
+ `git-gc` Cleanup unnecessary files and optimize the local repository

## hosts

```hosts
127.0.0.1       localhost
127.0.1.1       OP7050

# The following lines are desirable for IPv6 capable hosts
::1     ip6-localhost ip6-loopback
fe00::0 ip6-localnet
ff00::0 ip6-mcastprefix
ff02::1 ip6-allnodes
ff02::2 ip6-allrouters

# GitHub Start
13.250.177.223    github.com
13.250.177.223       gist.github.com
192.30.255.116    api.github.com
185.199.110.153   assets-cdn.github.com
151.101.76.133    raw.githubusercontent.com
151.101.76.133    gist.githubusercontent.com
151.101.76.133    cloud.githubusercontent.com
151.101.76.133    camo.githubusercontent.com
151.101.76.133    avatars0.githubusercontent.com
151.101.76.133    avatars1.githubusercontent.com
151.101.76.133    avatars2.githubusercontent.com
151.101.76.133    avatars3.githubusercontent.com
151.101.76.133    avatars4.githubusercontent.com
151.101.76.133    avatars5.githubusercontent.com
151.101.76.133    avatars6.githubusercontent.com
151.101.76.133    avatars7.githubusercontent.com
151.101.76.133    avatars8.githubusercontent.com
# GitHub End
```
