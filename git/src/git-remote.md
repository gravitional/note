# git-2

## 远程分支

`远程跟踪分支`是`远程分支`状态的`引用`(reference).
它们是你不能移动的本地引用, 当你做任何网络通信操作时, 它们会自动移动. 远程跟踪分支像是你上次连接到远程仓库时, 那些分支所处状态的书签.

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

`git fetch` 命令会将数据拉取到你的本地仓库 -- 它并不会自动合并或修改你当前的工作. 当准备好时你必须手动将其合并入你的工作

现在 `Paul` 的 `master` 分支可以在本地通过 `pb/master` 访问到 -- 你可以将它合并到自己的某个分支中,

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

例如删除远程分支`dev`, 命令: `git push origin :dev`, 注意`:`前的空格

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

`git push [-u | --set-upstream] [<repository> [<refspec>...  ]]`

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
[--no-verify] [<repository> [<refspec>...  ]]
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

`git pull [<options>] [<repository> [<refspec>... ]]`

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
例如, 想要将 `pb` 重命名为`paul`, 可以用`git remote rename`这样做:

```bash
$ git remote rename pb paul
"no output"
$ git remote
origin
paul
```

值得注意的是这同样也会修改你的远程分支名字.那些过去引用`pb/master` 的现在会引用`paul/master`,
如果因为一些原因想要移除一个远程仓库 -- 你已经从服务器上搬走了或不再想使用某一个特定的镜像了, 又或者某一个贡献者不再贡献了 -- 可以使用 `git remote rm` :

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

该命令可以删除本地版本库上那些失效的远程追踪分支, 具体用法是, 假如你的远程版本库名是`origin`,则使用如下命令先查看哪些分支需要清理:

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

轻量标签本质上是提交`校验和`, 将其存储到一个文件中 -- 没有保存任何其他信息.
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

你必须使用 `git push <remote> :refs/tags/<tagname>` 来更新你的远程仓库:
`$ git push origin :refs/tags/v1.4-lw`
`To /git@github.com:schacon/simplegit.git`
`- [deleted]         v1.4-lw`

### checkout 到某个 标签

如果你想查看某个标签所指向的文件版本, 可以使用`git checkout`命令, 虽然说这会使你的仓库处于"分离头指针(`detacthed HEAD`)"状态 -- 这个状态有些不好的副作用:

```bash
$ git checkout 2.0.0
Note: checking out '2.0.0'.
```

比如说你正在修复旧版本的错误 -- 这通常需要创建一个新分支:

`$ git checkout -b version2 v2.0.0`
`Switched to a new branch 'version2'`

## git 删除历史中的大文件

[仓库体积过大, 如何减小?  ](https://gitee.com/help/articles/4232#article-header2)
[Git清理删除历史提交文件](https://www.jianshu.com/p/7ace3767986a)

常见的`Git`清理方式有两种, 一种是使用`BFG`工具,
另外一种是使用`git filter-branch`手动处理.

注意: 无论使用哪种方式, 都涉及破坏性操作, 使用时应严格谨慎.在开始操作之前, 请使用`--mirror`参数克隆备份你的`Git`仓库.

使用`BFG`的方式, 简单易操作, 使用方法可参考`BFG Repo-Cleaner` .
本文主要介绍的是使用 `git filter-branch` 的方式进行瘦身操作.

先进行垃圾回收, 并压缩一些文件

```bash
git gc --prune=now
```

`Git` 最初向磁盘中存储对象使用松散的格式, 后续会将多个对象打包为一个二进制的包文件(packfile), 以节省磁盘空间

+ `.pack`文件存储了对象的内容
+ `.idx`文件存储了包文件的偏移信息, 用于stage具体的对象

打包对象时, 查找命名和大小相近的文件, 保留文件不同版本之间的差异(最新一版保存完整内容, 访问频率最高)

### 查找大文件

使用`git rev-list --objects --all`显示所有`commit`及其所关联的所有对象:

```bash
git rev-list --objects --all | grep -E "$(git verify-pack -v .git/objects/pack/*.idx | sort -k 3 -n | tail -10 | awk '{print$1}')"
```

命令说明:

+ `git-rev-list` ; 列出可以从给定的`提交`中通过`父链接`到达的`提交`, 但不包括可以通过前缀`^`到达的提交.
默认情况下, 输出结果按反时间顺序排列.
    + `--objects`;  打印列出的提交所引用的任何对象的`ID`. `--objects foo ^bar`的意思是: "如果我已经有了提交对象`bar`, 但没有`foo`, 请把我需要下载的所有对象`ID`发给我".
    + `--all` ; 假设 `refs/`中的所有 `refs`, 连同 `HEAD` 都被当作`<commit>`, 列在命令行中.

***

+ `git verify-pack -v *.idx`: 查看压缩包内容.
  `git verify-pack` 读取给出的`idx`文件指定的, 用`git pack-objects`命令创建的`Git`打包档案, 并验证`idx`文件和相应的打包文件.
    + 当指定选项`-v` 时, 对没有`deltified`的对象, 使用的格式是:

    SHA-1, 类型, 体积, packfile中的体积, packfile中的偏移量

    + 对`deltified`的对象使用的格式为:

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
+ `--index-filter`: 过滤`Git`仓库的`index`, 该过滤命令作用于`git rm -rf --cached --ignore-unmatch  <dir/filename>`.
它不会`checkout`到`working directory`, 只修改`index`的文件, 速度快.
+ `--cached`会删除`index`中的文件
+ `--ignore-unmatch`: 如果没匹配到文件, 不会报错, 会继续执行命令
+ 最后一个参数`<dir/filename>`是要被删除的文件的名字.
+ `--prune-empty`: 指示`git filter-branch` 完全删除所有的空`commit`.
+ `-–tag-name-filter`: 将每个`tag`指向重写后的`commit`.
+ `cat`命令会在收到`tag`时返回`tag`名称
+ `--`用来分割 `rev-list` 和 `filter-branch` 选项
+ `--all`参数告诉`Git`我们需要重写所有分支或引用.

注意: `git rm` 这一行命令使用双引号`"git rm -rf --cached --ignore-unmatch <dir/filename>"`

默认会警告: `git-filter-branch`有大量的问题, 会产生错误的历史记录重写.
在继续进行之前按`Ctrl-C`中止, 然后使用另一个替代的过滤工具, 如 `git filter-repo` (https://github.com/newren/git-filter-repo/)来代替.
参见 `filter-branch` 手册页了解更多细节; 要消除这个警告. 设置 `FILTER_BRANCH_SQUELCH_WARNING=1`.

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

可以从 `size` 的值看出, 这个大文件还在你的松散对象中, 并没有消失; 但是它不会在推送或接下来的克隆中出现, 这才是最重要的.
如果真的想要删除它, 可以通过有 `--expire` 选项的 `git prune` 命令来完全地移除那个对象:

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
一定要小心谨慎! 一定要小心谨慎! 一定要小心谨慎!

先进行备份工作, 以免出现问题:

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

把已瘦身的仓库同步到远程仓库, 使用`--mirror`参数:

```bash
git push --mirror git@github.com:gravitional/note.git
```

为了确保都已同步, 再执行以下命令:

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
# git 会自动检测到远程服务器进行了 force update
git fetch origin
git reset --hard origin/master
# 和上面的一样, 需要删除旧提交, 清理本地仓库
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

注意: 下方命令中的 `path/to/large/files` 是大文件所在的路径, 千万不要弄错!

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

在应用外部更改之前, 你应该使自己的工作空间保持良好状态并在本地提交, 这样在发生冲突时不会毁坏文件.
另请参见`git-stash`. 当本地未提交的更改与`git pull / git merge`可能需要更新的文件重叠时, `git pull`和`git merge`将停止而不进行任何操作.
为了避免在在合并提交时, 记录下不相关的更改, 如果相对于`HEAD`在`index`记录了任何更改, 则`git pull`和`git merge`也将中止. (取决于所使用的合并策略, 但通常stage必须与`HEAD`匹配.)
如果所有已命名的提交都已经是`HEAD`的祖先, 则`git merge`会提前退出, 并显示消息`已经更新`.

### 快进式合并

当前分支`head`通常是要合并的提交的祖先.这是最常见的情况, 尤其是使用`git pull`时: 你正在跟踪上游 repository, 尚未提交任何本地更改, 现在想更新到较新的上游修订版.
在这种情况下, 不需要新的 commit 来存储合并的历史记录;  相反, 将`HEAD`(以及`index`)更新到新的commit即可, 而不创建额外的`merge commit`.

这个行为可以通过`--no-ff`选项来抑制.

### 真正的合并

除了`fast-forward merge`之外, 要合并的分支必须有一个共同的父节点.在进行真正的合并操作时, 将会提交一个合并的版本,
协调所有要合并的分支中的更改, 并且将`HEAD`, `index`, and `working tree` 更新为该版本.
只要修改不重叠, 工作树中可以有修改, 合并操作将保留这些修改.

如果不清楚如何协调修改, 则会发生以下情况:

+ `HEAD`指针保持不变.
+ `MERGE_HEAD` 引用被设置为指向要合并进来的另一个分支头.
+ 对于不矛盾的合并文件, 将会在`index`中和工作树中都更新.
+ 对于冲突的文件/路径, `index`将记录三个版本: `stage 1`存储共同祖先的版本, `stage 2`存储` HEAD`的修改,
`stage 3`存储`MERGE_HEAD`的修改(可以使用`git ls-files -u`检查这些`stage`).工作树中包含合并的结果:即使用熟悉的冲突标记` <<< === >>>`进行三方合并的结果.
+ 不进行其他更改.特别是, 在开始合并之前进行的本地修改将保持不变, 指向它们的`index`条目也保持不变, 即匹配`HEAD`.

如果合并产生了复杂的冲突, 则可以使用git`merge --abort`恢复到合并之前.

### 合并冲突的表示

在合并期间, 将更新工作树以反映合并结果.
在对共同祖先版本所做的更改中, 不重叠的更改被原封不动地合并到最终结果中(即你更改了文件的某区域, 而其他人没有修改这个地方, 反之亦然).
但是, 当双方对同一区域进行更改时, `Git`不能随意选择一边, 会将两边的修改都列出来, 让用户选择.

默认情况下, Git使用与`RCS`套件中`合并`程序相同的样式来呈现这种冲突, 如下所示:

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

默认格式不显示原文件在冲突区域中的内容. 你无法判断有多少行被删除或替换.
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
这是`pulling`或`merging`分支时的默认合并策略.`recursive`策略可以采用以下选项:

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
如果`their`版本的改动都是空白内容, 则使用`our`版本; 如果`our`版本引入了空白更改, 但`their`版本包含实质性更改, 则使用`their`版本; 否则, 合并将以通常的方式进行.
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

看到冲突后, 你可以做两件事:

+ 决定不合并. 你唯一需要进行的清理工作就是将`index`文件重置为`HEAD` 提交以撤消`2`, 并清理`2`和`3`所做的工作树更改.运行`git merge --abort`即可.
+ 解决冲突. `Git`将在工作树中标记冲突, 将文件修改好之后将它们添加到`index`中.使用`git commit`或`git merge --continue`完成封印.
后一个命令在调用`git commit`之前将检查是否有一个(中断的)合并正在进行中.

你可以使用多种工具来解决冲突:

+ 使用合并工具. `git mergetool`启动一个图形化的合并工具, 它将帮助你完成合并.
+ 查看差异. `git diff`将显示三方差异, 并突出显示`HEAD`和`MERGE_HEAD`版本的变化.
+ 查看每个分支的修改. `git log --merge -p <path>`将首先显示`HEAD`版本的差异, 然后显示`MERGE_HEAD`版本.
+ 看一下原件. `git show :1:filename`显示共同祖先, `git show :2:filename`显示`HEAD`版本, `git show :3:filename`显示`MERGE_HEAD`版本.

### 分支管理策略

`Git`分支十分强大, 在团队开发中应该充分应用.
合并 **临时分支**到 **feature分支** 后(并删除 **临时分支** ),
如果加上了 `--no-ff` 参数就可以用普通模式合并, 合并后的 **log** 有分支, 能看出来曾经做过合并,
而默认的 `fast forward` 合并就看不出来曾经做过合并.

## 临时储藏

```bash
 git stash list [<options>]
       git stash show [<options>] [<stash>]
       git stash drop [-q|--quiet] [<stash>]
       git stash ( pop | apply ) [--index] [-q|--quiet] [<stash>]
       git stash branch <branchname> [<stash>]
       git stash [push [-p|--patch] [-k|--[no-]keep-index] [-q|--quiet]
                    [-u|--include-untracked] [-a|--all] [-m|--message <message>]
                    [--] [<pathspec>...]]
       git stash clear
       git stash create [<message>]
       git stash store [-m|--message <message>] [-q|--quiet] <commit>
```

### git-stash

[WIP缩写的正确理解](https://blog.csdn.net/wq6ylg08/article/details/88965520)

当你想记录`工作树`和`stage`的当前状态, 但又想回到干净的`工作树`时, 请使用`git stash`.
该命令保存你的 `本地修改` , 并把当前的`工作树`, 还原到匹配 `HEAD` 提交的状态, 也就是上次提交的状态.

+ `git stash list`; 列出之前存放起来的, 对`工作树`的更改;
+ `git stash show`; 检查存放起来的更改, 可以有多个
+ `git stash apply` ; 将更改恢复到当前的工作树, 也可以应用到其他 `commit`.
+ `git stash push` ; 不带任何参数调用的`git stash`, 等于 `git stash push`, 往`stash` 栈中压入一个记录.

默认情况下,  某个 `stash` 显示为`WIP on branchname ...`, 但是在创建`存储项`时, 可以在命令行上提供更具描述性的消息.
`WIP`: Work in progress, 正在工作过程中, 引申含义为 "目前工作树中的代码正在编写中, 这部分代码不能独立运行, 是半成品".

你新创建的 `stash` 存储在 `refs/stash` 中.
更早的`stash`es 可以在这个`引用`(reference)的 `reflog` 中找到, 并能通过普通的 `reflog` 语法进行`引用`.
例如`stash@{0}`是最近创建的 `stash`, `stash@{1}` 是之前创建的 `stash`, `stash@{2.hours.ago}` 也可以.
还可以只给定 `stash index` 来引用`存储`. 例如, 整数 `n` 相当于 `stash@{n}`.
总结就是:

+ `git stash` : 储存工作现场
+ `git stash list`: 查看工作现场列表
+ `git stash apply`: 恢复改动, 但是恢复后, `stash` 栈中保存的内容不会消失.
+ `git stash drop`: 删除 `stash` 栈中的内容.
+ `git stash pop`: 从恢复的同时, 删除 `stash` 栈中对应修改.

`push`, `pop` 和通常一样, 是对`栈`中数据的两种操作.

如果没有给出 `<stash>`, 则默认为 `stash@{0}`, 否则 `<stash>` 的形式必须为 `stash@{<revision>}`.

### 使用场景

当工作只进行到一半, 还没法`提交`, 预计还需`1`天时间完成.
但是, 必须在两个小时内修复该bug, 怎么办?

幸好, `Git`还提供了一个`stash`功能, 可以把当前工作现场 `储藏` 起来, 等以后 `恢复` 现场后继续工作:

```bash
$ git stash
Saved working directory and index state WIP on dev: f52c633 add merge
```

现在用`git status`查看`工作树`, 会发现是`干净的`(除非存在没有被`Git`管理的文件), 因此可以放心地创建分支来修复`bug`.

太棒了, 原计划两个小时的bug修复只花了5分钟! 现在, 是时候接着回到`dev`分支干活了!

```bash
$ git checkout dev
Switched to branch 'dev'

$ git status
On branch dev
nothing to commit, working tree clean
```

`工作树`是`clean`的, 刚才的工作现场存到哪去了?
用`git stash list`命令看看:

```bash
$ git stash list
stash@{0}: WIP on dev: f52c633 add merge
```

工作现场还在, `Git` 把 `stash` 内容保存起来了, 可以恢复, 有两种办法:

+ 一是用`git stash apply`恢复修改, 但是恢复后, `stash`内容并不删除, 你需要用`git stash drop`来删除;
+ 另一种方式是用`git stash pop`, 恢复的同时把`stash`内容也删除
