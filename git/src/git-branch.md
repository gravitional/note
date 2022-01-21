# git-branch

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

`reset` 如果不加参数, 那么默认使用 `--mixed` 参数.它的行为是: 保留`working tree`, 并且清空`index`.
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

## checkout 还原文件

```bash
git checkout [-q] [-f] [-m] [<branch>]
git checkout [-q] [-f] [-m] --detach [<branch>]
git checkout [-q] [-f] [-m] [--detach] <commit>
git checkout [-q] [-f] [-m] [[-b|-B|--orphan] <new_branch>] [<start_point>]
git checkout [-f|--ours|--theirs|-m|--conflict=<style>] [<tree-ish>] [--] <pathspec>...
git checkout [-f|--ours|--theirs|-m|--conflict=<style>] [<tree-ish>] --pathspec-from-file=<file> [--pathspec-file-nul]
git checkout (-p|--patch) [<tree-ish>] [--] [<pathspec>... ]
```

用 **index**或者`<tree-ish>`(通常是一个`commit`)里面的内容替换`working tree`里面的 paths.

当给出一个`<tree-ish>`的时候, the **paths** that match the `<pathspec>`会在**index** and in the **working tree**里面都更新.

`index `中可能包含有之前合并失败的`entries`.默认情况下, 如果你想checkout 一个这样的`entries`, 会失败, 什么都不会发生.
使用`-f`选项忽略未合并的`entries`.

可以选择`merge`的特定一方的内容, 使用选项`--ours` or `--theirs`.

使用`-m`选项, 可以抛弃对`working tree`的更改, 恢复到 the original conflicted merge result

### 孤儿分支

    git checkout [-q] [-f] [-m] [[-b|-B|--orphan] <new_branch>] [<start_point>]

创建一个新的孤儿分支, 命名为 `<new_branch>`, 从 `<start_point>` 开始, 并切换到它.
在这个新分支上的第一次提交将没有父分支, 它将是一个新历史的根, 与所有其他分支和提交完全断开.

`index`和`working tree`会被调整, 如同你之前运行过 `git checkout <start_point>` .
这让你可以通过简单地运行 `git commit -a` 来进行 root commit, 从而启动新的历史,
记录一组与 `<start_point>` 大致相似的文件路径.

当你想发布一个`commit`的树形式, 而不想暴露其完整的历史时, 这可能很有用.
你可能需要这样的功能, 来发布一个开源项目的分支, 这个分支的当前树是 "干净的", 但它的全部历史包含了专有的或其他冗杂的代码.

如果你想开始一个不相连历史, 记录一组与 `<start_point>` 完全不同的文件路径,
那么你应该在创建孤儿分支后, 立即清除 `index`和`working tree`, 即在工作树的顶层运行 `git rm -rf`.
之后, 你就可以准备你的新文件了, 通过从其他地方拷贝, 从压缩包中提取, 或者重新填充工作树等等.

## 分支管理

那么, `Git`又是怎么知道当前在哪一个分支上呢?  也很简单, 它有一个名为`HEAD`的特殊指针.请注意它和许多其它版本控制系统(如`Subversion`或`CVS`)里的 `HEAD` 概念完全不同. 在`Git`中, 它是一个指针, 指向当前所在的本地分支(译注: 将 `HEAD` 想象为当前分支的别名).在本例中, 你仍然在`master`分支上. 因为`git branch`命令仅仅创建一个新分支, 并不会自动切换到新分支中去.

由于`Git`的分支实质上仅是包含所指对象校验和(长度为`40`的`SHA-1`值字符串)的文件, 所以它的创建和销毁都异常高效.创建一个新分支就相当于往一个文件中写入`41`个字节(`40`个字符和`1`个换行符), 如此的简单能不快吗?

这与过去大多数版本控制系统形成了鲜明的对比, 它们在创建分支时, 将所有的项目文件都复制一遍, 并保存到一个特定的目录.完成这样繁琐的过程通常需要好几秒钟, 有时甚至需要好几分钟.所需时间的长短, 完全取决于项目的规模.而在 `Git` 中, 任何规模的项目都能在瞬间创建新分支.同时, 由于每次提交都会记录父对象, 所以寻找恰当的合并基础(译注: 即共同祖先)也是同样的简单和高效. 这些高效的特性使得 `Git` 鼓励开发人员频繁地创建和使用分支.

### 各种本地分支命令

`Git`鼓励大量使用分支:

+ 查看分支: `git branch`
    + `git br -a`
    + `git br -vv`
    + `git br -avv`

+ 创建分支: `git branch name`
+ 切换分支:  `git switch name`
+ 新建+切换 到新分支:  `git checkout -b branchname`

+ 合并某分支到当前分支: `git merge name`

+ 删除分支: `git branch -d name`

### 创建分支

```bash
git branch [--track | --no-track] [-f] <branchname> [<start-point>]
```

命令的上述形式创建了名为 `<branchname>` 的新分支头, 它指向当前的`HEAD`, 或者如果给定了 `<start-point>`.

`<start-point>`有一个特殊形式: 如果`A`和`B`恰好有一个合并基, 您可以使用`A...B`作为指定这个合并基的快捷方式.
最多可以省略`A`和`B`中的一个, 它默认为`HEAD`. 也可以用 `git-checkout` 的特殊语法

```bash
git checkout -b
git checkout -b "branchname " "startpoint"
```

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

例如删除远程分支`dev`, 命令: `git push origin :dev`, 注意`:`前的空格

### 查看分支详情

`git branch`
`-v`
`-vv`
`-avv`
`--verbose`

If --list is given, or if there are no non-option arguments, existing branches are listed;
it is in list mode, show sha1 and commit subject line for each head, along with relationship to upstream branch (if any).
If given twice, print the path of the linked worktree (if any) and the name of the upstream branch, as well (see also git remote show `remote`).
Note that the current worktree"s `HEAD` will not have its path printed (it will always be your current directory).

### 创建并切换到分支

`git checkout -b`
`git checkout -b "branchname " "startpoint"`

The new branch **head**  will point to **this commit**. It may be given as a **branch name**, a **commit-id**, or **a tag**.
If this option is omitted, the **current HEAD** will be used instead.

`git branch [--track | --no-track] [-f] <branchname> [<start-point>]`
`-t`
`--track`

当创建一个新分支的时候, 设置 `branch.<name>.remote` and `branch.<name>.merge`项目来标记新分支的"上游".
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

首先, 用`git log`或`sourcetree`工具查看一下你想选择哪些`commits`进行合并, 例如:

比如`feature` 分支上的`commit 82ecb31` 非常重要, 它含有一个`bug`的修改, 或其他人想访问的内容.
无论什么原因, 你现在只需要将`82ecb31` 合并到`master`, 而不合并`feature`上的其他`commits`, 所以我们用`git cherry-pick`命令来做:

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

首先需要基于`feature`创建一个新的分支, 并指明新分支的最后一个`commit`:

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

第一个命令:  切换到feature分支;
第二个命令: 合并master分支上`f`文件到`feature`分支上,
将`master`分支上 `f` 文件追加补丁到`feature`分支上的`f`文件.
你可以接受或者拒绝补丁内容.

如果只是简单的将`feature`分支的文件`f.txt` copy到`master`分支上;

```bash
git checkout master
git checkout feature f.txt
```

[Git合并指定文件到另一个分支](https://www.cnblogs.com/yanglang/p/11436304.html)
