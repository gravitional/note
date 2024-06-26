# Git 子模块

[7.11 Git 工具 - 子模块](https://git-scm.com/book/zh/v2/Git-%E5%B7%A5%E5%85%B7-%E5%AD%90%E6%A8%A1%E5%9D%97)

+ 具有 `submodule` 的母仓库, 其根目录会有 `.gitmodules` 文件,
另外在 `.git/config` 中也会有子模块条目.
子模块条目记录了 **子模块的名称和路径**, 例如

```conf
#--- .gitmodules
[submodule "dependencies/fmt"]
    path = dependencies/fmt
    url = https://gitee.com/mirrors_trending/fmt.git

#--- .git/config
[submodule "dependencies/fmt"]
```

+ 嵌套在 父仓库中的子仓库,
其 `.git` 目录被合并到 `root` 仓库的 `.git` 目录中,
子仓库的 `.git` 是一个引用文件, 存储了真正的 git data 目录

```conf
gitdir: ../../.git/modules/dependencies/fmt
```

+ 子模块仓库所在目录, 例如 `dependencies/fmt`, 会被 `git`特殊处理,
当成一个 `commit`, 而不再跟踪 其中的文件改动.
当运行 `git add/status/diff` 等命令时, `dependencies/fmt` 会作为整体考虑,
效果如下

```bash
# --------- git status
$git status
...
modified: ../dependencies/fmt # fmt子仓库目录 显示为一个整体条目
...
# ---------- git diff
$git diff
//========================================== format pring
template <typename... Args>
diff --git a/dependencies/fmt b/dependencies/fmt # fmt 仓库记录类型为 160000, 只跟踪 commit 改动
index 4fcc317..db1ee42 160000
--- a/dependencies/fmt
+++ b/dependencies/fmt
@@ -1 +1 @@
Subproject commit [-4fcc317dc9828bc75ede10a4d424fb7dc3e08989-]{+db1ee420e09d5c23767d72e63bb44934a85414d9+}
```

## 开始使用子模块

我们将要演示, 如何 **主项目+几个子项目** 的复合项目上开发.
我们首先将一个 **已存在的 Git 仓库** 添加为**正在工作的仓库**的子模块.

你可以通过在 `git submodule add` 命令后面,
加上想要跟踪的项目的 `相对或绝对URL` 来添加新的子模块.
在本例中, 我们将会添加一个名为 `DbConnector` 的库.

```bash
# git submodule add https://xxx CustomDirectory
$ git submodule add https://github.com/chaconinc/DbConnector

Cloning into 'DbConnector'...
...
Checking connectivity... done.
```

默认情况下, submodule 会将**子项目**放到 与**仓库同名**的目录中,
本例中是 `DbConnector`.
如果你想要放到其他地方, 那么可以在命令结尾添加**自定义路径**.

如果这时运行 `git status`, 你将看到:

```bash
$ git status
On branch master
Your branch is up-to-date with 'origin/master'.

Changes to be committed:
  (use "git reset HEAD <file>..." to unstage)

 new file:   .gitmodules
 new file:   DbConnector
```

首先应当注意到新的 `.gitmodules` 文件.
该配置文件保存了 项目URL 与 已经拉取的本地目录之间 的映射:

```bash
[submodule "DbConnector"]
 path = DbConnector
 url = https://github.com/chaconinc/DbConnector
```

如果有多个子模块, 该文件中就会有多条记录.
要重点注意的是, 该文件也像 `.gitignore` 文件一样受到(通过)版本控制.
它会和该项目的其他部分一同被拉取推送.
从而 克隆该项目的人 知道去哪里 获得子模块.

### Note

由于 `.gitmodules` 文件中的 URL 是人们首先尝试克隆/拉取的地方,
因此请尽可能确保你使用的 URL 大家都能访问.
例如, 若你要使用的推送 URL 与他人的拉取 URL 不同, 那么请使用他人能访问到的 URL.

你也可以根据自己的需要,
通过在本地执行 `git config submodule.DbConnector.url <私有URL>` 来覆盖这个选项的值.
如果可行的话, 一个相对路径会很有帮助.

在 `git status` 输出中列出的另一个是 **项目文件夹记录**(project folder entry).
如果你运行 `git diff`, 会看到类似下面的信息:

```bash
$ git diff --cached DbConnector
diff --git a/DbConnector b/DbConnector
new file mode 160000
index 0000000..c3f01dc
--- /dev/null
+++ b/DbConnector
@@ -0,0 +1 @@
+Subproject commit c3f01dc8862123d317dd46284b05b6892c7b29bc
```

虽然 `DbConnector` 是工作目录中的一个**子目录**,
但Git还是会将它视作一个**子模块**.
当你不在那个目录中时, Git 并**不会跟踪它的内容**,
而是将它看作**子模块仓库**中的某个具体的**提交**.
(也就是把**子模块所在的本地目录**看成**子git仓库**的某个**commit**).

如果你想看到更漂亮的差异输出,
可以给 `git diff` 传递 `--submodule` 选项.

```bash
$ git diff --cached --submodule
diff --git a/.gitmodules b/.gitmodules
new file mode 100644
index 0000000..71fc376
--- /dev/null
+++ b/.gitmodules
@@ -0,0 +1,3 @@
+[submodule "DbConnector"]
+       path = DbConnector
+       url = https://github.com/chaconinc/DbConnector
Submodule DbConnector 0000000...c3f01dc (new submodule)
```

当你提交时, 会看到类似下面的信息:

```bash
$ git commit -am 'added DbConnector module'
[master fb9093c] added DbConnector module
 2 files changed, 4 insertions(+)
 create mode 100644 .gitmodules
 create mode 160000 DbConnector
```

注意 `DbConnector` 记录的`160000`模式.
这是 Git 中的一种特殊模式,
它本质上意味着你 recording **a commit** as **a directory entry**,
而非把它当成一个 `subdirectory` 或者 `file`.

最后, 推送这些更改:

```bash
$ git push origin master
```

## 克隆含有子模块的项目

接下来我们将会克隆一个含有 **submodule** 的项目.
当你在克隆这样的项目时, 默认会创建**子模块所在的目录**,
但其中还没有任何文件:

```bash
$ git clone https://github.com/chaconinc/MainProject
Cloning into 'MainProject'...
...

$ cd MainProject
$ ls -la
...
drwxr-xr-x  13 schacon  staff  442 Sep 17 15:21 .git
-rw-r--r--   1 schacon  staff   92 Sep 17 15:21 .gitmodules
drwxr-xr-x   2 schacon  staff   68 Sep 17 15:21 DbConnector
-rw-r--r--   1 schacon  staff  756 Sep 17 15:21 Makefile
...

$ cd DbConnector/
$ ls
#空的#
```

其中有 `DbConnector` 目录, 不过是空的. 你必须运行两个命令:

```bash
git submodule init  # 用来初始化 子模块的本地配置文件,
git submodule update # 从 DbConnector项目中抓取所有数据, 并检出父项目中 要求的子模块 commit.
```

```bash
$ git submodule init
Submodule 'DbConnector' (https://github.com/chaconinc/DbConnector) registered for path 'DbConnector'

$ git submodule update
Cloning into 'DbConnector'...
...
Submodule path 'DbConnector': checked out 'c3f01dc8862123d317dd46284b05b6892c7b29bc'
```

现在 `DbConnector`子目录处在**早前提交时**的状态了.

不过还有更简单一点的方式.
如果给 `git clone` 命令传递 `--recurse-submodules` 选项,
它就会自动**初始化并更新**仓库中的每一个子模块, 包括可能存在的**嵌套子模块**.

```bash
$ git clone --recurse-submodules https://github.com/chaconinc/MainProject
Cloning into 'MainProject'...
...
Submodule 'DbConnector' (https://github.com/chaconinc/DbConnector) registered for path 'DbConnector'

Cloning into 'DbConnector'...
...
Submodule path 'DbConnector': checked out 'c3f01dc8862123d317dd46284b05b6892c7b29bc'
```

如果你之前克隆了项目, 但忘记了 `--recurse-submodules`,
那么可以运行

```bash
git submodule update --init
```

它相当于将 `git submodule init 和 git submodule update` 合并成一步.

如果还要 **初始化**, 抓取并检出任何嵌套子模块, 请使用简洁的

```bash
git submodule update --init --recursive
```

## 在主项目上工作(在包含子模块的项目上工作)

现在我们有一份包含子模块的项目副本, 我们将会同时在主项目和子模块项目上与队员协作.

### 从子模块的远端拉取上游修改

在项目中使用子模块的最简模型, 就是只使用子项目并不时地获取 `更新`,
而并不在你的检出中进行 `任何更改`.
简单说就是只使用别人的仓库, 不作修改.

我们来看一个简单的例子.

如果想要在子模块中查看新工作, 可以进入到目录中运行 git fetch 与 git merge,
合并上游分支来更新本地代码.

```bash
$ git fetch
From https://github.com/chaconinc/DbConnector
   c3f01dc..d0354fc  master     -> origin/master

$ git merge origin/master
Updating c3f01dc..d0354fc
...
```

如果你现在返回到主项目并运行 `git diff --submodule`,
就会看到子模块被更新的同, 获得了一个包含新添加提交的列表.
如果你不想每次运行 `git diff` 时都输入 `--submodule`,
那么可以将 `diff.submodule` 设置为 "log" 来将其作为默认行为.

```bash
$ git config --global diff.submodule log
$ git diff
Submodule DbConnector c3f01dc..d0354fc:
  > more efficient db routine
  > better connection routine
```

如果在此时提交, 那么你会将子模块锁定为 其他人更新时的新代码.

如果你不想在子目录中手动抓取与合并, 那么还有种更容易的方式.
运行 `git submodule update --remote`,
Git 将会进入子模块然后抓取并更新.

```bash
$ git submodule update --remote DbConnector
remote: Counting objects: 4, done.
remote: Compressing objects: 100% (2/2), done.
remote: Total 4 (delta 2), reused 4 (delta 2)
Unpacking objects: 100% (4/4), done.
From https://github.com/chaconinc/DbConnector
   3f19983..d0354fc  master     -> origin/master
Submodule path 'DbConnector': checked out 'd0354fc054692d3906c85c3af05ddce39a1c0644'
```

此命令默认会假定你想要更新并检出子模块仓库的 master 分支.
不过你也可以设置为想要的其他分支.
例如, 你想要 `DbConnector` 子模块跟踪仓库的 `stable` 分支,
那么既可以在 `.gitmodules` 文件中设置 (这样其他人也可以跟踪它),
也可以只在本地的 `.git/config` 文件中设置.
让我们在 `.gitmodules` 文件中设置它:

```bash
$ git config -f .gitmodules submodule.DbConnector.branch stable
# 效果是在 .gitmodule中添加 branch = stable

$ git submodule update --remote
...
Submodule path 'DbConnector': checked out 'c87d55d4c6d4b05ee34fbc8cb6f7bf4585ae6687'
```

如果不用 `-f .gitmodules` 选项, 那么它只会为你做修改.
但是在仓库中保留跟踪信息更有意义一些, 因为其他人也可以得到同样的效果.

这时我们运行 git status, Git 会显示子模块中有"新提交".

```bash
$ git status
On branch master
Your branch is up-to-date with 'origin/master'.

Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git checkout -- <file>..." to discard changes in working directory)

  modified:   .gitmodules
  modified:   DbConnector (new commits)

no changes added to commit (use "git add" and/or "git commit -a")
```

如果你设置了配置选项 `status.submodulesummary`, Git 也会显示你的子模块的更改摘要:

```bash
$ git config status.submodulesummary 1

$ git status
On branch master
Your branch is up-to-date with 'origin/master'.

Changes not staged for commit:
...

  modified:   .gitmodules
  modified:   DbConnector (new commits)

Submodules changed but not updated:

* DbConnector c3f01dc...c87d55d (4):
  > catch non-null terminated lines
```

这时如果运行 `git diff`, 可以看到我们修改了 `.gitmodules` 文件,
同时还有几个已拉取的提交需要提交到我们自己的子模块项目中.

```bash
$ git diff

diff --git a/.gitmodules b/.gitmodules
index 6fc0b3d..fd1cc29 100644
--- a/.gitmodules
+++ b/.gitmodules
@@ -1,3 +1,4 @@
 [submodule "DbConnector"]
        path = DbConnector
        url = https://github.com/chaconinc/DbConnector
+       branch = stable
 Submodule DbConnector c3f01dc..c87d55d:
  > catch non-null terminated lines
  > more robust error handling
  > more efficient db routine
  > better connection routine
```

这非常有趣, 因为我们可以直接看到将要提交到子模块中的提交日志.
提交之后, 你也可以运行 `git log -p` 查看这个信息.

```bash
$ git log -p --submodule
...
 [submodule "DbConnector"]
        path = DbConnector
        url = https://github.com/chaconinc/DbConnector
+       branch = stable
Submodule DbConnector c3f01dc..c87d55d:
  > catch non-null terminated lines
  > more robust error handling
  > more efficient db routine
  > better connection routine
```

当运行 `git submodule update --remote` 时,
Git 默认会尝试更新 所有 子模块,  所以如果有很多子模块的话, 你可以传递想要更新的子模块的名字.

## 从项目远端拉取上游更改

现在, 让我们站在协作者的视角, 他有自己的 MainProject 仓库的本地克隆,
只是执行 `git pull` 获取你新提交的更改还不够:

```bash
$ git pull
From https://github.com/chaconinc/MainProject
   fb9093c..0a24cfc  master     -> origin/master

Fetching submodule DbConnector
From https://github.com/chaconinc/DbConnector
   c3f01dc..c87d55d  stable     -> origin/stable

Updating fb9093c..0a24cfc
Fast-forward
 .gitmodules         | 2 +-
 DbConnector         | 2 +-
 2 files changed, 2 insertions(+), 2 deletions(-)
#--------------------------------------
$ git status
 On branch master
Your branch is up-to-date with 'origin/master'.
Changes not staged for commit:
...

  modified:   DbConnector (new commits)

Submodules changed but not updated:

* DbConnector c87d55d...c3f01dc (4):
  < catch non-null terminated lines
  < more robust error handling
  ...

no changes added to commit (use "git add" and/or "git commit -a")
```

默认情况下, git pull 命令会递归地抓取子模块的更改,
如上面第一个命令的输出所示. 然而, 它不会 **更新** 子模块.

这点可通过 `git status` 命令看到, 它会显示子模块"已修改", 且"有新的提交".
此外, 左边的尖括号(<)指出了新的提交, 表示这些提交已在 MainProject 中记录,
但尚未在本地的 DbConnector 中检出.
为了完成更新, 你需要运行 `git submodule update`:

```bash
$ git submodule update --init --recursive
Submodule path 'vendor/plugins/demo': checked out '48679c6302815f6c76f1fe30625d795d9e55fc56'

$ git status
 On branch master
Your branch is up-to-date with 'origin/master'.
nothing to commit, working tree clean
```

请注意, 为安全起见, 如果 MainProject 提交了你刚拉取的新子模块,
那么应该在 `git submodule update` 后面添加 `--init` 选项,
如果子模块有嵌套的子模块, 则应使用 `--recursive` 选项.

如果你想自动化此过程,
那么可以为 git pull 命令添加 `--recurse-submodules` 选项(从 Git 2.14 开始).
这会让 Git 在拉取后运行 git submodule update, 将子模块置为正确的状态.
此外, 如果你想让 Git 总是以 `--recurse-submodules` 拉取,
可以将配置选项 submodule.recurse 设置为 `true`
(从 Git 2.15 开始可用于 git pull).
此选项会让 Git 为所有支持 `--recurse-submodules` 的命令使用该选项(除 clone 以外).

在为父级项目拉取更新时, 还会出现一种特殊的情况:
在你拉取的提交中, 可能 `.gitmodules` 文件中记录的子模块的 URL 发生了改变.
比如, 若子模块项目**改变了它的托管平台**, 就会发生这种情况.
此时, 若父级项目引用的子模块提交不在仓库中本地配置的 `子模块远端`(url) 上,
那么执行 `git pull --recurse-submodules` 或 `git submodule update` 就会失败.
为了补救, 需要借助 `git submodule sync` 命令:

```bash
# 将新的 URL 复制到本地配置中
$ git submodule sync --recursive
# 从新 URL 更新子模块
$ git submodule update --init --recursive
```

## 在子模块上工作

你很有可能正在使用子模块,
因为你确实想在子模块中编写代码的同时, 还想在主项目上编写代码(或者跨子模块工作).
否则你大概只能用简单的依赖管理系统(如 Maven 或 Rubygems)来替代了.

现在我们将通过一个例子来演示如何在子模块与主项目中同时做修改,
以及如何同时提交与发布那些修改.

到目前为止, 当我们运行 `git submodule update` 从子模块仓库中抓取修改时,
Git 将会获得这些改动并更新子目录中的文件, 但是会将子仓库留在一个称作"游离的 HEAD"的状态.
这意味着没有本地工作分支(例如 "master" )跟踪改动.

如果没有工作分支跟踪更改, 也就意味着即便你将更改提交到了子模块,
这些更改也很可能会在下次运行 git submodule update 时丢失.
如果你想要在子模块中跟踪这些修改, 还需要一些额外的步骤.

为了将子模块设置得更容易进入并修改, 你需要做两件事.
首先, 进入每个子模块并检出其相应的工作分支.
接着, 若你做了更改就需要告诉 `Git` 它该做什么, 然后运行 `git submodule update --remote` 来从上游拉取新工作.
你可以选择将它们合并到你的本地工作中, 也可以尝试将你的工作变基到新的更改上.

首先, 让我们进入子模块目录然后检出一个分支.

```bash
$ cd DbConnector/
$ git checkout stable
Switched to branch 'stable'
```

然后尝试用 `merge` 选项来更新子模块.
为了手动指定它, 我们只需给 `update` 添加 `--merge` 选项即可.
这时我们将会看到服务器上的这个子模块有一个改动并且它被合并了进来.

```bash
$ cd ..
$ git submodule update --remote --merge
remote: Counting objects: 4, done.
remote: Compressing objects: 100% (2/2), done.
remote: Total 4 (delta 2), reused 4 (delta 2)
Unpacking objects: 100% (4/4), done.
From https://github.com/chaconinc/DbConnector
   c87d55d..92c7337  stable     -> origin/stable
Updating c87d55d..92c7337
Fast-forward
 src/main.c | 1 +
 1 file changed, 1 insertion(+)
Submodule path 'DbConnector': merged in '92c7337b30ef9e0893e758dac2459d07362ab5ea'
```

如果我们进入 DbConnector 目录, 可以发现新的改动已经合并入本地 stable 分支.
 现在让我们看看当我们对库做一些本地的改动而同时其他人推送另外一个修改到上游时会发生什么.

```bash
$ cd DbConnector/
$ vim src/db.c
$ git commit -am 'unicode support'
[stable f906e16] unicode support
 1 file changed, 1 insertion(+)
```

如果我们现在更新子模块, 就会看到当我们在本地做了更改时上游也有一个改动, 我们需要将它并入本地.

```bash
$ cd ..
$ git submodule update --remote --rebase
First, rewinding head to replay your work on top of it...
Applying: unicode support
Submodule path 'DbConnector': rebased into '5d60ef9bbebf5a0c1c1050f242ceeb54ad58da94'
```

如果你忘记 `--rebase` 或 `--merge`, Git 会将子模块更新为服务器上的状态.
并且会将项目重置为一个游离的 HEAD 状态.

```bash
$ git submodule update --remote
Submodule path 'DbConnector': checked out '5d60ef9bbebf5a0c1c1050f242ceeb54ad58da94'
```

即便这真的发生了也不要紧, 你只需回到目录中再次检出你的分支(即还包含着你的工作的分支)
然后手动地合并或变基 `origin/stable`(或任何一个你想要的远程分支)就行了.

如果你没有提交子模块的改动, 那么运行一个子模块更新也不会出现问题,
此时 Git 会 **只抓取更改而并不会覆盖子模块目录中未保存的工作**.

```bash
$ git submodule update --remote

remote: Counting objects: 4, done.
...
From https://github.com/chaconinc/DbConnector
   5d60ef9..c75e92a  stable     -> origin/stable
error: Your local changes to the following files would be overwritten by checkout:
    scripts/setup.sh
Please, commit your changes or stash them before you can switch branches.

Aborting
Unable to checkout 'c75e92a2b3855c9e5b66f915308390d9db204aca' in submodule path 'DbConnector'
```

如果你做了一些与上游改动冲突的改动, 当运行更新时 Git 会让你知道.

```bash
$ git submodule update --remote --merge

Auto-merging scripts/setup.sh
CONFLICT (content): Merge conflict in scripts/setup.sh
Recorded preimage for 'scripts/setup.sh'
Automatic merge failed; fix conflicts and then commit the result.
Unable to merge 'c75e92a2b3855c9e5b66f915308390d9db204aca' in submodule path 'DbConnector'
```

你可以进入子模块目录中然后就像平时那样修复冲突.

## 发布子模块改动

现在我们的子模块目录中有一些改动.
其中有一些是我们通过更新从上游引入的, 而另一些是本地生成的,
由于我们还没有推送它们, 所以对任何其他人都不可用.

```bash
$ git diff
Submodule DbConnector c87d55d..82d2ad3:
  > Merge from origin/stable
  ...
  > add new option for conn pooling
```

如果我们在主项目中提交并推送但并不推送子模块上的改动,
其他尝试检出我们修改的人会遇到麻烦, 因为他们无法得到依赖的子模块改动.
那些改动只存在于我们本地的拷贝中.

为了确保这不会发生, 你可以让 Git 在推送到主项目前检查所有子模块是否已推送.
git push 命令接受可以设置为 "check" 或 "on-demand" 的 `--recurse-submodules` 参数.
如果任何提交的子模块改动没有推送那么 "check" 选项会直接使 push 操作失败.

```bash
$ git push --recurse-submodules=check
The following submodule paths contain changes that can
not be found on any remote:
  DbConnector

Please try

 git push --recurse-submodules=on-demand

or cd to the path and use

 git push

to push them to a remote.
```

如你所见, 它也给我们了一些有用的建议, 指导接下来该如何做.
最简单的选项是进入每一个子模块中然后手动推送到远程仓库,
确保它们能被外部访问到, 之后再次尝试这次推送.果你想要对所有推送都执行检查,
那么可以通过设置 `git config push.recurseSubmodules check` 让它成为默认行为.

另一个选项是使用 "on-demand" 值, 它会尝试为你这样做.

```bash
$ git push --recurse-submodules=on-demand

Pushing submodule 'DbConnector'
Counting objects: 9, done.
...
To https://github.com/chaconinc/DbConnector
   c75e92a..82d2ad3  stable -> stable

Counting objects: 2, done.
...
To https://github.com/chaconinc/MainProject
   3d6d338..9a377d1  master -> master
```

如你所见, Git 进入到 DbConnector 模块中然后在推送主项目前推送了它.
如果那个子模块因为某些原因推送失败, 主项目也会推送失败.
你也可以通过设置 git config push.recurseSubmodules on-demand 让它成为默认行为.

## 合并子模块改动

如果你和其他人同时改动了一个子模块引用, 那么可能会遇到一些问题.
也就是说, 如果子模块的历史已经分叉,
并且在父项目中分别提交到了分叉的分支上, 那么你需要做一些工作来修复它.

如果一个提交是另一个的直接祖先(一个快进式合并), 那么 Git 会简单地选择之后的提交来合并, 这样没什么问题.

不过, Git 甚至不会尝试去进行一次简单的合并.
如果子模块提交已经分叉且需要合并, 那你会得到类似下面的信息:

```bash
$ git pull
remote: Counting objects: 2, done.
...
From https://github.com/chaconinc/MainProject
   9a377d1..eb974f8  master     -> origin/master

Fetching submodule DbConnector
warning: Failed to merge submodule DbConnector (merge following commits not found)
Auto-merging DbConnector
CONFLICT (submodule): Merge conflict in DbConnector
Automatic merge failed; fix conflicts and then commit the result.
```

所以本质上 Git 在这里指出了, 子模块历史中的两个分支记录点已经分叉, 并且需要合并.
它将其解释为 **merge following commits not found**
(未找到接下来需要合并的提交), 虽然这有点令人困惑, 不过之后我们会解释为什么是这样.

为了解决这个问题, 你需要弄清楚子模块应该处于哪种状态.
奇怪的是, Git 并不会给你多少能帮你摆脱困境的信息, 甚至连两边提交历史中的 SHA-1 值都没有.
幸运的是, 这很容易解决.  如果你运行 git diff,
就会得到试图合并的两个分支中记录的提交的 SHA-1 值.

```bash
$ git diff
diff --cc DbConnector
index eb41d76,c771610..0000000
--- a/DbConnector
+++ b/DbConnector
```

所以在本例中, `eb41d76` 是我们的子模块中大家共有的提交,
而 `c771610` 是上游拥有的提交.

如果我们进入子模块目录中, 它应该已经在 `eb41d76` 上了, 因为合并没有动过它.
如果不是的话, 无论什么原因, 你都可以简单地创建并检出一个指向它的分支.

来自另一边的提交的 SHA-1 值比较重要. 它是需要你来合并解决的.
你可以尝试直接通过 SHA-1 合并, 也可以为它创建一个分支然后尝试合并.
我们建议后者, 哪怕只是为了一个更漂亮的合并提交信息.

所以, 我们将会进入子模块目录, 基于 `git diff` 的第二个 SHA-1 创建一个分支然后手动合并.

```bash
$ cd DbConnector
$ git rev-parse HEAD
eb41d764bccf88be77aced643c13a7fa86714135

$ git branch try-merge c771610
(DbConnector) $ git merge try-merge
Auto-merging src/main.c
CONFLICT (content): Merge conflict in src/main.c
Recorded preimage for 'src/main.c'
Automatic merge failed; fix conflicts and then commit the result.
```

我们在这儿得到了一个真正的合并冲突,
在修改代码解决了合并冲突之后, 只需要在 main project 中提交这次合并.

```bash
$ vim src/main.c (1)
$ git add src/main.c
$ git commit -am 'merged our changes'
Recorded resolution for 'src/main.c'.
[master 9fd905e] merged our changes

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

$ git add DbConnector (4)
$ git commit -m "Merge Tom's Changes" (5)
[master 10d2c60] Merge Tom's Changes
```

1. 首先解决冲突
1. 然后返回到主项目目录中
1. 再次检查 SHA-1 值
1. 解决冲突的子模块记录
1. 提交我们的合并

这可能会让你有点儿困惑, 但它确实不难.

有趣的是, Git 还能处理另一种情况. 如果子模块目录中存在着这样一个**合并提交**,
它的历史中包含了**两边的提交**, 那么 Git 会建议你将它作为一个可行的解决方案.
它看到有人在子模块项目的某一点上合并了包含这两次提交的分支, 所以你可能想要那个.

这就是为什么前面的错误信息是 "merge following commits not found",
因为它 找不到 **这样的合并提交**.
但让人困惑的是 **谁能想到它做了这种尝试呢**?

如果它找到了一个可以接受的合并提交, 你会看到类似下面的信息:

```bash
$ git merge origin/master

warning: Failed to merge submodule DbConnector (not fast-forward)
Found a possible merge resolution for the submodule:
 9fd905e5d7f45a0d4cbc43d1ee550f16a30e825a: > merged our changes
If this is correct simply add it to the index for example
by using:

  git update-index --cacheinfo 160000 9fd905e5d7f45a0d4cbc43d1ee550f16a30e825a "DbConnector"

which will accept this suggestion.
Auto-merging DbConnector
CONFLICT (submodule): Merge conflict in DbConnector
Automatic merge failed; fix conflicts and then commit the result.
```

Git 建议的命令是更新索引, 就像你运行了 `git add` 那样, 这样会清除冲突然后提交.
不过你可能不应该这样做. 你可以轻松地进入子模块目录,
查看差异是什么, 快进到这次提交, 恰当地测试, 然后提交它.

```bash
$ cd DbConnector/
$ git merge 9fd905e
Updating eb41d76..9fd905e
Fast-forward

$ cd ..
$ git add DbConnector
$ git commit -am 'Fast forwarded to a common submodule child'
```

这些命令完成了同一件事, 但是通过这种方式 至少可以验证 代码合理,
以及确保 在完成时子模块目录中 存在代码.

## 子模块的技巧

你可以做几件事情来让用子模块工作轻松一点儿.

### 子模块遍历

有一个 `foreach` 子模块命令, 它能在每一个子模块中运行任意命令.
如果项目中包含了大量子模块, 这会非常有用.

例如, 假设我们想要开始开发一项新功能或者修复一些错误, 并且需要在几个子模块内工作.
我们可以轻松地保存所有子模块的工作进度.

```bash
$ git submodule foreach 'git stash'

Entering 'CryptoLibrary'
No local changes to save

Entering 'DbConnector'
Saved working directory and index state WIP on stable: 82d2ad3 Merge from origin/stable
HEAD is now at 82d2ad3 Merge from origin/stable
```

然后我们可以创建一个新分支, 并将所有子模块都切换过去.

```bash
$ git submodule foreach 'git checkout -b featureA'

Entering 'CryptoLibrary'
Switched to a new branch 'featureA'

Entering 'DbConnector'
Switched to a new branch 'featureA'
```

你应该明白. 能够生成一个主项目与所有子项目的改动的统一差异是非常有用的.

```bash
$ git diff; git submodule foreach 'git diff'

Submodule DbConnector contains modified content
...
@@ -245,6 +245,8 @@ static int handle_alias(int *argcp, const char ***argv)
...
+     url = url_decode(url_orig);
...

Entering 'DbConnector'

@@ -93,6 +93,11 @@ char *url_decode_mem(const char *url, int len)
...
+char *url_decode(const char *url)
+{
+       return url_decode_mem(url, strlen(url));
+}
```

在这里, 我们看到子模块中定义了一个函数并在主项目中调用了它.
这明显是个简化了的例子, 但是希望它能让你明白这种方法的用处.

## 有用的别名

你可能想为其中一些命令设置别名,
因为它们可能会非常长而你又不能设置选项作为它们的默认选项.
我们在 Git 别名 介绍了设置 Git 别名,  但是如果你计划在 Git 中大量使用子模块的话, 这里有一些例子.

```bash
$ git config alias.sdiff '!'"git diff && git submodule foreach 'git diff'"
$ git config alias.spush 'push --recurse-submodules=on-demand'
$ git config alias.supdate 'submodule update --remote --merge'
```

这样当你想要更新子模块时可以简单地运行 `git supdate`,
或 `git spush` 检查子模块依赖后推送.
