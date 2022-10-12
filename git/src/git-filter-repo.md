# 清理大文件, filter-repo

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
+ [从项目生成新的导出](http://code.ihep.ac.cn/help/user/project/settings/import_export.html#export-a-project-and-its-data), 并下载它.
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

解压结果会包含 `project.bundle`文件, 它是由 [git bundle](https://git-scm.com/docs/git-bundle) 创建的.

+ 新建目录, 从 `bundle` 克隆一个新鲜的 `repository ` .

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

使用 `git-push` 的 `--mirror` 选项是等价的:

```bash
git push --mirror git@github.com:gravitional/note.git
# 为了确保都已同步, 再执行以下命令:
git push --all --force
git push --tags --force
```

### 更新其他的clone

在过滤存储库, 并重写提交历史后, 将更改`强制推送`到远程服务器之后.
现在要更新该仓库的每一份 `clone` , 仅靠常用的 `pull` 是无法做到这一点的.

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
