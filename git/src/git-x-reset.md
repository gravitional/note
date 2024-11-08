# git reset 例子

[Git 分支 - 分支简介](https://git-scm.com/book/zh/v2/Git-%E5%88%86%E6%94%AF-%E5%88%86%E6%94%AF%E7%AE%80%E4%BB%8B)

`HEAD` 是二级指针, 指向例如 `master`, `develop` 等分支头, 如图所示,
![img](https://git-scm.com/book/zh/v2/images/head-to-master.png)

`git-reset` 默认操作 `HEAD` 指向的分支头, 如 `develop`.

## Undo add

撤销 index 中的改动, 匹配 `HEAD`.

```bash
$ edit                                     (1)
$ git add frotz.c filfre.c
$ mailx                                    (2)
$ git reset                                (3)
$ git pull git://info.example.com/ nitfol  (4)
```

1. 您正在愉快地工作, 发现这些文件中的改动井然有序.
您不想在运行 `git diff` 时看到它们, 因为您打算处理其他文件, 而这些文件的改动会分散您的注意力.
2. 有人让你 pull, 这些改动听起来值得合并.
3. 但是, 您已经弄脏了索引(即您的索引与 HEAD 提交不匹配).
但你知道你要进行的拉取不会影响到 `frotz.c` 或 `filfre.c`, 所以你恢复了这两个文件的索引更改.
你在工作树中的修改仍然保留.
4. 然后你就可以进行 pull 和 merge, 对 `frotz.c` 和 `filfre.c` 的改动将保留在工作树中.

## Undo a commit and redo

```bash
$ git commit ...
$ git reset --soft HEAD^      (1)
$ edit                        (2)
$ git commit -a -c ORIG_HEAD  (3)
```

1. 最常见的情况是当你发现刚才提交的内容不完整,
或者你拼错了提交信息, 或者两者都有. 使工作树保持 `reset` 前的状态.
2. 修正工作树文件.
3. `reset` 会将旧的 `head` 复制到 `.git/ORIG_HEAD`,
并从日志信息开始重做提交. 如果不需要进一步编辑, 可以使用 `-C` 选项.
参见 [git-commit[1]][] 的 `--amend` 选项.

## Undo a commit, making it a topic branch

```bash
$ git branch topic/wip          (1)
$ git reset --hard HEAD~3       (2)
$ git switch topic/wip          (3)
```

1. 您已经有一些 `commits`, 但意识到将它们放在主分支还为时过早.
你想在 特性分支(topic branch) 中继续完善它们, 因此在当前 `HEAD` 上创建了 `topic/wip` 分支.
2. 回退 `master` 分支, 撤销这三次提交.
3. 切换到 `topic/wip` 分支, 继续工作.

## Undo commits permanently

```bash
$ git commit ...
$ git reset --hard HEAD~3   (1)
```

最后三个提交(`HEAD`, `HEAD^` 和 `HEAD~2`)很糟糕, 您不想再看到它们.
如果你已经将这些提交交给了其他人, 请不要这样做.
(请参阅 [git-rebase[1]][] 中的 **从上游缓存中恢复** 章节, 了解这样做的影响).

## Undo a merge or pull

```bash
$ git pull                         (1)
Auto-merging nitfol
CONFLICT (content): Merge conflict in nitfol
Automatic merge failed; fix conflicts and then commit the result.
$ git reset --hard                 (2)
$ git pull . topic/branch          (3)
Updating from 41223... to 13134...
Fast-forward
$ git reset --hard ORIG_HEAD       (4)
```

1. 尝试从上游更新导致了大量冲突;您现在还没准备好花大量时间合并, 所以决定稍后再做.
2. 由于 `pull` 没有进行合并提交, 所以 `git reset --hard` 和 `git reset --hard HEAD` 同义,
可以清除索引文件和工作树中的混乱.
3. 将一个特性分支合并到当前分支, 这导致了一次 `fast-forward`.
4. 但你决定该特性分支还不能公开, 那么你可以用 `pull` 或 `merge` 来合并该特性分支.
`pull` 或 `merge` 总是会在 `ORIG_HEAD` 中保留 当前分支的原始顶端(tip),
因此重置为 `reset --hard ORIG_HEAD` 会将索引文件和工作树恢复到该状态, 并将分支顶端重置为 该提交.

## Undo a merge or pull inside a dirty working tree

```bash
$ git pull                         (1)
Auto-merging nitfol
Merge made by recursive.
 nitfol                |   20 +++++----
 ...
$ git reset --merge ORIG_HEAD      (2)
```

1. 即使您的 working tree中 可能有局部修改,
但当您知道另一个分支中的改动与之不重叠时, 您就可以放心运行 `git pull` 了.
2. 检查合并结果后, 你可能会发现另一个分支的改动并不令人满意.
运行 `git reset --hard ORIG_HEAD` 会让你回到原来的位置, 但会丢弃本地的改动, 这是你不想要的.
`git reset --merge` 将保留本地修改

## Interrupted workflow

假设您正在进行一项大型变更时, 被一个紧急修复请求打断.
您工作树中的文件还没到提交的时候, 但您需要到另一个分支进行快速错误修复.

```bash
$ git switch feature  ;# you were working in "feature" branch and
$ work work work      ;# got interrupted
$ git commit -a -m "snapshot WIP"                 (1)
$ git switch master
$ fix fix fix
$ git commit ;# commit with real log
$ git switch feature
$ git reset --soft HEAD^ ;# go back to WIP state  (2)
$ git reset                                       (3)
```

1. 该提交将被删除, 所以日志信息中只写 throw-away log message 即可.
2. 这将从提交历史中删除 WIP 提交, 并将工作树设置为制作快照前的状态.
3. 此时, 索引文件中仍有你在 `snapshot WIP` 中提交的 WIP变更.
这将更新索引, 把 `WIP文件` 变成 未提交 状态.

另请参见 [git-stash[1]][].

## Reset a single file in the index

假设您在索引中添加了一个文件, 但后来决定不再提交.
您可以使用 `git reset` 从索引中删除该文件, 同时保留您的改动.

```bash
$ git reset -- frotz.c                      (1)
$ git commit -m "Commit files in index"     (2)
$ git add frotz.c                           (3)
```

1. 这将从索引中删除文件, 同时将其保留在工作目录中.
2. 提交索引中的所有其他更改.
3. 再次将文件添加到索引中.

## Keep changes in working tree while discarding some previous commits

假设你正在处理某项工作并提交了它, 然后又继续处理了一些工作,
但现在你认为工作树中的内容应该放在另一个分支中, 而这个分支与你之前提交的内容毫无关系.
你可以启动一个 新分支 并重新设置它, 同时保留工作树中的更改.

```bash
$ git tag start
$ git switch -c branch1
$ edit
$ git commit ...                            (1)
$ edit
$ git switch -c branch2                     (2)
$ git reset --keep start                    (3)
```

1. 这将是您在 `branch1` 中的第一次编辑提交.
2. 在理想情况下, 您可以在创建并切换到 `branch2` 时意识到之前的提交不属于 new topic
(即 `git switch -c branch2 start`), 但人无完人.
3. 不过您可以在切换到 `branch2` 后使用 `reset --keep` 删除不需要的提交.

## Split a commit apart into a sequence of commits

假设您创建了许多逻辑上独立的变更, 并将它们一起提交.
后来, 您觉得让每个逻辑块都与自己的提交相关联可能会更好.
您可以使用 git reset 在不改变本地文件内容的情况下倒退历史,
然后连续使用 `git add -p` 交互式地选择要包含在每次提交中的逻辑块,
并使用 `git commit -c` 预填充提交信息.

```bash
$ git reset -N HEAD^                        (1)
$ git add -p                                (2)
$ git diff --cached                         (3)
$ git commit -c HEAD@{1}                    (4)
...                                         (5)
$ git add ...                               (6)
$ git diff --cached                         (7)
$ git commit ...                            (8)
```

1. 首先, 重置提交到前一次历史, 这样我们就能移除原始提交, 但保留工作树中的所有改动.
`-N` 确保任何用 `HEAD` 添加的新文件仍被标记, 以便 `git add -p` 能找到它们.
2. 接下来, 我们使用 `git add -p` 工具交互式地选择要添加的差异块.
它会依次询问你关于每个差异块的信息, 你可以使用简单的命令,
比如 "是的, 包括这个", "不, 不包括这个",
甚至是非常强大的 "编辑" 工具.
3. 一旦确定了要包含的差异块, 就应该使用 `git diff --cached` 来验证第一次提交的准备情况.
这会显示所有已移入 index 并即将提交的改动.
4. 接下来, 提交索引中的改动. `-c` 选项指定从第一次提交的 原始信息 中预先填充提交信息.
这有助于避免重复输入. `HEAD@{1}` 是一个特殊的符号,
for the commit that HEAD used to be at prior to the original reset commit (1 change ago)
详情参见 [git-reflog[1]][]. 您也可以使用任何其他有效的提交引用.
5. 你可以多次重复步骤 2-4, 将原始代码拆分成任意数量的提交.
6. 现在, 你已经把许多改动分割成了自己的提交, 可能不再使用 `git add` 的 patch模式, 而是选择所有剩余的未提交改动.
7. 再次检查是否包含了你想要的内容. 您可能还想确认一下 `git diff` 是否显示了剩余的待提交改动.
8. 最后, 创建最终提交.

[git-commit[1]]: https://git-scm.com/docs/git-commit
[git-rebase[1]]: https://git-scm.com/docs/git-rebase
[git-stash[1]]: https://git-scm.com/docs/git-stash
[git-reflog[1]]: https://git-scm.com/docs/git-reflog
