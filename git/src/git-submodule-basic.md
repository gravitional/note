# git submodule

[gitsubmodules - Mounting one repository inside another](https://book.git-scm.com/docs/gitsubmodules)
[git-submodule - Initialize, update or inspect submodules](https://book.git-scm.com/docs/git-submodule/zh_HANS-CN)
[Where is the gitlink entry located for Git submodules?](https://stackoverflow.com/questions/66955714/where-is-the-gitlink-entry-located-for-git-submodules)

## git submodule init

```bash
git submodule init [--] [<path>…​]
```

通过在 `.git/config` 中设置 `submodule.$name.url`,
初始化 `index` 中记录的 **子模块信息**(来自其他作者的 添加和提交).
它使用 `.gitmodules` 中的相同设置作为模板. 例如下列实例

```yaml
#---- .git/config 中的配置
[submodule "dependencies/fmt"]
    url = https://gitee.com/mirrors_trending/fmt.git
[submodule "dependencies/cpp11-range"]
    url = git@gitee.com:graviton/cpp11-range.git

#---- .gitmodules 中的配置
[submodule "dependencies/fmt"]
    path = dependencies/fmt
    url = https://gitee.com/mirrors_trending/fmt.git
[submodule "dependencies/cpp11-range"]
    path = dependencies/cpp11-range
    url = git@gitee.com:graviton/cpp11-range.git
```

如果 URL 是相对的, 将使用 默认远程地址 解析.
如果没有 默认远程, 则当前版本库将被假定为 `upstream`.

可选的 `<path>` 参数会限制哪些子模块将被初始化.
如果没有指定路径, 且已配置 `submodule.active`,
则将初始化配置为`active`的子模块, 否则将初始化所有子模块.

如果存在, 它还将复制 `submodule.$name.update` 的值.
该命令不会更改 `.git/config` 中已有信息.

然后, 你可以根据本地设置在 `.git/config` 中的 **submodule clone URLs**,
并继续执行 `git submodule update`; 如果你不打算自定义任何子模块位置,
也可以直接使用 `git submodule update --init` 而不需要**手动运行init**

有关默认远程的定义, 请参阅 `add` 子命令.

## deinit

```bash
deinit [-f|--force] (--all|[--] <path>…​)
```

Unregister the given submodules, i.e. remove the whole submodule.$name section from .git/config together with their work tree. Further calls to git submodule update, git submodule foreach and git submodule sync will skip any unregistered submodules until they are initialized again, so use this command if you don't want to have a local checkout of the submodule in your working tree anymore.

When the command is run without pathspec, it errors out, instead of deinit-ing everything, to prevent mistakes.

If --force is specified, the submodule's working tree will be removed even if it contains local modifications.

If you really want to remove a submodule from the repository and commit that use git-rm(1) instead. See gitsubmodules(7) for removal options.

## git submodule update

```bash
update [--init] [--remote] [-N|--no-fetch] [--[no-]recommend-shallow] [-f|--force] [--checkout|--rebase|--merge] [--reference <repository>] [--depth <depth>] [--recursive] [--jobs <n>] [--[no-]single-branch] [--filter <filter spec>] [--] [<path>…​]
```

通过克隆缺失的子模块, 获取子模块中缺失的提交以及 更新子模块的 `工作树`,
完成更新 已注册的子模块, 使其符合 super projct 的期望.

`更新` 有多种方式, 取决于命令行选项和
`submodule.<name>.update` 配置变量的值. 命令行选项优先于配置变量.
如果命令行选项和配置变量都没有给出, 则会执行 `checkout`.
命令行和通过 `submodule.<name>.update` 配置支持的 更新动作如下:

`checkout`
超项目 中记录的提交将在子模块中以分离的 HEAD 签出.

如果指定 `--force`,
即使 `containing repository` 的索引中指定的提交,
与子模块中 已签出的提交 已经匹配,
子模块也会被签出(使用 `git checkout --force`).

`rebase`
子模块的当前分支将 rebase 到超项目中 `记录的提交` 上.

`merge`
`超项目中记录的提交` 将被合并到 子模块的当前分支 中.

以下更新动作 只能通过 `submodule.<name>.update` 配置变量使用:

`custom command`
执行包含单个参数(超工程中记录的提交的 sha1) 的任意 shell 命令.
当 `submodule.<name>.update` 设置为 !command 时,
感叹号后的剩余部分就是自定义命令.

`none`
子模块不会更新.

如果子模块尚未初始化, 而你只想直接使用存储在 `.gitmodules` 中的设置,
可以使用 `--init` 选项自动初始化子模块.

如果指定 `--recursive`, 该命令将递归到已注册的子模块,
并更新其中任何嵌套的子模块.

如果指定了 `--filter <filter spec>`,
给定的 `partial clone filter` 将应用于子模块.
有关过滤器规格的详情, 请参阅 git-rev-list(1).

## 状态, `status [--cached] [--recursive] [--] [<path>…​]`

显示子模块的状态

这将打印每个子模块当前 已checkout的commit的 `SHA-1`, 
以及子模块路径和 `git describe` 对 `SHA-1` 的输出.
如果子模块未初始化, 则每个 `SHA-1` 都可能以 `-` 为前缀;
如果当前签出的子模块提交 与 上层仓库索引中的 `SHA-1` 不匹配,
则以 `+` 为前缀; 如果子模块有合并冲突, 则以 `U` 为前缀. 

+ 如果指定 `--cached`, 该命令将打印 superproject 中记录的每个子模块的 `SHA-1`. 

+ 如果指定 `--recursive`, 该命令将递归到嵌套子模块, 并显示它们的状态. 

如果你只对当前初始化的子模块相对于 `index` 或 `HEAD` 中记录的提交的变化感兴趣, 
[git-status[1]](https://git-scm.com/docs/git-status) 和 [git-diff[1]](https://git-scm.com/docs/git-diff) 也会提供这方面的信息(还能报告子模块工作树的变化). 

### 日志, `git log --submodule[=<format>]`

指定 子模块差异的显示方式. 
当指定 `--submodule=short` 时, 将使用简短格式. 
这种格式只显示范围开始和结束时的提交名称. 
指定 `--submodule` 或 `--submodule=log` 时, 将使用日志格式. 
这种格式会像 [git-submodule[1] summary](https://git-scm.com/docs/git-submodule) 那样列出范围内的提交. 
如果指定 `--submodule=diff`, 则使用 diff 格式. 
这种格式显示子模块内容在提交范围之间的 inline diff. 
默认使用 `diff.submodule`, 如果配置选项未设置, 则使用 `short` 格式. 

## 总结, `summary [--cached|--files] [(-n|--summary-limit) <n>] [commit] [--] [<path>…​]`

显示 `给定提交`(默认为 `HEAD`)和 `工作树`/`索引` 之间的提交摘要.
对于某个 子模块, 会显示从给定的 superproject commit 到索引或工作树(由 `--cached` 切换)之间的一系列提交. 
如果给出选项 `--files`, 则显示子模块中从 superproject index 到子模块工作树之间的一系列提交
(该选项不允许使用 `--cached` 选项或提供明确的 commit). 
在[git-diff[1]](https://git-scm.com/docs/git-diff)中使用 `--submodule=log` 选项也能提供该信息. 

## 查看子模块 SHA-1 更改

```bash
# 首父节点
git diff HEAD^..
# 次父节点
git diff HEAD^2..
```

## 更新所有子模块到 remote 最新

[Pull latest changes for all git submodules](https://stackoverflow.com/questions/1030169/pull-latest-changes-for-all-git-submodules)

如果您是第一次检出 `repo`, 需要先使用 `--init` 命令: 

```bash
git submodule update --init --recursive
```

在 git 1.8.2 或更高版本中, 添加了 `--remote` 选项, 以支持更新到远程分支的最新提示: 

```bash
git submodule update --recursive --remote
```

这样做的另一个好处是会考虑 `.gitmodules` 或 `.git/config` 文件中指定的任何 "非默认" 分支
(如果有的话, 默认分支是 origin/master, 在这种情况下, 这里的其他答案也同样有效). 

对于 1.7.3 或更高版本的 git, 你可以使用(但以下关于更新的问题仍然适用): 

```bash
git submodule update --recursive
# or
git pull --recurse-submodules
```

如果你想把子模块拉到最新提交, 而不是 父仓库当前指向的 子仓库提交. 
详见 [git-submodule(1)](https://www.kernel.org/pub/software/scm/git/docs/git-submodule.html). 
