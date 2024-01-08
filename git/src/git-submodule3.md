# git submodule

[gitsubmodules - Mounting one repository inside another](https://book.git-scm.com/docs/gitsubmodules)
[git-submodule - Initialize, update or inspect submodules](https://book.git-scm.com/docs/git-submodule/zh_HANS-CN)
[Where is the gitlink entry located for Git submodules?](https://stackoverflow.com/questions/66955714/where-is-the-gitlink-entry-located-for-git-submodules)

## init

`init [--] [<path>…​]`

通过在 `.git/config` 中设置 `submodule.$name.url`,
初始化 `index` 中记录的子模块(来自其他作者的 添加和提交).
它使用 `.gitmodules` 中的相同设置作为模板.
如果 URL 是相对的, 将使用默认远程地址解析.
如果没有默认远程, 则当前版本库将被假定为 `upstream`.

可选的 `<path>` 参数会限制哪些子模块将被初始化.
如果没有指定路径, 且已配置 `submodule.active`,
则将初始化配置为`active`的子模块, 否则将初始化所有子模块.

如果存在, 它还将复制 `submodule.$name.update` 的值.
该命令不会更改 `.git/config` 中的现有信息.
然后, 你可以根据本地设置在 `.git/config` 中自定义子模块克隆 URL,
并继续执行 `git submodule update`;如果你不打算自定义任何子模块位置,
也可以直接使用 `git submodule update --init` 而不需要 显式运行init

有关默认远程的定义, 请参阅 `add` 子命令.

## update

```bash
update [--init] [--remote] [-N|--no-fetch] [--[no-]recommend-shallow] [-f|--force] [--checkout|--rebase|--merge] [--reference <repository>] [--depth <depth>] [--recursive] [--jobs <n>] [--[no-]single-branch] [--filter <filter spec>] [--] [<path>…​]
```

通过克隆缺失的子模块, 获取子模块中缺失的提交以及更新子模块的 `工作树`,
完成更新 已注册的子模块, 使其符合 super projct的期望.

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
