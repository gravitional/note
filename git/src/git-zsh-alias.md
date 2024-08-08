# zsh 定义的 git别名

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
+ `gbD`='git branch -D' ; 与 `--delete --force`相同, 强制删除分支.
+ `gba`='git branch -a'; `--all`, 列出远程跟踪分支和本地分支. 与 `--list` 结合使用, 以匹配可选的模式.
+ `gbd`='git branch -d'; `--delete`, 删除分支. 该分支必须完全被合并到上游, 或者在`HEAD`中, 如果没有使用 `--track` 或 `--set-upstream-to` 设置上游.
+ `gbr`='git branch --remote'; ` --remotes`, 列出或删除远程跟踪分支(与 `-d` 一起使用).

选项:

+ `-D`: 与 `--delete --force`相同.
+ `-d, --delete` ;删除分支. 该分支必须完全被合并到上游, 如果没有使用 `--track` 或 `--set-upstream-to` 设置上游, or in `HEAD`.
+ `-f, --force`: 将 `<branchname>` 重置为 `<startpoint>`, 即使 `<branchname>` 已经存在.  如果没有 `-f`, `git branch` 将拒绝更改现有分支.
    + 结合 `-d` (或 `--delete` ), 允许删除分支, 而不管其合并状态如何.
    + 结合 `-m` (或 `--move` ), 即使新分支名称已经存在, 也允许重命名分支, 同样适用于 `-c` (或 `--copy` ).
+ `-m` `-M`: 对分支进行重命名, 并且把 `reflog` 出现的分支名字一并更改. 如果新分支已经存在, 使用 `-M` 强迫重命名

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
+ `-v`, `--verbose`: 在提交信息的尾部, 展示 `HEAD` 和将要提交 `commit` 的 `diff` .
    `diff` 的输出行没有前置的`#`. 并且不是提交信息的一部分. 参考 git-config(1) 中的 `commit.verbose` 配置变量.
    如果使用两次,i.e. `-vv` , 则额外展示 `working tree` 和 下次 `commit` 的区别.
+ `--amend`: 创造一个新的 `commit` , 代替当前分支的 `tip` . 提交信息基于上次的 `commit` .
+ `-m`: 添加提交信息, 可以给出多个 `-m` , 会被当作多个段落被合并.
+ `-s`, `-S` :签名相关

### checkout

+ `gcb`='git checkout -b' ; 创建并切换到新分支
+ `gco`='git checkout'
+ `gcp`='git cherry-pick'
+ `gcpa`='git cherry-pick --abort'
+ `gcpc`='git cherry-pick --continue'

选项:

+ `git checkout -b|-B <new_branch> [<start point>]`;
+ 指定 `-b` 选项会创建新分支, 如同调用了 `git branch` 一样, 然后check out到新分支一样.
可以使用`--track ` or ` --no-track`选项, 它们会被传递给 `git branch` . 为了方便起见, `--track ` without ` -b`意味着创建新分支.
如果给的是 `-B` , 新分支会被创建, 或者直接 `reset` 已存在的分支, 相当于`git branch -f <branch> [<start point>] ; git checkout <branch>`

新版本的 git 也可以使用 `git-switch`, 更不容易混淆:

    git switch [<options>] (-c|-C) <new-branch> [<start-point>]

+ gsw='git switch' ; 切换到新分支
+ gswc='git switch -c' ; 创建并切换到新分支
+ git switch -C' ; 创建并切换到新分支, 如果名称已经存在, 强制切换到起始点, 默认是 HEAD

### git-cherry-pick

+ `git-cherry-pick` :从已经存在的一系列 `commits` 中应用改变

给出一个或者多个已经存在的 `commits` , 然后 `apply` 每个的 `change` , 对于每个改变生成一个 `commit` .
需要 `working tree` 是 `clean` 的.  (从 HEAD commit 之后没有修改过).

+ 选项:

`--abort`: 取消操作, 回复到pre-sequence 状态.
`--continue`: 继续操作, 利用`.git/sequencer.`中的信息. 可以在`cherry-pick ` or ` revert`失败, 解决冲突之后使用.

### git-log

`git-shortlog` - 总结 `git log` 的输出.

选项:

+ `-n`, `--numbered`:对输出结果进行排序, 按照每个提交者的提交数量, 而不是字母顺序.
+ `-s`, `--summary`: 压缩 `commit` 描述, 只总结 `commit` 数量.
+ `-g, --walk-reflogs`; 不是沿着 `commit`的祖先链(commit ancestry chain), 而是沿着时间顺序, 从最近的遍历到更早的 `reflog` 条目.
    使用这个选项时, 你不能指定排除某些提交,
    也就是说, 不能使用 `^commit`, `commit1...commit2` 和 `commit1...commit2` 的符号.

    如果使用 `--pretty` 格式, 而不是 `oneline` 和 `reference`(原因显而易见),
    这将导致输出中包含两行额外的信息, 从 `reflog` 中提取.
    输出中的 `reflog` 代号可以显示为 `ref@{Nth}`. 其中 `Nth` 是 `reflog` 中的逆序索引(reverse-chronological),
    或显示为 `ref@{timestamp}`, 带有该条目的时间戳, 取决于一些规则:

    1. 如果起点被指定为 `ref@{Nth}`, 显示 `index` 格式.
    2. 如果起点被指定为 `ref@{now}`, 显示 `时间戳`(timestamp) 格式.
    3. 如果两者都没有使用, 但在命令行中给出了`--date`, 则以 `--date` 所要求的格式显示时间戳.
    4. 否则, 显示 `index` 格式.

    在 `--pretty=oneline` 选项下, 这些信息将作为`commit`信息的前缀, 放在同一行.
    此选项不能与 `--reverse` 结合使用. 参见 git-reflog(1).
    在 `--pretty=reference` 下, 这些信息将完全不显示.

```bash
glgg = git log --graph
glgga = git log --graph --decorate --all
glgm = git log --graph --max-count=10
glod = git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset'
glods = git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset' --date=short
glol = git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset'
glola = git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset' --all
glols = git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset' --stat
glo = git log --oneline --decorate
glog = git log --oneline --decorate --graph
gloga = git log --oneline --decorate --graph --all
glp = git log --pretty=<format>
glg = git log --stat
glgp = git log --stat --patch
```

### gitk

+ ` gitk [<options>] [<revision range>] [--] [<path>...]`

```zsh
gk='\gitk --all --branches &!'
gke='\gitk --all $(git log -g --pretty=%h) &!'
```

>`git log -g --pretty=%h`  中的 `%h` 占位符表示缩写的 `commit` 哈希值, 即 7 位哈希
>`&|` 或 `&!`; 是 `bash/Zsh ` 的任务控制语法, 表示让当前 `shell` 忘记这个任务, 即 `disown`.
>这样即使关闭当前`shell`, 任务也可以继续运行.

+ `--all`:把`refs/`下的所有条目, 包括 `HEAD` 都用 `<commit>`的形式列出
+ `--branches[=<pattern>]`: 类似 `--all` , 但是要匹配 `shell glob` 模式, `?`, `*`, or `[`, `/*`
+ `--tags[=<pattern>]`: 类似`--branches`
+ `gitk`可以查看单个文件的提交历史, 使用`gitk filepath`

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
+ `git-gc` ; 清理不必要的文件, 优化本地存储库

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
