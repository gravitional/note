# bash 常用操作

[runoob-linux-tutorial](https://www.runoob.com/linux/linux-tutorial.html)

## 查看帮助

1. `命令 -- help`
1. `man 命令`

后者更加详细

首先帮助中尖括号`<>`和方括号`[]`以及省略号`...`的含义,

在方括号内的表达式`[` 和 `]`之间的字符是可选的(要去掉括号).
在尖括号内的表达式`<`和`>`之间的字符, 是必须替换的表达式(要去掉括号).

省略号表示该选项可以单个或多个

以`7z`的参数为例:

```bash
7z <command> [<switches>...] <archive_name> [<file_names>...]  [<@listfiles...>]
```

```bash
7z <命令> [[选项]...] <档案名称(压缩包名称)> [<被压缩的文件名>...] [<@文件列表(如txt文件中的文件列表)...>]
```

实例分析

```bash
7z a -tzip -p111 archive.zip txt.txt  txt.txt文件压缩为archive.zip, 压缩方式为zip 密码为111
7z x -tzip -p111 archive.zip            解压 密码为111
```

`a`为命令, 命令至少要有一个

`-t /-p`为选项(`switch`), 可选`0`个或多个, `zip`和`111`分别为`-t`和`-p`的参数, 中间不需要空格
选项(如`-r`)放前放后都行

`7z`的命令不区分大小写, 有些命令如`cp`要区分

示例

```bash
7z x archive.zip
#从压缩档案 archive.zip 中释放所有文件到当前文件夹.
7z x archive.zip -oc:\soft *.cpp
#从压缩档案 archive.zip 中释放 *.cpp 文件到 c:\soft 文件夹.
```

[linux命令行括号帮助含义](https://blog.csdn.net/yandaonan/article/details/56489513)

## alias

[Linux中使用 alias 来简化命令行输入](https://blog.csdn.net/kobejayandy/article/details/38710859)
[bash alias的使用](https://www.jianshu.com/p/63e91c67e39a)

`alias`(别名), 顾名思义, 其作用大概为命令的别名, 那么就可以它的好处就显而易见了. 可以用来缩短一些又长又难记的命令.

方法一:

+ `vim ~/.bashrc` ===> 打开bash的配置文件
+ 输入 `alias 新命令='旧命令 -选项/参数'` ===> 修改命令别名
+ `source ~/.bashrc` ===> 使得修改完的命令别名生效
+ 命令别名的增删改查都可以在`.bashrc`操作

方法二:

+ 增/改: 输入 `alias 新命令='旧命令 -选项/参数'`即完成了命令别名的添加.
+ 删: `unalias` 命令
+ 查: `alias` 命令

## 压缩/解压

### 7z

```bash
7z a win.configrc.7z -pxxxxx `@tom.configrc
7z x win.configrc.7z -pxxxxx
```

### tar

解包: `tar xvf FileName.tar`
打包: `tar cvf FileName.tar DirName`
(`tar` 仅仅是打包, 也有压缩的选项)
*****

### `.gz`

解压1: `gunzip FileName.gz`
解压2: `gzip -d FileName.gz`
压缩: `gzip FileName`

`.tar.gz` 和 `.tgz`
解压: `tar zxvf FileName.tar.gz`
压缩: `tar zcvf FileName.tar.gz DirName`
*****

### `.bz2`

解压1: `bzip2 -d FileName.bz2`
解压2: `bunzip2 FileName.bz2`
压缩:  `bzip2 -z FileName`

`.tar.bz2`
解压: `tar jxvf FileName.tar.bz2`
压缩: `tar jcvf FileName.tar.bz2 DirName`
*****

### `.bz`

解压1: `bzip2 -d FileName.bz`
解压2: `bunzip2 FileName.bz`
压缩: 未知

`.tar.bz`
解压: `tar jxvf FileName.tar.bz`
压缩: 未知
*****

### `.Z`

解压: `uncompress FileName.Z`
压缩: `compress FileName`
`.tar.Z`

解压: `tar Zxvf FileName.tar.Z`
压缩: `tar Zcvf FileName.tar.Z DirName`
*****

### `.zip`

解压: `unzip FileName.zip`
压缩: `zip FileName.zip DirName`
*****

### `.rar`

解压: `rar x FileName.rar`
压缩: `rar a FileName.rar DirName`
*****

### `.lha`

解压: `lha -e FileName.lha`
压缩: `lha -a FileName.lha FileName`
*****

### `.rpm`

解包: `rpm2cpio FileName.rpm | cpio -div`
*****

### `.deb`

解包: `ar p FileName.deb data.tar.gz | tar zxf -`
*****

### `sEx`

`.tar` `.tgz` `.tar.gz` `.tar.Z` `.tar.bz` `.tar.bz2` `.zip` `.cpio` `.rpm` `.deb` `.slp` `.arj` `.rar` `.ace` `.lha` `.lzh` `.lzx` `.lzs` `.arc` `.sda` `.sfx` `.lnx` `.zoo` `.cab` `.kar` `.cpt` `.pit` `.sit` `.sea`

解压: `sEx x FileName.*`
压缩: `sEx a FileName.* FileName`

`sEx`只是调用相关程序, 本身并无压缩, 解压功能, 请注意!

### gzip 命令

减少文件大小有两个明显的好处, 一是可以减少存储空间, 二是通过网络传输文件时, 可以减少传输的时间.
`gzip` 是在 `Linux` 系统中经常使用的一个对文件进行压缩和解压缩的命令, 既方便又好用.

语法: `gzip [选项] <压缩(解压缩)的文件名>`

该命令的各选项含义如下:

+ `-c` 将输出写到标准输出上, 并保留原有文件.
+ `-d` 将压缩文件解压.
+ `-l` 对每个压缩文件, 显示下列字段: 压缩文件的大小; 未压缩文件的大小; 压缩比; 未压缩文件的名字
+ `-r` 递归式地查找指定目录并压缩其中的所有文件或者是解压缩.
+ `-t` 测试, 检查压缩文件是否完整.
+ `-v` 对每一个压缩和解压的文件, 显示文件名和压缩比.
+ `-num` 用指定的数字 `num` 调整压缩的速度, `-1` 或 `--fast` 表示最快压缩方法(低压缩比);`-9` 或`--best`表示最慢压缩方法(高压缩比).系统缺省值为`6`.

指令实例:

+ `gzip *%` 把当前目录下的每个文件压缩成`.gz`文件.
+ `gzip -dv *` 把当前目录下每个压缩的文件解压, 并列出详细的信息.
+ `gzip -l *` 详细显示例1中每个压缩的文件的信息, 并不解压.
+ `gzip usr.tar` 压缩`tar`备份文件`usr.tar`, 此时压缩文件的扩展名为`.tar.gz`.

[Linux/Ubuntu下解压命令](https://blog.csdn.net/u013063153/article/details/53894711)

## 安装 Windows 字体

或者, 将所有的 `Windows` 字体复制到 /usr/share/fonts 目录下并使用一下命令安装字体:

```bash
mkdir /usr/share/fonts/WindowsFonts
cp /Windowsdrive/Windows/Fonts/* /usr/share/fonts/WindowsFonts
chmod 755 /usr/share/fonts/WindowsFonts/*
```

最后, 使用命令行重新生成`fontconfig` 缓存:

```bash
fc-cache
```

### vscode 调整字体

"editor.fontFamily": "Fira Code, Source Code Pro, Noto Sans CJK SC, monospace"

这个是用的谷歌的开源字体思源黑体.

`makrdown` 的话, 还有个"`markdown.preview.fontFamily`"设置.

>但是也许是我的版本较高, 版本`1.23.1`, 我的里面设置是这样子的:
>`"editor.fontFamily": "Consolas, Dengxian"`,
>英文字体用了`Consolas`, 如果不适用的字体就用`Dengxian`

我的设置

```json
"editor.fontFamily": "Fira Code Retina, Microsoft YaHei",
"editor.fontLigatures": true,
```

`makrdown` 的话, 还有个`"markdown.preview.fontFamily"`设置.

[refer1](https://zhuanlan.zhihu.com/p/40434062)
[refer2](https://segmentfault.com/a/1190000004168301)
[refer3][(https://www.v2ex.com/t/453862)

### oh-my-zsh 中定义的别名

为了方便使用, oh-my-zsh 定义了许多别名, 下面是其中一部分.

```bashrc
-='cd -'
...=../..
....=../../..
.....=../../../..
......=../../../../..
1='cd -'
2='cd -2'
3='cd -3'
4='cd -4'
5='cd -5'
6='cd -6'
7='cd -7'
8='cd -8'
9='cd -9'
_='sudo '
afind='ack -il'
diff='diff --color'
egrep='egrep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'
fgrep='fgrep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'
g=git
ga='git add'
gaa='git add --all'
gam='git am'
gama='git am --abort'
gamc='git am --continue'
gams='git am --skip'
gamscp='git am --show-current-patch'
gap='git apply'
gapa='git add --patch'
gapt='git apply --3way'
gau='git add --update'
gav='git add --verbose'
gb='git branch'
gbD='git branch -D'
gba='git branch -a'
gbd='git branch -d'
gbda='git branch --no-color --merged | command grep -vE "^(\+|\*|\s*($(git_main_branch)|development|develop|devel|dev)\s*$)" | command xargs -n 1 git branch -d'
gbl='git blame -b -w'
gbnm='git branch --no-merged'
gbr='git branch --remote'
gbs='git bisect'
gbsb='git bisect bad'
gbsg='git bisect good'
gbsr='git bisect reset'
gbss='git bisect start'
gc='git commit -v'
'gc!'='git commit -v --amend'
gca='git commit -v -a'
'gca!'='git commit -v -a --amend'
gcam='git commit -a -m'
'gcan!'='git commit -v -a --no-edit --amend'
'gcans!'='git commit -v -a -s --no-edit --amend'
gcb='git checkout -b'
gcd='git checkout develop'
gcf='git config --list'
gcl='git clone --recurse-submodules'
gclean='git clean -id'
gcm='git checkout $(git_main_branch)'
gcmsg='git commit -m'
'gcn!'='git commit -v --no-edit --amend'
gco='git checkout'
gcount='git shortlog -sn'
gcp='git cherry-pick'
gcpa='git cherry-pick --abort'
gcpc='git cherry-pick --continue'
gcs='git commit -S'
gcsm='git commit -s -m'
gd='git diff'
gdca='git diff --cached'
gdct='git describe --tags $(git rev-list --tags --max-count=1)'
gdcw='git diff --cached --word-diff'
gds='git diff --staged'
gdt='git diff-tree --no-commit-id --name-only -r'
gdw='git diff --word-diff'
gf='git fetch'
gfa='git fetch --all --prune --jobs=10'
gfg='git ls-files | grep'
gfo='git fetch origin'
gg='git gui citool'
gga='git gui citool --amend'
ggpull='git pull origin "$(git_current_branch)"'
ggpur=ggu
ggpush='git push origin "$(git_current_branch)"'
ggsup='git branch --set-upstream-to=origin/$(git_current_branch)'
ghh='git help'
gignore='git update-index --assume-unchanged'
gignored='git ls-files -v | grep "^[[:lower:]]"'
git-svn-dcommit-push='git svn dcommit && git push github $(git_main_branch):svntrunk'
gk='\gitk --all --branches'
gke='\gitk --all $(git log -g --pretty=%h)'
gl='git pull'
glg='git log --stat'
glgg='git log --graph'
glgga='git log --graph --decorate --all'
glgm='git log --graph --max-count=10'
glgp='git log --stat -p'
glo='git log --oneline --decorate'
globurl='noglob urlglobber '
glod='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset'\'
glods='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset'\'' --date=short'
glog='git log --oneline --decorate --graph'
gloga='git log --oneline --decorate --graph --all'
glol='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'\'
glola='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'\'' --all'
glols='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'\'' --stat'
glp=_git_log_prettily
glum='git pull upstream $(git_main_branch)'
gm='git merge'
gma='git merge --abort'
gmom='git merge origin/$(git_main_branch)'
gmt='git mergetool --no-prompt'
gmtvim='git mergetool --no-prompt --tool=vimdiff'
gmum='git merge upstream/$(git_main_branch)'
gp='git push'
gpd='git push --dry-run'
gpf='git push --force-with-lease'
'gpf!'='git push --force'
gpoat='git push origin --all && git push origin --tags'
gpristine='git reset --hard && git clean -dffx'
gpsup='git push --set-upstream origin $(git_current_branch)'
gpu='git push upstream'
gpv='git push -v'
gr='git remote'
gra='git remote add'
grb='git rebase'
grba='git rebase --abort'
grbc='git rebase --continue'
grbd='git rebase develop'
grbi='git rebase -i'
grbm='git rebase $(git_main_branch)'
grbs='git rebase --skip'
grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'
grev='git revert'
grh='git reset'
grhh='git reset --hard'
grm='git rm'
grmc='git rm --cached'
grmv='git remote rename'
groh='git reset origin/$(git_current_branch) --hard'
grrm='git remote remove'
grs='git restore'
grset='git remote set-url'
grss='git restore --source'
grt='cd "$(git rev-parse --show-toplevel || echo .)"'
gru='git reset --'
grup='git remote update'
grv='git remote -v'
gsb='git status -sb'
gsd='git svn dcommit'
gsh='git show'
gsi='git submodule init'
gsps='git show --pretty=short --show-signature'
gsr='git svn rebase'
gss='git status -s'
gst='git status'
gsta='git stash push'
gstaa='git stash apply'
gstall='git stash --all'
gstc='git stash clear'
gstd='git stash drop'
gstl='git stash list'
gstp='git stash pop'
gsts='git stash show --text'
gstu='git stash --include-untracked'
gsu='git submodule update'
gsw='git switch'
gswc='git switch -c'
gtl='gtl(){ git tag --sort=-v:refname -n -l "${1}*" }; noglob gtl'
gts='git tag -s'
gtv='git tag | sort -V'
gunignore='git update-index --no-assume-unchanged'
gunwip='git log -n 1 | grep -q -c "\-\-wip\-\-" && git reset HEAD~1'
gup='git pull --rebase'
gupa='git pull --rebase --autostash'
gupav='git pull --rebase --autostash -v'
gupv='git pull --rebase -v'
gwch='git whatchanged -p --abbrev-commit --pretty=medium'
gwip='git add -A; git rm $(git ls-files --deleted) 2> /dev/null; git commit --no-verify --no-gpg-sign -m "--wip-- [skip ci]"'
history=omz_history
l='ls -lah'
la='ls -lAh'
ll='ls -lh'
ls='ls --color=tty'
lsa='ls -lah'
md='mkdir -p'
mma='mathematica -singleLaunch'
rd=rmdir
which-command=whence
```
