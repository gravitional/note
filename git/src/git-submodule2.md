# Git中submodule的使用

[Git中submodule的使用](https://zhuanlan.zhihu.com/p/87053283)

## 背景

面对比较复杂的项目, 我们有可能会将代码根据功能拆解成不同的子模块.
`主项目` 对 `子模块` 有依赖关系, 却又并不关心 `子模块` 的内部开发流程细节.

这种情况下, 通常不会把所有源码都放在同一个 `Git` 仓库中.

有一种比较简单的方式, 是在当前工作目录下,
将子模块文件夹加入到 `.gitignore` 文件内容中,
这样主项目就能够无视子项目的存在.
这样做有一个弊端就是, 使用主项目的人需要有一个先验知识:
需要在当前目录下放置一份某版本的 `子模块` 代码.

还有另外一种方式可供借鉴, 可以使用 `Git` 的 `submodule` 功能, 也是这篇文章的主题.

实际上 Git 工具的 `submodule` 功能,
就是建立了当前项目与子模块之间的依赖关系:
子模块`路径`, 子模块的 `远程仓库`, 子模块的 `版本号`.

## 使用流程

[main]: https://gitee.com/mirrors_trending/OpenSSL-2022.git
[sub1]: https://github.com/username/sub1.git

假定我们有两个项目: `main` 和 `sub1`,
其中 `main` 表示主项目, 而 `sub1` 表示子模块项目.

其中 `main` 的远程仓库地址为 [main][],
而 `sub1` 的远程仓库地址为 [sub1][].

接下来, 我们希望在 `main` 中添加 `sub1` ,
而又保持 `sub1` 自身独立的版本控制.

### 创建 submodule

+ 使用 `git submodule add <submodule_url>` 命令可以在项目中创建 `子模块`.
演示:

    ```bash
    # 创建根项目 OpenSSL,  并进入根目录
    git clone https://gitee.com/mirrors_trending/OpenSSL-2022.git; cd  OpenSSL-2022
    # 添加子项目依赖
    git submodule add https://gitee.com/mirrors/tinyre.git
    ```

+ 此时项目仓库中会多出两个文件: `.gitmodules` 和 `tinyre` .
`.gitmodules` 就是子模块的相关信息;

    ```conf
    文件<.gitmodules>
    [submodule "tinyre"]
            path = tinyre
            url = https://gitee.com/mirrors/tinyre.git
    ```

    如果此前项目中已经存在 `.gitmodules` 文件, 则会在文件内容中多出上述三行记录.
    事实上, 此时在 `.git/config` 文件中也会多出一些信息,
    在 `.git/modules` 文件夹下也会多出一份内容.

+ 如果使用 `git diff` 查看, 会有类似下面的输出

    ```diff
    diff --git a/.gitmodules b/.gitmodules
    ...

    diff --git a/tinyre b/tinyre
    ...
    -Subproject commit 485d194331eba4c97f9d8aa46deff88939ed8910
    ```

    `Subproject commit ...` 是 git 根据 `commit` 哈希自动生成的,
    子模块当前版本的 `版本号` 信息, 不对应实际的文件.

+ 通常此时可以使用

    ```bash
    git commit -m "add submodule tinyre"
    ```

    提交一次, 表示引入了某个子模块.
    提交后在主项目仓库中, 会显示出 `子模块文件夹`, 并带上其所在仓库的 `版本号`.

    ![img](https://pic3.zhimg.com/v2-8b77b08acc39bfc9cb569116ff6905e2_r.jpg)

### 获取 submodule

上述步骤在创建 `子模块` 的过程中, 会自动将相关代码克隆到 `对应路径`,
但对于后续使用者而言, 对于主项目使用普通的 `clone` 操作,
`并不会` 拉取到子模块中的实际代码.

+ 使用以下命令进行克隆, 完成后 `OpenSSL-2022/tinyre` 文件夹是空的:

    ```bash
    cd test;
    git clone https://gitee.com/mirrors_trending/OpenSSL-2022.git
    ```

+ 如果也希望拉取 `子模块代码`,
一种方式是在克隆主项目的时候带上参数 `--recurse-submodules`,
这样会递归地将项目中所有子模块的代码拉取.

    ```bash
    cd /path/to/temp2
    git clone  --recurse-submodules  https://gitee.com/mirrors_trending/OpenSSL-2022.git
    ```

    此时 `OpenSSL-2022/tinyre` 文件夹是有内容的, 并且固定在某个 `Git` 提交的版本上.

+ 另外一种可行的方式是, 在当前 `主项目` 中执行:

    ```bash
    git submodule init
    git submodule update
    ```

    则会根据 `主项目` 的配置信息, `拉取` 更新子模块中的代码.

## 子模块内容的更新

对于 `子模块` 而言, 并不需要知道引用自己的 `主项目` 的存在.
对于自身来讲, 子模块就是一个完整的 `Git` 仓库,
按照正常的 `Git` 代码管理规范操作即可.

对于 `主项目` 而言, 子模块的内容发生变动时, 通常有三种情况:

1. 当前项目下, 子模块文件夹内的内容发生了 `未追踪的` 内容变动;
1. 当前项目下, 子模块文件夹内的内容发生了 `版本变化`;
1. 当前项目下, 子模块文件夹内的内容没变,  `远程有更新`;

### 情况1: 子模块有未跟踪的内容变动

对于第1种情况, 通常是在开发环境中, `直接修改` 子模块文件夹中的代码导致的.

此时在主项目中使用 `git status` 能够看到关于 `子模块尚未暂存` 以备提交的变更,
但是 `主项目` 不负责管理子模块的更新, 使用 `git add/commit` 对其也不会对子模块产生影响.

```bash
$ git status

位于分支 master
您的分支与上游分支 'origin/master' 一致.
尚未暂存以备提交的变更:
(使用 "git add <文件>..." 更新要提交的内容)
(使用 "git checkout -- <文件>..." 丢弃工作区的改动)
(提交或丢弃子模组中未跟踪或修改的内容)
修改:  sub1 (未跟踪的内容)
修改尚未加入提交(使用 "git add" 和/或 "git commit -a")
```

在此情景下, 通常需要进入 `子模块文件夹`,
按照子模块内部的版本控制体系 `提交代码`.

当提交完成后, 主项目的状态则进入了 `情况2`,
即当前项目下子模块文件夹内的内容发生了 `版本变化`.

### 情况2: 子模块有版本变化

当子模块版本变化时, 在主项目中使用 `git status` 查看仓库状态时, 会显示子模块有新的提交:

```bash
$ git status

位于分支 master
您的分支与上游分支 'origin/master' 一致.
尚未暂存以备提交的变更:
(使用 "git add <文件>..." 更新要提交的内容)
(使用 "git checkout -- <文件>..." 丢弃工作区的改动)
修改:  sub1 (新提交)
修改尚未加入提交(使用 "git add" 和/或 "git commit -a")
```

在这种情况下, 可以使用 `git add/commit` 将其添加到主项目的代码提交中,
`实际的改动` 就是那个 `子模块 文件 ` 所表示的版本信息:

```bash
git diff HEAD HEAD^
diff --git a/sub1 b/sub1
index ace9770..7097c48 160000
--- a/sub1
+++ b/sub1
@@ -1 +1 @@
-Subproject commit ace977071f94f4f88935f9bb9a33ac0f8b4ba935
+Subproject commit 7097c4887798b71cee360e99815f7dbd1aa17eb4
```

通常当 `子项目` 更新后, 主项目修改其 `所依赖的版本` 时,
会产生类似这种情景的 `commit` 提交信息.

### 情况3: 子模块远程有更新

通常来讲, 主项目与子模块的开发不会恰好是同时进行的.
通常是 `子模块` 负责维护自己的版本升级后,
推送到 `远程仓库`, 并告知 `主项目` 可以更新对子模块的 `版本依赖`.

在这种情况下, 主项目 是比较茫然的.

之前曾经提到, 主项目 可以使用 `git submodule update` 更新子模块的代码,
但那是指 当前主项目文件夹下,
`子模块文件内容` 与 当前主项目 `记录的子模块版本` 不一致时,
会参考后者进行更新.

但如今这种情况下, 当前主项目 `记录的子模块版本` 还没有变化,
在主项目看来当前情况一切正常.

此时, 需要让主项目 `主动进入` 子模块拉取新版代码, 进行升级操作.

通常流程是:

```bash
cd sub1
git pull origin master
```

子模块目录下的代码版本会发生变化, 转到 `情况2` 的流程进行 `主项目` 的提交.

当主项目的子项目特别多时, 可能会不太方便,
此时可以使用 `git submodule` 的一个命令 `foreach` 执行:

```bash
git submodule foreach 'git pull origin master'
```

### 情况汇总

终上所述, 可知在不同场景下子模块的更新方式如下:

+ 对于 `子模块`, 只需要管理好自己的版本, 并推送到 `远程分支` 即可;
+ 对于 `父模块`, 若 `子模块` 版本信息未提交,
需要更新 `子模块` 目录下的代码, 并执行 `commit` 操作提交 `子模块版本信息`;

+ 对于 `父模块`, 若 `子模块` 版本信息已提交,
需要使用 `git submodule update` ,
Git 会自动根据 `子模块版本` 信息更新所有子模块目录的相关代码.

## 删除子模块

网上流传了一些偏法, 主要步骤是直接移除模块,
并手动修改 `.gitmodules`, `.git/config` 和 `.git/modules` 内容.
包含了一大堆类似 `git rm --cached <sub-module>`,
`rm -rf <sub-moduel>`, `rm .gitmodules` 和 `git rm --cached` 之类的代码.
实际上这是一种比较野的做法, 不建议使用.

根据官方文档的说明,
应该使用 `git submodule deinit` 命令卸载一个子模块.
这个命令如果添加上参数 --force,
则子模块工作区内即使有本地的修改, 也会被移除.

```bash
git submodule deinit sub1
git rm sub1
```

执行 `git submodule deinit sub1` 命令的实际效果,
是自动在 `.git/config` 中删除了以下内容:

```bash
[submodule "sub1"]
url = https://github.com/username/sub1.git
```

执行 `git rm sub1` 的效果, 是移除了 `sub1` 文件夹,
并自动在 `.gitmodules` 中删除了以下内容:

```conf
[submodule "sub1"]
path = sub1
url = https://github.com/username/sub1.git
```

此时, 主项目中关于子模块的信息基本已经删除
(虽然貌似 `.git/modules` 目录下还有残余):

```bash
➜ main git:(master) ✗ gs
位于分支 master
您的分支与上游分支 'origin/master' 一致.
要提交的变更:
(使用 "git reset HEAD <文件>..." 以取消暂存)
修改:  .gitmodules
删除:  sub1
```

可以提交代码:

```bash
git commit -m "delete submodule sub1"
```

至此完成对子模块的删除.

## 总结

当项目比较复杂, 部分代码希望独立为子模块进行版本控制时,
可以使用 `git submodule` 功能.

使用 `git submodule` 功能时, `主项目仓库` 并不会包含子模块的文件,
只会保留一份子模块的 `配置信息` 及 `版本信息`, 作为主项目版本管理的一部分.

本篇文章简单介绍了 `git submodule` 的添加和删除,
以及项目开发过程中主项目与子模块不同状态时刻的操作方式.

## solver常用命令

+ 初始化仓库

```bash

git clone http://... --recursive --branch develop .

git submodule update --recursive --init src/common
git submodule update --recursive --init src/structure

git submodule foreach --recursive 'git checkout develop || true'
git config --local http.proxy http://...
```

+ 更新代码

```bash
git pull
git submodule foreach --recursive 'git pull || true'
```

+ 切换分支xxx(位于主仓库)

```bash
git checkout xxx
git submodule foreach --recursive 'git checkout xxx || true'
```

solver 为主仓库, 管理一级仓库(版本信息);
thirdparty 为第三方 sdk;
common, structure, fluid, electromagnetics, 
multibody 为一级子仓库, 管理二级子仓库(版本信息);
其余为二级子仓库, 管理代码.

各组员 在二级仓库提交代码后, 
若涉及不同仓库则必须在相应的一级子仓库中提交对应更新(二级子仓库的版本信息), 二者需时刻保持一致.

+ 提交子仓库

```bash
git add <子仓库路径>
git commit -m 'log message'
git push
```

+ 回退版本(建议另开一个文件夹只用于回退版本, 不做任何修改)

```bash
git checkout xxx(主仓库solver 或一级仓库的commit号,并位于相应位置)
git submodule update <path子模块1>
git submodule update <path子模块2>
...
```
