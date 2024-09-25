# Git 大文件存储 Git LFS

[详解 Git 大文件存储(Git LFS)](https://zhuanlan.zhihu.com/p/146683392)
[Git LFS 操作指南](https://gitee.com/help/articles/4235#article-header5)

Git作为世界上最优秀的分布式版本控制工具,
也是优秀的文件管理工具, 它赋予了项目成员对项目进行远程协同开发能力,
因此受到越来越多的行业从业人员的喜爱. 很多优秀的项目管理平台,
比如国内的Gitee, 国外的Github, 也都是以Git为核心操作. 但是有些用户,
尤其是游戏行业以及媒体行业人员可能会遇到一个问题,
那就是随着提交的文件越来越多, 项目越来越大, Git的响应速度越来越慢,
更烦人的是, 在提交到远程仓库的最后一刻,
系统可能会提示用户此次提交被拒绝, 原因是提交的文件太大,
触发平台额度限制(无论是哪个平台), 相信很多人在这一刻是崩溃的.

那么该如何避免这种崩溃事件的发生呢?

下面就介绍今天的主角Git LFS(Git Large File Storage), 即Git大文件存储技术.

在Git仓库中, 对于非文本文件, 如各种多媒体文件, 软件制品文件,
二进制文件等等, 这些文件往往体积比较大, 使用Git直接管理会导致仓库的体积迅速膨胀,
进而导致Git的许多操作变慢, 同时也影响仓库上传到远程端.

Git LFS相当于Git的一种插件式增强工具,
简单讲, 它是在Git仓库使用这些文件的 指针代替 实际文件,
而把实际文件存储在远程端LFS服务器, 同时在本地仓库中实时追踪这些文件的变动.

![a diagram showing how Git LFS works](https://images.gitee.com/uploads/images/2022/0419/143214_bc84b95c_551147.gif)

## 原理

根据 Git LFS 官方帮助文档描述:

Git LFS是基于Git的 .gitattributs 配置文件的特性,
用 smudge过滤器基于 指针文件寻找大文件内容,
用 clean过滤器在对大文件改动时, 创建指针文件的新版本.
同时还用 pre-push钩子将大文件上传到Git LFS服务器,
即在 git-push时,  如果提交中包含被LFS跟踪的大文件,
pre-push钩子会检测到, 并执行上传Git LFS服务器的动作.

因此, 如果一个仓库中包含LFS内容, 但是在推送时不想推送这类文件, 只要加上 --no-verify选项就行, 即:

```bash
git push --no-verify
```

`--no-verify` 选项告诉 git push完全跳过 pre-push钩子.

前面提到被LFS管理的文件, 本地仓库中保存的内容实际上是指针文件, 其格式类似于下面这样:

```bash
$ git show HEAD:2.svg

version https://git-lfs.github.com/spec/v1
oid sha256:158213f90f8b27012034c6f58db63e1861b12aa122d98910de311bf1cb1e50a0
size 14651
(END)
```

`version` 表示LFS的版本
oid表示文件对象的唯一hash值
size表示文件的大小

## 安装

注意: 以下安装以及命令使用演示,
均是基于Linux中命令行的方式进行, Windows上的图像界面操作, 由于时间关系, 暂不涉及.

安装依赖:  Git >=1.8.5

Linux 系统:

curl -s https://packagecloud.io/install/repositories/github/git-lfs/script.deb.sh | sudo bash

sudo apt-get install git-lfs
https://packagecloud.io/github/git-lfs/install#bash

其它操作系统上的安装, 见官方安装文档:

https://github.com/git-lfs/git-lfs#installing

## 配置

第一步: 在Git仓库中为仓库设置相关配置:

```bash
git lfs install
```

Tips:

这个命令会自动改变Git配置文件 `.gitconfig`, 而且是全局性质的, 会自动在配置文件中增加如下配置:

```conf
[filter "lfs"]
clean = git-lfs clean -- %f
smudge = git-lfs smudge -- %f
process = git-lfs filter-process
required = true
```

第二步: 选择要用LFS追踪的文件:

```bash
$ git lfs track "*.svg"
# 或者具体到某个文件
$ git lfs track "2.png"
$ git lfs track "example.lfs"
```

Tips:

这个命令会更改仓库中的 .gitattributes配置文件(如果之前不存在这个文件, 则会自动新建):
查看如下:

```bash
$ cat .gitattributes
*.svg filter=lfs diff=lfs merge=lfs -text
*.png filter=lfs diff=lfs merge=lfs -text
```

好奇的同学可能要问了, 如果想知道自己到底追踪了哪些文件, 怎么办?

好办, 一条命令解决!

通过 git lfs ls-files 可以随时查看正在被LFS追踪的文件:

```bash
$ git lfs ls-files
9a3c7dae41 * 1.png
d61cf5835a * 2.png
158213f90f * 3.svg
```

git add file 之后文件才可能被追踪, 也才能查看得到

可能还会有人好奇, 如果不想LFS追踪某个文件, 怎么办?

好办, 还是一条命令解决:

```bash
$ git lfs untrack "1.png"
```

解决了好奇同学的问题, 我们接着前面的第二步来, 选择好需要LFS管理的文件之后, 最好先保存一下配置:

第三步: 保存并提交配置:

```bash
$ git add .gitattributes
$ git commit -m "add .gitattributes"
```

配置总结:

安装Git LFS之后, 只需三步, 即可在仓库中配置LFS功能, 即:

```bash
#step 1
$ git lfs install
#step 2
$ git lfs track files
# step 3
$ git add .gitattributes
```

实际上, 由于第一步是全局配置, 所以执行一次即可, 后续有其它仓库需要使用LFS, 则不需要再次执行, 除非中途取消了LFS配置.

Tips:  运行 git lfs uninstall 即可取消LFS的全局配置

使用场景
场景一:
有一天你在Gitee上寻找感兴趣的项目, 很快你就找到一个有价值的游戏项目, 并且决定马上fork并clone下来:

```bash
$ git clone git@gitee.com:hightest/lfs-demo.git my-project
Cloning into 'lfs-copy'...
Enter passphrase for key '/home/git/.ssh/id_ed25519':
...
Filtering content: 100% (5/5), 1.51 MiB | 257.00 KiB/s, done.
```

你只是稍微修改了一个示例文件example.lfs, 然后顺便git diff一下, 看下修改变化:

```bash
$cd my-project
# edit example.lfs
$ git diff
diff --git a/example.lfs b/example.lfs
index 9550b5b..8bfca2b 100644
--- a/example.lfs
+++ b/example.lfs
@@ -1,3 +1,3 @@
version https://git-lfs.github.com/spec/v1
-oid sha256:fa3b58d0150ccbaed40ab94fd5574ae8225e83117c076b586ef08ff38be8d923
-size 69
+oid sha256:d8f84506d6b9e804852c3b15b921893606b4c2cbe388d1cc118bd42101eed2a8
+size 63
(END)
```

git diff显示的修改变动不是你期望的, 为啥会出现这个差异呢?

如果你看过前面的原理部分, 那你马上就能看明白, 这是LFS指针文件的差异, 这说明你下载的这个仓库是用了LFS来管理了文件.

此时仓库的实际存储的文件大小只有132 Bytes, 而它的实际大小是9.18 MiB, 大小相差几个数量级.

这样做的好处非常明显, 对于很大的文件, 可以只用很小的空间来管理它.

![image.png](https://images.gitee.com/uploads/images/2021/0928/000246_0b965b05_7670704.png)

场景二:
作为一名游戏开发人员, 你一直想设计开发一款好玩的游戏, 场景一中使用的项目给了你灵感,
你决定在这个基础上进行深度开发, 你在这个仓库里面加入了很多图片文件,
 音效文件等游戏资源文件, 开始每次git add/commit/push都很顺利,
 但有一次你把这些文件打包成 biger.zip, 想一次推送到远程仓库, 结果最后推送失败, 系统提示如下:

```bash
$ git push origin master

Enter passphrase for key '/home/git/.ssh/id_ed25519':
...
remote: Please remove the file from history and try again. (https://gitee.com/help/articles/4232)
To gitee.com:hightest/lfs-demo.git
 ! [remote rejected] master -> master (pre-receive hook declined)
error: failed to push some refs to 'gitee.com:hightest/lfs-demo.git'
```

很明显, 由于推送的单个文件太大, 超过配额300 MB, 所以推送被拒绝.

同时, 也能明显感觉得到, Git的各种基本操作变得卡顿, 延迟.

这个时候, 根据场景一的启发, 你想到可以使用Gtiee的LFS服务, 将大文件使用LFS管理, 而仓库只保存它的指针信息, 就能避免此问题.

使用LFS管理历史大文件:
如果一个仓库中原来已经提交了一些大文件, 此时即使运行 git lfs track也不会有效的.

为了将仓库中现存的大文件应用到LFS, 需要用 git lfs migrate导入到LFS中:

```bash
$ git lfs migrate import --include-ref=master --include="biger.zip"
migrate: override changes in your working copy?  All uncommitted changes will be lost! [y/N] y
migrate: changes in your working copy will be overridden ...
migrate: Sorting commits: ..., done.
migrate: Rewriting commits: 100% (11/11), done.
  master        f9be3c554e9010ea5e0e23a6a0c6e53dca6c23b0 -> 53d5e655fe7cfd985f75384b92ac5414ad2ff394
migrate: Updating refs: ..., done.
migrate: checkout: ..., done.
```

--include-ref 选项指定导入的分支

如果向应用到所有分支, 则使用--everything选项

--include 选项指定要导入的文件. 可以使用通配符, 批量导入.

上述操作会改写提交历史, 如果不想改写历史, 则使用 --no-rewrite选项, 并提供新的commit信息:

```bash
$ git lfs migrate import --no-rewrite -m "lfs import"
```

将本地历史提交中的文件纳入到LFS管理后, 如果重改了历史, 再次推送代码时, 需要使用强制推送.

这里选择改变提交历史, 所以还需要使用 --force强制推送:

```bash
$ git push origin master --force

Enter passphrase for key '/home/git/.ssh/id_ed25519':
...
remote: Powered by GITEE.COM [GNK-6.1]
To gitee.com:hightest/lfs-demo.git
 + cefd169...53d5e65 master -> master (forced update)
```

至此, 已经将历史提交中的大文件迁移到远程LFS服务器, 本地Git仓库, 只保留这个大文件的指针文件, 所以推送也不会再触发额度限制. 推送成功之后, 远程仓库就与本地保存一致, 如场景一中的图示, 它只管理这个大文件的指针文件.

推送成功后在仓库管理页面可以看到:

![image.png](https://images.gitee.com/uploads/images/2021/0928/015631_38f12078_7670704.png)

这里显示的是LFS Server实际管理的文件的大小, 而Git仓库管理的大小则为134 Bytes!

![image.png](https://images.gitee.com/uploads/images/2021/0928/100812_6f9ec420_7670704.png)

场景三:
作为Git重度使用者的你, 日常工作中必须使用Git管理你的文件,
但是经历过上面对历史提交重写, 并上传LFS服务器的你,
学会了一开始在仓库中配置LFS功能, 保证每一次提交, 推送都保持完美.

在一个新的项目中, 在初始阶段, 你已经配置好了LFS.
此时有更大一个文件 biggerthanbigger.zip, 大小是778M, 远远超过单个文件大小限制.

使用LFS管理新增大文件

```bash
$ cd new-project
$ git add biggerthanbigger.zip
$ git commit -m "add bigger than bigger zip file"
```

然后提交到远程仓库,  因为使用了LFS服务, 如果不出意外, 这次不会被拒绝.

```bash
$ git push origin master
Enter passphrase for key '/home/git/.ssh/id_ed25519':
...
To gitee.com:hightest/new-project.git
   dfe8b09..5f03bab  master -> master
```

但实际上, 意外是很可能发生的!

因为推送的文件过于大, 很有可能因为超过LFS的配额而推送失败, 虽然LFS是专门用来管理大文件的, 但是也不能无限制存放大文件, 毕竟这不是网盘.

关于Gitee LFS大文件的限制情况, 请看下面的配额说明.

Gitee LFS 配额说明
目前LFS功能只对付费企业开放, 详细配额如下:

免费版	基础版	标准版	高级版	尊享版	私有化部署版
0 GB	1 GB	1 GB	1 GB	1 GB	不限
查询链接: https://gitee.com/enterprises#price

对于已经开通LFS功能的企业, 如果想查看企业各个仓库的LFS容量使用情况, 进入企业工作台(Dashboard), 假设你的企业名为My-Enterprise, 则地址为https://e.gitee.com/My-Enterprise/dashboard:

![image.png](https://images.gitee.com/uploads/images/2021/0928/102835_3598e735_7670704.png)

对于企业下各个仓库的LFS容量使用情况, 还是进入企业工作台, 在管理面板中, 选择拓展应用项, 点击 LFS 即可查看详情:

![image.png](https://images.gitee.com/uploads/images/2021/0928/021431_a4e572f1_7670704.png)

可以看到企业内, 每个使用LFS功能的仓库的配额使用情况, 以及总的使用量. 点击仓库名称, 还可以跳转到这个仓库的管理页面.

如果企业对大文件的需求比较大, 目前的1 GB容量不够用, 不用着急, Gitee有解决方案, 那就是自主扩容.

还是在企业管理工作台面板, 在最顶层, 有[续费/升级/扩容]链接, 可直达升级扩容界面:

![image.png](https://images.gitee.com/uploads/images/2021/0928/022039_7e90f00c_7670704.png)

根据企业需求, 最高可配置100G的LFS空间, 这对于大多数项目来说, 空间是充足的.

## 总结:

Git LFS是一个易于安装, 易于配置, 使用高效的Git拓展工具,
它能有效的管理仓库中的大文件, 避免仓库体积过大, 影响项目管理效率.
同时, Gitee平台对Git LFS功能有很好的支持, 再加上灵活的空间配额管理, 让企业实现自动扩容,
方便的解决了企业在项目开发中遇到的仓库大文件问题.

## 参考链接

https://git-lfs.github.com/
https://git-scm.com/docs/gitattributes
https://zzz.buzz/zh/2016/04/19/the-guide-to-git-lfs/
