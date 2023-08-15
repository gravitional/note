# git tags 标签创建与管理

`git tag`  列出已有的标签

`git tag -l 'v1.8.5*'` 查找  `'v1.8.5*'`

## 创建附注标签

即完整标签

```bash
git tag -a v1.4 -m "my version 1.4"
```

`-m` 选项指定了一条将会存储在标签中的信息

```bash
git show v1.4
```

`git show`命令可以看到标签信息与对应的提交信息

## 轻量标签

```bash
git tag v1.4-lw
```

轻量标签本质上是提交 `校验和`, 将其存储到一个文件中 -- 没有保存任何其他信息.
创建轻量标签, 不需要使用 `-a, -s` 或 `-m` 选项, 只需要提供标签名字

## 后期打标签

`git tag -a v1.2 9fceb02`

在命令的末尾指定提交的校验和(或部分校验和)

## 推送标签

`git push origin v1.5`
`git push origin [tagname]`
`git push origin --tags`

默认情况下, `git push`命令并不会传送标签到远程仓库服务器上.
在创建完标签后你必须显式地推送标签到共享服务器上,这个过程就像共享`远程分支`一样

如果想要一次性推送很多标签, 也可以使用带有`--tags`选项的`git push`命令. 这将会把所有不在远程仓库服务器上的标签全部传送到那里.

```bash
$ git push origin --tags
Counting objects: 1, done.
Writing objects: 100% (1/1), 160 bytes | 0 bytes/s, done.
Total 1 (delta 0), reused 0 (delta 0)
To git@github.com:schacon/simplegit.git
 * [new tag]         v1.4 -> v1.4
 * [new tag]         v1.4-lw -> v1.4-lw
```

现在, 当其他人从仓库中克隆或拉取, 他们也能得到你的那些标签.

## 删除标签

```bash
git tag -d <tagname>
```

例如 `git tag -d v1.4-lw` 将删除标签 'v1.4-lw' (was e7d5add)

```bash
git push <remote> :refs/tags/<tagname>
```

你必须使用 `git push <remote> :refs/tags/<tagname>` 来更新你的远程仓库:
`$ git push origin :refs/tags/v1.4-lw`
`To /git@github.com:schacon/simplegit.git`
`- [deleted]         v1.4-lw`

## checkout 到某个 标签

如果你想查看某个标签所指向的文件版本, 可以使用`git checkout`命令,
虽然说这会使你的仓库处于"分离头指针(`detacthed HEAD`)"状态 -- 这个状态有些不好的副作用:

```bash
$ git checkout 2.0.0
Note: checking out '2.0.0'.
```

比如说你正在修复旧版本的错误 -- 这通常需要创建一个新分支:

```bash
git checkout -b version2 v2.0.0
Switched to a new branch 'version2'
```
