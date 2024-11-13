# Git 对象, 路径指定

## Git Objects

Git is a content-addressable filesystem. Great. What does that mean?
It means that at the core of Git is a simple `key-value` data store.
What this means is that you can insert any kind of content into a Git repository,
for which Git will hand you back a unique key you can use later to retrieve that content.

As a demonstration, let"s look at the plumbing command((建筑物的)管路系统,自来水管道)
`git hash-object`, which takes some data, stores it in your `.git/objects`
directory (the object database), and gives you back the unique key that now refers to that data object.

First, you initialize a new Git repository and verify
that there is (predictably) nothing in the objects directory:

```git
$ git init test
Initialized empty Git repository in /tmp/test/.git/
$ cd test
null
$ find .git/objects
.git/objects
.git/objects/info
.git/objects/pack
$ find .git/objects -type f
null
```

Git has initialized the objects directory and created pack
and info subdirectories in it, but there are no regular files.
Now, let"s use `git hash-object` to create a new data object and manually store it in your new Git database:

```git
$ echo 'test content' | git hash-object -w --stdin
d670460b4b4aece5915caf5c68d12f560a9fe3e4
```

In its simplest form, `git hash-object` would take the content you handed to it and merely return the unique key that would be used to store it in your Git database.
The `-w` option then tells the command to not simply return the key, but to write that object to the database.
Finally, the --stdin option tells `git hash-object` to get the content to be processed from stdin; otherwise, the command would expect a filename argument at the end of the command containing the content to be used.

The output from the above command is a `40-character checksum hash`. This is the SHA-1 hash — a checksum of the content you"re storing plus a header, which you"ll learn about in a bit. Now you can see how Git has stored your data:

```git
$ find .git/objects -type f
.git/objects/d6/70460b4b4aece5915caf5c68d12f560a9fe3e4
```

If you again examine your objects directory, you can see that it now contains a file for that new content.
This is how Git stores the content initially — as a single file per piece of content, named with the SHA-1 checksum of the content and its header. The subdirectory is named with the first 2 characters of the SHA-1, and the filename is the remaining 38 characters.

Once you have content in your object database, you can examine that content with the `git cat-file` command. This command is sort of a Swiss army knife for inspecting Git objects. Passing `-p` to `cat-file` instructs the command to first figure out the type of content, then display it appropriately:

```git
$ git cat-file -p d670460b4b4aece5915caf5c68d12f560a9fe3e4
test content
```

## Tree 对象

`tree`, 解决了git中储存文件名的问题, 并且允许你把一组文件存在一起.

git 存储文件的方式类似于`Unix`系统, 但是有点简化.所有的内容被存储为`tree`and`blob`, `tree`对应UNIX 文件夹,
`blob`对应`inodes`or `file contents`

一个`tree`包含一个或多个条目, 每一个条目是一个`SHA-1 hash`of a blob or `subtree`with its associated mode, type, and filename. 比如, 项目中最新的一个tree看起来大概像:

```git
$ git cat-file -p master^{tree}
100644 blob a906cb2a4a904a152e80877d4088654daad0c859      README
100644 blob 8f94139338f9404f26296befa88755fc2598c289      Rakefile
040000 tree 99f1a6d12cb4b6f19c8655fca46c3ecf317074e0      lib
```

The `master^{tree}` syntax 指定了master分支的最新提交.
注意the `lib` subdirectory isn't a `blob` but a `pointer` to another `tree`:

```git
$ git cat-file -p 99f1a6d12cb4b6f19c8655fca46c3ecf317074e0
100644 blob 47c6340d6459e05787f644c2447d2595f5d3a54b      simplegit.rb
```

注意, 取决于你用的shell, 当你使用`master^{tree}` syntax时, 可能会遇到一些错误

In CMD on Windows, the `^` character is used for escaping, so you have to double it to avoid this: `git cat-file -p master^^{tree}`.
When using PowerShell, parameters using `{}` characters have to be quoted to avoid the parameter being parsed incorrectly: `git cat-file -p 'master^{tree}'`.

If you"re using `ZSH`, the `^` character is used for globbing, so you have to enclose the whole expression in quotes: `git cat-file -p "master^{tree}"`.

*** 你可以相当容易的创建你自己的tree

git 通常用 `staging` area ( `index`)中的state创建`tree`, 然后写入一系列tree对象.所以首先来`staging`一些文件

先创建一个`index`with a single entry--the first version of your `test.txt file`, 使用管道命令`git update-index`, 使用这个命令, 手动添加earlier version of the `test.txt`to a new staging area.

你需要用`--add` option, 因为这个文件在your staging area中还不存在,
还有`--cacheinfo`, because the file you're adding isn't in your directory but is in your database.

Then, you specify the `mode`, `SHA-1`, and `filename`:

```git
git update-index --add --cacheinfo 100644 \
83baae61804e65cc73a7201a7252750c76066a30 test.txt
```

在本例中, 你指定的模式是`100644`, 代表它是正常文件.其他选项有`100755`, 代表可执行文件;
还有`120000`, 代表符号链接.

这些模式来自于 普通Unix mode, 但是比较简化--这是对files(blobs)合法的三个mode.

现在, 用 `git write-tree` 把`stage`写入到一个`tree` object 中.
不用加上`-w` option--这个命令会自动创建一个tree from the state of the index , 如果它还不存在.

```git
$ git write-tree
d8329fc1cc938780ffdd9f94e0d364e0ea74f579
$ git cat-file -p d8329fc1cc938780ffdd9f94e0d364e0ea74f579
100644 blob 83baae61804e65cc73a7201a7252750c76066a30      test.txt
```

你可以验证, 它的确是一个tree object, using `git cat-file -t` command you saw earlier:

```git
$ git cat-file -t d8329fc1cc938780ffdd9f94e0d364e0ea74f579
tree
```

你可以创建一个新tree, 包含 `test.txt`的新版本和一个新文件:

```git
$ git read-tree --prefix=bak d8329fc1cc938780ffdd9f94e0d364e0ea74f579
null
$ git write-tree
3c4e9cd789d88d8d89c1073707c3585e41b0e614
$ git cat-file -p 3c4e9cd789d88d8d89c1073707c3585e41b0e614
040000 tree d8329fc1cc938780ffdd9f94e0d364e0ea74f579      bak
100644 blob fa49b077972391ad58037050f2a75f74e3671e92      new.txt
100644 blob 1f7a7a472abf3dd9643fd615f6da379c4acb3e3a      test.txt
```

## Commit 对象

要创建一个提交对象, 你需要调用`commit-tree`, 并指定一棵树的`SHA-1`, 把想要提交的对象直接放在树的前面, 如果有的话. 从第一个树开始:

```git
$ echo 'first commit' | git commit-tree d8329f
fdf4fc3344e67ab068f836878b6c4951e3b15f3d
```

由于创建时间和作者数据不同, 你会得到不同的哈希值.
在本章的后面, 用你自己的`checksums`替换提交和标签的哈希值. 现在你可以用 `git cat-file` 来查看你的新提交对象了.

```git
git cat-file -p fdf4fc3
tree d8329fc1cc938780ffdd9f94e0d364e0ea74f579
author Scott Chacon <schacon@gmail.com> 1243040974 -0700
committer Scott Chacon <schacon@gmail.com> 1243040974 -0700

first commit
```

提交对象的格式很简单:
它指定了当时项目快照的顶层树.
父级提交(如果有的话); 作者/提交人信息(使用您的用户名和电子邮件配置以及一个时间戳).
一个空行, 然后是提交信息.

接下来, 你要提交另外两个对象, 每个对象都引用在它之前的提交.

```bash
echo 'second commit' | git commit-tree 0155eb -p fdf4fc3
cac0cab538b970a37ea1e769cbbde608743bc96d
echo 'third commit'  | git commit-tree 3c4e9c -p cac0cab
1a410efbd13591db07496601ebc7a059dd55cfe9
```

这三个提交对象分别指向你创建的三个快照树中的一个.
奇怪的是, 你现在有了一个真正的 `Git` 历史, 如果你使用上次提交的 `SHA-1` , 就可以用 `git log` 命令查看:

```bash
git log --stat 1a410e
commit 1a410efbd13591db07496601ebc7a059dd55cfe9
Author: Scott Chacon <schacon@gmail.com>
Date:   Fri May 22 18:15:24 2009 -0700
...
```

真了不起. 你刚刚完成了建立 `Git` 历史的底层操作, 而没有使用任何前端命令.
这就是`Git`在运行`git add`和 `git commit` 命令时所做的事情--它为发生变化的文件存储`blob`, 更新stage, 写出树, 写入提交对象, 引用顶层树, 以及最邻近的前一个提交.

这三个主要的Git对象--`blob`, `树`和`提交`--最初是作为单独的文件存储在你的`.git/objects`目录下. 下面是示例目录中的所有对象, 并注释了它们的存储内容.

```git
$ find .git/objects -type f
.git/objects/01/55eb4229851634a0f03eb265b69f5a2d56f341 # tree 2
.git/objects/1a/410efbd13591db07496601ebc7a059dd55cfe9 # commit 3
...
```

## Tree-ish

`Tree-ish` 是一个术语, 指的是一个标识符, 最终指引到一个(子)目录树(Git 把目录成为"tree" and "tree objects")

如果想指定文件夹`foo`, 正确的方法是使用`git`的"tree-ish"语法:

```bash
HEAD:README, :README, master:./README
```

`:`(冒号)前面是tree-ish object, 冒号后面是具体的路径, 也就是说`master:foo`是正确的语法, 而不能写成`master/foo`.
注: `master`是分支的名字, `foo`是具体的路径.

### refspec

应该是`Reference Specification`的缩写, 字面意思就是具体的引用.
它其实是一种格式, `git`通过这种格式的判断来获取不同引用下的数据.

你可以具体参考: [Reference Specification](http://git-scm.com/book/zh/ch9-5.html)

`Refspec` 的格式是一个可选的 `+` 号, 接着是 `<src>:<dst>` 的格式, 这里 `<src>` 是远端上的引用格式, `<dst>` 是将要记录在本地的引用格式.

可选的 `+` 号告诉 `Git` 在即使不能"Fast Forward"的情况下, 也去强制更新它.
缺省情况下 `refspec` 会被 `git remote add` 命令所自动生成,
`Git` 会获取远端上 `refs/heads/` 下面的所有引用, 并将它写入到本地的 `refs/remotes/origin/`. 所以, 如果远端上有一个 `master` 分支, 你在本地可以通过下面这种方式来访问它的历史记录:

```bash
$ git log origin/master
$ git log remotes/origin/master
$ git log refs/remotes/origin/master
```

它们全是等价的, 因为 Git 把它们都扩展成 `refs/remotes/origin/master`.
如果你想让 Git 每次只拉取远程的 `master` 分支, 而不是远程的所有分支, 你可以把 `fetch` 这一行修改成这样:
`fetch = +refs/heads/master:refs/remotes/origin/master`

[git中的refspec是什么意思?](http://www.imooc.com/wenda/detail/503063)

## 路径指定 `pathspec`

在`git`命令中用来限制路径的模式

一般就和`linux`中常用的表示方法一样.
可以使用通配符, `*` and `?` 也可以匹配目录分隔符`\`.

比如 `Documentation/*.jpg`, 也会匹配`Documentation/chapter_1/figure_1.jpg`.

单个`:`表示"there is no pathspec". 应该和其他路径指定结合使用.
`:`形式分为short form and long form

以冒号(`:`)开头的 pathspec 具有特殊含义.
在short形式中, 领头的冒号`:`后面是零个或多个 `magic signature` letters
(可选择以另一个冒号`:`结束), 其余部分是要与路径匹配的模式.

`magic signature` 由ASCII 符号组成, 这些符号既不是字母数字, glob, regex 特殊字符, 也不是`冒号`.
如果`模式`的开头字符 不属于 `神奇签名` 符号集, 且不是`冒号`,
则可以省略终止 `神奇签名` 的可选冒号.

在长格式中, 前导冒号`:`后面是一个开放括号`(`,
一个用逗号分隔的零个或多个 `magic words`列表, 和一个封闭括号`)`,
其余部分是要与路径相匹配的模式.

只有冒号的 pathspec 表示 `没有 pathspec`.
这种形式不应与其他 pathspec 结合使用.

+ shortform `:a:`
+ long form `:(a,b,c...)`

可以使用下面这些`magic words`:

+ `top`: magic word `top`(对应的magic signature 是`/`),
表示从工作树的根目录开始匹配, 即使在一个子目录运行命令.

+ `literal`: 通配符如`*` and `?`被当成普通字符.
+ icase: 忽略大小写

### glob

`Git` 将模式视为`shell`模式, 适合由带有 `FNM_PATHNAME` 标志的 `fnmatch(3)` 解析.
模式中的通配符不会匹配路径名中的`/`.
例如, `Documentation/*.html`匹配 `Documentation/git.html`,
但不匹配 `Documentation/ppc/ppc.html` 或 `tools/perf/Documentation/perf.html`.

在与全路径名匹配的模式中, 两个连续的星号`**`可能有特殊含义.

+ `**/`这种形式表示在所有目录中进行匹配.
例如, `**/foo`匹配任何地方的文件或目录`foo`, 与模式`foo`相同.
`**/foo/bar`匹配任何地方的, 直接在`foo`目录下的`bar`, 无论`bar`是文件还是目录.

+ 斜线后面的`**`, 也就是`/**`匹配目录下的所有内容.
例如, `abc/**`匹配`abc`目录下的所有文件, 相对于`.gitignore`文件的位置, 不限制深度.

+ `/**/`这种形式匹配零个或多个目录. 例如, `a/**/b`匹配 `a/b`, `a/x/b`, `a/x/y/b` 等等.

+ 其他连续的星号被认为是无效的.
+ `Glob` magic 和 `literal` magic 不兼容.

### attr

`attr`后面是一个以空格分隔的`属性要求`的列表. 所有这些要求都必须按顺序满足, 才匹配某个路径.
这是在通常的`non-magic`路径模式匹配之外的. 见`gitattributes[5]`.

对路径的每个属性要求是下列形式之一:

+ `ATTR`; 要求属性`ATTR`被设置.
+ `-ATTR`要求属性`ATTR`不被设置.
+ `ATTR=VALUE`要求将属性`ATTR`设置为字符串`VALUE`.
+ `!ATTR`要求属性`ATTR`是未指定的.

请注意, 当与树对象匹配时, 属性仍然是从`working tree`中获得, 而不是从给定的树对象中获得.

+ exclude 排除指定的路径, 也可以使用`!` or `^`

## 包文件

[Git 内部原理 - 包文件](https://git-scm.com/book/en/v2/Git-Internals-Packfiles)

Git 使用`zlib`压缩文件的内容.

Git 最初向磁盘中存储对象时所使用的格式被称为`松散(loose)`对象格式.
但是, Git 会时不时地将多个这些对象打包成一个称为`包文件(packfile)`的二进制文件, 以节省空间和提高效率.
当版本库中有太多的松散对象, 或者你手动执行 `git gc` 命令, 或者你向远程服务器执行推送时, `Git` 都会这样做.
要看到打包过程, 你可以手动执行 `git gc` 命令让 `Git` 对对象进行打包:

```bash
git gc
```

这个时候再查看`objects`目录, 你会发现大部分的对象都不见了, 与此同时出现了一对新文件:

```bash
$ find .git/objects -type f
...
.git/objects/pack/pack-978e03944f5c581011e6998cd0e9e30000905586.idx
.git/objects/pack/pack-978e03944f5c581011e6998cd0e9e30000905586.pack
```

它们分别是包文件和一个stage.
包文件包含了刚才从文件系统中移除的所有对象的内容.
stage文件包含了包文件的偏移信息, 我们通过stage文件就可以快速定位任意一个指定对象.
有意思的是运行`gc`命令前磁盘上的对象大小约为`15K`, 而这个新生成的包文件大小仅有`7K`.
通过打包对象减少了一半的磁盘占用空间.

Git 是如何做到这点的?  Git 打包对象时, 会查找命名及大小相近的文件, 并只保存文件不同版本之间的差异内容.
你可以查看包文件, 观察它是如何节省空间的.
`git verify-pack` 这个底层命令可以让你查看已打包的内容:

```bash
git verify-pack -v .git/objects/pack/pack-978e03944f5c581011e6998cd0e9e30000905586.idx
```

你会看到类似类似下面的格式

```git
033b4... blob   9 20 7262 1 \
  b042a...
b042a... blob   22054 5799 1463
```

`033b4` 这个数据对象引用了数据对象 `b042a`.
命令输出内容的第三列显示的是各个对象在包文件中的大小,
可以看到 `b042a` 占用了 `22K` 空间, 而 `033b4` 仅占用 `9` 字节.
同样有趣的地方在于, 第二个版本完整保存了文件内容, 而原始的版本反而是以差异方式保存的 --
这是因为大部分情况下需要快速访问文件的最新版本.

最妙之处是你可以随时重新打包. `Git` 时常会自动对仓库进行重新打包以节省空间.
当然你也可以随时手动执行 `git gc` 命令来这么做.

## revision 的写法

`revision` 参数 `<rev>`一般是`commit`,
它使用拓展的 `SHA-1` 语法(an extended `SHA-1` syntax)

### sha1

`<sha1>`, 例如 `dae86e1950b1277e545cee180551750029cfe735`, `dae86e`

The full `SHA-1` object name (40-byte hexadecimal string),
or a leading substring that is  within the repository

### `<describeOutput>`, e.g. `v1.7.4.2-679-g3bee7fb`

Output from git describe;
i.e. a closest tag,  optionally followed by a dash and a number of commits,
followed by a dash, a g, and an abbreviated object name.

### refname

`<refname>`, e.g. `master`, `heads/master`, `refs/heads/master`

A symbolic `ref` name. E.g.  `master` typically means the commit object referenced by `refs/heads/master`.
If you happen to have both `heads/master` and `tags/master`,you can explicitly say heads/master to tell Git which one you mean.

### @

#### `@`

只有 `@`, 将被理解成 `HEAD` 的简写.

+ `<refname>@{<date>}`, e.g. `master@{yesterday}`, `HEAD@{5 minutes ago}`

A ref followed by the suffix `@` with a 日期包围在大括号中
(e.g.  `{yesterday}`, `{1 month 2 weeks 3 days 1 hour 1 second ago}`
or `{1979-02-26 18:30:00}`) specifies the value of the ref at a prior point in time.

这个后缀只能用在 `ref name` 后面.它会寻找给定时间内的状态, 比如上星期,
如果你想寻找时间段内的, 用 `--since` and `--until`.

#### `<refname>@{<n>}, e.g. master@{1}`

`ref`后面跟着 `@`, 再加上 大括号和一个序号(例如 `{1}`, `{15}`)
指定 `ref`的 第 `n` 个先前值.

例如 `master@{1}` 是 `master` 最近的值(immediate prior value), 
0-based, 即 master 是 `0`
`git diff ..@{1}` 就是跟上一次的差别.
而 `master@{5}` 是 master 之前的 5th 提交.

这个后缀只能紧跟着 `ref` 名称, 并且 ref 必须存在 log (`$GIT_DIR/logs/<refname>`).

+ `@{<n>}`, e.g. `@{1}`
如果省略前面的`ref`指定的话, 默认指的是 `当前分支`
For example, if you are on branch blabla then @{1} means the same as blabla@{1}.

#### `@{-<n>}, e.g. @{-1}`

The construct `@{-<n>}` means the `<n>th branch/commit` checked out before the current one.

#### `<branchname>@{upstream}`, e.g. `master@{upstream}`, `@{u}`

`<branchname>@{push}`, e.g. `master@{push}`, `@{push}`

上游分支, 推送分支,

Here"s an example to make it more clear:

```git
$ git config push.default current # 配置默认
$ git config remote.pushdefault myfork #默认远程push 分支
$ git checkout -b mybranch origin/master #创建并切换到新分支 mybranch, 上游是`origin/master`

$ git rev-parse --symbolic-full-name @{upstream} # 给出 上游分支
refs/remotes/origin/master

$ git rev-parse --symbolic-full-name @{push} #给出默认push分支
refs/remotes/myfork/mybranch
```

在这个例子中, 我们建立了一个triangular workflow(三角形工作流),
从一个位置pull然后push到另一个位置, 如果是一个普通的工作流,
那么`@{push}` is the same as `@{upstream}`.

后缀`@{push}` or `@{upstream}`大小写不敏感

### `^` caret 和 `~` 波浪号

`<rev>^`, e.g. `HEAD^`, `v1.5.1^0`

`<rev>^`等价于`<rev>^1`, `^<n>`意思是当前 `ref` 的第 `n` 个父节点,
指的是同一个`level`上的, 也就是水平方向的.

As a special rule,` <rev>^0` 指向自身, 可以用`tag`(tag object)指向提交(commit object)

### `<rev>~<n>`, e.g. `master~3`

revision 参数的后缀 `~<n>` 指的是 第`n`个首位父节点,
I.e.  `<rev>~3`等价于`<rev>^^^`, 等价于 `<rev>^1^1^1`.

参见下面的图示

***
`<rev>^{<type>}`, e.g. `v0.99.8^{commit}`

A suffix `^` followed by 大括号中的类型名 means dereference the object at `<rev>`  recursively until an object of type `<type>` is found or the object cannot be dereferenced anymore (in which case, barf).

For example, if `<rev>` is a commit-ish, `<rev>^{commit}` describes the corresponding commit object.
Similarly, if `<rev>` is a tree-ish, `<rev>^{tree}` describes the corresponding tree object.
`<rev>^0` is a short-hand for `<rev>^{commit}`.

`rev^{object}` can be used to make sure `rev` names an object that exists,

without requiring `rev` to be a tag, and without dereferencing rev;  because a tag is already an object, it does not have to be dereferenced even once to get to an object.

`rev^{tag}` can be used to ensure that `rev` identifies an existing tag object.

+ `<rev>^{}`, e.g. `v0.99.8^{}`

`<rev>^{}` 意思是这个object可能是个tag, and  the tag recursively until a non-tag object is found.

+ `<rev>^{/<text>}`, e.g. `HEAD^{/fix nasty bug}`

这个形式等价于下面的`:/fix nasty bug` , 除了它返回  the youngest matching commit which is reachable from the `<rev>` before `^`.

### `:` colon

`:/<text>`, e.g. `:/fix nasty bug`

引用一个commit, 它的提交信息匹配特性的正则表达式. 正则表达式可以匹配commit message的任意部分.

匹配某些字符开头, 用`:/^foo`, 序列`/!`有特殊含义, `:/!-foo` 反相匹配, `:/!!foo`匹配`!foo`本身,

Any other sequence beginning with :`/!`  is reserved for now.

***
`<rev>:<path>`, e.g. `HEAD:README`, `:README`, `master:./README`

给出tree-ish object `<rev>`下的`path`对应的文件( blob or tree),

`:path` (`:`前面没有指定`rev`) 表示的是`index`中的内容,  A path starting with `./` or `../` is relative to the current working directory.

给出的路径会被转换成相对于 working tree的根目录, 对于引用跟当前working tree 有相同目录结构的commit or tree是很有用的.

***
`:<n>:<path>`, e.g. `:0:README`, `:README`

冒号后面的数字可以取`0` to `3`, 引用相应`index`中的`blob` object, 缺省数字的话相当于`0`,
在`merge`的时候, `stage 1`(也就是index)指代common ancestor, stage 2是目标分支(一般是当前分支)
`stage 3`是被合并过来的分支

***
这里有一个图示, by Jon Loeliger.

nodes B and C 是 A 的父节点, 父亲的顺序从左到右.

```graph
G         H   I       J
 \        /       \    /
  D    E          F
   \     |       /     \
    \    |     /        |
     \   |  /           |
        B             C
        \            /
            \      /
                A
```

```git
A =      = A^0
B = A^   = A^1     = A~1
C = A^2  = A^2
D = A^^  = A^1^1   = A~2
E = B^2  = A^^2
F = B^3  = A^^3
G = A^^^ = A^1^1^1 = A~3
H = D^2  = B^^2    = A^^^2  = A~2^2
I = F^   = B^3^    = A^^3^
J = F^2  = B^3^2   = A^^3^2
```
