# git-3

## rebase 变基

在`Git`中整合来自不同分支的修改主要有两种方法: `merge`以及`rebase`

reference: [scm tutorial](https://git-scm.com/book/zh/v2/Git-%E5%88%86%E6%94%AF-%E5%8F%98%E5%9F%BA#rrbdiag_g)

### 变基的风险

呃, 奇妙的变基也并非完美无缺, 要用它得遵守一条准则: 不要对在你的仓库外有副本的分支执行变基.

如果你遵循这条金科玉律, 就不会出差错. 否则, 人民群众会仇恨你, 你的朋友和家人也会嘲笑你, 唾弃你.

变基操作的实质是丢弃一些现有的提交, 然后相应地新建一些内容一样但实际上不同的提交. 如果你已经将提交推送至某个仓库, 而其他人也已经从该仓库拉取提交并进行了后续工作, 此时, 如果你用 `git rebase`命令重新整理了提交并再次推送, 你的同伴因此将不得不再次将他们手头的工作与你的提交进行整合, 如果接下来你还要拉取并整合他们修改过的提交, 事情就会变得一团糟.

我自己理解的变基就是:

1. 先寻找一个命令参数中提到的 **提交** 使用的共同 **父节点**,
2. 计算所有在 **父节点** 之后各次 **提交** 进行的所有更改, 然后按照 `git rebase` 命令的安排,
    把它们依次播放/运用/apply,
3. 然后重新生成各次提交, 新生成的提交就按照我们希望的那样排列了

### scm-book 例子 1

在上面这个例子中, 运行:

```bash
$ git checkout experiment
"no output"
$ git rebase master
First, rewinding head to replay your work on top of it...
Applying: added staged command
```

它的原理是首先找到这两个分支(即当前分支 `experiment`, 变基操作的目标基底分支 `master`)的最近共同祖先 `C2`, 然后对比当前分支相对于该祖先的历次提交, 提取相应的修改并存为临时文件, 然后将**当前分支**指向目标基底 `C3`, 最后以此将之前另存为临时文件的修改依序应用

### scm-book 例子 2

假设你希望将 `client` 中的修改合并到主分支并发布, 但暂时并不想合并 `server` 中的修改, 因为它们还需要经过更全面的测试. 这时, 你就可以使用 `git rebase` 命令的 `--onto` 选项, 选中在 `client` 分支里但不在 `server` 分支里的修改(即 `C8` 和 `C9`), 将它们在 `master` 分支上重放:

```bash
git rebase --onto master server client
```

以上命令的意思是:
"取出 `client` 分支,
找出处于 `client` 分支和 `server`  分支的共同祖先之后的修改,
然后把它们在 `master` 分支上重放一遍".
这理解起来有一点复杂, 不过效果非常酷.

即

```bash
git rebase --onto master[被施加重放的分支] server[父节点/修改起始点 参考分支] client[提取重放内容的分支]
```

## 自定义 git

### gitignore

忽略某些文件时, 需要编写`.gitignore`;
`.gitignore`文件本身要放到版本库里, 并且可以对`.gitignore`做版本管理!

[git设置忽略文件和目录](https://www.cnblogs.com/wtil/p/11676092.html)

每个目录都可以有自己的 `.gitignore` 文件, 并覆盖上一层的设定. 例如:

```conf
.idea   //忽略 `.idea`文件/文件夹, 以及文件夹下的文件
*.iml   //忽略以 `.iml`结尾的文件
```

#### 例子

```git
# 忽略*.o和*.a文件
*.[oa]

# 忽略*.b和*.B文件, my.b除外
*.[bB]
!my.b

# 忽略dbg文件和dbg目录
dbg

# 只忽略dbg目录, 不忽略dbg文件
dbg/

# 只忽略dbg文件, 不忽略dbg目录
dbg
!dbg/

# 只忽略当前目录下的dbg文件和目录, 子目录的dbg不在忽略范围内
/dbg
```

以`#`开始的行, 被视为注释.

+ `?`: 代表任意的一个字符
+ `*`: 代表任意数目的字符
+ `{!ab}`: 必须不是此类型
+ `{ab,bb,cx}`: 代表`ab`,`bb`,`cx`中任一类型即可
+ `[abc]`: 代表`a`,`b`,`c`中任一字符即可
+ `[ ^abc]`: 代表必须不是`a`,`b`,`c`中任一字符

***
添加忽略之后, 已经提交到版本库中的文件是无法忽略的.
只能clone到本地, 删除后, 再进行忽略.

`.gitignore`只能忽略那些原来没有被track的文件,
如果某些文件已经被纳入了版本管理中, 则修改`.gitignore`是无效的.

正确的做法是在每个clone下来的仓库中手动设置不要检查特定文件的更改情况.
`git update-index --assume-unchanged PATH` 在PATH处输入要忽略的文件.
另外 git 还提供了另一种 `exclude` 的方式来做同样的事情,
不同的是 `.gitignore` 这个文件本身会提交到版本库中去, 用来保存的是公共的需要排除的文件.
而 `.git/info/exclude` 这里设置的则是你自己本地需要排除的文件, 它不会影响到其他人, 也不会提交到版本库中去

Python我一般添加这个三个

```git
.idea
*.iml
__pycache__
```

## git 术语

### index

[whats-the-deal-with-the-git-index](https://gitguys.com/topics/whats-the-deal-with-the-git-index/)

The git "index" is where you place files you want committed to the git repository.

Before you "commit" (checkin) files to the git repository, you need to first place the files in the git "index".

The git index goes by many names. But they all refer to the same thing. Some of the names you may have heard:

+ Index
+ Cache
+ Directory cache
+ Current directory cache
+ Staging area
+ Staged files

The Index Isn"t The Working Directory.

## 比较二进制文件

你可以使用 `Git 属性`来有效地比较两个二进制文件.
秘诀在于, 告诉 Git 怎么把你的二进制文件转化为文本格式, 从而能够使用普通的 diff 方式进行对比.

比如: 对 Microsoft Word 文档进行版本控制. 大家都知道, Microsoft Word 几乎是世上最难缠的编辑器, 尽管如此, 大家还是在用它. 如果想对 Word 文档进行版本控制, 你可以把文件加入到 Git 库中, 每次修改后提交即可.
 把下面这行文本加到你的 `.gitattributes` 文件中:

```bash
*.docx diff=word
```

这告诉 Git 当你尝试查看包含变更的比较结果时, 所有匹配 `.docx` 模式的文件都应该使用`word`过滤器.
`word`过滤器是什么?  我们现在就来设置它.
我们会对 Git 进行配置, 令其能够借助 `docx2txt` 程序将 Word 文档转为可读文本文件, 这样不同的文件间就能够正确比较了.

首先, 你需要安装 `docx2txt`;
它可以从 [https://sourceforge.net/projects/docx2txt](https://sourceforge.net/projects/docx2txt) 下载. 按照 `INSTALL` 文件的说明, 把它放到你的可执行路径下.
接下来, 你还需要写一个脚本把输出结果包装成 `Git` 支持的格式. 在你的可执行路径下创建一个叫 `docx2txt` 文件, 添加这些内容:

```bash
#!/bin/bash
docx2txt.pl "$1" -
```

别忘了用 `chmod a+x` 给这个文件加上可执行权限. 最后, 你需要配置 `Git` 来使用这个脚本:

`$ git config diff.word.textconv docx2txt`

现在如果在两个快照之间进行比较, Git 就会对那些以 .docx 结尾的文件应用`word`过滤器, 即 `docx2txt`.
这样你的 Word 文件就能被高效地转换成文本文件并进行比较了.

你还能用这个方法比较图像文件.
其中一个办法是, 在比较时对图像文件运用一个过滤器, 提炼出 `EXIF` 信息 -- 这是在大部分图像格式中都有记录的一种元数据.
如果你下载并安装了 `exiftool` 程序, 可以利用它将图像转换为关于元数据的文本信息, 这样比较时至少能以文本的形式显示发生过的变动:  将以下内容放到你的 `.gitattributes` 文件中:

`*.png diff=exif`

配置 Git 以使用此工具:

`$ git config diff.exif.textconv exiftool`
