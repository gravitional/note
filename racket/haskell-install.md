# Haskell

[ghcup](https://www.haskell.org/ghcup/#)
[如何安装 Haskell 工具链?](https://mirror.xyz/0xdB4907968b599f0fb530693eF457BdE801544031/sG9PAIIVhL7urJNSze_xnjg409QYGZFgvmnqxUriuOk)
[Haskell 安装](https://zhuanlan.zhihu.com/p/455688955)

## 安装 GHCup, GHC 和其他工具

GHCup 是一个 Haskell 工具链的版本管理器.
简单来说, 它可用于安装不同版本的 GHC, Cabal, HLS 等工具.

## 准备工作

GHCup 是一个近期出现的工具, 因此国内目前只有中科大和上海交大有镜像.
在安装 GHCup 之前, 我们要做一件额外的工作, 创建 ~/.cabal 目录, 并创建 ~/.cabal/config 文件, 填入如下内容:

```yaml
repository mirrors.ustc.edu.cn
  url: https://mirrors.ustc.edu.cn/hackage/
  secure: True
```

这是因为 GHCup 在安装 Cabal 时会进行初始化(会下载一个 100MB 的文件),
但此时我们还没有替换 Hackage 源!这一步首先替换 Hackage 源. 之后安装过程就会如丝般顺滑.

## 执行安装

在终端中运行如下命令:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://mirrors.ustc.edu.cn/ghcup/sh/bootstrap-haskell | BOOTSTRAP_HASKELL_YAML=https://mirrors.ustc.edu.cn/ghcup/ghcup-metadata/ghcup-0.0.7.yaml sh
```

在我的安装过程中,  ghcup 和 cabal 安装在如下位置, 在安装脚本的输出中显示:

```bash
C:\ghcup
C:\cabal
```

## 配置 GHCup 源

[ustc Hackage 源使用帮助](https://mirrors.ustc.edu.cn/help/hackage.html)

我们上面用了环境变量临时修改了 GHCup 元数据地址, 这里我们把镜像写到配置里让国内源永久生效.
修改 `~/.ghcup/config.yaml`(如不存在就创建), 添加如下内容:

```yaml
url-source:
    OwnSource: https://mirrors.ustc.edu.cn/ghcup/ghcup-metadata/ghcup-0.0.7.yaml
```

## GHCup 的用法

ghcup 有一个很好用的命令叫 tui, 所有 ghcup 的操作均可以从这里完成, 运行 ghcup tui 会显示如下界面.

![img](https://pic4.zhimg.com/80/v2-4517ee6198e026cfb2bc0c4b4ae6bfe7_1440w.webp)

其中打双对勾的是当前选中版本.
比如你安装了 GHC 9.2.1 和 GHC 8.10.7 两个版本, 但是目前选中的是 8.10.7,
那么运行 ghc 命令会调用 8.10.7 版本而非 9.2.1 版本.
你可以安装多个版本, 但一次只能选中一个版本(用 s 键选中别的版本).
好了, 剩下的命令都顾名思义, 自己试试即可.

## VSCode 提示我要下载 HLS

有如下原因:

+ 你没有使用 GHCup 安装 HLS, 重装一下.  如果还不行, 再看下面的.
+ 你没有添加 GHCup 的路径到 PATH 中. 如果始终不行, 参考下面的配置.

在 Haskell 扩展页面, 点击页面上的小齿轮打开扩展配置.

找到 "Haskell: Server Executable Path" 配置项.
输入 `~/.ghcup/bin/haskell-language-server-wrapper`.
重启 VSCode.

这时候重新打开文件应该就可以正确调用到 GHCup 安装的 HLS 上了.

## 编辑器提示我 HLS 反复重启都失败了, 怎么回事?

最可能的原因是你启用了 HLS 目前不支持的新版本 GHC.
如果想使用 HLS, 请务必在 ghcup tui 中选中一个有 `hls-powered` 标记的 GHC 版本(比如 9.0.1, 8.10.7 等).

## GHC 安装失败

Mac 上安装时, 出现以下错误, 是因为没有安装 Xcode Command Line Tools. 安装后即可恢复正常.

## Hackage ustc 源

[Hackage 源使用帮助](https://mirrors.ustc.edu.cn/help/hackage.html)

Cabal 使用说明

+ 执行 `cabal user-config init`
+ 修改 `~/.cabal/config`

Cabal ≥ 1.24 (GHC 8.0)

找到官方仓库:

```yaml
repository hackage.haskell.org
  url: http://hackage.haskell.org/
  -- secure: True
  -- root-keys:
  -- keys-threshold: 3
```

改为科大源:

```yaml
repository mirrors.ustc.edu.cn
  url: https://mirrors.ustc.edu.cn/hackage/
  secure: True
```
