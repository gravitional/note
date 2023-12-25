# Haskell

安装之前首先切换 Cabal USTC Mirror

## Hackage USTC 源

[Hackage 源使用帮助](https://mirrors.ustc.edu.cn/help/hackage.html)

GHCup 类似 Rustup, 可以用于安装 Haskell 工具链.
建议搭配 Hackage 和 Stackage 源使用.

### 使用方法

参考如下步骤可安装完整的 Haskell 工具链.

>备注
>以下命令会安装并配置 GHCup 0.0.7 版本的元数据.
>可查看 https://mirrors.ustc.edu.cn/ghcup/ghcup-metadata/ 目录的内容, 并选择需要安装的 GHCup 版本的 yaml 文件替换以下命令中的 URL.

### 第一步(可选):使用科大源安装 GHCup 本体

如已经安装 GHCup, 可跳到下一步.
Linux, FreeBSD, macOS 用户: 在终端中运行如下命令

```bash
curl --proto '=https' --tlsv1.2 -sSf https://mirrors.ustc.edu.cn/ghcup/sh/bootstrap-haskell | BOOTSTRAP_HASKELL_YAML=https://mirrors.ustc.edu.cn/ghcup/ghcup-metadata/ghcup-0.0.7.yaml sh
```

Windows 用户: 以非管理员身份在 PowerShell 中运行如下命令

```PowerShell
$env:BOOTSTRAP_HASKELL_YAML = 'https://mirrors.ustc.edu.cn/ghcup/ghcup-metadata/ghcup-0.0.7.yaml'
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://mirrors.ustc.edu.cn/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true
```

### 第二步 : 配置 GHCup 使用科大源

编辑 `~/.ghcup/config.yaml` 增加如下配置:

```yaml
url-source:
    OwnSource: https://mirrors.ustc.edu.cn/ghcup/ghcup-metadata/ghcup-0.0.7.yaml
```

第三步(可选) : 配置 Cabal 和 Stack 使用科大源,
请参考文档 Hackage 源使用帮助 和 Stackage 源使用帮助 .

>警告
>科大 GHCup 源仅支持较新的 GHCup 版本(元数据格式版本仅支持 0.0.6 及以上).
>如果你使用的 GHCup 版本比较旧, 请参考上述步骤安装新版本 GHCup.

### 预发布版本

使用预发布频道可以安装尚未正式发布的测试版本.
要启用预发布源, 将 `~/.ghcup/config.yaml` 文件中
 `url-source` 一节修改如下:

```yaml
url-source:
    OwnSource:
    - https://mirrors.ustc.edu.cn/ghcup/ghcup-metadata/ghcup-0.0.7.yaml
    - https://mirrors.ustc.edu.cn/ghcup/ghcup-metadata/ghcup-prereleases-0.0.7.yaml
```

## Hackage 源使用帮助

[Hackage 源使用帮助](https://mirrors.ustc.edu.cn/help/hackage.html)
[地址](https://mirrors.ustc.edu.cn/hackage/)

说明; Hackage 镜像

### Stack 使用说明

编辑 `~/.stack/config.yaml`, 增加下列参数

>= v2.9.3:

```yaml
package-index:
  download-prefix: https://mirrors.ustc.edu.cn/hackage/
  hackage-security:
    keyids:
    - 0a5c7ea47cd1b15f01f5f51a33adda7e655bc0f0b0615baa8e271f4c3351e21d
    - 1ea9ba32c526d1cc91ab5e5bd364ec5e9e8cb67179a471872f6e26f0ae773d42
    - 280b10153a522681163658cb49f632cde3f38d768b736ddbc901d99a1a772833
    - 2a96b1889dc221c17296fcc2bb34b908ca9734376f0f361660200935916ef201
    - 2c6c3627bd6c982990239487f1abd02e08a02e6cf16edb105a8012d444d870c3
    - 51f0161b906011b52c6613376b1ae937670da69322113a246a09f807c62f6921
    - 772e9f4c7db33d251d5c6e357199c819e569d130857dc225549b40845ff0890d
    - aa315286e6ad281ad61182235533c41e806e5a787e0b6d1e7eef3f09d137d2e9
    - fe331502606802feac15e514d9b9ea83fee8b6ffef71335479a2e68d84adc6b0
    key-threshold: 3 # number of keys required

    # ignore expiration date, see https://github.com/commercialhaskell/stack/pull/4614
    ignore-expiry: true
```

## Cabal USTC源 使用说明

执行 `cabal user-config init`
修改 `~/.cabal/config`

Cabal ≥ 1.24 (GHC 8.0)
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

>备注
>首次 cabal update 时会提示
>Warning: No mirrors found for http://mirrors.ustc.edu.cn/hackage/,  该警告可忽略.

>警告
>为了保证与老版本 cabal 的兼容性,  secure 值设置为 False 可能导致 cabal 无法获取到最新的包信息.

## Linux 安装 GHCup, GHC 和其他工具

[ghcup](https://www.haskell.org/ghcup/#)
[如何安装 Haskell 工具链?](https://mirror.xyz/0xdB4907968b599f0fb530693eF457BdE801544031/sG9PAIIVhL7urJNSze_xnjg409QYGZFgvmnqxUriuOk)
[Haskell 安装](https://zhuanlan.zhihu.com/p/455688955)

GHCup 是一个 Haskell 工具链的版本管理器.
简单来说, 它可用于安装不同版本的 GHC, Cabal, HLS 等工具.

### 准备工作

GHCup 是一个近期出现的工具, 因此国内目前只有中科大和上海交大有镜像.
在安装 GHCup 之前, 我们要做一件额外的工作, 创建 `~/.cabal` 目录, 并创建 `~/.cabal/config` 文件, 填入如下内容:

```yaml
repository mirrors.ustc.edu.cn
  url: https://mirrors.ustc.edu.cn/hackage/
  secure: True
```

这是因为 GHCup 在安装 Cabal 时会进行初始化(会下载一个 100MB 的文件),
但此时我们还没有替换 Hackage 源!这一步首先替换 Hackage 源.
之后安装过程就会如丝般顺滑.

### 执行安装

在终端中运行如下命令:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://mirrors.ustc.edu.cn/ghcup/sh/bootstrap-haskell | BOOTSTRAP_HASKELL_YAML=https://mirrors.ustc.edu.cn/ghcup/ghcup-metadata/ghcup-0.0.7.yaml sh
```

在我的安装过程中, ghcup 和 cabal 安装在如下位置, 在安装脚本的输出中显示:

```bash
C:\ghcup
C:\cabal
```

### 配置 GHCup 源

[ustc Hackage 源使用帮助](https://mirrors.ustc.edu.cn/help/hackage.html)

我们上面用了环境变量临时修改了 GHCup 元数据地址, 这里我们把镜像写到配置里让国内源永久生效.
修改 `~/.ghcup/config.yaml`(如不存在就创建), 添加如下内容:

```yaml
url-source:
    OwnSource: https://mirrors.ustc.edu.cn/ghcup/ghcup-metadata/ghcup-0.0.7.yaml
```

### GHCup tui

ghcup 有一个很好用的命令叫 tui, 运行 `ghcup tui` 会显示如下界面.

![img](https://pic4.zhimg.com/80/v2-4517ee6198e026cfb2bc0c4b4ae6bfe7_1440w.webp)

其中打`双对勾`的是当前选中版本.
比如你安装了 GHC 9.2.1 和 GHC 8.10.7 两个版本, 但是目前选中的是 8.10.7,
那么运行 ghc 命令会调用 8.10.7 版本而非 9.2.1 版本.
你可以安装多个版本, 但一次只能选中一个版本(用 `s` 键选中别的版本).
好了, 剩下的命令都顾名思义, 自己试试即可.

如果想安装 HLS 的话, 随时可以从这里安装.

### VSCode 提示我要下载 HLS

有如下原因:

+ 你没有使用 GHCup 安装 HLS, 重装一下.  如果还不行, 再看下面的.
+ 你没有添加 GHCup 的路径到 PATH 中. 如果始终不行, 参考下面的配置.

在 Haskell 扩展页面, 点击页面上的小齿轮打开扩展配置.

找到 "Haskell: Server Executable Path" 配置项.
输入 `~/.ghcup/bin/haskell-language-server-wrapper`.
重启 VSCode.

这时候重新打开文件应该就可以正确调用到 GHCup 安装的 HLS 上了.

### 编辑器提示我 HLS 反复重启都失败了, 怎么回事?

最可能的原因是你启用了 HLS 目前不支持的新版本 GHC.
如果想使用 HLS, 请务必在 ghcup tui 中选中一个有 `hls-powered` 标记的 GHC 版本(比如 9.0.1, 8.10.7 等).

### GHC 安装失败

Mac 上安装时, 出现以下错误, 是因为没有安装 Xcode Command Line Tools. 安装后即可恢复正常.

## 手动安装

[Manual installation](https://www.haskell.org/ghcup/install/#manual-installation)

### 安装 ghcup 二进制文件

下载 [ghc 二进制文件](https://downloads.haskell.org/~ghcup/x86_64-mingw64-ghcup.exe)
放入例如 `C:\ghcup\bin`

### 安装 MSYS2,

[下载 MSYS2](https://repo.msys2.org/distrib/msys2-x86_64-latest.exe),
默认安装位置是 `C:\msys64`

### 添加环境变量, 更新 PATH

`Path` 变量加入 `C:\ghcup\bin`
新建 `GHCUP_MSYS2`, 值为 `C:\msys64`
新建 `GHCUP_INSTALL_BASE_PREFIX`, 输入device directory, 默认为 `C:\`
新建 `CABAL_DIR`, 值为 device directory + cabal subdir, 默认为 `C:\cabal`

### Install tools

在powershell中运行

```powershell
ghcup install ghc --set recommended
ghcup install cabal latest
ghcup install stack latest
ghcup install hls latest
cabal update
```

### Update msys2

在powershell中运行

```powershell
ghcup run -m -- pacman --noconfirm -Syuu
ghcup run -m -- pacman --noconfirm -Syuu
ghcup run -m -- pacman --noconfirm -S --needed curl autoconf mingw-w64-x86_64-pkgconf
ghcup run -m -- pacman --noconfirm -S ca-certificates
```

### 更新 cabal 配置

+ 转到例如 `C:\cabal`(基于您在1中选择的device)
+ 打开文件 `config`
+ 取消注释 `extra-include-dirs` (the --)
添加值(取决于你在 2. 中选择的安装目录), 例如 `C:\msys64\mingw64\include`...
所以最后一行应该是 `extra-include-dirs: C:\msys64\mingw64\include`...

+ uncomment `extra-lib-dirs`并做同样的操作, 添加`C:\msys64\mingw64\lib`

+ uncomment `extra-prog-path`并将其设置为 `C:\ghcup\bin, C:\cabal\bin, C:\msys64\mingw64\bin, C:\msys64\usr\bin`,
这取决于你在1.和2.中的安装目的地.

### 设置 msys2 shell

运行

```bash
ghcup run -m -- sed -i -e 's/db_home:.*$/db_home: windows/' /etc/nsswitch.conf
```

使 msys2 shell 中的 "HOME "与 windows 中的 "HOME "一致

+ 从 `C:\msys64\msys2_shell.cmd`制作一个桌面快捷方式,
这将允许你启动一个正确的 msys2 shell

+ 运行

```bash
ghcup run -m -- sed -i -e 's/#MSYS2_PATH_TYPE=.*/MSYS2_PATH_TYPE=inherit/' /c/msys64/msys2.ini
```

+ 运行

```bash
ghcup run -m -- sed -i -e 's/rem set MSYS2_PATH_TYPE=inherit/set MSYS2_PATH_TYPE=inherit/' /c/msys64/msys2_shell.cmd
```

一切就绪.
现在可以在空目录下运行 cabal init
目录下运行 cabal init 来启动项目.
