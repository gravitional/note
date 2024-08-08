# haskell cabal 使用

## cabal install --lib; 在 GHC 软件包环境中添加库

[Adding libraries to GHC package environments](https://cabal.readthedocs.io/en/stable/cabal-commands.html#adding-libraries-to-ghc-package-environments)

也可以使用 `--lib` flag来 `安装` 库. 例如, 这条命令将 构建 并 安装 最新的 `Cabal` 库:

```bash
Cabal install --lib Cabal
```

这是通过管理 `GHC软件包环境文件` 来实现的.
默认情况下, 它会写入 `~/.ghc/$ARCH-$OS-$GHCVER/environments/default` 中的全局环境.
`install` 提供了 `--package-env` flag 来控制修改其中的哪个环境.

此命令将修改当前目录下的 `环境文件`:

```bash
cabal install --lib Cabal --package-env .
```

这条命令将修改 `~/foo` 目录下的环境文件:

```bash
cabal install --lib Cabal --package-env foo/
```

请注意, 这两条命令的结果会被其他样式的命令覆盖, 因此不建议在 `项目目录` 下使用.

该命令将修改当前目录下 `local.env` 文件中的环境:

```bash
cabal install --lib Cabal --package-env local.env
```

这条命令将修改名为 `myenv` 的全局环境:

```bash
cabal install --lib Cabal --package-env myenv
```

如果要在当前目录下, 创建一个名称不包含`扩展名`的 命名环境文件, 则必须将其引用为 `./myenv`.

有关如何使用这些环境的更多信息, 请参阅 GHC 手册的这一部分.

## 查看已经安装的软件的 位置等信息

例如 [random 包](https://downloads.haskell.org/~ghc/6.0/docs/html/base/System.Random.html)是 `System.Random` 模块
安装时的名称就叫 [random](https://hackage.haskell.org/package/random), 与 hackage 链接末端名称相同.

```bash
https://hackage.haskell.org/package/random
```

安装前后都可以使用 下面的命令查看具体信息

```bash
cabal install --lib -v random
```

输出结果是

```bash
Error: cabal-3.10.3.0.exe: Packages requested to install already exist in environment file at
C:\Users\qingz\AppData\Roaming\ghc\x86_64-mingw32-9.8.2\environments\default.
```

输出中提到 GHC package 的默认环境文件 `xxxx/environments/default`.
打开此文件会看到 `已安装的包信息` 和 `database 文件位置`

```bash
clear-package-db
global-package-db
package-db C:\cabal\store\ghc-9.8.2\package.db
package-id base-4.19.1.0-6554
package-id random-1.2.1.2-524f59b209070cee2b0871125a908a77baf3dafa
```

打开 `C:\cabal\store\ghc-9.8.2\package.db`, 里面有包的具体目录信息
`C:\cabal\store\ghc-9.8.2\random-xxx\lib` 里面是 包的链接库, 在 windows上是 `xx.a`
`C:\ghcup\msys64\mingw64\lib` 里面是 `pkgconfig` 文件.

```bash
import-dirs:
    C:\cabal\store\ghc-9.8.2\random-1.2.1.2-524f59b209070cee2b0871125a908a77baf3dafa\lib

library-dirs:
    C:\cabal\store\ghc-9.8.2\random-1.2.1.2-524f59b209070cee2b0871125a908a77baf3dafa\lib
    C:\ghcup\msys64\mingw64\lib

library-dirs-static:
    C:\cabal\store\ghc-9.8.2\random-1.2.1.2-524f59b209070cee2b0871125a908a77baf3dafa\lib
    C:\ghcup\msys64\mingw64\lib

dynamic-library-dirs:
    C:\cabal\store\ghc-9.8.2\random-1.2.1.2-524f59b209070cee2b0871125a908a77baf3dafa\lib
    C:\ghcup\msys64\mingw64\lib

data-dir:
    C:\cabal\store\ghc-9.8.2\random-1.2.1.2-524f59b209070cee2b0871125a908a77baf3dafa\share
```

## ghc-pkg, ghc包管理器

[5.Using GHC/5.9.Packages](https://downloads.haskell.org/ghc/latest/docs/users_guide/packages.html)
