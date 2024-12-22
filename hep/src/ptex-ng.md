# ptex-ng

## 编译安装

[clerkma/ptex-ng](https://github.com/clerkma/ptex-ng), 下载 src.zip

安装 ruby 和 [bison](https://sourceforge.net/projects/winflexbison/),
修改 bison 可执行文件名称为 `bison.exe`.

打开 `Developer PowerShell for Visual Studio 2022`,
首先修改环境变量,

```pwsh
if ($true) {
    # `'',` 强制 -join 添加首部的 `;`
    $paths = '', 'C:/Users/qingz/bin/bison'
    $split = ';'
    # 检查重复
    # $oldPath = $Env:PATH -split ';'
    # foreach ( $p in $paths) {
    #     if ($p -notin $oldPath) {
    #         $Env:PATH += $split + $p
    #     }
    # }
    # 不检查重复
    $Env:PATH += ($paths -join $split)
    $Env:PATH
}
```

```pwsh
.\build-mruby.bat
```
