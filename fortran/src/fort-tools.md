# fortran 工具

## gfortran 下载安装

[安装 GFortran](https://fortran-lang.org/zh_CN/learn/os_setup/install_gfortran/)
[Equation Solution](http://www.equation.com/servlet/equation.cmd?fa=fortran)

## vscode fortran, fortls

`fortls` 是 Fortran 语言的 Language server,

```bash
pip install fortls
```

在 vscode 的 settings.json 中可以配置, 搜索 `fortls path` 可以找到配置入口,
其中可以使用 `~` 符号表示家目录.

```conf
Path to the Fortran language server (fortls).
~/bin/fortls.exe
```

## findent 格式化工具

[findent 4.2.6](https://pypi.org/project/findent/)

Normal installation using pip

```bash
pip install findent
```

### 使用

将文件 `in.f90` 格式化为 `out.f90`

```bash
findent < in.f90 > out.f90
```

使用 4-空格 缩进格式, 并将 固定格式 `in.f` 转换为 自由格式 `out.f90`

```bash
findent -i4 -ofree < in.f > out.f90
```

格式化和重构当前目录下所有扩展名为 `.f` 的文件

```bash
wfindent -i4 -Rr *.f
```
