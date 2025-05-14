# python 安装目录

安装在 `C:\MyProgram\python3`, 刚安装完，目录结构如下:

```bash
DLLs/
Doc/
include/
    cpython/
    internal/
Lib/
    site-packages/
        pip/
        pip-25.0.1.dist-info/
        README.txt

libs/
    python313.lib
Scripts/
    pip.exe
    pip3.13.exe
    pip3.exe
tcl/
python.exe
python313.dll
...
```

运行`python -m pip install numpy scipy colorama`, 只安装3个包之后

```bash
Lib/
    site-packages/
        pip/
        pip-25.0.1.dist-info/
        README.txt

        colorama/
        colorama-0.4.6.dist-info/

        numpy/
        numpy.libs/
        numpy-2.2.5.dist-info/
        numpy-2.2.5-cp313-cp313-win_amd64.whl

        scipy/
        scipy.libs/
        scipy-1.15.3.dist-info/
        scipy-1.15.3-cp313-cp313-win_amd64.whl

```