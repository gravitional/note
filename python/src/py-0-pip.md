# python pip 包管理

## pip 编程化使用

[How can I Install a Python module within code?](https://stackoverflow.com/questions/12332975/how-can-i-install-a-python-module-within-code)
[pip User Guide](https://pip.pypa.io/en/latest/user_guide/#using-pip-from-your-program)
[using pip and virtual environments](https://packaging.python.org/en/latest/guides/installing-using-pip-and-virtual-environments/)

官方推荐的, 从脚本安装软件包的方法是通过 subprocess 调用 `pip` 的命令行界面.
这里介绍的大多数其他答案都不受 pip 支持.
此外, 自 pip v10 起, 所有代码都被移至 `pip._internal`,
以便向用户明确说明不允许以编程方式使用 `pip`.

使用 `sys.executable` 可确保调用与当前 runtime 相关联的 `pip`.

```python
import subprocess
import sys

def install(package):
    if sys.executable:
        subprocess.run([sys.executable, "-m", "pip", "install", package], check=True)
    else:
        raise RuntimeError('Can not find python interpreter executable')
```

[sys.executable](https://docs.python.org/3/library/sys.html#sys.executable)
一个字符串, 给出 `Python解释器` 可执行二进制文件的绝对路径.
如果 Python 无法获取其可执行文件的真实路径, `sys.executable` 将是 `空字符串` 或 `None`.

## pip freeze 批量离线安装包

[离线批量安装python包](https://zhuanlan.zhihu.com/p/528753336)

制作 `req.txt`, 导出项目中所有依赖包
比如下载 django 1.8.11版本的包, `req.txt` 的内容就是:

```conf
django==1.8.11
```

使用 `pip freeze` 会输出所有在本地已安装的包(但不包括 pip, wheel, setuptools 等自带包),
若需要输出内容与 `pip list` 一致, 需使用 `pip freeze --all`.

bash 使用方法:

```bash
pip freeze --all > req.txt
```

nushell 使用方法:

```nu
pip freeze --all | save -f req.txt 
```

适用场合:
由于 `pip freeze` 与 `pip list` 内容区别不大,
所以若想要用其作为工程依赖包列表, 需要配合 Python 虚拟环境 `virtualenv` 使用.

### 将所有包下载到目标目录中

例如: 想将包放在 `pypkg` 目录下

```bash
pip download -d pypkg -r req.txt
```

### 将文件打包后放到离线服务器上, 并进行解压缩, 然后安装

```bash
pip install --no-index --find-links=. -r req.txt
```

如果前面使用了 `pip freeze --all` 选项输出 package 列表,
则使用下列方式安装, 可以同时更新 `pip`

```bash
python -m pip install --no-index --find-links=. -r req.txt
```

## pip wheel 包

[python wheel 安装包的制作与安装](https://zhuanlan.zhihu.com/p/354110980)
[python库打包分发setup.py编写指南](http://www.coolpython.net/python_senior/project/op_py_setup_install.html)

## pip 配置文件目录

[pip 在Windows 10下的配置文件在哪里](https://zhuanlan.zhihu.com/p/113905629)

简单命令就可以, 注意命令行里面的 `-v`

```bash
pip config list -v
For variant 'global', will try loading 'C:\ProgramData\pip\pip.ini'
For variant 'user', will try loading 'C:\Users\xxxusername\pip\pip.ini'
For variant 'user', will try loading 'C:\Users\xxxusername\AppData\Roaming\pip\pip.ini'
For variant 'site', will try loading 'd:\xxxusername\scoop\apps\python\current\pip.ini'
global.index-url='http://mirrors.aliyun.com/pypi/simple/'
install.trusted-host='mirrors.aliyun.com'
```

看下上面的路径哦, 按图索骥即可哦

## pip 常用环境

```bash
python -V # 打印 python 版本
python -m pip install numpy scipy matplotlib jupyterlab colorama pandas
pip install numpy scipy matplotlib jupyterlab colorama pandas
```

`pywinpty` 依赖 rust, 如果安装失败, 可以下载预编译 wheel, 手动安装
[pywinpty](https://pypi.org/project/pywinpty/#files)
[Error installing Jupyter & pywinpty (Python)](https://stackoverflow.com/questions/51260909/error-installing-jupyter-pywinpty-python)

```bash
pip install c:/xxx/pywinpty-xxx-win_amd64.whl
```
