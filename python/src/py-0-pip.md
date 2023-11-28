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

制作 `requirement.txt`, 导出项目中所有依赖包
比如下载 django 1.8.11版本的包, requirements.txt的内容就是:

```conf
django==1.8.11
```

使用 `pip freeze` 会输出所有在本地已安装的包(但不包括 pip, wheel, setuptools 等自带包),
若需要输出内容与 pip list 一致, 需使用 `pip freeze --all`.

使用方法:

```bash
pip freeze > requirements.txt
```

适用场合:
由于 `pip freeze` 与 `pip list` 内容区别不大,
所以若想要用其作为工程依赖包列表, 需要配合 Python 虚拟环境 `virtualenv` 使用.

+ 将所有包下载到目标目录中

例如: 想将包放在 `pypkg` 目录下

```bash
pip download -d pypkg -r requirements.txt
```

+ 将文件打包后放到离线服务器上, 并进行解压缩, 然后安装

```bash
pip install --no-index --find-links=pypkg -r requirements.txt
```

## pip wheel 包

[python wheel 安装包的制作与安装](https://zhuanlan.zhihu.com/p/354110980)
[python库打包分发setup.py编写指南](http://www.coolpython.net/python_senior/project/op_py_setup_install.html)
