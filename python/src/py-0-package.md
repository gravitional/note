# python 包相关

## utf-8 声明

[python文件头部声明# coding=utf-8](https://blog.csdn.net/daningliu/article/details/121617391)
[Python中的encoding=utf-8是什么意思-Sevk](https://www.zhihu.com/question/340887244/answer/2189150616)
[Python 中文编码](https://www.runoob.com/python/python-chinese-encoding.html)

为了代码能够在linux和windows环境下都可以运行, 中文编码不报错, 建议在编写python程序时, 在文件开头加上这两句.
注意: `# coding=utf-8` 的 `=` 号两边不要空格

```python
#!/usr/bin/env python
# coding=utf-8
# 或者
# -*- coding: UTF-8 -*-
```

`-*-` 是源自于 emacs 的语法

[50.2.4.1 Specifying File Variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Specifying-File-Variables.html)

指定 `文件局部变量值` 有两种方法: 在 `第一行` 中指定, 或使用 `局部变量列表` 指定.
下面是在第一行指定变量值的方法:

```vim
-*- mode: modename; var: value; ... -*-
```

你可以用这种方法指定任意数量的变量/值对, 每对都用 `冒号` 和 `分号` 表示.
如果存在特殊的变量/值对 `mode: modename;`, 则指定一种主要模式
(不含后缀 `-mode`).
`values`按字面意思使用, 不进行计算.

可以使用 `M-x add-file-local-variable-prop-line` 代替手工添加条目.
该命令会提示输入 变量 和 值, 并以适当方式将它们添加到第一行.
`M-x delete-file-local-variable-prop-line` 提示输入变量, 并将其从行中删除.
`M-x copy-dir-locals-to-file-locals-prop-line`
命令将当前目录下的局部变量复制到第一行(请参阅每目录局部变量).

下面是一个示例, 第一行指定了 `Lisp` 模式, 并设置了两个数值变量:

```vim
;; -*- mode:  Lisp; fill-column:  75; comment-column:  50; -*-
```

## 安装到自定义文件夹

[pip安装python包到指定文件夹](https://blog.csdn.net/Diobld/article/details/126899370)

指定安装路径为 `D:\Python36\site-packages\torch`

```bash
pip3 install torch --target=D:/Python36/site-packages/torch
#  或者使用 简短形式
pip install -t D:/Python36/site-packages/torch torch
```

## 导入失败

[AttributeError: module has no attribute" or an ImportError or NameError](https://stackoverflow.com/questions/36250353/importing-a-library-from-or-near-a-script-with-the-same-name-raises-attribute)

出现这种情况是因为名为 `requests.py` 的本地模块, 与你试图使用的已安装请求模块有 重叠(冲突).
当前目录已被预输入 `sys.path`, 因此 `本地名称` 优先于 `已安装的名称`.

出现这种情况时, 还有一个额外的调试技巧,
那就是仔细查看跟踪回溯(`Traceback`), 发现 `相关脚本` 的名称与 `试图导入` 的模块 重名:
将模块重命名为其他名称, 以避免名称冲突.

Python 可能会在 `requests.py` 文件旁边生成 `requests.pyc` 文件
(在 Python 3 中位于 `__pycache__` 目录中).
重命名后, 也请删除该文件, 因为解释器仍会引用该文件, 从而重新产生错误.
但是, 如果删除了 `py` 文件, `__pycache__` 中的 `pyc` 文件应该不会影响您的代码.

## 查询模块的安装位置

[python如何查询模块的文件位置](https://blog.csdn.net/yijinaqingan/article/details/89892010)

### 使用内置变量 __file__

```python
import requests
print(requests.__file__)
C:\Python\Python36\lib\site-packages\requests\__init__.py
```

同理可以用于 [获取脚本所在路径: 执行脚本时的执行时的目录和脚本文件所在目录](https://blog.csdn.net/nixiang_888/article/details/109174340)

两个目录的区分

1. 执行脚本时, 命令行所处的目录: 即工作目录, working directory
2. 脚本文件所在目录: 获得脚本文件 `xxx.py` 在文件系统中的位置

例子: 目录结构

```bash
|-C:/test
|   |-rootpath
|   |   |-path.py
|   |   |-sub_path
|   |   |   |-sub_path.py
# 其中, path.py中调用了sub_path.py
```

```bash
# 在c:/test目录下, 执行
python c:/test/rootpath/path.py
```

`os.getcwd()`: 执行脚本时所在目录
在 `path.py` 和 `sub_path.py` 中的 `os.getcwd()` 的命令均获得 `c:/test`

`sys.path[0]` 和 `sys.argv[0]`: 被初始执行的脚本文件所在目录
在 `path.py` 中的 `sys.path[0]` 和 `sys.argv[0]` 的命令均获得 `c:/test/rootpath`
在 `sub_path.py` 中的 `sys.path[0]` 和 `sys.argv[0]` 的命令均获得 c:/test/rootpath

`os.path.split(os.path.realpath(__file__))[0]` : __file__所在脚本文件所在目录
在 `path.py` 中的 `os.path.split(os.path.realpath(__file__))[0]` 的命令均获得 `c:/test/rootpath`
在 `sub_path.py` 中的 `os.path.split(os.path.realpath(__file__))[0]` 的命令均获得 `c:/test/rootpath/sub_path`

注意当打包文件为 `exe` 文件时, 尽量使用第三种情况, 不会出错

### 使用 `pip` 命令行

使用 `pip list` 查看已安装的包名
然后用 `pip show` 包名, 就可以看到安装到哪了
通常安装在python安装目录下的 `lib/site-packages` 目录下

### 使用 `python -m site`

[Python site-packages目录的位置](https://blog.csdn.net/qq_19446965/article/details/124759600)

网站包目录有两种类型, 全局目录 和 每个用户目录.
运行时会列出全局站点软件包("dist-packages")目录 `sys.path`:

```bash
python -m site
sys.path = [...]
```

`USER_BASE` 和 `USER_SITE` 其实就是用户自定义的启用 `Python` 脚本和依赖安装包的基础路径.

`user_site` 其实就是个人的 `site-packages` 默认安装路径了.

也可以得到更简洁的列表, 请执行以下操作:

```bash
python -c 'import site; print(site.getsitepackages())'
```

注意: 使用`virtualenv` 时, `getsitepackages()`不可用,
但是上面的 `sys.path` 可以正确列出 `virtualenv` 的 `site-packages` 目录.

在Python 3中, 也可以使用 `sysconfig` 模块:

```bash
python3 -c 'import sysconfig; print(sysconfig.get_paths()["purelib"])'
```

查看当前用户的 `站点包目录`:

```bash
python -m site --user-site
```

如果指向不存在的目录, 请检查Python的退出状态,
并查看 `python -m site --help` 说明

提示: 运行 `pip list --user` 或 `pip freeze --user`
为您提供每个用户站点软件包的所有已安装列表.

### 查询模块信息

要查询某个模块的信息, 可以在python的交互界面, 输入

```python
help('modules')
```

根据回显
输入具体的模块名,获取这个模块的帮助信息

```python
help('datetime')
```

启动 `python` 命令行,输入以下两行命令

```python
import networkx
networkx.version

import requests
requests.__version__
```

## 导入模块

[python内置模块的导入](https://hackerqwq.github.io/2021/09/05/python%E5%86%85%E7%BD%AE%E6%A8%A1%E5%9D%97%E7%9A%84%E5%AF%BC%E5%85%A5%E5%8F%8A%E8%A6%86%E7%9B%96%E7%9F%A5%E8%AF%86/)

### import模块的几种方式

+ import 模块名
+ from 模块名 import 方法名
+ from 模块名 import *
+ import 模块名 as 别名
+ from 模块名 import 方法名 as 别名

官网还提供了 `__import__` 和 `importlib.import_module` 两种方法

[python doc: Modules](https://docs.python.org/3/tutorial/modules.html)

简单来说就是当导入操作发生的时候,
python 会到 `sys.modules` 里面寻找是否已经导入过了,
如果没有导入过才会按照 `sys.path` 的路径来寻找要导入的模块

`sys.modules` 中存的是已经导入过的模块, 内置模块在程序运行开始时就已经导入

+ 需要注意的是 `sys.modules` 中的键一旦删除, 后面的导入操作都不起作用
+ `sys.modules` 中的键删除了, 但是如果其他对象还有引用该模块的话, 该模块还是能使用的

`sys.path` 中存的是当 python 开始搜索模块时, 搜索的路径, `sys.path` 包含以下几个路径

+ 输入脚本的当前目录;
+ PYTHONPATH环境变量;
+ python安装时的系统目录;

### 覆盖内置模块

根据python导入模块的机制可以发现,
如果可以修改了 `sys.path` 然后再导入我们的同名恶意模块, 就可以实现恶意代码执行

首先修改 `sys.path`

+ 可以直接添加

```python
sys.path.insert(0,r"C:/Users/HackerQWQ/Desktop/test")
sys.path.append("/tmp/test")
```

+ 也修改 `PYTHONPATH` 环境变量

```bash
vim ~/.bashrc

#添加
export PYTHONPATH=$PYTHONPATH:/root/ws
#生效
source ~/.bashrc
```

python2 的话在 `/usr/lib/python2.7/sites-packages` 下添加 `.pth` 文件

```conf
/tmp/test
```

然后再调用 `importlib.reload` 模块, 即可执行恶意模块的命令.

#### 例子

```python
import sys
import os
import importlib

sys.path.insert(0,r"C:/Users/HackerQWQ/Desktop/test")
importlib.reload(os)
print(os.system("whoami"))
```

### 使用 `as` 导入

[Python3 覆盖内置同名库](https://blog.csdn.net/lanren_007/article/details/115479222)

实际上, `Python3` 中有些特殊的库, 即使本地目录中有同名文件, 在导入时也不会被覆盖.
比如 `site`, `sys`, `os`, `builtins` 等.
原因是: 一些 内建模块(built-in) 以及 和 Python运行时相关 的库名不会被覆盖.

解决方案:
可以修改自定义模块的名字 (常规操作)
或者是新建一个不存在名称冲突的文件夹, 将这个文件夹设置成包,
把名称冲突的模块放到包里, 通过包来导入, 导入后再通过 `as` 重命名
下面这种做法就可以覆盖内置的重名模块了:

```python
import packagename.site as site
```
