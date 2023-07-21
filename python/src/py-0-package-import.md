# python package, 包, 模块

[7.11. import 语句](https://docs.python.org/zh-cn/3/reference/simple_stmts.html#import)
[6. 模块](https://docs.python.org/zh-cn/3/tutorial/modules.html#modules)
[python模块详解](https://zhuanlan.zhihu.com/p/33913131)

## ss

+ `import xxx` 形式可以 `import Foo` 或者 `import Foo.Bar`
+ 仅仅 `import A` 可能无法使用`A` 内部的模块，要导入到具体模块或者变量的层次, 或者在 `__init__.py` 初始化绑定.
+ `import A.b` 中的 `b` 需要是子模块, 不能是文件中的标识符, 导入后的标识符是 `A.b` 而不是 `b`.

+ `from A import b` 可以导入 `A` 中的标识符 `A.b`, 并绑定到当前命名空间, 后续只需使用 `b` 引用.
+ `from` 形式只能 `from A.B.C import D`, 而不能写成 `from A.B import C.B`,
+ 类似 `from .moduleY import spam` 的相对导入只能在 `package` 中使用, 而不能在 `脚本` 中使用.

+ 在 `sys.path` 中添加 `dirA` 并不会自动递归搜索 `dirA` 的子目录.
所以在导入时不能跳过, 需要写成 `import A.B.module.D` 这种完全路径的形式,
假设 `A` 已经加入搜索路径.
通常使用 `python xxx.py` 运行脚本时, 会自动添加 `xxx.py` 所在的目录到 `sys.path`.

+ `.moduleY` 指代 当前脚本的同级目录下的 `moduleY` 模块

+ 绝对导入可以使用 `import <>` 或 `from <> import <>` 语法,
但 `相对导入` 只能写成 `from .X import 标识符` 的形式; 其中的原因在于 `import XXX.YYY.ZZZ`
应当提供 `XXX.YYY.ZZZ` 作为可用表达式, 但 `.moduleY` 不是一个有效的表达式

+ 多行导入可以写成

```python
from posixpath import (
    abspath as abspath,
    basename as basename,
    commonpath as commonpath
    )
```

## ImportError: attempted relative import with no known parent package

[Python报错ImportError: attempted relative import with no known parent package](https://blog.csdn.net/weixin_43958105/article/details/114012590)
[Python 导包八种方法](https://blog.csdn.net/sinat_38682860/article/details/111404997)
[import path -- 导入路径](https://docs.python.org/zh-cn/3/glossary.html#term-import-path)
[5. 导入系统](https://docs.python.org/zh-cn/3/reference/import.html#)
[Python函数进阶:  Hook 钩子函数](https://zhuanlan.zhihu.com/p/339718510)

使用下列命令调用 python 脚本时

```bash
python xxx/xxx/pkg/pkg_test.py
```

会将脚本所在目录添加到模块搜索路径 `sys.path`, 参见 [sys.path][] 中的解释.
但是不会将 `pkg` 所在的目录添加到路径

报错: `无法识别的包文件(no known parent package)`, 是由于
`Python解释器` 无法查找到 `当前执行脚本`(即当前模块), 所属的 `最小包结构`(package).

[sys.path]: https://docs.python.org/zh-cn/3/library/sys.html#sys.path

## import path -- 导入路径

由多个位置(或 路径条目)组成的列表, 会被模块的 [path based finder][] 用来查找导入目标.
在导入时, 此位置列表通常来自 `sys.path`, 但对次级包来说也可能来自上级包的 `__path__` 属性.

[path based finder]: https://docs.python.org/zh-cn/3/glossary.html#term-path-based-finder

## sys.path

一个由字符串组成的列表, 用于指定模块的 `搜索路径`.
初始化自环境变量 PYTHONPATH, 再加上一条与安装有关的默认路径.

默认情况下, 在程序启动时被初始化, 一个潜在的不安全路径被 `前缀` 到 `sys.path` 中
(插入到 `PYTHONPATH` 生成的条目之前):

+ `python -m module` 命令行: 预置当前工作目录.
+ `python script.py` 命令行: 预置脚本的目录. 如果是符号链接, 则解析符号链接.
+ `python -c code` 和 `python` (REPL)命令行: 预置一个空字符串, 这意味着当前工作目录.

要不预置这个可能不安全的路径, 请使用 `-P` 命令行选项, 或 `PYTHONSAFEPATH` 环境变量.

一个程序可以自由地修改 `sys.path` 列表.
应该只把字符串 添加到 `sys.path` 中;
所有其他数据类型在 `import` 时都会被忽略.

参见 [site][] 模块, 该模块描述了如何使用 .pth 文件来扩展 sys.path.

[site]: https://docs.python.org/zh-cn/3/library/site.html#module-site

## 添加到 sys.path

[10.9 将文件夹加入到sys.path](https://python3-cookbook.readthedocs.io/zh_CN/latest/c10/p09_add_directories_to_sys_path.html)

如果您使用模块级的变量来精心构造一个适当的绝对路径,
有时你可以解决硬编码目录的问题, 比如 `__file__`.
举个例子:

```python
import sys
from os.path import abspath, join, dirname
sys.path.insert(0, join(abspath(dirname(__file__)), 'src'))
```

这将 `src` 目录添加到 `path` 里, 和执行插入步骤的代码在同一个目录里.
