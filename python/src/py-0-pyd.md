# python 生成 .pyd 文件

[c++生成, 运行, 调试python扩展(.pyd)](https://blog.csdn.net/guoziyang8114/article/details/122032071)

## 生成.pyd文件

### pyd的本质

`pyd` 就是 `dll`(Linux上是 `.so`)文件, 并做了特殊约定:

+ 导出的模块函数名格式必须是 `PyInit_模块名`, 如:

```cpp
//test.cpp
#include <Python.h>
#include <iostream>

PyObject* PrintHello(PyObject *self, PyObject *args)
{
 std::cout << "Hello, I am form c++" << std::endl;
 Py_INCREF(Py_None);
 return Py_None;
}
//描述方法
PyMethodDef Methods[] = {
 {"PrintHelloFn", PrintHello, METH_VARARGS, "aSdasdasd"},
 {NULL, NULL}
};

static struct PyModuleDef cModPyDem =
{
    PyModuleDef_HEAD_INIT,
    "PrintHello", /* name of module */
    "",          /* module documentation, may be NULL */
    -1,          /* size of per-interpreter state of the module, or -1 if the module keeps state in global variables. */
    Methods
};

//函数名必须为这样的格式:  PyInit_模块名
PyMODINIT_FUNC PyInit_PrintHello(void)
{
    return PyModule_Create(&cModPyDem);

}
```

+ `pyd` 文件名在 `debug` 模式下必须是:
`模块名_d.pyd`(必须使用对应的 `Python_d.exe`, 而不是 `python.exe`),
比如 `PrintHello_d.pyd`
在 `release` 下必须是: `模块名.pyd`

### 如何生成.pyd文件

有两种方法:

+ 将之前的 `test.cpp` 编译成一个 `dll` 文件, 然后依据上面的规则将文件改名: `PrintHello_d.pyd`
+ 编写 `setup.py` 文件

```python
from distutils.core import setup, Extension
from Cython.Build import cythonize

ext = Extension("PrintHello",
                sources=[ "test.cpp"],
                language="c++"
               )

setup(name="PrintHello",
      version="0.0.1",
      ext_modules=cythonize(ext))
```

然后执行: `python setup.py build_ext`

### 运行.pyd文件

+ 切换当前工作目录到.pyd文件目录
+ 执行 python (如果是调试模式需要执行python_d)
+ 执行 import PrintHello 即可

### 调试.pyd文件

主要使用附加到进程的方式调试.

(1) 调试如下脚本:

```python
import os
print(os.getpid())
import PrintHello
PrintHello.PrintHelloFn()
```

注意: 在最后一行打断点.
当执行 `import PrintHello` 的时候才会加载 `.pyd` 文件, 所以断点要打在第三行后面.
使用 `getpid()` 是为了方便下一步的附加到进程

(2) 启动任何一个 `IDE`, 以 `附加到进程` 的方式启动调试,
`进程id` 可以参考(1)中 `os.getpid()` 打印的结果.
这时候能够在 `c++` 打上断点, 如果打不上断点就说明做错了

(3) 继续运行(1)中的脚本, 这时候应该能看到c++中命中了断点

### 在构件中添加 --debug 选项

[pass --debug](https://stackoverflow.com/questions/61692952/how-to-pass-debug-to-build-ext-when-invoking-setup-py-install)

A. If I am not mistaken, one could achieve that
by adding the following to a `setup.cfg` file alongside the setup.py file:

```conf
[build_ext]
debug = 1
```

B.1. For more flexibility, I believe it should be possible to be explicit on the command line:

```bash
path/to/pythonX.Y setup.py build_ext --debug install
```

B.2. Also if I understood right it should be possible to define so-called aliases

```conf
# setup.cfg
[aliases]
release_install = build_ext install
debug_install = build_ext --debug install
```

```bash
path/to/pythonX.Y setup.py release_install
path/to/pythonX.Y setup.py debug_install
```

## python numpy py 模块导入失败

[c++调用python Py_DEBUG 错误](https://blog.csdn.net/my393661/article/details/84639865)

在 MSVC2019平台, `c++`项目中引入 python 库, 并引入 `numpy` 模块
在 `Debug`构建模式下进行调试, `import numpy` 初始化的时候, 会报以下错误

```log
from numpy.core._multiarray_umath import(
ModuleNotFoundError: No module named 'numpy.core._multiarray_umath'
```

这个错误也可能在其它模块, 或者调用 python 内置库时出现,
因为通常安装下, python 本体或者 numpy 模块都没有提供相应的 `debug` 版本的 `LIB/DLL`

在 [nmupy 的安装文档中][] 写到:
与其在 `windows` 上以 `DEBUG 模式` 编译项目,
不如尝试在 `RELEASE 模式` 下编译项目, 并使用 调试符号, 不做任何优化(with debug symbols and no optimization).
Windows 上的纯 `DEBUG` 模式会更改 Python 希望找到的 `DLL` 的名称,
因此如果你希望真正在 `DEBUG` 模式下工作, 就需要重新编译包括 `NumPy` 在内的整个 `Python` 模块栈.

最简单的解决方案是, 修改python 安装附送的 c/c++ `include` 头文件库中的 `pyconfig.h`,
把相应的宏注释掉, 不使用 debug 版本的库.

```c++
//324行
#   if defined(_DEBUG)
#    //pragma comment(lib,"python310_d.lib")
#    pragma comment(lib,"python310.lib")
#   elif defined(Py_LIMITED_API)
#    pragma comment(lib,"python3.lib")
#   else
#    pragma comment(lib,"python310.lib")
#   endif /* _DEBUG */
...

// 369行
#ifdef _DEBUG
// # define Py_DEBUG   //kare
#endif
```

[nmupy 的安装文档中]: https://numpy.org/doc/stable/user/troubleshooting-importerror.html#debug-build-on-windows
