# python 打包, pyinstaller

[Using PyInstaller](https://pyinstaller.org/en/v5.13.2/usage.html)
[别再问我怎么Python打包成exe了!](https://www.zhihu.com/tardis/zm/art/162237978?source_id=1005)
[Python打包EXE方法汇总整理](https://zhuanlan.zhihu.com/p/355304094)

首先安装pyinstaller, 使用安装命令:

```bash
pip3 install pyinstaller
```

+ 简单打包命令

```bash
pyinstaller -F setup.py
```

`-F` 参数表示覆盖打包, 这样在打包时, 不管我们打包几次, 都是最新的.
打包完成的程序在 `dist` 目录下面

+ 其他打包命令

```bash
Pyinstaller -F -w setup.py # 不带控制台的打包
Pyinstaller -F -i xx.ico setup.py # 打包指定exe图标打包
```

## debug

### 多进程打包

如果py程序是多进程的话, 使用pyinstaller打包会出现错误, 这个时候只要加上一行代码

在:

```python
if __name__=='__mian__':
    #新增下面一行代码即可打包多进程
    multiprocessing.freeze_support()
```

### word文件打包

如果要打包那种操作word的文件代码,
用pyinstaller工具把使用到python-docx库的脚本打包成exe可执行文件后,

双击运行生成的exe文件, 报错:

```bash
docx.opc.exceptions.PackageNotFoundError:
Package not found at 'C:\Users\ADMINI~1.PC-\AppData\Local\Temp\_MEI49~1\docx\templates\default.docx'
```

经过在stackoverflow上搜索, 发现有人遇到过类似的问题
(问题链接: cx_freeze and docx - problems when freezing),
经过尝试, 该问题的第二个回答可以解决这个问题:

大概的解决步骤是这样的:

找到python-docx包安装路径下的一个名为default.docx的文件,
用 everything 搜索一下, 它在我本地所在的路径是:

```bash
E:\code\env\.env\Lib\site-packages\docx\templates
```

把找到的 `default.docx` 文件复制到我的py脚本文件所在的目录下.
修改脚本中创建Document对象的方式:

```python
# 从原来的创建方式:
# document = Document()
# 修改为:
import os
document = Document(docx=os.path.join(os.getcwd(), 'default.docx'))
```

再次用 `pyinstaller` 工具打包脚本为exe文件
把default.docx文件复制到与生成的exe文件相同的路径下,
再次运行exe文件, 顺利运行通过.
