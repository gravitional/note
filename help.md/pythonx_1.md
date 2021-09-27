# python.x

[python标准库文档](https://docs.python.org/3/library/index.html)

## 如何阅读python文档

1.2. 标注

句法和词法解析的描述采用经过改进的 `BNF` 语法标注. 这包含以下定义样式:

```python
name      ::=  lc_letter (lc_letter | "_")*
lc_letter ::=  "a"..."z"
```

第一行表示 `name` 是一个 `lc_letter` 之后跟零个或多个 `lc_letter` 和下划线.
而一个 `lc_letter` 则是任意单个 `'a'` 至 `'z'` 字符. (实际上在本文档中始终采用此规则来定义词法和语法规则的名称. )

每条`规则`的开头是一个名称 (即该规则所定义的名称) 加上 `::=`. 竖线 (`|`) 被用来分隔可选项;它是此标注中最灵活的操作符.
星号 (`*`) 表示前一项的零次或多次重复;
类似地,加号 (`+`) 表示一次或多次重复,而由方括号括起的内容 (`[ ]`) 表示出现零次或一次 (或者说,这部分内容是可选的).
`*` 和 `+` 操作符的绑定是最紧密的;圆括号用于分组.
固定字符串包含在引号内. 空格的作用仅限于分隔形符.
每条规则通常为一行;有许多个可选项的规则可能会以竖线为界分为多行.

在词法定义中 (如上述示例),还额外使用了两个约定:

+ 由三个点号`...`分隔的两个字符字面值表示在指定 (闭) 区间范围内的任意单个 `ASCII` 字符.
+ 由尖括号 (`<...>`) 括起来的内容是对于所定义符号的非正式描述;即可以在必要时用来说明 '控制字符' 的意图.

虽然所用的标注方式几乎相同,但是词法定义和句法定义是存在很大区别的:
词法定义作用于输入源中单独的字符,而句法定义则作用于由词法分析所生成的`形符`(`tokens`)流.
在下一章节 ("词法分析") 中使用的 `BNF` 全部都是词法定义;在之后的章节中使用的则是句法定义.

### 查看python库文档

[查看python库文档](https://www.jianshu.com/p/a6219430b65c)

安装完python第三方库以后, 经常需要查询其文档, 其实python就自带文档查看器. 可以查看所有内置库和第三方库的文档, 虽然不是很详尽, 但是总比没有的好.     在命令行窗口

```python
python -m pydoc -p 60000
```

简单解释一下: 

`python -m pydoc`表示打开`pydoc`模块, `pydoc`是查看python文档的首选工具;`-p 6000`表示在`60000`端口上启动`server`

然后在浏览器中访问`http://localhost:60000/`

## Python运行外部程序

[Python运行外部程序的几种方法][]

[Python运行外部程序的几种方法]: https://blog.csdn.net/xiligey1/article/details/80267983

### subprocess --- 子进程管理

[subprocess --- 子进程管理](https://docs.python.org/zh-cn/3/library/subprocess.html#subprocess.run)

```python
subprocess.run(args, *, stdin=None, input=None, stdout=None, stderr=None, capture_output=False, shell=False, cwd=None, timeout=None, check=False, encoding=None, errors=None, text=None, env=None, universal_newlines=None, **other_popen_kwargs)
```

常用参数

`args` 被所有调用需要, 应当为一个字符串, 或者一个参数序列(比如列表`[a,b,c]`).
提供一个参数序列通常更好, 它可以使模组处理需要转义或者quote的字符(例如允许文件名中的空格).
如果传递单个字符串, 则要么 `shell` 参数必须为 `True` (见下文), 要么该字符串指定的程序不需要参数.

`stdin`,  `stdout` 和 `stderr` 分别指定了执行的程序的标准输入, 输出和标准错误`file handles`.
合法的值有 `PIPE` ,  `DEVNULL` ,  一个现存的文件描述符(一个正整数), 一个现存的文件对象以及 `None`.
`PIPE` 表示应该新建一个对子进程的管道.  `DEVNULL` 表示使用特殊的文件 `os.devnull`.
当使用默认设置 `None` 时, 将不会进行重定向, 子进程的`file handles`将继承自父进程.
另外,  `stderr` 可以是 `STDOUT`, 表示来自于子进程的` stderr data`应该被捕获到与 `stdout` 相同的` file handle`.

如果 `encoding` 或 `errors` 被指定, 或者 `text` (也名为 `universal_newlines` )为`True`, 则文件 objects `stdin` ,  `stdout` 与 `stderr` 将会使用在此次调用中指定的 `encoding` 和 `errors` 以文本模式打开.
未制定则使用默认的 `io.TextIOWrapper`.

对于 `stdin` ,  输入的换行符`\n` 将被转换为默认的换行符 `os.linesep`.
对于 `stdout` 和 `stderr` ,  所有输出的换行符都被转换为`\n`.
更多的信息可以参考`io.TextIOWrapper`的文档, 当它的构造函数中的`newline`参数被设置为`None`时.

如果未使用文本模式,  `stdin` ,  `stdout` 和 `stderr` 将会以二进制流模式打开. 则不会发生编码和换行符(`line ending`)的转换.

注意: 
file objects `Popen.stdin` ,  `Popen.stdout` 和 `Popen.stderr` 的换行符属性不会被 `Popen.communicate()` 方法更新.

如果 `shell` 设为 `True`,, 则使用指定的`shell` 执行指定的指令.
这样可以方便的使用一些`shell`的特性, 比如 shell 管道, 文件名`wildcards`, 环境变量展开以及 `~` (展开到用户家目录).
注意 Python 自己也实现了许多类似 shell 的特性(例如 `glob`, `fnmatch`, `os.walk()`, `os.path.expandvars()`,
`os.path.expanduser()` 和 `shutil` ).

示例

```python
>>> subprocess.run(["ls", "-l"])  # doesn't capture output
CompletedProcess(args=['ls', '-l'], returncode=0)

>>> subprocess.run("exit 1", shell=True, check=True)
Traceback (most recent call last):
  ...
subprocess.CalledProcessError: Command 'exit 1' returned non-zero exit status 1

>>> subprocess.run(["ls", "-l", "/dev/null"], capture_output=True)
CompletedProcess(args=['ls', '-l', '/dev/null'], returncode=0,
stdout=b'crw-rw-rw- 1 root root 1, 3 Jan 23 16:23 /dev/null\n', stderr=b'')
```

`subprocess.run` 返回的结果可能包含`\n`后缀, 这个时候可以使用字符串的`.removesuffix('\n')`方法去掉换行符.

### popen

[Popen 构造函数](https://docs.python.org/zh-cn/3/library/subprocess.html#subprocess.Popen)

`subprocess`模块中, 在底层上, 进程的创建与管理由 `Popen` 类处理.
它提供了很大的灵活性, 开发者能够处理没有被便利函数覆盖的情况.

```python
class subprocess.Popen(args, bufsize=-1, executable=None, stdin=None, stdout=None, stderr=None, preexec_fn=None, close_fds=True, shell=False, cwd=None, env=None, universal_newlines=None, startupinfo=None, creationflags=0, restore_signals=True, start_new_session=False, pass_fds=(), *, group=None, extra_groups=None, user=None, umask=-1, encoding=None, errors=None, text=None)
```

在一个新的进程中执行子程序.
在 `POSIX`平台, 此`class`使用类似于 `os.execvp()` 的行为来执行子程序.
在 `Windows`平台, 此`class`使用了 `Windows CreateProcess()` 函数.

### os.system运行外部程序

使用os.system函数运行其他程序或脚本

```python
import os
os.system('notepad python.txt')
```

***
使用`ShellExecute`函数运行其他程序

`ShellExecute(hwnd,op,file,params,dir,bShow)`

+ `hwnd`: 父窗口的句柄,若没有则为`0`
+ `op`: 要进行的操作,为`open`,`print` or `空`
+ `file`: 要运行的程序或脚本
+ `params`:  要向程序传递的参数,如果打开的是文件则为`空`
+ `dir`: 程序初始化的`目录`
+ `bShow`: 是否显示窗口

```python
ShellExecute(0, 'open', 'notepad.exe', 'python.txt', '', 1)
ShellExecute(0,'open','http://www.baidu.com','','',1)
ShellExecute(0,'open','F:\\Love\\Lady Antebellum - Need You Now.ape','','',1)
ShellExecute(0,'open','D:\Python\Code\Crawler\HanhanBlog.py','','',1)
```

***
使用`CreateProcess`函数

```python
import win32process
from win32process import CreateProcess
CreateProcess('c:\\windows\\notepad.exe', '', None, None, 0,
        win32process.CREATE_NO_WINDOW, None, None,
        win32process.STARTUPINFO())
```

***
用subprocess.call()

[python调用外部程序][]

[python调用外部程序]: https://blog.csdn.net/u011722133/article/details/80430439

`status = subprocess.call("mycmd"   " myarg", shell=True)`

比如

```python
#!/usr/bin/env python3
import os
import subprocess
# use os.system
os.system("ls  -lah /home/tom/Downloads")
print('\nuse another\n')
# use subprocess.call
subprocess.call("ls  -lah /home/tom/Downloads",shell=True)
```

## Python获取帮助

[Python获取帮助的3种方式][]

[Python获取帮助的3种方式]: https://blog.csdn.net/DQ_DM/article/details/45672623

***
help()

`help`函数是Python的一个内置函数.
函数原型: `help([object])`.
可以帮助我们了解该对象的更多信息.
If no argument is given, the interactive help system starts on the interpreter console.

***
`dir`函数是Python的一个内置函数.
函数原型: `dir([object])`
可以帮助我们获取该对象的大部分相关属性.
Without arguments, return the list of names in the current local scope.

***
在Python中有一个奇妙的特性,文档字符串,又称为DocStrings.
用它可以为我们的模块, 类, 函数等添加说明性的文字,使程序易读易懂,
更重要的是可以通过Python自带的标准方法将这些描述性文字信息输出.

上面提到的自带的标准方法就是`__doc__`. 前后各两个下划线.
注: 当不是函数, 方法, 模块等调用`doc`时,而是具体对象调用时,会显示此对象从属的类型的构造函数的文档字符串.

### 操作文件和目录

Python内置的`os`模块也可以直接调用操作系统提供的接口函数.

```python
>>> import os
>>> os.name # 操作系统类型
'posix'
# 给出详细的系统信息
>>> os.uname()
('Linux', 'OP7050', '4.15.0-112-generic', '#113-Ubuntu SMP Thu Jul 9 23:41:39 UTC 2020', 'x86_64')
# 给出环境变量
>>> os.environ
environ({'VERSIONER_PYTHON_PREFER_32_BIT': 'no', 'TERM_PROGRAM_VERSION': '326', 'LOGNAME': 'michael', 'USER': 'michael', 'PATH': '/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin:/usr/local/mysql/bin', ...})
# 要获取某个环境变量的值
>>> os.environ.get('PATH')
```

***
操作文件和目录的函数一部分放在`os`模块中,一部分放在`os.path`模块中

+ 查看当前目录的绝对路径: `os.path.abspath('.')`
+ 合并路径: `os.path.join('/Users/michael', 'testdir')`
+ 拆分路径: `os.path.split()`
+ 创建一个目录: `os.mkdir('/Users/michael/testdir')`
+ 删掉一个目录: `os.rmdir('/Users/michael/testdir')`
+ 获得文件扩展名: `os.path.splitext()`

`os.mkdir(path[, mode])`: 以数字mode的mode创建一个名为`path`的文件夹.默认的 `mode` 是 `0777 `

这些合并, 拆分路径的函数**并不要求目录和文件要真实存在**,它们只对字符串进行操作.

文件操作使用下面的函数. 假定当前目录下有一个`test.txt`文件: 

+ 对文件重命名: `os.rename('test.txt', 'test.py')`
+ 删掉文件: `os.remove('test.py')`

但是复制文件的函数居然在`os`模块中不存在!原因是复制文件并非由操作系统提供的系统调用.
幸运的是`shutil`模块提供了`copyfile()`的函数,你还可以在`shutil`模块中找到很多实用函数,它们可以看做是`os`模块的补充.

***
利用Python的特性来过滤文件. 比如我们要列出当前目录下的所有**目录**,只需要一行代码: 

```python
>>> [x for x in os.listdir('.') if os.path.isdir(x)]
['.lein', '.local', '.m2', '.npm', '.ssh', '.Trash', '.vim', 'Applications', 'Desktop', ...]
```

要列出所有的`.py`文件,也只需一行代码: 

```python
>>> [x for x in os.listdir('.') if os.path.isfile(x) and os.path.splitext(x)[1]=='.py']
['apis.py', 'config.py', 'models.py', 'pymonitor.py', 'test_db.py', 'urls.py', 'wsgiapp.py']
```

### shutil模块

安装
`pip install shutil`

***
复制文件

1. `shutil.copy('来源文件','目标地址')` : 返回值是复制之后的路径
2. `shutil.copy2()` ,`shutil.copy()` :差不多,复制后的结果保留了原来的所有信息(包括状态信息)
3. `shutil.copyfile(来源文件,目标文件)`  : 将一个文件的内容拷贝的另外一个文件当中,返回值是复制之后的路径
4. `shutil.copyfileobj(open(来源文件,'r'),open('目标文件','w'))` :将一个文件的内容拷贝的另外一个文件当中
5. `shutil.copytree(来源目录,目标目录)` : 复制整个文件目录 (无论文件夹是否为空,均可复制,而且会复制文件夹中的所有内容)
6. `copymode()`,`copystat()` 不常用

***
删除文件

1. `shutil.rmtree(目录路径)` #移除整个目录,无论是否空(删除的是文件夹,如果删除文件`os.unlink(path)`)
2. `shutil.move(来源地址,目标地址)`#移动文件

***
案列分享

有时候在进行大量文件复制的过程中,会出现同样名字被覆盖的问题,看到很多案列感觉麻烦,懒人有懒人的办法

```python
#!/usr/bin/env python3
import os,shutil,time
# 判断文件名已经存在

print(
os.path.split(os.path.abspath('.'))
)

file_path=os.path.join('/home','tom','note','python')
filename='os.txt'

if os.path.exists(os.path.join('.','')):
#把原来的文件名进行改掉
#主要是如果循环多,重复的名字多,所以用时间戳进行代替,不会弄重复
    os.rename(
    os.path.join(file_path,filename),
    os.path.join(file_path,str(time.time())+filename)
    )
```

[python操作文件,强大的shutil模块][]

[python操作文件,强大的shutil模块]: https://www.jianshu.com/p/e62b05f08179

### 序列化

在程序运行的过程中,所有的变量都是在内存中,比如,定义一个`dict`:`d = dict(name='Bob', age=20, score=88)`

我们把变量从内存中变成可存储或传输的过程称之为序列化,在Python中叫`pickling`,在其他语言中也被称之为`serialization`,`marshalling`,`flattening`等等,都是一个意思.

序列化之后,就可以把序列化后的内容写入磁盘,或者通过网络传输到别的机器上.
反过来,把变量内容从序列化的对象重新读到内存里称之为反序列化,即`unpickling`.
Python提供了`pickle`模块来实现序列化.

首先,我们尝试把一个对象序列化并写入文件: 

```python
>>> import pickle
>>> d = dict(name='Bob', age=20, score=88)
>>> pickle.dumps(d)
b'...'
```

`pickle.dumps()`方法把任意对象序列化成一个`bytes`,然后,就可以把这个`bytes`写入文件.
或者用另一个方法`pickle.dump()`直接把对象序列化后写入一个`file-like Object`: 

```python
>>> f = open('dump.txt', 'wb')
>>> pickle.dump(d, f)
>>> f.close()
```

看看写入的`dump.txt`文件,一堆乱七八糟的内容,这些都是Python保存的对象内部信息.

当我们要把对象从磁盘读到内存时,可以先把内容读到一个`bytes`,然后用`pickle.loads()`方法反序列化出对象,也可以直接用`pickle.load()`方法从一个`file-like Object`中直接反序列化出对象.
我们打开另一个Python命令行来反序列化刚才保存的对象: 

```python
>>> f = open('dump.txt', 'rb')
>>> d = pickle.load(f)
>>> f.close()
>>> d
{'age': 20, 'score': 88, 'name': 'Bob'}
```

变量的内容又回来了!

Pickle的问题和所有其他编程语言特有的序列化问题一样,就是它只能用于Python,并且可能不同版本的Python彼此都不兼容,因此,只能用Pickle保存那些不重要的数据,不能成功地反序列化也没关系.

### 文件读写

读写文件是最常见的IO操作. Python内置了读写文件的函数,用法和C是兼容的.

读写文件前,我们先必须了解一下,在磁盘上读写文件的功能都是由操作系统提供的,现代操作系统不允许普通的程序直接操作磁盘,所以,读写文件就是请求操作系统打开一个文件对象(通常称为文件描述符),然后,通过操作系统提供的接口从这个文件对象中读取数据(读文件),或者把数据写入这个文件对象(写文件).

#### 读文件

读文件使用Python内置的`open()`函数,传入文件名和标示符: 

+ 打开文件(读取): `f = open('/Users/thomas/desktop/test.py', 'r')`
+ 打开文件(写入)`f = open('/Users/michael/test.txt', 'w')`
+ 关闭文件: `f.close()`
+ 读取内容: `f.read()`
+ 每次最多读取`size`个字节的内容: `read(size)`方法
+ 每次读取一行内容:  `readline()`
+ 一次读取所有内容并按行返回`list`: `readlines()`
+ 要读取二进制文件,比如图片, 视频等等,用'`rb`'模式: ` f = open('/Users/michael/test.jpg', 'rb')`
+ 给`open()`函数传入编码参数(一般是uft-8): `f = open('/Users/michael/gbk.txt', 'r', encoding='gbk')`
+ 忽略未识别字符`f = open('/Users/michael/gbk.txt', 'r', encoding='gbk', errors='ignore')`

Python引入了`with`语句来自动帮我们调用`close()`方法: 

```python
with open('/path/to/file', 'r') as f:
    print(f.read())
```

写文件和读文件是一样的,唯一区别是调用`open()`函数时,传入标识符`'w'`或者`'wb'`表示写文本文件或写二进制文件: 

```python
>>> f = open('/Users/michael/test.txt', 'w')
>>> f.write('Hello, world!')
>>> f.close()
```

你可以反复调用`write()`来写入文件,但是务必要调用`f.close()`来关闭文件. 当我们写文件时,操作系统往往不会立刻把数据写入磁盘,而是放到内存缓存起来,空闲的时候再慢慢写入. 所以,还是用`with`语句来得保险: 

```python
with open('/Users/michael/test.txt', 'w') as f:
    f.write('Hello, world!')
```

要写入特定编码的文本文件,请给`open()`函数传入`encoding`参数,将字符串自动转换成指定编码.

如果我们希望追加到文件末尾怎么办?可以传入`'a'`以追加(`append`)模式写入.

所有模式的定义及含义可以参考 [Python的官方文档][].

[Python的官方文档]: https://docs.python.org/3/library/functions.html#open

## python 空语句

`pass`

## 条件判断和循环

***
判断样式

```python
age = 3
if age >= 18:
    print('adult')
elif age >= 6:
    print('teenager')
else:
    print('kid')
```

***
`for x in ...`循环就是把每个元素代入变量`x`,然后执行缩进块的语句.
Python提供一个`range()`函数,可以生成一个整数序列,再通过`list()`函数可以转换为`list`.

```python
>>> list(range(5))
[0, 1, 2, 3, 4]
```

```python
sum = 0
for x in range(101):
    sum = sum + x
print(sum)
```

***
第二种循环是`while`循环,只要条件满足,就不断循环,条件不满足时退出循环.
比如我们要计算`100`以内所有奇数之和,可以用`while`循环实现: 

```python
sum = 0
n = 99
while n > 0:
    sum = sum + n
    n = n - 2
print(sum)
```

如果要提前结束循环,可以用`break`语句: 
在循环过程中,也可以通过`continue`语句,跳过当前的这次循环,直接开始下一次循环.

## formfactor 脚本

### 复制结果的脚本

```python
#!/usr/bin/env python3
import os,shutil,time,gfm
# 复制到论文中的都是 ci==1.50 的结果
user_name='tom'
# 配置计算结果目录,论文目录,论文压缩文件目录
originpath=os.getcwd()
paper_path=os.path.join('/home',user_name,'private','paper-2.prd/')
desk_path=os.path.join('/home',user_name,'Desktop','paper.ff/')
# 复制计算结果到论文目录
shutil.copy('fig.baryons.ge.charge.L-0.90.ci-1.50.pdf', paper_path+'fig4.pdf')
shutil.copy('fig.baryons.ge.neutral.L-0.90.ci-1.50.pdf', paper_path+'fig5.pdf')
shutil.copy('fig.baryons.gm.charge.L-0.90.ci-1.50.pdf', paper_path+'fig2.pdf')
shutil.copy('fig.baryons.gm.neutral.L-0.90.ci-1.50.pdf', paper_path+'fig3.pdf')
# cd 到论文目录,重新编译论文
os.chdir(paper_path)
os.system('./build.sh')
# 如果桌面有压缩文件目录,就删除,shutil.copytree需要目标不存在
if  os.path.isdir(desk_path):
    shutil.rmtree(desk_path)
    # 把论文目录的东西复制到桌面目录中
    shutil.copytree('.',desk_path)
else:
    shutil.copytree('.',desk_path)

print("+++++++\nthe file copied from paper_path\n+++++++")
os.listdir(desk_path)

## 切换到桌面整理目录
os.chdir(desk_path )
print("+++++++\ndelete auxilary files\n +++++++")

rm_list=['*.aux','*.lof','*.log','*.lot','*.fls','*.out',
'*.toc', '*.fmt','*.fot','*.cb','*.cb2','*.ptc','*.xdv','*.fdb_latexmk',
'*.synctex.gz','*.swp','*.ps1','*.sh','*.bib','*.bbl','*.blg',
'*.py','*.pyc','__pycache__'
]

for aux in rm_list:
    gfm.rma('.',aux)

print("+++++++\nthe file left in",os.getcwd(),"\n+++++++")
os.listdir(desk_path)

# 产生论文压缩文件
os.system(('7z a ../paper.7z '+desk_path))
# 回到原来的文件夹
os.listdir(originpath)
```

## 模块

Python本身就内置了很多非常有用的模块,只要安装完毕,这些模块就可以立刻使用.

### 模块写法

我们以内建的`sys`模块为例,编写一个`hello`的模块: 

```python
#!/usr/bin/env python3
# -*- coding: utf-8 -*-

' a test module '

__author__ = 'Michael Liao'

import sys

def test():
    args = sys.argv
    if len(args)==1:
        print('Hello, world!')
    elif len(args)==2:
        print('Hello, %s!' % args[1])
    else:
        print('Too many arguments!')

if __name__=='__main__':
    test()
```

第`1`行和第`2`行是标准注释,
第`1`行注释可以让这个`hello.py`文件直接在`Unix/Linux/Mac`上运行,第`2`行注释表示`.py`文件本身使用标准`UTF-8`编码;

第`4`行是一个字符串,表示模块的文档注释,**任何模块代码的第一个字符串都被视为模块的文档注释**;

第`6`行使用`__author__`变量把作者写进去,这样当你公开源代码后别人就可以瞻仰你的大名;

以上就是Python模块的标准文件模板,当然也可以全部删掉不写,但是,按标准办事肯定没错.

后面开始就是真正的代码部分.

你可能注意到了,使用`sys`模块的第一步,就是导入该模块: 
`import sys`

导入`sys`模块后,我们就有了变量`sys`指向该模块,利用`sys`这个变量,就可以访问`sys`模块的所有功能.

`sys`模块有一个`argv`变量,用`list`存储了命令行的所有参数. `argv`至少有一个元素,因为第一个参数永远是该`.py`文件的名称,例如: 

运行`python3 hello.py Michael`获得的`sys.argv`就是`['hello.py', 'Michael]`.

最后,注意到这两行代码: 

```python
if __name__=='__main__':
    test()
```

当我们在命令行运行`hello`模块文件时,Python解释器把一个特殊变量`__name__`置为`__main__`,而如果在其他地方导入该`hello`模块时,`if`判断将失败,因此,这种`if`测试可以让一个模块通过命令行运行时执行一些额外的代码,最常见的就是运行测试.

我们可以用命令行运行`hello.py`看看效果: 

```bash
$ python3 hello.py
Hello, world!
$ python hello.py Michael
Hello, Michael!
```

如果启动Python交互环境,再导入hello模块: 

```python
$ python3
Python 3.4.3...
>>> import hello
```

导入时,没有打印`Hello, word!`,因为没有执行`test()`函数.

调用`hello.test()`时,才能打印出Hello, word!: 

```python
>>> hello.test()
Hello, world!
```

### 导入自己的Python模块

[编写你自己的Python模块][]

[编写你自己的Python模块]: https://www.cnblogs.com/yuanrenxue/p/10675135.html

每一个 Python 程序同时也是一个模块. 你只需要保证它以 `.py` 为扩展名即可. 下面的案例会作出清晰的解释.

***
案例(保存为 `mymodule.py`): 

```python
def say_hi():
    print('Hi, this is mymodule speaking.')

__version__ = '0.1'
```

上方所呈现的就是一个简单的模块, 与我们一般所使用的 Python 的程序并没有什么特殊的区别. 我们接下来将看到如何在其它 Python 程序中使用这一模块.

要记住该模块应该放置于与其它我们即将导入这一模块的程序相同的目录下,或者是放置在`sys.path`所列出的其中一个目录下.

另一个模块(保存为`mymodule_demo.py`): 

```python
import mymodule

mymodule.say_hi()
print('Version', mymodule.__version__)
```

输出: 

```python
$ python mymodule_demo.py
Hi, this is mymodule speaking.
Version 0.1
```

***
下面是一个使用 `from...import` 语法的范本(保存为 `mymodule_demo2.py`): 

```python
from mymodule import say_hi, __version__

say_hi()
print('Version', __version__)
```

`mymodule_demo2.py` 所输出的内容与 `mymodule_demo.py` 所输出的内容是一样的.

在这里需要注意的是,如果导入到 `mymodule` 中的模块里已经存在了` __version__` 这一名称,那将产生冲突.
这可能是因为每个模块通常都会使用这一名称来声明它们各自的版本号.
因此最好使用 `import` 语句,尽管这会使你的程序变得稍微长一些.

你还可以使用: 

```python
from mymodule import *
```

这将导入诸如 `say_hi` 等所有公共名称,但不会导入 `__version__` 名称,因为后者以双下划线开头.

警告: 要记住你应该避免使用 `import`的这种形式,即 `from mymodule import `.

>Python 的一大指导原则是''明了胜过晦涩,你可以通过在 Python 中运行 `import this` 来了解更多内容.

### 作用域

在一个模块中,我们可能会定义很多函数和变量,但有的函数和变量我们希望给别人使用,有的函数和变量我们希望仅仅在模块内部使用. 在Python中,是通过`_`前缀来实现的.

正常的函数和变量名是公开的(`public`),可以被直接引用,比如: `abc`,`x123`,`PI`等;

类似`__xxx__`这样的变量是特殊变量,可以被直接引用,但是有特殊用途,比如上面的`__author__`,`__name__`就是特殊变量,hello模块定义的文档注释也可以用特殊变量`__doc__`访问,我们自己的变量一般不要用这种变量名;

类似`_xxx`和`__xxx`这样的函数或变量就是非公开的(private),不应该被直接引用,比如`_abc`,`__abc`等;

之所以我们说,private函数和变量''不应该''被直接引用,而不是''不能''被直接引用,是因为Python并没有一种方法可以完全限制访问private函数或变量,但是,从编程习惯上不应该引用private函数或变量.

private函数或变量不应该被别人引用,那它们有什么用呢?请看例子: 

```python
def _private_1(name):
    return 'Hello, %s' % name

def _private_2(name):
    return 'Hi, %s' % name

def greeting(name):
    if len(name) > 3:
        return _private_1(name)
    else:
        return _private_2(name)
```

我们在模块里公开`greeting()`函数,而把内部逻辑用private函数隐藏起来了,这样,调用`greeting()`函数不用关心内部的private函数细节,这也是一种非常有用的代码封装和抽象的方法,即: 

外部不需要引用的函数全部定义成private,只有外部需要引用的函数才定义为public.

如果一个函数定义中包含`yield`关键字,那么这个函数就不再是一个普通函数,而是一个`generator`: 

### 通配符删除文件

[Python 通配符删除文件][]

[Python 通配符删除文件]: https://blog.csdn.net/mathcompfrac/article/details/75331440

```python
# -*- coding: utf-8 -*-
"""
使用通配符,获取所有文件,或进行操作.
"""

__author__ = '飞鸽传说'

import glob,os

# 给出当前目录下的文件的generator
def files(curr_dir = '.', ext = '*.aux'): # 指定默认变量,目录和拓展名
    """给出当前目录下的文件"""
    for i in glob.glob(os.path.join(curr_dir, ext)):
        yield i

# 给出所有文件的generator
def all_files(rootdir, ext):
    """给出当前目录下以及子目录的文件"""
    for name in os.listdir(rootdir):
        if os.path.isdir(os.path.join(rootdir, name)):#判断是否为文件夹
            try:
                for sub_file in all_files(os.path.join(rootdir, name), ext):
                    # 递归地给出所有子文件夹中的文件
                    yield sub_file
            except:
                pass
    for cwd_file in files(rootdir, ext):
        yield cwd_file

# 删除文件的函数,默认不显示
def remove_files(rootdir, ext, show = False):
    """删除rootdir目录下的符合的文件"""
    for cwd_file in files(rootdir, ext):
        if show:
            print cwd_file
        os.remove(cwd_file)

# 删除所有文件的函数,默认不显示
def remove_all_files(rootdir, ext, show = False):
    """删除rootdir目录下以及子目录下符合的文件"""
    for rec_file in all_files(rootdir, ext):
        if show:
            print rec_file
        os.remove(rec_file)

# 如果在命令行运行,Python解释器把一个特殊变量__name__置为__main__
if __name__ == '__main__':
    #这一行删除预设的文件
    remove_all_files('.', '*.aux', show = True)
    # remove_all_files('.', '*.exe', show = True)
    remove_files('.', '*.aux', show = True)
    # for i in files('.','*.c'):
        # print i
```

## 函数

```python
def write_result(str):
  writeresult=file(r'D:\eclipse4.4.1 script\my_selenium\model\test_result.log','a+')
  str1=writeresult.write(str+'\n')
  writeresult.close()
  return str
```

函数参数类型,一共五种
可以这么理解, 参数一共有两大类, 位置参数(无名参数)和字典参数(署名参数). 想象python解释器去解释函数的参数列表: 

1. **位置参数**和**字典参数**初看起来, 形式上是一样的, 单纯从各自的形式上, 将无法区分,
所以需要约定一个分割符, 左边的是**位置参数**和**默认参数**, 然后右边是**不定字典参数**和**字典参数**
这个分隔符就懒省事儿地选成`*`, 而且也可以用来表示**不定参数**.
2. 确定长度(长度为`1`)的变量放在参数列表两端, 如**位置参数**和**字典参数**, 不确定长度的变量(**不定参数**和**不定字典参数**)放在中间.
因为长度可变的参数如果放在两边, 由于它可以自由伸缩占据参数位置, 就会把别的变量位置掩盖. 所以要在两端用固定长度的变量封住.

由此可以确定五种类型参数的次序

```python
(A, B=25, *C, **D,E,)
(位置参数A, 默认参数B, 不定参数*C, 不定字典**D, 字典参数E)
```

不定在中间, 定长在两边.
左边是位置, 右边是字典.
何处是分隔, `*`号来体现.

### 特殊参数

[4.7.3.1. 位置或关键字参数](https://docs.python.org/zh-cn/3/tutorial/controlflow.html#positional-or-keyword-arguments)

默认情况下, 函数的参数传递形式可以是位置参数或是显式的关键字参数.
为了确保可读性和运行效率, 限制允许的参数传递形式是有意义的,
这样开发者只需查看函数定义即可确定参数项是仅按位置, 按位置也按关键字, 还是仅按关键字传递.

函数的定义看起来可以像是这样: 

```python
def f(pos1, pos2, #只能是位置参数 (Positional only)
/, pos_or_kwd, #位置或关键字参数 (Positional or keyword)
*, kwd1, kwd2): # 只能是关键字参数(Keyword)
```

在这里 `/` 和 `*` 是可选的.
如果使用这些符号则表明可以通过何种形参将参数值传递给函数: 仅限位置, 位置或关键字, 以及仅限关键字.
关键字形参也被称为命名形参.

如果函数定义中未使用 `/` 和 `*`, 则参数可以按`位置`或按`关键字`传递给函数

如果是 `positional-only` 的形参, 则其位置是重要的, 并且该形参不能作为关键字传入.
`positional-only`形参要放在 `/` (正斜杠) 之前.  这个 `/` 被用来从逻辑上分隔`positional-only`形参和其它形参.
如果函数定义中没有 `/`, 则表示没有`positional-only`形参.
在 `/` 之后的形参可以为 位置或关键字 或 仅限关键字.

要将形参标记为 `keyword-only` , 即指明该形参必须以关键字参数的形式传入,
应在参数列表的第一个 `keyword-only ` 形参之前放置一个 `*`.

函数举例: 

请考虑以下示例函数定义并特别注意 `/` 和 `*` 标记:

```python
>>> def standard_arg(arg):
...     print(arg)
...
>>> def pos_only_arg(arg, /):
...     print(arg)
...
>>> def kwd_only_arg(*, arg):
...     print(arg)
...
>>> def combined_example(pos_only, /, standard, *, kwd_only):
...     print(pos_only, standard, kwd_only)
```

第一个函数定义 `standard_arg` 是最常见的形式, 对调用方式没有任何限制, 参数可以按位置也可以按关键字传入:

```python
>>> standard_arg(2)
2
>>> standard_arg(arg=2)
2
```

第二个函数 `pos_only_arg` 在函数定义中带有 `/`, 限制仅使用位置形参:

```python
>>> pos_only_arg(1)
1
>>> pos_only_arg(arg=1)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: pos_only_arg() got an unexpected keyword argument 'arg'
```

最后, 请考虑这个函数定义, 它的位置参数 `name`  和 `**kwds` 之间可能产生潜在冲突,
由于关键字名称可能也是 `name` :

```python
def foo(name, **kwds):
    return 'name' in kwds
```

任何调用都不可能让它返回 `True` , 因为关键字 `name` 将总是绑定到第一个形参.  例如:

```python
>>> foo(1, **{'name': 2})
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: foo() got multiple values for argument 'name'
>>>
```

但使用 `/` (仅限位置参数) 就可以避免这个问题, 因为它可以区分开作为位置参数的 `name` ,
以及作为关键字参数名称的 `'name'` :

```python
def foo(name, /, **kwds):
    return 'name' in kwds
>>> foo(1, **{'name': 2})
True
```

换句话说, 限制形参为仅限位置, 可以防止它在 `**kwds` 中使用产生的歧义. 也就是最好写成`(name, /, **kwds)`这样的形式

### python 参数传递

[Python函数中修改变量](https://blog.csdn.net/qq_41987033/article/details/81675514)

python 中的变量赋值是一种`绑定`(bind), `x=1`即把名字`x`绑定到对象`1`上.
在Python中, 同样的值对应唯一一个地址.
列表是对象地址的集合, 本身也是一个对象. 修改这个对象的分量, 会导致所有绑定到这个对象的名称受到影,列表是可变对象, 不可 hashable.

装饰器

map reduce

## python库

### glob

[python标准库之glob介绍][]

[python标准库之glob介绍]: https://www.cnblogs.com/luminousjj/p/9359543.html

`glob` 文件名模式匹配,不用遍历整个目录判断每个文件是不是符合.

#### 通配符*

星号`*`匹配零个或多个字符

```python
import glob
for name in glob.glob('dir/*'):
    print (name)
```

列出子目录中的文件,必须在模式中包括子目录名: 

```python
import glob

#用子目录查询文件
print ('Named explicitly:')
for name in glob.glob('dir/subdir/*'):
    print ('\t', name)
#用通配符* 代替子目录名
print ('Named with wildcard:')
for name in glob.glob('dir/*/*'):
    print ('\t', name)
```

#### 单个字符通配符?

用问号`?`匹配任何单个的字符.

```python
import glob

for name in glob.glob('dir/file?.txt'):
    print (name)
```

#### 字符范围

当需要匹配一个特定的字符,可以使用一个范围

```python
import glob
for name in glob.glob('dir/*[0-9].*'):
    print (name)
```

## python vscode 调试

`open launch.json` 打开调试文件

有两种标准配置,或者在`code`的集成终端中运行,或者在外部终端运行: 

```json
{
    "name": "Python: Current File (Integrated Terminal)",
    "type": "python",
    "request": "launch",
    "program": "${file}",
    "console": "integratedTerminal"
},
{
    "name": "Python: Current File (External Terminal)",
    "type": "python",
    "request": "launch",
    "program": "${file}",
    "console": "externalTerminal"
}
```

还可以添加其他设置如`args`,但它不属于标准配置的一部分.  比如,你经常运行 `startup.py`,并使用参数 `--port 1593`, 则可以添加如下配置: 

```bash
 {
     "name": "Python: startup.py",
     "type": "python",
     "request": "launch",
     "program": "${workspaceFolder}/startup.py",
     "args" : ["--port", "1593"]
 },
```

+ `name` ;  `vscode` 下拉列表中的名字
+ `type`;  要使用的调试器类型;对于 `Python`代码,将此设置为 `python`.
+ `request` ;  指定调试开始的`模式`.
+ `launch`;  在`program`中指定的文件上启动调试器.
+ `attach`;  将调试器附加到一个已经运行的进程.请看`Remote debugging`的例子.
+ `program`: 程序的路径. `${file}`,当前激活的编辑器,可以是绝对路径,也可以是相对路径,如: `"program": "${workspaceFolder}/pokemongo_bot/event_handlers/__init__.py"`
+ `python`: 用来debug的python 解释器的全路径. 如果不指定,使用`python.pythonPath`,等价于`${config:python.pythonPath}`,
也可以使用环境变量. 还可以向解释器传递参数,`"python": ["<path>", "<arg>",...]`.
+ `args` ; 传递给 python 程序的参数. 如`"args": ["--quiet", "--norepeat", "--port", "1593"]`
+ `stopOnEntry` ; 当设置为`true`时,在地一行停下. 默认忽略,在第一个间断点停下.
+ `console` ;  指定程序如何输出结果,可以设置成`"internalConsole"`,`"externalTerminal"`,`"integratedTerminal" (default)`
+ `cwd` 指定当前工作目录,默认为`${workspaceFolder}` (打开`vscode`的目录)
+ `redirectOutput` ; 是否重定向debug输出. 选择`XXterminal`时,默认关闭. (不在VS code debug window中输出)
+ `justMyCode` ;  `true`或忽略,只调试用户写的代码. `false`也调试标准库函数.
+ `django` ;  当设置为 `true` 时,会激活 `Django` 网络框架特有的调试功能. 
+ `sudo` ; 设置为`true`,且调试窗口选择为`externalTerminal`时,可以提升权限
+ `pyramid` ;  当设置为 `true`时,确保用必要的`pserve`命令启动一个`Pyramid`应用程序.
+ `env` ; 设置可选的环境变量, 为 `debugger` 进程, 除了系统变量之外. `值`必须为字符串.
+ `envFile` ; 包含`环境变量`定义的`文件`的可选路径.参见 `Configuring Python environments - environment variable definitions file`.
+ `gevent`;  如果设置为 `true`,可以对 `gevent monkey-patched` 的代码进行调试.

## 字符串格式化输出

### python 格式化输出

[更漂亮的输出格式](https://docs.python.org/zh-cn/3/tutorial/inputoutput.html#formatted-string-literals)

格式化字符串字面值 (常简称为 `f`-字符串)能让你在字符串前加上 `f` 和 `F` 并将表达式写成 `{expression}` 来在字符串中包含 `Python` 表达式的值. 字符串用引号或三引号表示.

```bash
>>> year = 2016
>>> event = 'Referendum'
>>> f'Results of the {year} {event}'
'Results of the 2016 Referendum'
```

python 也可以使用类似shell中`echo`的彩色输出,
`print("\033[1;47m\033[1;32m Testing output... \033[0;0m")`

使用`\033[0;0m`使终端回到之前的颜色

### 三引号

字符串字面值可以跨行连续输入. 一种方式是用三重引号: `"""..."""` 或 `'''...'''`.
字符串中的回车换行会自动包含到字符串中,如果不想包含,在行尾添加一个 `\` 即可. 如下例:

```python
print("""\
Usage: thingy [OPTIONS]
     -h                        Display this usage message
     -H hostname               Hostname to connect to
""")
```

将产生如下输出(注意最开始的换行没有包括进来):

```python
Usage: thingy [OPTIONS]
     -h                        Display this usage message
     -H hostname               Hostname to connect to
```

字符串可以用 `+` 进行连接(粘到一起),也可以用 `*` 进行重复:

```python
>>> # 3 times 'un', followed by 'ium'
>>> 3 * 'un' + 'ium'
'unununium'
```

相邻的两个或多个 字符串字面值 (引号引起来的字符)将会自动连接到一起.

```python
>>> 'Py' 'thon'
'Python'
```

把很长的字符串拆开分别输入的时候尤其有用:

```python
>>> text = ('Put several strings within parentheses '
...         'to have them joined together.')
>>> text
'Put several strings within parentheses to have them joined together.'
```

只能对两个字面值这样操作,变量或表达式不行:

```python
>>> prefix = 'Py'
>>> prefix 'thon'  # can't concatenate a variable and a string literal
  File "<stdin>", line 1
    prefix 'thon'
                ^
SyntaxError: invalid syntax
>>> ('un' * 3) 'ium'
  File "<stdin>", line 1
    ('un' * 3) 'ium'
                   ^
SyntaxError: invalid syntax
```

如果你想连接变量,或者连接变量和字面值,可以用 `+` 号:

```python
>>> prefix + 'thon'
'Python'
```

字符串是可以被 索引 (下标访问)的,第一个字符索引是 `0`. 单个字符并没有特殊的类型,只是一个长度为一的字符串:

```python
>>> word = 'Python'
>>> word[0]  # character in position 0
'P'
>>> word[5]  # character in position 5
'n'
```

索引也可以用负数,这种会从右边开始数:

```python
>>> word[-1]  # last character
'n'
>>> word[-2]  # second-last character
'o'
>>> word[-6]
'P'
```

## 括号 dict comprehensions

### 大括号{}

创建字典,或者字典推导式`{x: x**2 for x in (2, 4, 6)}`

### 中括号[]

创建数组, 也可以使用列表推导式

```bash
[x**2 for x in range(10)]
[(x, y) for x in [1,2,3] for y in [3,1,4] if x != y]
```

`['key']`也用来从字典中取出值
`{'jack': 4098, 'sape': 4139, 'guido': 4127}['jack']`

### 小括号()

创建元组(固定不变的数组)

小括号不带逗号: 表示对括号内的单一表达式求值.
小括号可以用来把一个式子分成多行.

## python 邮件

[解放双手,用Python实现自动发送邮件](https://zhuanlan.zhihu.com/p/89868804)

Python有两个内置库: `smtplib`和`email`,能够实现邮件功能,`smtplib`库负责发送邮件,`email`库负责构造邮件格式和内容.

邮件发送需要遵守`SMTP`协议,Python内置对`SMTP`的支持,可以发送纯文本邮件, `HTML`邮件以及带附件的邮件.

脚本如下: 

```python
#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# 先导入相关的库和方法
import sys # 命令行调用参数
from pathlib import Path# 处理文件路径和文件名
import smtplib,email# 邮件发送和构造
from email.mime.text import MIMEText # 负责构造文本
from email.mime.image import MIMEImage # 负责构造图片
from email.mime.multipart import MIMEMultipart # 负责将多个对象集合起来
from email.header import Header

# 定义一个彩色的打印函数
def echo2(x):
    print(f'+++++++++++++++++++++++\n\033[1;47m\033[1;32m{str(x)}\033[0;0m\n++++++++++++++++++++ ')

# 命令行参数, 第二个参数, 即 sys.argv[1], 是要发送的附件的位置, 支持 ~user
attach_file=(Path(str(sys.argv[1]))).expanduser()
echo2(attach_file) #打印出文件的路径
attach_name=attach_file.name
echo2(attach_name) # 打印出文件的名字

## 设置邮箱域名, 发件人邮箱, 邮箱授权码, 收件人邮箱
# SMTP服务器,
mail_host = 'smtp.qq.com'
# 发件人邮箱
mail_sender ='xxx@qq.com'
# 邮箱授权码,注意这里不是邮箱密码,如何获取邮箱授权码,请看本文最后教程
mail_license = 'xxx'
# 收件人邮箱, 可以为多个收件人
mail_receivers = ['xxx@qq.com',]

# 构建MIMEMultipart对象代表邮件本身, 可以往里面添加文本, 图片, 附件等
mm = MIMEMultipart('related')

## 设置邮件头部内容
# 邮件主题
subject_content = """Python邮件测试"""
# 设置发送者,注意严格遵守格式,里面邮箱为发件人邮箱
mm["From"] = "tom<xxx@qq.com>"
# 设置接受者,注意严格遵守格式,里面邮箱为接受者邮箱
mm["To"] = "xxx<xxx@qq.com>"
# 设置邮件主题
mm["Subject"] = Header(subject_content,'utf-8')

## 添加正文文本
# 邮件正文内容
body_content = """你好, 这是一个测试邮件!"""
# 构造文本,参数1: 正文内容, 参数2: 文本格式, 参数3: 编码方式
message_text = MIMEText(body_content,"plain","utf-8")
# 向MIMEMultipart对象中添加文本对象
mm.attach(message_text)

# 添加图片
# # 二进制读取图片
# image_data = open('a.jpg','rb')
# # 设置读取获取的二进制数据
# message_image = MIMEImage(image_data.read())
# # 关闭刚才打开的文件
# image_data.close()
# # 添加图片文件到邮件信息当中去
# mm.attach(message_image)

## 如果存在的话, 就添加附件, 并发送邮件
if attach_file.exists():
    atta = MIMEText(open(str(attach_file), 'rb').read(), 'base64', 'utf-8') # 构造附件
    atta["Content-Disposition"] = f'attachment; filename={str(attach_name)}'# 设置附件信息
    mm.attach(atta)# 添加附件到邮件信息当中去
    ## 发送邮件# 创建SMTP对象
    stp = smtplib.SMTP()
    stp.connect(mail_host, 587)  # 设置发件人邮箱的域名和端口, 端口地址为465
    stp.set_debuglevel(1)# set_debuglevel(1)可以打印出和SMTP服务器交互的所有信息
    stp.login(mail_sender,mail_license)# 登录邮箱, 传递参数1: 邮箱地址, 参数2: 邮箱授权码
    # 发送邮件, 传递参数1: 发件人邮箱地址, 参数2: 收件人邮箱地址, 参数3: 把邮件内容格式改为str
    stp.sendmail(mail_sender, mail_receivers, mm.as_string())
    echo2("邮件发送成功")
    stp.quit()# 关闭SMTP对象
else:
    echo2(f'there is no such file {str(attach_name)}')
```

保存成`mail_attach.py`, 移动到`~/bin/`目录, 再用一个`bash`脚本`tomyself.sh`调用这个模块.

```python
cp ~/private/backup/tomyself.sh ~/private/backup/mail_attach.py  ~/bin
(cd ~/bin; chmod +x tomyself.sh mail_attach.py)
source ~/.zshrc
```

```bash
#!/bin/bash
# -*- coding: utf-8 -*-
# 将命令行传入的第一个参数当成附件的路径, 展开后传入 py 脚本
python3 ~/bin/mail_attach.py $(realpath -e $1)
```

## 接受命令行参数

[2.1.1. 传入参数](https://docs.python.org/zh-cn/3/tutorial/interpreter.html#argument-passing)

如果可能的话,解释器会读取命令行参数,转化为字符串列表存入 `sys` 模块中的 `argv` 变量中.
执行命令 `import sys` 你可以导入这个模块并访问这个列表.
这个列表最少也会有一个元素;如果没有给定输入参数,`sys.argv[0]` 就是个空字符串.
如果脚本名是标准输入,`sys.argv[0]` 就是 `-`. 使用 `-c command` 时,`sys.argv[0]` 就会是 `-c`.
如果使用选项 `-m module`,`sys.argv[0]` 就是模块的包含目录的全名.
在 `-c command` 或 `-m module` 之后的选项不会被解释器处理,而会直接留在 `sys.argv` 中给命令或模块来处理.

### import 模块

倒入模块的时候,有几种不同的语法

+ `import fibo`
把`fibo`模块(`fibo.py`文件)中的定义导入当前模块,用 `fibo.fib2`等方式访问.

+ `from fibo import fib, fib2` 把`fibo`模块内的函数等等直接导入本模块的符号表
+
+ `from fibo import *` 导入`fibo`模块内定义的所有名称,
这会调入所有非以下划线(`_`)开头的名称.  在多数情况下,Python程序员都不会使用这个功能,
因为它在解释器中引入了一组未知的名称,而它们很可能会覆盖一些你已经定义过的东西.
不过,在交互式编译器中为了节省打字可以这么用.

+ 可以用`as`将名称绑定到欲导入的模块,方便使用

```python
import fibo as fib
from fibo import fib as fibonacci
```

## 文件系统路径pathlib

[pathlib --- 面向对象的文件系统路径](https://docs.python.org/zh-cn/3/library/pathlib.html#pathlib.PurePath.name)

路径类被分为提供纯计算操作而没有 I/O 的 纯路径,以及从纯路径继承而来但提供 I/O 操作的 具体路径.

如果你以前从未使用过此模块或者不确定在项目中使用哪一个类是正确的,则 Path 总是你需要的.
它在运行代码的平台上实例化为一个具体路径.

在一些用例中纯路径很有用,例如: 

+ 如果你想要在 Unix 设备上操作 `Windows` 路径(或者相反).
你不应在 `Unix` 上实例化一个 WindowsPath,但是你可以实例化 `PureWindowsPath`.
+ 你只想操作路径但不想实际访问操作系统. 在这种情况下,实例化一个纯路径是有用的,因为它们没有任何访问操作系统的操作.

实例化: 也就是建立属于一个类的对象.

### 基础使用

导入主类:`from pathlib import Path`
导入纯粹路名操作类: `from pathlib import PurePath`

+ `PurePath.name`:一个表示最后路径组件的字符串,排除了驱动器与根目录,如果存在的话:

```python
PurePosixPath('my/library/setup.py').name
out:'setup.py'
# UNC 驱动器名不被考虑:
PureWindowsPath('//some/share/setup.py').name
out: 'setup.py'
PureWindowsPath('//some/share').name
out: ''
```

要获得路径的字符串形式,直接`str`,`Path`类中有`'__str__'`内部调用方法,比如
`str(PurePosixPath('my/library/setup.py'))`

### 对应的os模块的工具

可以参考[os 与 PurePath/Path 对应相同的函数的表](https://docs.python.org/zh-cn/3/library/pathlib.html#correspondence-to-tools-in-the-os-module)

os 和 os.path, pathlib

常用的操作有

+ `Path.resolve()` :将路径绝对化,解析任何符号链接.`..` 组件也将被消除(只有这一种方法这么做):

```python
>>> p = Path('docs/../setup.py')
>>> p.resolve()
PosixPath('/home/antoine/pathlib/setup.py')
```

+ `Path.chmod()` : 改变文件的模式和权限,和 `os.chmod()` 一样:
+ `Path.mkdir(mode=0o777, parents=False, exist_ok=False)`: 新建给定路径的目录. 如果给出了 `mode` ,它将与当前进程的 `umask` 值合并来决定文件模式和访问标志.
+ `Path.rename(target)`将文件或目录重命名为给定的 `target`,并返回一个新的指向 `target` 的 `Path` 实例
+ `Path.rmdir()`: 移除此目录. 此目录必须为空的.
+ `Path.unlink()`:移除此文件或符号链接. 如果路径指向目录,则用 `Path.rmdir()` 代替.
+ `Path.cwd()`: 返回一个新的表示当前目录的路径对象(和 `os.getcwd()` 返回的相同):
+ `Path.exists()`:此路径是否指向一个已存在的文件或目录:
+ `Path.expanduser()`:返回展开了包含 `~` 和 `~user` 的构造,就和 `os.path.expanduser()` 一样:
+ `Path.home()`:返回一个表示当前用户家目录的新路径对象
+ `Path.iterdir()`:当路径指向一个目录时,产生该路径下的对象的路径:
+ ` PurePath.joinpath(*other)`: 将每个 `other` 参数中的项目连接在一起,如`PurePosixPath('/etc').joinpath('init.d', 'apache2')`
+ `PurePath.name`: 一个表示最后路径组件的字符串,排除了驱动器与根目录,如果存在的话:
+ `PurePath.parent`:此路径的逻辑父路径, 这是一个单纯的 lexical operation
+ `Path.samefile(other_path)`: 返回此目录是否指向与可能是字符串或者另一个路径对象的 other_path 相同的文件
+ `PurePath.suffix`:最后一个组件的文件扩展名,如果存在:
+ `PurePath.suffixes` : 路径的文件扩展名列表,如`PurePosixPath('my/library.tar.gar').suffixes`

## pypi 镜像

### 清华大学pypi镜像使用帮助

[清华大学开源软件镜像站](https://mirrors.tuna.tsinghua.edu.cn/help/pypi/)

pypi 镜像每 5 分钟同步一次.

临时使用

```bash
pip install -i https://pypi.tuna.tsinghua.edu.cn/simple some-package
```

注意,`simple` 不能少, 是 `https` 而不是 `http`

***
设为默认

升级 `pip` 到最新的版本 (>=10.0.0) 后进行配置: 

```bash
pip3 install pip -U
pip3 config set global.index-url https://pypi.tuna.tsinghua.edu.cn/simple
```

如果您到 pip 默认源的网络连接较差,临时使用本镜像站来升级 `pip3`: 

```bash
pip3 install -i https://pypi.tuna.tsinghua.edu.cn/simple pip -U
```

### 手动修改配置文件

[PyPI使用国内源](https://www.cnblogs.com/sunnydou/p/5801760.html)

如果想配置成默认的源,方法如下: 

需要创建或修改配置文件(一般都是创建),

`linux`的文件在`~/.pip/pip.conf`,

`windows`在`%HOMEPATH%\pip\pip.ini`),

修改内容为: 

```bash
[global]
index-url = http://pypi.douban.com/simple
[install]
trusted-host=pypi.douban.com
```

这样在使用`pip`来安装时,会默认调用该镜像.

## class 类型

### 查看内置类型

[内置类型](https://docs.python.org/zh-cn/3/library/stdtypes.html#built-in-types)

#### 字符串类型

` str.format(*args, **kwargs)`

执行字符串格式化操作.
调用此方法的字符串的组成部分, 可以是`literal text`, 也可以是替换域, 用花括号 `{}` 括起来.
替换域中可以是一个位置参数的数字索引, 或者是一个关键字参数的名称.
返回的字符串副本中, 每个替换域都会变成相应的值.

```python
>>>"The sum of 1 + 2 is {0},while 4+6 is {1}".format(1+2,4+6)
'The sum of 1 + 2 is 3,while 4+6 is 10'
```

字符串方法: 

+ `str.removesuffix(suffix, /)`
如果字符串以 `suffix` 字符串结尾, 并且 `suffix` 非空, 返回 `string[:-len(suffix)]` . 否则, 返回原始字符串的副本: 
+ `str.replace(old, new[, count])`
返回字符串的副本, 其中出现的所有子字符串 `old` 都将被替换为 `new` .  如果给出了可选参数 `count`, 则只替换前 `count` 个.
+ `str.lstrip([chars])`
  返回原字符串的副本, 移除其中的前导字符.  `chars` 参数为指定要移除字符的字符串.
  如果省略或为 `None` , 则 `chars` 参数默认移除空格符.  实际上 `chars` 参数并非指定单个前缀;而是会移除参数值的所有组合:

```python
>>> '   spacious   '.lstrip()
'spacious   '
>>> 'www.example.com'.lstrip('cmowz.')
'example.com'
```

`str.removeprefix(prefix, /)`
如果字符串以 `prefix` 字符串开头, 返回 `string[len(prefix):]` . 否则, 返回原始字符串的副本: 

```python
>>> 'TestHook'.removeprefix('Test')
'Hook'
>>> 'BaseTestCase'.removeprefix('Test')
'BaseTestCase'
```

`str.rsplit(sep=None, maxsplit=-1)`

返回一个由字符串内单词组成的列表, 使用 `sep` 作为分隔字符串.
如果给出了 `maxsplit` , 则最多进行 `maxsplit` 次拆分, 从 最右边 开始.
如果 `sep` 未指定或为 `None` , 任何空白字符串都会被作为分隔符.
除了从右边开始拆分, `rsplit()` 的其他行为都类似于下文所述的 `split()`.

`str.rstrip([chars])`
返回原字符串的副本, 移除其中的末尾字符.  `chars` 参数为指定要移除字符的字符串.
如果省略或为 `None` , 则 `chars` 参数默认移除空格符.  实际上 `chars` 参数并非指定单个后缀;而是会移除参数值的所有组合:

```python
>>> '   spacious   '.rstrip()
'   spacious'
>>> 'mississippi'.rstrip('ipz')
'mississ'
```

`str.removesuffix(suffix, /)`
如果字符串以 `suffix` 字符串结尾, 并且 `suffix` 非空, 返回 `string[:-len(suffix)]` .
否则, 返回原始字符串的副本: 

```python
>>> 'MiscTests'.removesuffix('Tests')
'Misc'
>>> 'TmpDirMixin'.removesuffix('Tests')
'TmpDirMixin'
```

### 类的特殊属性

[Difference between _, __ and __xx__ in Python](http://igorsobreira.com/2010/09/16/difference-between-one-underline-and-two-underlines-in-python.html)

### 单下划线

Python没有真正的私有方法,因此在方法或属性开头加下划线表示您不应访问此方法,因为它不是API的一部分.
使用属性时很常见: 

```python
class BaseForm(StrAndUnicode):
    ...

    def _get_errors(self):
        "Returns an ErrorDict for the data provided for the form"
        if self._errors is None:
            self.full_clean()
        return self._errors

    errors = property(_get_errors)
```

此摘录摘自django源代码 (`django/forms/forms.py`).
这意味着`errors`是一个属性,并且是`API`的一部分,但是此属性调用的方法` _get_errors`是''私有''的,因此您不应访问它.

### __xxx

开头有两个下划线,这个约定引起很多混乱. 它不是用来标记私有方法,而是用来避免方法被子类覆盖. 让我们来看一个例子: 

```python
class A(object):
    def __method(self):
        print "I'm a method in A"

    def method(self):
        self.__method()

a = A()
a.method()
# The output here is
# I'm a method in A
# 符合我们的预期. 现在让我们继承A类并自定义__method

class B(A):
    def __method(self):
        print "I'm a method in B"

b = B()
b.method()
# and now the output is...
#I'm a method in A
```

如你所见,`A.method()`没有像我们期望的那样调用`B.__method()`.
实际上,这是`__`的正确行为. 因此,当您创建以`__`开头的方法时,表示你想避免它被重写,你想让这个方法只在这个类内部被访问.

`python`是怎么做的?很简单,它只是重命名方法. 看一看: 

```python
a = A()
a._A__method()  # never use this!! please!
# I'm a method in A
```

如果您尝试访问`a.__method()`,那么它也不起作用,就像我说的那样,`__method`只能在类本身内部访问.
注意,私有变量,也就是一个下划线的`_method2()`,是可以从外部访问到的.

### 前后双下划线

带有前后双下划线`__twounderlines__`的方法是提供给python调用的,而不是让用户显式调用的.
当你看到类似`__this__`这种方法,规则很简单,不要使用它.
它可以用来重新定义一些运算符.

```python
name = "igor"
print(name.__len__())
print(len(name))
# 结果都是4
number = 10
number.__add__(20)
number + 20
# 结果都是30
## 比如 重新定义加减
class CrazyNumber(object):
    def __init__(self, n):
        self.n = n
    def __add__(self, other):
        return self.n - other
    def __sub__(self, other):
        return self.n + other
    def __str__(self):
        return str(self.n)

num = CrazyNumber(10)
print (num)           # 10
print (num + 5)       # 5
print (num - 20)      # 30
```

使用`_xxx`单个下划线,来表示该方法或属性是私有的,不属于API;
使用`__xx`两个下划线来创建像是`native python objects`的对象,或者你想自定义一些行为.
一般不需要使用`__xx__`,除非你想避免方法在继承之后,被子类被重写.

## 判断类型

`isinstance(object, classinfo)`

如果参数 `object` 是参数 `classinfo` 的实例或者是其 (直接, 间接或 虚拟) 子类则返回 `True`.
如果 `object` 不是给定类型的对象,函数将总是返回 `False` .
如果 `classinfo` 是类型对象元组(或由其他此类元组递归组成的元组),那么如果 `object` 是其中任何一个类型的实例就返回 `True` .
如果 `classinfo` 既不是类型,也不是类型元组或类型元组的元组,则将引发 `TypeError` 异常.

## 模块

['编译过的'Python文件](https://docs.python.org/zh-cn/3/tutorial/modules.html#the-module-search-path)

为了加速模块载入, Python在 `__pycache__` 目录里缓存了每个模块的编译后版本, 名称为 `module.version.pyc` , 其中名称中的版本字段对编译文件的格式进行编码;
它一般使用Python版本号. 例如, 在CPython版本3.3中, `spam.py`的编译版本将被缓存为 `__pycache__/spam.cpython-33.pyc`.
此命名约定允许来自不同发行版和不同版本的Python的已编译模块共存.

Python根据编译版本检查源的修改日期, 以查看它是否已过期并需要重新编译. 这是一个完全自动化的过程. 此外, 编译的模块与平台无关, 因此可以在具有不同体系结构的系统之间共享相同的库.

一个从 `.pyc` 文件读出的程序并不会比它从 `.py` 读出时运行的更快, `.pyc `文件唯一快的地方在于载入速度.

### 包

包是一种通过用''带点号的模块名''来构造 Python 模块命名空间的方法.
例如, 模块名 `A.B` 表示 `A` 包中名为 `B` 的子模块.
正如模块的使用使得不同模块的作者不必担心彼此的全局变量名称一样, 使用加点的模块名可以使得 `NumPy` 或 `Pillow` 等多模块软件包的作者不必担心彼此的模块名称一样.

包结构的例子

```python
 sound/                          Top-level package
      __init__.py               Initialize the sound package
      formats/                  Subpackage for file format conversions
              __init__.py
              wavread.py
              wavwrite.py
              aiffread.py
              aiffwrite.py
              auread.py
              auwrite.py
              ...
      effects/                  Subpackage for sound effects
              __init__.py
              echo.py
              surround.py
              reverse.py
              ...
      filters/                  Subpackage for filters
              __init__.py
              equalizer.py
              vocoder.py
              karaoke.py
              ...
```

当导入这个包时, Python搜索 sys.path 里的目录, 查找包的子目录.

必须要有 `__init__.py` 文件才能让 Python 将包含该文件的目录当作包.  这样可以防止例如叫做`string`这样常见名称的目录中的模块无法被搜索到.
在最简单的情况下, `__init__.py` 可以只是一个空文件, 但它也可以执行包的初始化代码或设置 `__all__` 变量, 具体将在后文介绍.

可以按照下面的语法调用

```python
from sound.effects.echo import echofilter
```

## 错误和异常

[错误和异常](https://docs.python.org/zh-cn/3/tutorial/errors.html)

至少有两种可区分的错误: *语法错误* 和 *异常*.

### 语法错误

语法错误又称解析错误,如

```python
>>> while True print('Hello world')
  File "<stdin>", line 1
    while True print('Hello world')
                   ^
SyntaxError: invalid syntax
```

这里少了一个`:`号

### 异常

即使语句或表达式在语法上是正确的, 但在尝试执行时, 它仍可能会引发错误.  在执行时检测到的错误被称为*异常*, `Exception`.
异常不一定会导致严重后果: 你将很快学会如何在Python程序中处理它们.  但是, 大多数异常并不会被程序处理, 此时会显示如下所示的错误信息:

```python
>>> 10 * (1/0)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
ZeroDivisionError: division by zero
>>> 4 + spam*3
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
NameError: name 'spam' is not defined
>>> '2' + 2
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: Can't convert 'int' object to str implicitly
```

错误信息的最后一行告诉我们程序遇到了什么类型的错误. 异常有不同的类型, 而其类型名称将会作为错误信息的一部分中打印出来: 
上述示例中的异常类型依次是: `ZeroDivisionError`,  `NameError` 和 `TypeError`. 作为异常类型打印的字符串是发生的内置异常的名称.
对于所有内置异常都是如此, 但对于用户定义的异常则不一定如此(虽然这是一个有用的规范). 标准的异常类型是`built-in identifiers`(而不是`reserved keywords`).

错误消息的开头部分以`stack traceback`的形式显示发生异常的上下文.  通常它会列出源代码行的堆栈回溯;但是不会显示从标准输入读取的行.

`Built-in Exceptions` 列出了内置异常和它们的含义.

### 处理异常

`try ... except ... finally`

```python
>>> try:
...     raise Exception('spam', 'eggs')
... except Exception as inst:
...     print(type(inst))    # the exception instance
...     print(inst.args)     # arguments stored in .args
...     print(inst)
```

### 抛出异常

`raise` 语句允许程序员强制指定异常

```python
>>> raise NameError('HiThere')
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
NameError: HiThere
```

```python
raise ValueError  # shorthand for 'raise ValueError()'
```

### 异常链, 可以用来转换异常

```python
# exc must be exception instance or None.
raise RuntimeError from exc
```

```python
>>> def func():
...     raise IOError
...
>>> try:
...     func()
... except IOError as exc:
...     raise RuntimeError('Failed to open database') from exc
...
```

### 定义清理操作

try 语句有另一个可选子句`finally`, 它在任何情况下都执行, 相当于"清理操作", 例如:

```python
>>> try:
...     raise KeyboardInterrupt
... finally:
...     print('Goodbye, world!')
...
Goodbye, world!
KeyboardInterrupt
Traceback (most recent call last):
  File "<stdin>", line 2, in <module>
```

### 预定义的清理操作

某些对象预定义了标准清理操作, 无论使用该对象的操作是成功还是失败, 清理操作都会被执行. 比如我们尝试打开一个文件并打印到屏幕:

```python
for line in open("myfile.txt"):
    print(line, end="")
```

问题在于, 在这部分代码执行完后, 会使文件在一段不确定的时间内处于打开状态, 对于较大的应用程序来说可能是个问题.
`with` 语句允许类似文件的对象, 能够确保使用后被清理:

```python
with open("myfile.txt") as f:
    for line in f:
        print(line, end="")
```

执行完语句后, 即使在处理行时遇到问题, 文件 `f` 也始终会被关闭. 和文件一样, 提供预定义清理操作的对象将在其文档中说明.
