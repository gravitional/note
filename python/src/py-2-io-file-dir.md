# python io, 文件读写, 文件操作

## 文件读写

读写文件是最常见的IO操作. Python内置了读写文件的函数,用法和C是兼容的.

读写文件前,我们先必须了解一下,在磁盘上读写文件的功能都是由操作系统提供的,
现代操作系统不允许普通的程序直接操作磁盘,所以,读写文件就是请求操作系统打开一个文件对象(通常称为文件描述符),
然后,通过操作系统提供的接口从这个文件对象中读取数据(读文件),或者把数据写入这个文件对象(写文件).

### 读文件

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

写文件和读文件是一样的,唯一区别是调用`open()`函数时,
传入标识符`'w'`或者`'wb'`表示写文本文件或写二进制文件:

```python
>>> f = open('/Users/michael/test.txt', 'w')
>>> f.write('Hello, world!')
>>> f.close()
```

你可以反复调用`write()`来写入文件,但是务必要调用`f.close()`来关闭文件.
当我们写文件时,操作系统往往不会立刻把数据写入磁盘,而是放到内存缓存起来,
空闲的时候再慢慢写入. 所以,还是用`with`语句来得保险:

```python
with open('/Users/michael/test.txt', 'w') as f:
    f.write('Hello, world!')
```

要写入特定编码的文本文件,请给`open()`函数传入`encoding`参数,将字符串自动转换成指定编码.
如果我们希望追加到文件末尾怎么办?可以传入`'a'`以追加(`append`)模式写入.
所有模式的定义及含义可以参考 [Python的官方文档](https://docs.python.org/3/library/functions.html#open).

## 操作文件和目录

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
environ({'VERSIONER_PYTHON_PREFER_32_BIT': 'no',
'TERM_PROGRAM_VERSION': '326', 'LOGNAME': 'michael', 'USER': 'michael',
'PATH': '/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin:/usr/local/mysql/bin', ...})
# 要获取某个环境变量的值
>>> os.environ.get('PATH')
```

### os.path

操作文件和目录的函数一部分放在`os`模块中,一部分放在`os.path`模块中

+ 查看当前目录的绝对路径: `os.path.abspath('.')`
+ 合并路径: `os.path.join('/Users/michael', 'testdir')`
+ 拆分路径: `os.path.split()`
+ 创建一个目录: `os.mkdir('/Users/michael/testdir')`
+ 删掉一个目录: `os.rmdir('/Users/michael/testdir')`
+ 获得文件扩展名: `os.path.splitext()`

`os.mkdir(path[, mode])`: 以数字mode的mode创建一个名为`path`的文件夹.
默认的 `mode` 是 `0777 `

这些合并, 拆分路径的函数**并不要求目录和文件要真实存在**,它们只对字符串进行操作.

文件操作使用下面的函数. 假定当前目录下有一个`test.txt`文件:

+ 对文件重命名: `os.rename('test.txt', 'test.py')`
+ 删掉文件: `os.remove('test.py')`

但是复制文件的函数居然在`os`模块中不存在!原因是复制文件并非由操作系统提供的系统调用.
幸运的是`shutil`模块提供了`copyfile()`的函数,你还可以在`shutil`模块中找到很多实用函数,它们可以看做是`os`模块的补充.

### 过滤文件

利用Python的特性来过滤文件.
比如我们要列出当前目录下的所有 **目录**,只需要一行代码:

```python
>>> [x for x in os.listdir('.') if os.path.isdir(x)]
['.lein', '.local', '.m2', '.npm', '.ssh', '.Trash', '.vim', 'Applications', 'Desktop', ...]
```

要列出所有的`.py`文件,也只需一行代码:

```python
>>> [x for x in os.listdir('.') if os.path.isfile(x) and os.path.splitext(x)[1]=='.py']
['apis.py', 'config.py', 'models.py', 'pymonitor.py', 'test_db.py', 'urls.py', 'wsgiapp.py']
```

判断后缀也可以使用 `PurePath.suffix` 接口

## shutil模块

[shutil — High-level file operations](https://docs.python.org/3/library/shutil.html)

安装 `pip install shutil`

### 复制文件

1. `shutil.copy('来源文件','目标地址')` : 返回值是复制之后的路径
2. `shutil.copy2()` ,`shutil.copy()` :差不多,复制后的结果保留了原来的所有信息(包括状态信息)
3. `shutil.copyfile(来源文件,目标文件)`  : 将一个文件的内容拷贝的另外一个文件当中,返回值是复制之后的路径
4. `shutil.copyfileobj(open(来源文件,'r'),open('目标文件','w'))` :将一个文件的内容拷贝的另外一个文件当中
5. `shutil.copytree(来源目录,目标目录)` : 复制整个文件目录 (无论文件夹是否为空,均可复制,而且会复制文件夹中的所有内容)
6. `copymode()`,`copystat()` 不常用

### 删除文件

1. `shutil.rmtree(目录路径)` #移除整个目录,无论是否空(删除的是文件夹,如果删除文件`os.unlink(path)`)
2. `shutil.move(来源地址,目标地址)`#移动文件

### examples

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

[python操作文件,强大的shutil模块](https://www.jianshu.com/p/e62b05f08179)

## 序列化

在程序运行的过程中,所有的变量都是在内存中,比如,定义一个`dict`:`d = dict(name='Bob', age=20, score=88)`

我们把变量从内存中变成可存储或传输的过程称之为序列化,在Python中叫`pickling`,
在其他语言中也被称之为`serialization`,`marshalling`,`flattening`等等,都是一个意思.

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

当我们要把对象从磁盘读到内存时,可以先把内容读到一个`bytes`,
然后用`pickle.loads()`方法反序列化出对象,
也可以直接用`pickle.load()`方法从一个`file-like Object`中直接反序列化出对象.

我们打开另一个Python命令行来反序列化刚才保存的对象:

```python
>>> f = open('dump.txt', 'rb')
>>> d = pickle.load(f)
>>> f.close()
>>> d
{'age': 20, 'score': 88, 'name': 'Bob'}
```

变量的内容又回来了!

Pickle的问题和所有其他编程语言特有的序列化问题一样,就是它只能用于Python,
并且可能不同版本的Python彼此都不兼容,因此,只能用Pickle保存那些不重要的数据,不能成功地反序列化也没关系.

## 文本比较

[filecmp --- 文件及目录的比较](https://docs.python.org/zh-cn/3/library/filecmp.html#module-filecmp)
