# Python运行外部程序

[Python运行外部程序的几种方法](https://blog.csdn.net/xiligey1/article/details/80267983)

## subprocess --- 子进程管理

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

如果 `encoding` 或 `errors` 被指定,
或者 `text` (也名为 `universal_newlines` )为`True`,
则文件 objects `stdin` ,  `stdout` 与 `stderr` 将会使用在此次调用中指定的 `encoding` 和 `errors` 以文本模式打开.
未制定则使用默认的 `io.TextIOWrapper`.

对于 `stdin` ,  输入的换行符`\n` 将被转换为默认的换行符 `os.linesep`.
对于 `stdout` 和 `stderr` ,  所有输出的换行符都被转换为`\n`.
更多的信息可以参考`io.TextIOWrapper`的文档, 当它的构造函数中的`newline`参数被设置为`None`时.

如果未使用文本模式,  `stdin` ,  `stdout` 和 `stderr` 将会以二进制流模式打开.
则不会发生编码和换行符(`line ending`)的转换.

注意:
file objects `Popen.stdin`,  `Popen.stdout` 和 `Popen.stderr`
的换行符属性不会被 `Popen.communicate()` 方法更新.

如果 `shell` 设为 `True`,, 则使用指定的`shell` 执行指定的指令.
这样可以方便的使用一些`shell`的特性, 比如 shell 管道,
文件名`wildcards`, 环境变量展开以及 `~` (展开到用户家目录).
注意 Python 自己也实现了许多类似 shell 的特性
(例如 `glob`, `fnmatch`, `os.walk()`, `os.path.expandvars()`,
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

`subprocess.run` 返回的结果可能包含`\n`后缀,
这个时候可以使用字符串的`.removesuffix('\n')`方法去掉换行符.

## popen

[Popen 构造函数](https://docs.python.org/zh-cn/3/library/subprocess.html#subprocess.Popen)

`subprocess`模块中, 在底层上, 进程的创建与管理由 `Popen` 类处理.
它提供了很大的灵活性, 开发者能够处理没有被便利函数覆盖的情况.

```python
class subprocess.Popen(args, bufsize=-1, executable=None, stdin=None, stdout=None, stderr=None, preexec_fn=None, close_fds=True, shell=False, cwd=None, env=None, universal_newlines=None, startupinfo=None, creationflags=0, restore_signals=True, start_new_session=False, pass_fds=(), *, group=None, extra_groups=None, user=None, umask=-1, encoding=None, errors=None, text=None)
```

在一个新的进程中执行子程序.
在 `POSIX`平台, 此`class`使用类似于 `os.execvp()` 的行为来执行子程序.
在 `Windows`平台, 此`class`使用了 `Windows CreateProcess()` 函数.

### subprocess.check_call

[subprocess.check_call](https://docs.python.org/3/library/subprocess.html#subprocess.check_call)

需要捕获 `stdout` or `stderr`, 则应该使用 `run()` 代替 `check_call`:

```python
subprocess.run(..., check=True)
```

如果要抑制 `stdout` or `stderr`, 使用值 `DEVNULL`.

## os.system运行外部程序

使用 `os.system` 函数运行其他程序或脚本

```python
import os
os.system('notepad python.txt')
```

### 使用`ShellExecute`函数运行其他程序

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

### 使用`CreateProcess`函数

```python
import win32process
from win32process import CreateProcess
CreateProcess('c:\\windows\\notepad.exe', '', None, None, 0,
        win32process.CREATE_NO_WINDOW, None, None,
        win32process.STARTUPINFO())
```

### 用 `subprocess.call()`

[python调用外部程序](https://blog.csdn.net/u011722133/article/details/80430439)

```python
status = subprocess.call("mycmd myarg", shell=True)
```

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

## subprocess 直接输出到命令行

[Python subprocess output to stdout](https://stackoverflow.com/questions/6062340/python-subprocess-output-to-stdout)
[subprocess — Subprocess management](https://docs.python.org/3/library/subprocess.html#popen-constructor)

Simply don't send the output to a pipe:

```python
proc = subprocess.Popen (command_args, shell=False)
proc.communicate()
```

如下, Popen 构造函数中, stdin, stdout, stderr 的默认值均为 `None`,
默认情况下, 创建的 `子进程` 将继承当前进程的 `stdout`, 这意味着所有内容都将直接打印到终端上.
这也是代码不将结果存储在变量中的原因, 由于我们没有将输出发送到 `管道`(`pipe`).

```python
class subprocess.Popen(args, bufsize=- 1, executable=None, stdin=None, stdout=None, stderr=None,...)
```

所谓 `管道`, 即在创建 `Popen` 对象时,
可以指定创建 `stdin`, `stdout` 和 `stderr` 三个文件句柄, 可以像文件那样进行读写操作.
这里的 `管道`, 是 `subprocess` 模块代码内部创建的文件句柄,
而不是指 父进程 `终端` 里面的 `管道操作符`

```python
import subprocess

s = subprocess.Popen("python", stdout=subprocess.PIPE, stdin=subprocess.PIPE, shell=True)
s.stdin.write(b"import os\n")
s.stdin.write(b"print(os.environ)")
s.stdin.close()

out = s.stdout.read().decode("utf-8")
s.stdout.close()
print(out)
```
