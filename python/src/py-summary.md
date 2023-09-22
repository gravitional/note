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

安装完 `python` 第三方库以后, 经常需要查询其文档, 其实`python`就自带文档查看器.
可以查看所有内置库和第三方库的文档, 虽然不是很详尽, 但是总比没有的好.
在命令行窗口输入

```python
python -m pydoc -p 60000
```

简单解释:

`python -m pydoc`表示打开`pydoc`模块, `pydoc`是查看`python`文档的首选工具;
`-p 6000`表示在`60000`端口上启动 `http server`. 然后在浏览器中访问`http://localhost:60000/`

## Python获取帮助

[Python获取帮助的3种方式](https://blog.csdn.net/DQ_DM/article/details/45672623)

### `help()`

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

## python 空语句

```python
pass
```

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

默认情况下, 函数的参数传递形式可以是 `位置参数` 或是 `显式的关键字参数`.
为了确保可读性和运行效率, 限制允许的参数传递形式是有意义的,
这样开发者只需查看函数定义, 即可确定参数项是 `仅按位置`,  按 `位置`也按 `关键字`, 还是 `仅按关键字` 传递.

+ 函数的定义看起来可以像是这样:

```python
def f(pos1, pos2,   #只能是位置参数 (Positional only)
/, pos_or_kwd,      #位置或关键字参数 (Positional or keyword)
*, kwd1, kwd2):     # 只能是关键字参数(Keyword)
```

+ 在这里 `/` 和 `*` 是可选的. 若使用这些符号, 则意在表明, 允许通过何种 `形参`, 将 `参数值` 传递给函数:
`仅限位置`, `位置`或`关键字`, 以及 `仅限关键字`.
`关键字形参` 也被称为 `命名形参`.

+ 如果函数定义中未使用 `/` 和 `*`, 则参数可以按`位置`或按`关键字`传递给函数

+ 如果是 `仅限位置` 的形参, 则其在参数序列中的 `位次` 是重要的, 并且该形参不能作为 `关键字` 传入.
`仅限位置` 形参要放在 `/` (正斜杠) 之前.
符号 `/` 被用来从逻辑上分隔 `仅限位置` 的形参 和 其它种类的形参.
如果函数定义中没有 `/`, 则表示没有 `仅限位置` 的形参.

+ 在 `/` 之后的形参可以为 `位置` , 或 `关键字`, 或 `仅限关键字`.

+ 要将形参标记为 `仅限关键字`, 即指明该 `形参` 必须以 `关键字参数` 的形式传入,
应在 `参数列表` 出现的首个 `仅限关键字` 形参前面放置一个星号(`*`).

+ 函数举例: 请考虑以下示例函数定义并特别注意 `/` 和 `*` 标记:

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

+ 第一个函数定义 `standard_arg` 是最常见的形式,
对调用方式没有任何限制, 参数可以按 `位置` 也可以按 `关键字` 传入.

```python
>>> standard_arg(2)
2
>>> standard_arg(arg=2)
2
```

+ 第二个函数 `pos_only_arg` 在函数定义中带有 `/`, 只能使用 `位置形参`:

```python
>>> pos_only_arg(1)
1
>>> pos_only_arg(arg=1)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: pos_only_arg() got an unexpected keyword argument 'arg'
```

+ 最后, 请考虑这个函数定义, 它的 位置参数 `name`  和 `**kwds` 之间可能产生潜在冲突,
由于 `kwds` 中, 某个关键字可能也叫`name` :

```python
def foo(name, **kwds):
    return 'name' in kwds
```

任何调用都不能让它返回 `True` , 因为关键字 `name` 将总是绑定到首个形参.
即被首个形参占用或遮蔽. 例如:

```python
>>> foo(1, **{'name': 2})
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: foo() got multiple values for argument 'name'
>>>
```

+ 通过使用 `/` -- 仅限 `位置参数`, 就可以避免这个问题,
因为它可以区分开作为位置参数的 `name` , 以及作为 关键字参数名称 的 `'name'` :

```python
def foo(name, /, **kwds):
    return 'name' in kwds
>>> foo(1, **{'name': 2})
True
```

换句话说, 限制 `形参` 只能根据 `位置` 接收实参, 可以防止它跟 `**kwds` 中重名参数混淆.
也就是最好写成 `(name, /, **kwds)` 这样的形式.

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

`['key']`也用来从字典中取出值.
`{'jack': 4098, 'sape': 4139, 'guido': 4127}['jack']`

### 小括号()

创建元组(固定不变的数组)

小括号不带逗号: 表示对括号内的单一表达式求值.
小括号可以用来把一个式子分成多行.

## python 邮件

[解放双手, 用Python实现自动发送邮件](https://zhuanlan.zhihu.com/p/89868804)

Python有两个内置库: `smtplib`和`email`,能够实现邮件功能,`smtplib`库负责发送邮件,`email`库负责构造邮件格式和内容.

邮件发送需要遵守`SMTP`协议,Python内置对`SMTP`的支持,可以发送纯文本邮件, `HTML`邮件以及带附件的邮件.

脚本如下:

[mail_attach.py](../my-scripts/my-email-attach.py)

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
