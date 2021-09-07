# python-7.md

ref: [这是小白的Python新手教程](https://www.liaoxuefeng.com/wiki/1016959663602400)

## 图形界面

Python支持多种图形界面的第三方库, 包括：

+ `Tk`
+ `wxWidgets`
+ `Qt`
+ `GTK`

等等. 

但是Python自带的库是支持`Tk`的`Tkinter`, 使用`Tkinter`, 无需安装任何包, 就可以直接使用. 本章简单介绍如何使用Tkinter进行GUI编程. 

### Tkinter

我们来梳理一下概念：

我们编写的Python代码会调用内置的Tkinter, Tkinter封装了访问Tk的接口；
Tk是一个图形库, 支持多个操作系统, 使用`Tcl`语言开发；
Tk会调用操作系统提供的本地GUI接口, 完成最终的GUI. 

所以, 我们的代码只需要调用Tkinter提供的接口就可以了. 

### 第一个GUI程序

使用Tkinter十分简单, 我们来编写一个GUI版本的“Hello, world!”. 

第一步是导入Tkinter包的所有内容：

```python
from tkinter import *
```

第二步是从`Frame`派生一个`Application`类, 这是所有`Widget`的父容器：

```python
class Application(Frame):
    def __init__(self, master=None):
        Frame.__init__(self, master)
        self.pack()
        self.createWidgets()

    def createWidgets(self):
        self.helloLabel = Label(self, text='Hello, world!')
        self.helloLabel.pack()
        self.quitButton = Button(self, text='Quit', command=self.quit)
        self.quitButton.pack()
```

在GUI中, 每个Button, Label, 输入框等, 都是一个Widget. 
Frame则是可以容纳其他Widget的Widget, 所有的Widget组合起来就是一棵树. 

`pack()`方法把Widget加入到父容器中, 并实现布局. 
`pack()`是最简单的布局, `grid()`可以实现更复杂的布局. 

在`createWidgets()`方法中, 我们创建一个Label和一个Button, 当Button被点击时, 触发`self.quit()`使程序退出. 

第三步, 实例化Application, 并启动消息循环：

```python
app = Application()
# 设置窗口标题:
app.master.title('Hello World')
# 主消息循环:
app.mainloop()
```

GUI程序的主线程负责监听来自操作系统的消息, 并依次处理每一条消息. 因此, 如果消息处理非常耗时, 就需要在新线程中处理. 

运行这个GUI程序, 可以看到下面的窗口：

```python
tk-hello-world
```

点击“Quit”按钮或者窗口的“x”结束程序. 

#### 输入文本

我们再对这个GUI程序改进一下, 加入一个文本框, 让用户可以输入文本, 然后点按钮后, 弹出消息对话框. 

```python
from tkinter import *
import tkinter.messagebox as messagebox

class Application(Frame):
    def __init__(self, master=None):
        Frame.__init__(self, master)
        self.pack()
        self.createWidgets()

    def createWidgets(self):
        self.nameInput = Entry(self)
        self.nameInput.pack()
        self.alertButton = Button(self, text='Hello', command=self.hello)
        self.alertButton.pack()

    def hello(self):
        name = self.nameInput.get() or 'world'
        messagebox.showinfo('Message', 'Hello, %s' % name)

app = Application()
# 设置窗口标题:
app.master.title('Hello World')
# 主消息循环:
app.mainloop()
```

当用户点击按钮时, 触发`hello()`, 通过`self.nameInput.get()`获得用户输入的文本后, 使用`tkMessageBox.showinfo()`可以弹出消息对话框. 

#### 小结-gui 程序

Python内置的`Tkinter`可以满足基本的GUI程序的要求, 如果是非常复杂的GUI程序, 建议用操作系统原生支持的语言和库来编写. 

### 海龟绘图

在1966年, Seymour Papert和Wally Feurzig发明了一种专门给儿童学习编程的语言——`LOGO`语言, 它的特色就是通过编程指挥一个小海龟(turtle)在屏幕上绘图. 

海龟绘图(`Turtle Graphics`)后来被移植到各种高级语言中, Python内置了`turtle`库, 基本上100%复制了原始的`Turtle Graphics`的所有功能. 

我们来看一个指挥小海龟绘制一个长方形的简单代码：

```python
# 导入turtle包的所有内容:
from turtle import *

# 设置笔刷宽度:
width(4)

# 前进:
forward(200)
# 右转90度:
right(90)

# 笔刷颜色:
pencolor('red')
forward(100)
right(90)

pencolor('green')
forward(200)
right(90)

pencolor('blue')
forward(100)
right(90)

# 调用done()使得窗口等待被关闭, 否则将立刻关闭窗口:
done()
```

在命令行运行上述代码, 会自动弹出一个绘图窗口, 然后绘制出一个长方形：

从程序代码可以看出, 海龟绘图就是指挥海龟前进, 转向, 海龟移动的轨迹就是绘制的线条. 要绘制一个长方形, 只需要让海龟前进, 右转`90`度, 反复`4`次. 

调用`width()`函数可以设置笔刷宽度, 调用`pencolor()`函数可以设置颜色. 更多操作请参考[turtle库的说明]. 

[turtle库的说明]: https://docs.python.org/3.3/library/turtle.html#turtle-methods

绘图完成后, 记得调用`done()`函数, 让窗口进入消息循环, 等待被关闭. 否则, 由于Python进程会立刻结束, 将导致窗口被立刻关闭. 

`turtle`包本身只是一个绘图库, 但是配合Python代码, 就可以绘制各种复杂的图形. 例如, 通过循环绘制`5`个五角星：

```python
from turtle import *

def drawStar(x, y):
    pu()
    goto(x, y)
    pd()
    # set heading: 0
    seth(0)
    for i in range(5):
        fd(40)
        rt(144)

for x in range(0, 250, 50):
    drawStar(x, 0)

done()
```

使用递归, 可以绘制出非常复杂的图形. 例如, 下面的代码可以绘制一棵分型树：

```python
from turtle import *

# 设置色彩模式是RGB:
colormode(255)

lt(90)

lv = 14
l = 120
s = 45

width(lv)

# 初始化RGB颜色:
r = 0
g = 0
b = 0
pencolor(r, g, b)

penup()
bk(l)
pendown()
fd(l)

def draw_tree(l, level):
    global r, g, b
    # save the current pen width
    w = width()

    # narrow the pen width
    width(w * 3.0 / 4.0)
    # set color:
    r = r + 1
    g = g + 2
    b = b + 3
    pencolor(r % 200, g % 200, b % 200)

    l = 3.0 / 4.0 * l

    lt(s)
    fd(l)

    if level < lv:
        draw_tree(l, level + 1)
    bk(l)
    rt(2 * s)
    fd(l)

    if level < lv:
        draw_tree(l, level + 1)
    bk(l)
    lt(s)

    # restore the previous pen width
    width(w)

speed("fastest")

draw_tree(l, 4)

done()
```

执行上述程序需要花费一定的时间, 最后的效果如下：

## 网络编程

自从互联网诞生以来, 现在基本上所有的程序都是网络程序, 很少有单机版的程序了. 

计算机网络就是把各个计算机连接到一起, 让网络中的计算机可以互相通信. 网络编程就是如何在程序中实现两台计算机的通信. 

举个例子, 当你使用浏览器访问新浪网时, 你的计算机就和新浪的某台服务器通过互联网连接起来了, 然后, 新浪的服务器把网页内容作为数据通过互联网传输到你的电脑上. 

由于你的电脑上可能不止浏览器, 还有QQ, Skype, Dropbox, 邮件客户端等, 不同的程序连接的别的计算机也会不同, 所以, 更确切地说, 网络通信是两台计算机上的两个进程之间的通信. 比如, 浏览器进程和新浪服务器上的某个Web服务进程在通信, 而QQ进程是和腾讯的某个服务器上的某个进程在通信. 

网络通信就是两个进程在通信

网络编程对所有开发语言都是一样的, Python也不例外. 用Python进行网络编程, 就是在Python程序本身这个进程内, 连接别的服务器进程的通信端口进行通信. 

本章我们将详细介绍Python网络编程的概念和最主要的两种网络类型的编程. 

### TCP/IP简介

虽然大家现在对互联网很熟悉, 但是计算机网络的出现比互联网要早很多. 

计算机为了联网, 就必须规定通信协议, 早期的计算机网络, 都是由各厂商自己规定一套协议, IBM, Apple和Microsoft都有各自的网络协议, 互不兼容, 这就好比一群人有的说英语, 有的说中文, 有的说德语, 说同一种语言的人可以交流, 不同的语言之间就不行了. 

为了把全世界的所有不同类型的计算机都连接起来, 就必须规定一套全球通用的协议, 为了实现互联网这个目标, 互联网协议簇(Internet Protocol Suite)就是通用协议标准. Internet是由inter和net两个单词组合起来的, 原意就是连接“网络”的网络, 有了Internet, 任何私有网络, 只要支持这个协议, 就可以联入互联网. 

因为互联网协议包含了上百种协议标准, 但是最重要的两个协议是TCP和IP协议, 所以, 大家把互联网的协议简称TCP/IP协议. 

通信的时候, 双方必须知道对方的标识, 好比发邮件必须知道对方的邮件地址. 互联网上每个计算机的唯一标识就是IP地址, 类似`123.123.123.123`. 如果一台计算机同时接入到两个或更多的网络, 比如路由器, 它就会有两个或多个IP地址, 所以, IP地址对应的实际上是计算机的网络接口, 通常是网卡. 

IP协议负责把数据从一台计算机通过网络发送到另一台计算机. 数据被分割成一小块一小块, 然后通过IP包发送出去. 由于互联网链路复杂, 两台计算机之间经常有多条线路, 因此, 路由器就负责决定如何把一个IP包转发出去. IP包的特点是按块发送, 途径多个路由, 但不保证能到达, 也不保证顺序到达. 

IP地址实际上是一个32位整数(称为IPv4), 以字符串表示的IP地址如`192.168.0.1`实际上是把32位整数按8位分组后的数字表示, 目的是便于阅读. 

IPv6地址实际上是一个128位整数, 它是目前使用的IPv4的升级版, 以字符串表示类似于`2001:0db8:85a3:0042:1000:8a2e:0370:7334`. 

TCP协议则是建立在IP协议之上的. TCP协议负责在两台计算机之间建立可靠连接, 保证数据包按顺序到达. TCP协议会通过握手建立连接, 然后, 对每个IP包编号, 确保对方按顺序收到, 如果包丢掉了, 就自动重发. 

许多常用的更高级的协议都是建立在TCP协议基础上的, 比如用于浏览器的HTTP协议, 发送邮件的SMTP协议等. 

一个TCP报文除了包含要传输的数据外, 还包含源IP地址和目标IP地址, 源端口和目标端口. 

端口有什么作用？在两台计算机通信时, 只发IP地址是不够的, 因为同一台计算机上跑着多个网络程序. 一个TCP报文来了之后, 到底是交给浏览器还是QQ, 就需要端口号来区分. 每个网络程序都向操作系统申请唯一的端口号, 这样, 两个进程在两台计算机之间建立网络连接就需要各自的IP地址和各自的端口号. 

一个进程也可能同时与多个计算机建立链接, 因此它会申请很多端口. 

了解了TCP/IP协议的基本概念, IP地址和端口的概念, 我们就可以开始进行网络编程了. 

### TCP编程

`Socket`是网络编程的一个抽象概念. 通常我们用一个`Socket`表示“打开了一个网络链接”, 而打开一个`Socket`需要知道目标计算机的IP地址和端口号, 再指定协议类型即可. 

[Socket原理](https://www.jianshu.com/p/066d99da7cbd)

socket: 插座, 插孔

#### 什么是socket

在计算机通信领域, `socket` 被翻译为“套接字”, 它是计算机之间进行通信的一种约定或一种方式. 
通过 `socket` 这种约定, 一台计算机可以接收其他计算机的数据, 也可以向其他计算机发送数据

`socket` 起源于`Unix`, 而`Unix`/`Linux`基本哲学之一就是`一切皆文件`, 都可以用`打开open –> 读写write/read –> 关闭close`模式来操作. 
我的理解就是`Socket`就是该模式的一个实现：即`socket`是一种特殊的文件, 一些`socket`函数就是对其进行的操作(读/写IO, 打开, 关闭). 
`Socket()`函数返回一个整型的`Socket`描述符, 随后的连接建立, 数据传输等操作都是通过该`Socket`实现的. 

#### 网络中进程如何通信

既然`Socket`主要是用来解决网络通信的, 那么我们就来理解网络中进程是如何通信的. 

本地进程间通信

+ 消息传递(管道, 消息队列, FIFO)
+ 同步(互斥量, 条件变量, 读写锁, 文件和写记录锁, 信号量)？【不是很明白】
+ 共享内存(匿名的和具名的, eg:channel)
+ 远程过程调用(RPC)

网络中进程如何通信

我们要理解网络中进程如何通信, 得解决两个问题：

1. 我们要如何标识一台主机, 即怎样确定我们将要通信的进程是在那一台主机上运行. 
1. 我们要如何标识唯一进程, 本地通过pid标识, 网络中应该怎样标识？

解决办法：

1. TCP/IP协议族已经帮我们解决了这个问题, 网络层的`ip地址`可以唯一标识网络中的主机
2. 传输层的`协议+端口`可以唯一标识主机中的应用程序(进程), 因此, 我们利用三元组(ip地址, 协议, 端口)就可以标识网络的进程了, 网络中的进程通信就可以利用这个标志与其它进程进行交互

### 客户端

大多数连接都是可靠的`TCP`连接. 创建`TCP`连接时, 主动发起连接的叫客户端, 被动响应连接的叫服务器. 

举个例子, 当我们在浏览器中访问新浪时, 我们自己的计算机就是客户端, 浏览器会主动向新浪的服务器发起连接. 
如果一切顺利, 新浪的服务器接受了我们的连接, 一个`TCP`连接就建立起来的, 后面的通信就是发送网页内容了. 

所以, 我们要创建一个基于`TCP`连接的`Socket`, 可以这样做：

```python
# 导入socket库:
import socket

# 创建一个socket:
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
# 建立连接:
s.connect(('www.sina.com.cn', 80))
```

创建`Socket`时, `AF_INET`指定使用`IPv4`协议, 如果要用更先进的`IPv6`, 就指定为`AF_INET6`. `SOCK_STREAM`指定使用面向流的`TCP`协议, 这样, 一个`Socket`对象就创建成功, 但是还没有建立连接. 

客户端要主动发起`TCP`连接, 必须知道服务器的`IP`地址和端口号. 新浪网站的`IP`地址可以用域名`www.sina.com.cn`自动转换到IP地址, 但是怎么知道新浪服务器的端口号呢？

答案是作为服务器, 提供什么样的服务, 端口号就必须固定下来. 
由于我们想要访问网页, 因此新浪提供网页服务的服务器必须把端口号固定在`80`端口, 因为`80`端口是`Web`服务的标准端口. 
其他服务都有对应的标准端口号, 例如`SMTP`服务是`25`端口, `FTP`服务是`21`端口, 等等. 
端口号小于`1024`的是`Internet`标准服务的端口, 端口号大于`1024`的, 可以任意使用. 

因此, 我们连接新浪服务器的代码如下：

```python
s.connect(('www.sina.com.cn', 80))
```

注意参数是一个`tuple`, 包含地址和端口号. 

建立`TCP`连接后, 我们就可以向新浪服务器发送请求, 要求返回首页的内容：

```python
# 发送数据:
s.send(b'GET / HTTP/1.1\r\nHost: www.sina.com.cn\r\nConnection: close\r\n\r\n')
```

`TCP`连接创建的是双向通道, 双方都可以同时给对方发数据. 但是谁先发谁后发, 怎么协调, 要根据具体的协议来决定. 
例如, `HTTP`协议规定客户端必须先发请求给服务器, 服务器收到后才发数据给客户端. 

发送的文本格式必须符合`HTTP`标准, 如果格式没问题, 接下来就可以接收新浪服务器返回的数据了：

```python
# 接收数据:
buffer = []
while True:
    # 每次最多接收1k字节:
    d = s.recv(1024)
    if d:
        buffer.append(d)
    else:
        break
data = b''.join(buffer)
```

接收数据时, 调用`recv(max)`方法, 一次最多接收指定的字节数, 因此, 在一个`while`循环中反复接收, 直到`recv()`返回空数据, 表示接收完毕, 退出循环. 

当我们接收完数据后, 调用`close()`方法关闭`Socket`, 这样, 一次完整的网络通信就结束了：

```python
# 关闭连接:
s.close()
```

接收到的数据包括`HTTP`头和网页本身, 我们只需要把`HTTP`头和网页分离一下, 把`HTTP`头打印出来, 网页内容保存到文件：

```python
header, html = data.split(b'\r\n\r\n', 1)
print(header.decode('utf-8'))
# 把接收的数据写入文件:
with open('sina.html', 'wb') as f:
    f.write(html)
```

现在, 只需要在浏览器中打开这个`sina.html`文件, 就可以看到新浪的首页了. 

#### TCP-update

[新浪强制HTTPS协议访问 所以 80端口改443 socket 改 ssl][]

[新浪强制HTTPS协议访问 所以 80端口改443 socket 改 ssl]: https://www.liaoxuefeng.com/discuss/969955749132672/1287985884561441

```python
import socket
import ssl

# s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s = ssl.wrap_socket(socket.socket())
s.connect(('www.sina.com.cn', 443))
s.send(b'GET / HTTP/1.1\r\nHost: www.sina.com.cn\r\nConnection: close\r\n\r\n')

buffer = []
d = s.recv(1024)
while d:
    buffer.append(d)
    d = s.recv(1024)
data = b''.join(buffer)

s.close()

header, html = data.split(b'\r\n\r\n', 1)
print(header.decode('utf-8'))

with open('sina.html', 'wb') as f:
    f.write(html)
```

#### 服务器

和客户端编程相比, 服务器编程就要复杂一些. 

服务器进程首先要绑定一个端口并监听来自其他客户端的连接. 如果某个客户端连接过来了, 服务器就与该客户端建立`Socket`连接, 随后的通信就靠这个`Socket`连接了. 

所以, 服务器会打开固定端口(比如`80`)监听, 每来一个客户端连接, 就创建该`Socket`连接. 由于服务器会有大量来自客户端的连接, 所以, 服务器要能够区分一个`Socket`连接是和哪个客户端绑定的. 一个`Socket`依赖4项：服务器地址, 服务器端口, 客户端地址, 客户端端口来唯一确定一个`Socket`. 

但是服务器还需要同时响应多个客户端的请求, 所以, 每个连接都需要一个新的进程或者新的线程来处理, 否则, 服务器一次就只能服务一个客户端了. 

我们来编写一个简单的服务器程序, 它接收客户端连接, 把客户端发过来的字符串加上`Hello`再发回去. 

首先, 创建一个基于`IPv4`和`TCP`协议的`Socket`：

```python
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
```

然后, 我们要绑定监听的地址和端口. 服务器可能有多块网卡, 可以绑定到某一块网卡的IP地址上, 也可以用`0.0.0.0`绑定到所有的网络地址, 还可以用`127.0.0.1`绑定到本机地址. `127.0.0.1`是一个特殊的IP地址, 表示本机地址, 如果绑定到这个地址, 客户端必须同时在本机运行才能连接, 也就是说, 外部的计算机无法连接进来. 

端口号需要预先指定. 因为我们写的这个服务不是标准服务, 所以用`9999`这个端口号. 请注意, 小于`1024`的端口号必须要有管理员权限才能绑定：

```python
# 监听端口:
s.bind(('127.0.0.1', 9999))
```

紧接着, 调用`listen()`方法开始监听端口, 传入的参数指定等待连接的最大数量：

```python
s.listen(5)
print('Waiting for connection...')
```

接下来, 服务器程序通过一个永久循环来接受来自客户端的连接, `accept()`会等待并返回一个客户端的连接:

```python
while True:
    # 接受一个新连接:
    sock, addr = s.accept()
    # 创建新线程来处理TCP连接:
    t = threading.Thread(target=tcplink, args=(sock, addr))
    t.start()
```

每个连接都必须创建新线程(或进程)来处理, 
否则, 单线程在处理连接的过程中, 无法接受其他客户端的连接：

```python
def tcplink(sock, addr):
    print('Accept new connection from %s:%s...' % addr)
    sock.send(b'Welcome!')
    while True:
        data = sock.recv(1024)
        time.sleep(1)
        if not data or data.decode('utf-8') == 'exit':
            break
        sock.send(('Hello, %s!' % data.decode('utf-8')).encode('utf-8'))
    sock.close()
    print('Connection from %s:%s closed.' % addr)
```

连接建立后, 服务器首先发一条欢迎消息, 然后等待客户端数据, 并加上`Hello`再发送给客户端. 如果客户端发送了`exit`字符串, 就直接关闭连接. 

要测试这个服务器程序, 我们还需要编写一个客户端程序：

```python
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
# 建立连接:
s.connect(('127.0.0.1', 9999))
# 接收欢迎消息:
print(s.recv(1024).decode('utf-8'))
for data in [b'Michael', b'Tracy', b'Sarah']:
    # 发送数据:
    s.send(data)
    print(s.recv(1024).decode('utf-8'))
s.send(b'exit')
s.close()
```

我们需要打开两个命令行窗口, 一个运行服务器程序, 另一个运行客户端程序, 就可以看到效果了：

需要注意的是, 客户端程序运行完毕就退出了, 而服务器程序会永远运行下去, 必须按`Ctrl+C`退出程序. 

#### 小结-TCP

用`TCP`协议进行`Socket`编程在Python中十分简单, 对于客户端, 要主动连接服务器的`IP`和指定端口, 对于服务器, 要首先监听指定端口, 然后, 对每一个新的连接, 创建一个线程或进程来处理. 通常, 服务器程序会无限运行下去. 

同一个端口, 被一个`Socket`绑定了以后, 就不能被别的`Socket`绑定了. 

#### 程序-TCP

服务器

```python
# -*- coding: utf-8 -*-
#server.py
import socket,threading,time
#创建一个基于IPv4和TCP协议的Socket：
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

# 监听端口:
s.bind(('127.0.0.1', 9999))

s.listen(5)
print('Waiting for connection...')

#每个连接都必须创建新线程(或进程)来处理, 
#否则, 单线程在处理连接的过程中, 无法接受其他客户端的连接：
def tcplink(sock, addr):
    print('Accept new connection from %s:%s...' % addr)
    sock.send(b'Welcome!')
    while True:
        data = sock.recv(1024)
        time.sleep(1)
        if not data or data.decode('utf-8') == 'exit':
            break
        sock.send(('Hello, %s!' % data.decode('utf-8')).encode('utf-8'))
    sock.close()
    print('Connection from %s:%s closed.' % addr)

#服务器程序通过一个永久循环来接受来自客户端的连接, 
#accept()会等待并返回一个客户端的连接:

while True:
    # 接受一个新连接:
    sock, addr = s.accept()
    # 创建新线程来处理TCP连接:
    t = threading.Thread(target=tcplink, args=(sock, addr))
    t.start()
```

客户端

```python
# -*- coding: utf-8 -*-
#client.py
#要测试这个服务器程序, 我们还需要编写一个客户端程序：
# 导入socket库:
import socket
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
# 建立连接:
s.connect(('127.0.0.1', 9999))
# 接收欢迎消息:
print(s.recv(1024).decode('utf-8'))
for data in [b'Michael', b'Tracy', b'Sarah']:
    # 发送数据:
    s.send(data)
    print(s.recv(1024).decode('utf-8'))
s.send(b'exit')
s.close()
```

### UDP编程

TCP是建立可靠连接, 并且通信双方都可以以流的形式发送数据. 相对TCP, UDP则是面向无连接的协议. 

使用UDP协议时, 不需要建立连接, 只需要知道对方的IP地址和端口号, 就可以直接发数据包. 但是, 能不能到达就不知道了. 

虽然用UDP传输数据不可靠, 但它的优点是和TCP比, 速度快, 对于不要求可靠到达的数据, 就可以使用UDP协议. 

我们来看看如何通过UDP协议传输数据. 和TCP类似, 使用UDP的通信双方也分为客户端和服务器. 服务器首先需要绑定端口：

```python
s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
# 绑定端口:
s.bind(('127.0.0.1', 9999))
```

创建`Socket`时, `SOCK_DGRAM`指定了这个`Socket`的类型是`UDP`. 绑定端口和`TCP`一样, 但是不需要调用`listen()`方法, 而是直接接收来自任何客户端的数据：

```python
print('Bind UDP on 9999...')
while True:
    # 接收数据:
    data, addr = s.recvfrom(1024)
    print('Received from %s:%s.' % addr)
    s.sendto(b'Hello, %s!' % data, addr)
```

`recvfrom()`方法返回数据和客户端的地址与端口, 这样, 服务器收到数据后, 直接调用`sendto()`就可以把数据用UDP发给客户端. 

注意这里省掉了多线程, 因为这个例子很简单. 

客户端使用UDP时, 首先仍然创建基于UDP的`Socket`, 然后, 不需要调用`connect()`, 直接通过`sendto()`给服务器发数据：

```python
s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
for data in [b'Michael', b'Tracy', b'Sarah']:
    # 发送数据:
    s.sendto(data, ('127.0.0.1', 9999))
    # 接收数据:
    print(s.recv(1024).decode('utf-8'))
s.close()
```

从服务器接收数据仍然调用`recv()`方法. 

仍然用两个命令行分别启动服务器和客户端测试, 结果如下：

#### code-udp

server

```python
#-*-coding:utf-8-*-
import socket
s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
# 绑定端口:
s.bind(('127.0.0.1', 9999))
print('Bind UDP on 9999...')
while True:
    # 接收数据:
    data, addr = s.recvfrom(1024)
    print('Received from %s:%s.' % addr)
    s.sendto(b'Hello, %s!' % data, addr)
```

client

```python
#-*-coding:utf-8-*-
import socket
s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
for data in [b'Michael', b'Tracy', b'Sarah']:
    # 发送数据:
    s.sendto(data, ('127.0.0.1', 9999))
    # 接收数据:
    print(s.recv(1024).decode('utf-8'))
s.close()
```

#### 小结-udp

`UDP`的使用与`TCP`类似, 但是不需要建立连接. 
此外, 服务器绑定`UDP`端口和`TCP`端口互不冲突, 
也就是说, `UDP`的`9999`端口与`TCP`的`9999`端口可以各自绑定. 
