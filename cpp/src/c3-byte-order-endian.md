# 字节序

[Endian](https://www.jianshu.com/p/a348f8fc9fc9)
[Big Endian 和 Little Endian 详解](https://blog.csdn.net/waitingbb123/article/details/80504093)

## 字节序

字节序(Endian)即字节顺序(Byte-Order), 又称为端序或尾序(Endianness).
描述的是计算机如何组织字节组成对应的数字, 是多字节数据存储和传输时字节的顺序,
是多字节数据在计算机内存(存储器)中存储或网络传输时(数字通信链路)各字节的存储排列顺序.

单字节的数据, 比如C或Java中的char类型的数据, 是没有字节序这一说法的, 因为获取它只需要读取一个字节.
而多字节数据, 由于存在多个字节, 所在在存储和传输时可以使用不同的顺序进行操作.
简单来说, 字节序是指超过一个字节的数据类型在内存中存储的顺序.

字节数据的字节序跟 `多字节类型` 的数据有关,
比如 `int`, `short`, `long`, 对单字节 `byte`, `char` 没有影响.

在几乎所有的硬件机器上, 多字节对象都被存储为连续的 `字节序列`.
字节序分为两种排列模式, 分别是 `大端字节序` 和 `小端字节序`.

+ 大端: 高位字节数据存放在内存低地址处, `低位字节`(字节左端) 数据存放在 内存高地址处(内存右端).
+ 小端: 高位字节数据存放在内存高地址处, 低位字节(字节左端) 数据存放在 内存低地址处(内存左端).

`大端BE` 是指低地址存放最高有效字节(MSB),`小端LE` 则是低地址存放最低有效字节(LSB).
`Big-Endian` 相当于 `逆反次序`, `Little-Endian` 相当于 `一致次序`.

`Big-Endian` 符合人类通常的读写顺序, 因为左边是`高位`.

![大端小端](https://upload-images.jianshu.io/upload_images/4933701-7d68dd0202001735.png?imageMogr2/auto-orient/strip|imageView2/2/w/562/format/webp)

举个例子:

如果我们将 `0x12 34 ab cd` 写入到以 `0x0000` 开始的内存中, 则结果为;

address  big-endian  little-endian

0x0000  0x12  0xcd
0x0001  0x34  0xab
0x0002  0xab  0x34
0x0003  0xcd  0x12

注: 每个内存地址存 `1个字节`, 2位16进制数是1个字节(`0xFF=11111111`);

约定数据流的 `字节` 从左到右书写, `Big-endian` 就是 `大地址` 对应 `结束`,
`结束` 对应 `编码的低位`.

C/C++语言编写的程序里数据存储顺序是跟 `编译平台所在的CPU` 相关的,
而JAVA编写的程序则唯一采用 `big endian` 方式来存储数据.

## 大端小端

为什么需要注意字节序的问题呢?

如果编写的程序只是在单机环境下运行, 并不和其它程序打交道, 那么完全可以忽略字节序的存在.
如果你的程序要跟别的程序产生交互, 比如C/C++语言编译的程序中,
数据存储顺序是跟编译平台所在的CPU相关的, Java编写的程序则采用大端BE的方式来存储数据.
如果使用 `C/C++` 在x86平台下编写的程序与Java程序互通时会产生什么结果呢?

比如将C程序中指向 `0X12345678` 的指针传递给Java程序, 由于Java程序采取大端方式存储数据,
很自然地会将传递的数据翻译为 `0x78563412`.
因此在将C程序传递给Java程序钱必须进行字节序的转换.

## 内存高低地址

C程序映射中内存的空间布局

最高内存地址 `0x FFFF FFFF`

栈区: 从 `高内存地址` 往 `低内存地址` 发展, 即 `栈底在高地址`, 栈顶在低地址.
堆区: 从低内存地址往高内存地址发展

全局区: 常量和全局变量
代码区
最低内存地址: `0x 0000 0000`

![内存空间分布](https://upload-images.jianshu.io/upload_images/4933701-175a0777cb2627da.png?imageMogr2/auto-orient/strip|imageView2/2/w/508/format/webp)

## UTF8 BOM

[UTF-8和BOM的一些说明](https://www.cnblogs.com/codingmengmeng/p/11028744.html)

### BOM的含义

`BOM` 即 `Byte Order Mark` 字节序标记.
`BOM` 是为 `UTF-16` 和 `UTF-32` 准备的, 用户标记字节序(byte order).
拿 `UTF-16` 来举例, 其是以 `两个字节` 为编码单元,
在解释一个UTF-16文本前, 首先要弄清楚每个编码单元的字节序.
例如收到一个 `奎` 的 `Unicode` 编码是 `594E`, `乙`的 Unicode编码 是 `4E59`.
如果我们收到UTF-16字节流 `594E`, 那么这是 `奎` 还是 `乙`?

Unicode规范中推荐的标记字节顺序的方法是 `BOM`:
在UCS编码中有一个叫做 `ZERO WIDTH NO-BREAK SPACE`(零宽度无间断空间)的字符, 它的编码是 `FEFF`.
而 `FEFF` 在UCS中是不能在的字符(即不可见), 所以不应该出现在实际传输中.
UCS规范建议我们在传输字节流前, 先传输字符 `ZERO WIDTH NO-BREAK SPACE`.
这样如果接收者接收到 `FEFF`, 就表明这个字节流是 `Big-Endian` 的;
如果收到 `FFFE`, 就表明这个字节流是 `Little-Endian` 的.
因此字符 `ZERO WIDTH NO-BREAK SPACE` 又被称为BOM.

`FEFF`; `Big-Endian`
`FFFE`; `Little-Endian`

`UTF-8` 是以 `字节` 为编码单元, 没有字节序的问题.

延伸一下:

+ `UTF-8`编码是以1个字节为单位进行处理的, 不会受CPU大小端的影响;需要考虑下一位时就 `地址+1`.
+ UTF-16, UTF-32是以2个字节和4个字节为单位进行处理的, 即1次读取2个字节或4个字节,
这样一来, 在存储和网络传输时就要考虑1个单位内2个字节或4个字节之间顺序的问题.

### UTF-8 BOM

UTF-8 BOM又叫UTF-8 签名, UTF-8不需要BOM来表明字节顺序, 但可以用BOM来表明编码方式.
当文本程序读取到以 `EF BB BF` 开头的字节流时, 就知道这是 `UTF-8` 编码了.
Windows就是使用BOM来标记文本文件的编码方式的.

补充:

`ZERO WIDTH NO-BREAK SPACE` 字符的UCS编码为FEFF(假设为大端),
对应的 UTF-8 编码为 `EF BB BF`

即以EF BB BF开头的字节流可表明这是段UTF-8编码的字节流.
但如果文件本身就是UTF-8编码的, `EF BB BF`这三个字节就毫无用处了.
所以, 可以说BOM的存在对于UTF-8本身没有任何作用.

### UTF-8文件中包含BOM的坏处

#### 对php的影响

php在设计时就没有考虑BOM的问题,
也就是说他不会忽略UTF-8编码的文件开头的那三个`EF BB BF`字符,
直接当做文本进行解析, 导致解析错误.

#### 在linux上执行SQL脚本报错

最近开发过程中遇到, windows下编写的SQL文件, 在linux下执行时, 总是报错.

在文件的开头, 无论是使用中文注释还是英文注释, 甚至去掉注释, 也会报错:

    SP2-0734: unknown command beginning "?<span "="">dec<span "="">lare ..." - rest of line ignored.

如下是文件开头部分

```sql
--create tablespace
declare
v_tbs_name varchar2(200):='hytpdtsmsshistorydb';
begin
```

报错如下:

```bash
SP2-0734: unknown command beginning "?--create ..." - rest of line ignored.
PL/SQL procedure successfully completed.
```

网上没有找到类似问题的解决办法, 且文件编码确认已经更改为utf-8, 该问题困惑了我很久.
最后查看一下BOM与 no BOM的区别, 尝试更改为no BOM, 就没有再出现错误.
修改完成后, 无论使用中文, 还是英文, 或者去掉注释, 都能正常执行.

### 血泪建议: UTF-8最好不要带BOM

UTF-8和 `带BOM的UTF-8` 的区别就是有没有 BOM. 即文件开头有没有 `U+FEFF`.

#### Linux中查看BOM的方法

使用less命令, 其它命令可能看不到效果:

```bash
less wordFile.txt
```

发现词语之前多了一个`<U+FEFF>`.

#### UTF-8去除BOM的方法

Linux下

+ vim打开文件
+ 执行:set nobomb
+ 保存:wq

或者使用

```bash
dos2unix filename
```

将windows格式文件转为Unix, Linux格式文件.
该命令不仅可将windows文件的换行符 `\r\n` 转为 `Unix`, Linux文件的换行符`\n`,
还可以将 `UTF-8 with BOM` 转换为 `UTF-8`.

ps:
遇到一个比较坑爹的情况,
1个UTF-8 Unicode (with BOM)文件中包含两个`<U+FEFF>`,
这是无论使用方法1 还是方法2, 都要执行两次才能将`<U+FEFF>`完全去除!!!

Windows下, 使用 [NotePad3](https://www.anopos.com/notepad3/) 打开这个文件,
然后选择 `文件 > 编码 > 设置文档为 > UTF-8`, 最后重新保存文件即可.
快捷键 `Shift+F8`, 或者按下 `F9` 选择其他编码
