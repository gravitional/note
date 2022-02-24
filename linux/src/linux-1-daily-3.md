# Linux目录含义

[Linux各目录含义](https://www.jianshu.com/p/142deb98ed5a)

+ FHS标准; `linux`系统的目录都遵循一个标准, 即由`Linux`基金会发布的 文件系统层次结构标准 (`Filesystem Hierarchy Standard`, FHS).
这个标准里面定义了linux系统该有哪些目录,各个目录应该存放什么,起什么作用等等:

目录  含义

+ `/bin`  `binary`,即用来存放二进制可执行文件,并且比较特殊的是`/bin`里存放的是所有一般用户都能使用的可执行文件,如:`cat`, `chmod`, `chown`, `mv`, `mkdir`, `cd` 等常用指令
+ `/boot`  存放开机时用到的引导文件
+ `/dev`  device(并不是`develop`哦),任何设备都以文件的形式存放在这个目录中
+ `/etc`  `Editable Text Configuration`(早期含义为`etcetera`,但是有争议),存放系统配置文件,如各种服务的启动配置,账号密码等
+ `/home`  用户的主目录,每当新建一个用户系统都会在这个目录下创建以该用户名为名称的目录作为该用户的主目录.并且在命令行中~代表当前用户的主目录,~yousiku表示yousiku这个用户的主目录
+ `/lib`  library,存放着系统开机时所需的函数库以及/bin和/sbin目录下的命令会调用的函数库
+ `/lib64`  存放相对于/lib中支持64位格式的函数库
+ `/media`  可移除的媒体设备,如光盘,DVD等
+ `/mnt`  `mount`,临时挂载的设备文件
+ `/opt`  `optional`,可选的软件包,即第三方软件.我们可以将除了系统自带软件之外的其他软件安装到这个目录下
+ `/proc`  `process`,该目录是一个虚拟文件系统,即该目录的内容存放于内存中而不是硬盘中,存放着系统内核以及进程的运行状态信息
+ `/root`  超级管理员root的主目录
+ `/run`  最近一次开机后所产生的各项信息,如当前的用户和正在运行中的守护进程等
+ `/sbin`  存放一些只有root账户才有权限执行的可执行文件,如init, ip, mount等命令
+ `/srv`  service,存放一些服务启动后所需的数据
+ `/sys`  system,与/proc类似也是一个虚拟文件系统,存放系统核心与硬件相关的信息
+ `/tmp`  temporary,存放临时文件,可以被所有用户访问,系统重启时会清空该目录
+ `/usr`  Unix Software Resource(并不是指user哦),存放着所有用户的绝大多数工具和应用程序(下文详细介绍)
+ `/var`  variable,存放动态文件,如系统日志,程序缓存等(下文详细介绍)

+ `/usr`目录; `Unix Software Resource` 意为 `Unix`系统软件资源.
系统自带的软件都装在这个目录下(好比Windows系统的`C:\Windows`),用户安装的第三方软件也在这个目录下(好比Windows系统的`C:\Program Files`).
不同的是, 在Windows系统上安装软件通常将该软件的所有文件放置在同一个目录下,但在Linux系统, 安装软件会将该软件的不同文件分别放置在`/usr`目录下的不同子目录下.
而不应该自行创建该软件自己的独立目录. `/usr`目录一般有以下子目录:

目录  含义

+ `/usr/bin`  即`/bin`,用链接文件到方式将`/bin`链接至此
+ `/usr/etc`  应用程序的配置文件
+ `/usr/games`  与游戏相关的数据
+ `/usr/include`  `c/c++`程序的头文件
+ `/usr/lib`  即`/lib`,用链接文件到方式将`/lib`链接至此
+ `/usr/lib64`  即`/lib64`,用链接文件到方式将`/lib64`链接至此
+ `/usr/libexec`  不常用的执行文件或脚本
+ `/usr/local`  应用程序的安装目录,每个应用程序目录下还会有对应的`bin`, `etc`, `lib`等目录
+ `/usr/sbin`  即`/sbin`,用链接文件到方式将`/sbin`链接至此
+ `/usr/share`  共享文件,通常是一些文字说明文件,如软件文档等
+ `/usr/src`  `source`,应用程序源代码
+ `/usr/tmp`  应用程序临时文件

## 输入法

添加删除输入法在系统设置目录, 直接搜索`settings-Region&Language--input sources`, 添加输入法是按照语言进行的, 先选择语言, 然后可以选择具体的输入法.
如`Intelligent Pinyin`

+ 切换输入法可以使用如下命令: `im-config -s fcitx`,
`-s`:    无动作;  对可能发生的事件进行模拟,但实际上不更改配置文件.

+ 如果要查看当前可用的输入法可以使用 `im-config -l` , 更多查看 `man im-config`.
+ `ibus-setup`: 图形界面程序, 用于设置`ibus`输入法框架

+ 查看环境变量`$XDG_CURRENT_DESKTOP`的值来看自己处于哪个图形环境.
+ 重启输入法; `ibus restart`:
+ 重启 ibus 守护进程;  `ibus-daemon -drx`
    + `-d --daemonize`:作为后台程序运行
    + `-r, --replace`: 如果有旧的`ibus-daemon`在运行, 就替换它.
    + `-x, --xim`: 运行`XIM`服务器

+ gedit设置默认编码UTF-8;
[gedit默认编码设置](https://blog.csdn.net/miscclp/article/details/39154639).
在终端下输入:

```bash
gsettings set org.gnome.gedit.preferences.encodings candidate-encodings "['UTF-8', 'GB18030', 'GB2312', 'GBK', 'BIG5', 'CURRENT', 'UTF-16']"
```

+ 导入ibus词库
[iBus拼音输入法导入搜狗词库](https://blog.csdn.net/betabin/article/details/7798668)

终端下输入`ibus-setup`--`Input Method`--`Chinese - intelligent pinyin`,
点击右侧的 `preference`--`user data`--`import`, 把制作好的词库导入进去即可. 测试:

    亥姆霍兹方程
    重整化群

### 定制自己的libpinyin

[ibus下定制自己的libpinyin](https://blog.csdn.net/godbreak/article/details/9031887)

智能拼音输入法从`ibus-pinyin`更名为`ibus-libpinyin`

`libpinyin`添加了词库导入功能,并刚刚修复相关`bug`,所以要先更新`libpinyin`到最新版.
在`libpinyin`的配置界面(可以从`语言选项`---`输入源`找到,实在找不到,`/usr/share/ibus-libpinyin/setup/main2.py`),可以找到**用户数据导入选项**.

这个要求文件:

1. 文件采用本地编码格式
2. 格式为每行`字符 拼音 位置(可选)`,且字符数和拼音数要对应,例如`你好 ni'hao 5`.

去搜狗词库下搜狗细胞词库文件,然后下个**深蓝词库转换器**(`exe`),`wine`中打开转换器,选择从搜狗细胞词库转换到手机`QQ`格式,转换结束后不要选择文件保存本地,编码格式不大对,在输出框里面全选复制粘贴到你的文本编辑器,保存为`.txt`后缀.
然后在`libpinyin`配置界面导入即可.导入完成后,`kill ibus-engine-libpinyin`进程,再切回拼音输入法.
