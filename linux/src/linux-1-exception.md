# linux 异常处理

## latex-bib文献管理:jabref

[JabRef中文手册](https://blog.csdn.net/zd0303/article/details/7676807)

### entry 时间戳

本功能可以在`选项->偏好设置->通用设置`中关闭或配置.
`JabRef`能自动的产生一个包含题录加入数据库的日期的域.

格式:

时间戳记的格式由包含指示词的字符串确定,指示词表示日期各部分的位置.
以下是一些可用的指示字母(示例在括号中给出,为:  2005年9月14日(星期三)下午5.45):

+ yy: year (05)
+ yyyy: year (2005)
+ MM: month (09)
+ dd: day in month (14)
+ HH: hour in day (17)
+ mm: minute in hour (45)

这些指示符可以与标点符号和空格一起使用. 几个例子:

+ `yyyy.MM.dd gives 2005.09.14`
+ `yy.MM.dd gives 05.09.14`
+ `yyyy.MM.dd HH:mm gives 2005.09.14 17:45`

## loop 设备

loop 设备 (循环设备)

[loop 设备 (循环设备)](https://blog.csdn.net/neiloid/article/details/8150629)

## loop 设备介绍

在类 UNIX 系统里,`loop` 设备是一种伪设备(pseudo-device),或者也可以说是仿真设备.它能使我们像块设备一样访问一个文件.

在使用之前,一个 `loop` 设备必须要和一个文件进行连接.
这种结合方式给用户提供了一个替代块特殊文件的接口.
因此,如果这个文件包含有一个完整的文件系统,那么这个文件就可以像一个磁盘设备一样被 `mount` 起来.

上面说的文件格式,我们经常见到的是 CD 或 DVD 的 ISO 光盘镜像文件或者是软盘(硬盘)的 `*.img` 镜像文件.
通过这种 `loop mount` (回环`mount`)的方式,这些镜像文件就可以被 `mount` 到当前文件系统的一个目录下.

至此,顺便可以再理解一下 `loop` 的含义:对于第一层文件系统,它直接安装在我们计算机的物理设备之上;
而对于这种被 `mount` 起来的镜像文件(它也包含有文件系统),它是建立在第一层文件系统之上,
这样看来,它就像是在第一层文件系统之上再绕了一圈的文件系统,所以称为 `loop`.

在 Linux 里,`loop` 设备的设备名形如:

```bash
ls /dev/loop*
/dev/loop0  /dev/loop2  /dev/loop4  /dev/loop6
/dev/loop1  /dev/loop3  /dev/loop5  /dev/loop7
... ...
```

例如,要在一个目录下 mount 一个包含有磁盘镜像的文件,需要分 2 步走:

```bash
losetup /dev/loop0 disk.img           #使磁盘镜像文件与循环设备连结起来
mount /dev/loop0 /home/groad/disk_test   #将循环设备 mount 到目录 disk_test 下
```

经过上面的两个命令后,镜像文件就如同一个文件系统挂载在 `disk_test` 目录下,当然我们也可以往镜像里面添加文件.

其实上面的两个步骤可以写成一个步骤:

```bash
mount -t minix -o loop ./disk.img ./disk_test
```

## snap

[Ubuntu使用snap安装常用软件](https://www.jianshu.com/p/4049b97151a1)

什么是`snap`, `snap`是一种全新的软件包管理方式, 它类似一个容器拥有一个应用程序所有的文件和库, 各个应用程序之间完全独立.
所以使用`snap`包的好处就是它解决了应用程序之间的依赖问题, 使应用程序之间更容易管理. 但是由此带来的问题就是它占用更多的磁盘空间.

`Snap`的安装包扩展名是`.snap`, 类似于一个容器, 它包含一个应用程序需要用到的所有文件和库(`snap`包包含一个私有的`root`文件系统, 里面包含了依赖的软件包).
它们会被安装到单独的目录; 各个应用程序之间相互隔离. 使用`snap`有很多好处, 首先它解决了软件包的依赖问题; 其次, 也使应用程序更容易管理.

现在支持`snap`的应用并不多, `snap`软件包一般安装在`/snap`目录下.

## 开机报错

[System program problem detected?](https://askubuntu.com/questions/1160113/system-program-problem-detected)

查看转储到您的磁盘上的崩溃报告. 目录是`/var/crash/`, 它将包含几个文件, 这些文件将您指向它所涉及的软件包以及崩溃的原因.
该目录描述为:

>`/var/crash`: 系统崩溃转储(可选)
>该目录包含系统故障转储.
>自本标准发布之日起, Linux不支持系统故障转储, 但其他可能符合FHS的系统也可能支持系统转储.

`Ubuntu`版本使用此(可选)目录来转储崩溃和执行崩溃的软件包, 称为`apport` (and `whoopsie`).
如果您想获得关于崩溃的真正详细的报告, 请安装`GDB`: `The GNU Project Debugger` with `sudo apt-get install gdb`.

+ 如何摆脱它

取决于您所说的`摆脱`. 理想的解决方法是检查报告中包含的内容, 然后尝试找到解决方法.
如果不需要包装或良性包装, 也可以将其清除. 多数情况下, 它是一项核心功能.

您可以选择以下任意一种来删除崩溃报告, 直到实际删除该软件包为止(如果错误来自于`apport`本身, 那将非常具有讽刺意味):

+ `sudo rm /var/crash/*`将删除旧的崩溃并停止通知您, 直到某些软件包再次崩溃为止.
+ 您可以通过`sudo systemctl disable apport`停止服务(并通过`sudo systemctl enable apport`再次启用它)
+ 如果不想看到崩溃报告, 可以通过`vim /etc/default/apport`将其禁用. 并将`enabled = 1`更改为` enabled = 0`. 反向编辑将再次启用它.
+ 您可以使用`sudo apt purge apport`(使用`sudo apt install apport`再次安装)
+还有一种桌面方法(`问题报告`选项):

[如何阅读和使用崩溃报告](https://askubuntu.com/questions/346953/how-to-read-and-use-crash-reports)有一些有趣的答案.
它有一个示例崩溃报告和一种跟踪崩溃的方法.
