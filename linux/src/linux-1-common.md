# linux common 未分类内容

## latex-bib文献管理: jabref

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

### loop 设备介绍

在类 UNIX 系统里,`loop` 设备是一种伪设备(pseudo-device),
或者也可以说是 **如真**设备, 它能使我们像块设备一样访问一个文件.

在使用之前,一个 `loop` 设备必须要和一个文件进行连接.
这种结合方式给用户提供了一个替代块特殊文件的接口.
因此,如果这个文件包含有一个完整的文件系统,那么这个文件就可以像一个磁盘设备一样被 `mount` 起来.

上面说的文件格式,我们经常见到的是 CD 或 DVD 的 ISO 光盘镜像文件或者是软盘(硬盘)的 `*.img` 镜像文件.
通过这种 `loop mount` (回环`mount`)的方式,这些镜像文件就可以被 `mount` 到当前文件系统的一个目录下.

至此,顺便可以再理解一下 `loop` 的含义:对于第一层文件系统, 它直接安装在我们计算机的物理设备之上;
而对于这种被 `mount` 起来的镜像文件(它也包含有文件系统), 它是建立在第一层文件系统之上,
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
