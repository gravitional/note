# windows dll 路径设置

[windows下dll动态链接库路径的设置](https://zhuanlan.zhihu.com/p/573450255)

在使用VS编译的项目中, 经常报如下错误:

![error](https://pic1.zhimg.com/v2-2a3ffce554b4d33b215f29bce65f16a4_r.jpg)

简单的做法是找到对应的dll文件放到可执行程序所在的目录下,
但如果有多个工程需要这个几个文件的话, 一一复制比较麻烦.

此时, 可以将文件复制到系统目录 `c:/windows/system32` 下即可.

若不想改变系统文件夹内容, 可以自己创建一个文件, 如`c:/DLLImport`,
然后将该目录加入环境变量, 重启电脑即可.

以win11为例: 右击此电脑 -> 属性 -> 高级系统设置 -> 环境变量 -> Path -> 编辑 -> 新建

![user path](https://pic2.zhimg.com/80/v2-11a33ce6e6d4d3957149c6d76f301cb1_720w.webp)
