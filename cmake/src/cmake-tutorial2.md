# cmake-tut2

[cmake 从入门到入土](https://blog.csdn.net/zhizhengguan/article/details/107034372)
[cmake: 设置编译选项](https://blog.csdn.net/zhizhengguan/article/details/111743586)
[CMake 生成漂亮的 VS 项目文件](https://zhuanlan.zhihu.com/p/441155027)

[Visual Studio中C++的包含目录, 附加包含目录和库目录和附加库目录的区别](https://blog.csdn.net/qq_27825451/article/details/103035258)

+ 包含目录, 附加包含目录, 库目录, 附加库目录

在配置opencv的时候必须要配置三样东西

+ 头文件包含路径: 即所谓的 `包含目录` 或者是 `附加包含目录`
+ 库文件路径: 即所谓的 `库目录`  或者是 `附加库目录`
+ 附加链接库: 显式指定需要的库文件

对于头文件和库文件的配置, 有两种方式可以实现,

Visual Studio C++ 工程中, 右键一个 Project 查看属性,
可以发现有两个地方设置Include的相关目录和lib的相关目录, 分别如下:

+ `VC++目录` -> `包含目录` 与 `库目录`

![imag](https://img-blog.csdnimg.cn/20191112185310493.png)

+ 附加包含目录与附加库目录

这两个不在一个选项卡中, 分别在不同的选项卡. 如下:

+ `C/C++`->`常规`->`附加包含目录`
+ `链接器`->`常规`->`附加库目录`

![imag](https://img-blog.csdnimg.cn/20191112185519360.png)
![imag2](https://img-blog.csdnimg.cn/20191112185529126.png)

### 它们到底有什么区别呢

其实他们最大的不同仅仅是在于编译器寻找头文件的顺序不同而已, 如下:

+ 包含目录 vs 附加包含目录

MSVC编译器寻找头文件的顺序
(这里与前面的那篇文章里面讲的Ming-w64中GCC和G++的搜索顺序是一样的):

+ 在 `源文件` 代码所在的文件夹中寻找 `头文件`(即包含文件);
+ 在使用 `MSVC` 编译的时候, 通过参数 `/I`(大写ai) 来指定的搜索目录;
`/I` 是由 `C/C++` -> `常规`-> `附加包含目录` 来设置的

+ 通过 `INCLUDE` 环境变量指定的搜索目录;
`INCLUDE`环境变量 是由 `VC++目录` -> `包含目录`  来设置的.

+ `库目录` vs `附加库目录`

同上面的道理,

MSVC编译器寻找库文件
(主要是指静态库, 因为在编译的时候只需要静态库, 动态库是在运行的时候才需要的)
的顺序:

+ `源文件` 代码所在的文件夹中寻找库文件(即静态库文件);
在使用MSVC编译的时候, 通过参数 `/link` 来指定的搜索目录;
`/link `是由 `链接器` -> `常规` -> `附加包含目录`  来设置的

+ 通过 `LIB` 环境变量指定的搜索目录;(相当于是 `库目录` 设置的)
`LIB` 环境变量 是由  `VC++目录` -> `包含目录`  来设置的.

所以在VS中开发C++程序, 我们可以通过上面的两种方式去选择配置.

### 有人推荐

就是说我们一般使用  `C/C++和链接器` 下面的设置, 而一般不建议使用 ` VC++目录`  下面的设置,
个人觉得这么做的原因是因为前者的搜索顺序在前面, 所以优先使用, 而且不需要环境变量,
但是对于VS来说, 我个人感觉差别不大, 即便不设置环境变量, 依然可以使用,
参见我的另一篇文章:
[VSCode开发C, C++环境搭建系列(三)——基于MSVC搭建](https://blog.csdn.net/qq_27825451/article/details/102981833)

## 附加链接库的指定

最后一步是`附加链接库` 的指定, 即显式指定在 `库目录` 中,
链接到哪一个静态库文件, 指定的方式都是一样的:
`属性`>`链接器`>`输入`>`附加依赖项`

![imag3](https://img-blog.csdnimg.cn/20191112191432711.png)
