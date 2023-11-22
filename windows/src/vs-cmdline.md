# Visual Studio 命令行编译C/C++程序

[带你玩转Visual Studio——命令行编译C/C++程序](https://blog.csdn.net/luoweifu/article/details/49847749)
[Windows11下配置Visual Studio2022 环境变量](https://blog.csdn.net/en_Wency/article/details/124767742)

## 配置环境

安装好 Visual Studio 之后,
按下快捷键 `Win+s` 搜索 `环境变量`, 配置如下环境变量.
推荐添加到 用户变量 中.

### Path 变量

各种命令行工具的位置(默认安装位置下)

```powershell
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.32.31326\bin\Hostx64\x64
```

`C:\Program Files\Microsoft Visual Studio\2022` 视安装情况可能不同,
`14.32.31326` 也可能不一样, 但原理是相同的.

### INCLUDE 变量配置

新建 `INCLUDE` 环境变量, 填充以下 头文件(include)目录.
我电脑上的路径为:

```powershell
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.32.31326\include;
C:\Program Files (x86)\Windows Kits\10\Include\10.0.19041.0\cppwinrt;
C:\Program Files (x86)\Windows Kits\10\Include\10.0.19041.0\shared;
C:\Program Files (x86)\Windows Kits\10\Include\10.0.19041.0\ucrt;
C:\Program Files (x86)\Windows Kits\10\Include\10.0.19041.0\um;
C:\Program Files (x86)\Windows Kits\10\Include\10.0.19041.0\winrt
```

第一个目录在 VS 的安装目录下, 第二个目录 `Windows Kits` 与安装时的选择有关.
例如 `C:\` 安装时就是在 `C:\Program Files (x86)` 目录下.
Windows 环境变量的值是用分号 `;` 分隔的.
设置界面如下:

![img1](https://img-blog.csdnimg.cn/3ba0c1d8127346a5bb0932845f0fe078.png)
![img2](https://img-blog.csdnimg.cn/82a00734632949fda02eb91cd781580b.png)

### LIB 变量配置

类似地, 新建环境变量 `LIB`, 填充一下库文件(libraray)目录
我电脑上的路径为:

```powershell
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.32.31326\lib\x64;
C:\Program Files (x86)\Windows Kits\10\Lib\10.0.19041.0\ucrt\x64;
C:\Program Files (x86)\Windows Kits\10\Lib\10.0.19041.0\ucrt_enclave\x64;
C:\Program Files (x86)\Windows Kits\10\Lib\10.0.19041.0\um\x64
```

具体路径前缀 `C:\Program Files\Microsoft Visual Studio`, `C:\Program Files (x86)`
取决于你安装的位置.

## 测试

在桌面右键创建一个 `txt` 文本文件, 修改名称为 `test.c`(`.c`是后缀), 就得到一个C格式文件.
双击打开, 写入代码:

```c
#include <stdio.h>
#include <stdlib.h>
int main()
{
    printf("hello world!");
    return 0;
}
```

`win+r` 输入 `powershell` 打开命令行,
输入 `cd desktop` 定位到 `test.c` 所在的路径,输入

```powershell
cl /EHsc test.c # 编译源代码
./test # 运行生成的程序
```

会生成 `.obj` 文件和 `.exe` 可执行文件:
接着输入 `./test` 就会生成打印 `hello world!`

## 使用 pwsh 修改 VS 环境变量 INCLUDE, LIB

```bash
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.37.32822\include;C:\Program Files (x86)\Windows Kits\10\Include\10.0.22621.0\cppwinrt;C:\Program Files (x86)\Windows Kits\10\Include\10.0.22621.0\shared;C:\Program Files (x86)\Windows Kits\10\Include\10.0.22621.0\ucrt;C:\Program Files (x86)\Windows Kits\10\Include\10.0.22621.0\um;C:\Program Files (x86)\Windows Kits\10\Include\10.0.22621.0\winrt;C:\cppLibs\boost\boost;C:\cppLibs\pthreads\include\include;

C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.37.32822\lib\x64;C:\Program Files (x86)\Windows Kits\10\Lib\10.0.22621.0\ucrt\x64;C:\Program Files (x86)\Windows Kits\10\Lib\10.0.22621.0\ucrt_enclave\x64;C:\Program Files (x86)\Windows Kits\10\Lib\10.0.22621.0\um\x64;C:\cppLibs\boost\boost\libs;C:\cppLibs\pthreads\lib\x64;C:\cppLibs\pthreads\dll\x64;
```
