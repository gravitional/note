# vscode cpp插件

[C/C++](https://marketplace.visualstudio.com/items?itemName=ms-vscode.cpptools)
[VS Code添加全局include path](https://blog.csdn.net/m0_46161993/article/details/110878187)

打开C/C++ EXTENSION的设置:

![img](https://img-blog.csdnimg.cn/2020120817415368.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L20wXzQ2MTYxOTkz,size_16,color_FFFFFF,t_70)

进入settings.json:

![settings](https://img-blog.csdnimg.cn/20201208174251663.png)

新版本json文件路径一般是

    .vscode/c_cpp_properties.json

添加如下:

```json
{
    "configurations": [
        {
            "name": "Linux",
            其他
        },
        {
            "name": "Win32",
            "includePath": [
                "${workspaceFolder}/**",
                "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.34.31933/include",
                "C:/Program Files (x86)/Windows Kits/10/Include/10.0.22000.0/cppwinrt",
                "C:/Program Files (x86)/Windows Kits/10/Include/10.0.22000.0/shared",
                "C:/Program Files (x86)/Windows Kits/10/Include/10.0.22000.0/ucrt",
                "C:/Program Files (x86)/Windows Kits/10/Include/10.0.22000.0/um",
                "C:/Program Files (x86)/Windows Kits/10/Include/10.0.22000.0/winrt",
                "C:/cppLibs/Boosts/include",
            ],
          其他
        }
    ],
    "version": 4
}
```

## code wsl c++环境配置

[vscode wsl子系统 c++环境配置基础篇](https://zhuanlan.zhihu.com/p/407125526)

下面介绍两种编译与运行方法:

### 编译

`tasks.json`: 该文件提供文件编译相关的信息, 选择主界面上方的Terminal(终端),
选中最后一项Configure Default Build Task:

![task](https://pic3.zhimg.com/v2-8e6434e01223d3a26cf10bb6f4ea8e96_r.jpg)

然后选择下面第三个, 即/usr/bin/g++

![g++](https://pic1.zhimg.com/v2-c03f3f6e6fa11d5e46a90f4d2de01db0_r.jpg)

会自动产生一个 `tasks.json` 文件, 不需要修改直接保存.
回到原本的文件, 按 `Ctrl-Shift-B` 即可编译, 按任意键关闭 输出窗口

返回wsl终端中, 运行 `./helloworld` 即可运行.
也可以 新建 `launch.json` 文件, 辅助调试.

## 调试

按照微软官网的方式, 选择主界面上方run(运行)下的Add Configuration添加配置

![add](https://pic1.zhimg.com/80/v2-69fb8de0ea46f836708b724f7d9c2454_720w.webp)

选择前者GDB

![gdb](https://pic4.zhimg.com/v2-1780ca125cfdf47e356eae8ce1d7ceab_r.jpg)

然后选择 `g++ build and debug active file`,
由于安装了c++插件, 会自动生产一个launch.json文件, 下面我们选择断点, 按F5即可进入调试模式

![debug](https://pic2.zhimg.com/80/v2-a533863adb5ce0c20c2cd3a486f5fc85_720w.webp)

屏幕上方有相关的调试操作, 不过相比gdb还是少了一点, 如果需要也可以使用gdb进行调试.

![db](https://pic3.zhimg.com/80/v2-46c43b549e9cfaaf031dfe642e87346a_720w.webp)
