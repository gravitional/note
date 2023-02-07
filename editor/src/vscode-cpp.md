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
