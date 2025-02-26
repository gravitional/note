# vscode 下载插件的离线安装包

[2025 年 VSCode 插件离线下载攻略：官方渠道一键获取](https://zhuanlan.zhihu.com/p/26003070992)

分析VSCode代码发现，插件的安装首先是下载插件.vsix文件，安装完后就删除了。因此可通过拼接 URL 的方式来获取.vsix文件的下载链接。

VSCode 插件的下载链接格式通常是这样的：

https://marketplace.visualstudio.com/_apis/public/gallery/publishers/{发布者}/vsextensions/{插件名}/{版本号}/vspackage
其中：

**发布者**：插件标识符中 `.` 之前的部分，例如 `ms-python`。
**插件名**：标识符中 `.` 之后的部分，例如 `python`。
**版本号**：可以在插件的版本历史中找到。
这种方法最简单直接，相比从第三方网站或相应插件的GitHub仓库中下载更加靠谱，关键还是官方的！

## VSCode Python开发环境常用的链接

https://marketplace.visualstudio.com/items?itemName=ms-python.python

在页面右下角 `More Info` 里面, 可以看到插件信息

+ `Version 2025.1.2025022102`
+ Released on 2016/1/19 23:03:11
+ Last updated 2025/2/21 18:47:10
+ Publisher Microsoft
+ `Unique Identifier ms-python.python`

注意其中的 `Version` 和 `Unique Identifier` 后面的信息
格式：发布者.插件名-版本号。
例如 `python` 插件的下载链接为

[ms-python.python-2025.1.2025022102@win32-x64.vsix](https://marketplace.visualstudio.com/_apis/public/gallery/publishers/ms-python/vsextensions/python/2025.1.2025022102/vspackage)
[ms-python.debugpy-2025.1.2025021701.vsix](https://marketplace.visualstudio.com/_apis/public/gallery/publishers/ms-python/vsextensions/debugpy/2025.1.2025021701/vspackage)
[ms-python.vscode-pylance-2025.2.101.vsix](https://marketplace.visualstudio.com/_apis/public/gallery/publishers/ms-python/vsextensions/vscode-pylance/2025.2.101/vspackage)
