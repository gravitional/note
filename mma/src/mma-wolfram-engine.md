# Wolfram Engine

[Mathematica 激活指南](https://tiebamma.github.io/InstallTutorial/)
[free Wolfram Engine](https://mathematica.stackexchange.com/questions/198839/how-to-add-a-front-end-to-the-free-wolfram-engine)

2019 年 5 月, Wolfram 推出了免费的 Wolfram Engine for Developers.
此软件实质上是一个没有笔记本界面, 也没有本地自带帮助的 Mathematica. 但是, 它是免费的!并且, 虽然没有自带笔记本, 但你可以用 `Jupyter` 笔记本.

## Windows 平台

+ 下载安装 `Python`:[https://www.python.org/](https://www.python.org/).
不要忘了勾选`"add python environment variables" / "add to PATH"`,否则需要自己添加`python.exe`到环境变量.
+ 从[github : WLforJupyter > releases](https://github.com/WolframResearch/WolframLanguageForJupyter/releases)下载`.paclet`文件.
+ 在`cmd(管理员)`执行:

```bash
pip install jupyter ## 等待安装jupyter笔记本
wolframscript ## 运行wolfram内核,下面的命令将在Wolfram 内核运行
PacletInstall @ ".paclet文件的路径" # 安装.paclet文件
<< WolframLanguageForJupyter`## 导入这个包
ConfigureJupyter["Add"] ## 配置Jupyter笔记本
```

然后输入`Quit`退出, 重新运行`CMD`, 输入`jupyter notebook`就会打开浏览器, 选择`New -> Wolfram Language`就可以运行`Wolfram`语言.

## Linux安装

首先安装好`wolframscript`和`Jupyter`

Clone [WolframLanguageForJupyter](https://github.com/WolframResearch/WolframLanguageForJupyter)仓库:
在终端中进入仓库, 运行`git clone https://github.com/WolframResearch/WolframLanguageForJupyter.git`
在克隆好的仓库中运行`./configure-jupyter.wls add`. 如果报错`Jupyter installation on Environment["PATH"] not found`, 可以尝试指定具体的路径:

```bash
configure-jupyter.wls add "Wolfram Engine二进制程序的绝对路径" "Jupyter二进制程序的绝对路径"
```

`Jupyter`二进制的目录一般在`~/.local/bin/jupyter `,` Wolfram binary`的位置可以在`wolframscript`中得到:

```bash
wolframscript
FileNameJoin[{$InstallationDirectory, "Executables", "WolframKernel"}]
```
