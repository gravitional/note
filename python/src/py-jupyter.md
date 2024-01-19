# jupyter

[The Jupyter Notebook](https://jupyter-notebook.readthedocs.io/en/latest/)

## 运行笔记本

安装`Jupyter Notebook`会默认安装`IPython` kernel.  可以使用 Python 进行工作.

在计算机上安装`Jupyter Notebook`后, 即可运行笔记本服务器.
您可以通过运行以下命令从命令行(在`Mac / Linux`上使用`Terminal`, 在Windows上使用`Command Prompt`)启动笔记本服务器.

```bash
jupyter notebook
```

终端中将打印有关笔记本服务器的一些信息, 包括Web应用程序的`URL`(默认情况下为`http://localhost:8888`)

***
以下代码应在当前运行的笔记本服务器中打开给定的笔记本, 并在必要时启动一个服务器.

```bash
jupyter notebook notebook.ipynb
```

***
使用自定义IP或端口启动笔记本电脑.

默认情况下, 笔记本服务器从端口`8888`启动. 如果端口`8888`不可用或正在使用中, 则笔记本服务器搜索下一个可用端口.
您也可以手动指定端口.  在此示例中, 我们将服务器的端口设置为`9999`:

```bash
jupyter notebook --port 9999
```

***
在不打开Web浏览器的情况下启动笔记本服务器:

```bash
jupyter notebook --no-browser
```

***

获得有关笔记本服务器选项的帮助

```bash
jupyter notebook --help
```

`--notebook-dir=<Unicode>`
The directory to use for notebooks and kernels.

## 快捷键

`Jupyter`笔记本电脑有两种不同的键盘输入模式.
编辑模式允许您在单元格中键入代码或文本, 并以绿色单元格边框指示.
命令模式将键盘绑定到笔记本级别的命令, 并由带有蓝色左边界的灰色单元格边框指示.

`enter`: 进入编辑模式
`esc`: 进入命令模式

笔记本界面, 菜单中有快捷键提示, 在命令模式按下`h`打开, 也可以自定义快捷键
常用如下, 按小写字母就可以

+ `ctrl+S` 保存
+ `X`: 剪切
+ `C`: 拷贝
+ `V`: 粘贴到下面
+ `shift+V`: 粘贴到上面
+ `D,D`: 删除cell
+ `Z`: 撤回删除
+ `ctrl+shit+-`: 分割cells
+ `shift+L`: 切换行号显示
+ `A`: 上方插入
+ `B`: 下方插入
+ `Y`: code 格式
+ `M`: markdwon 格式
+ `R` : rawNBConvert 格式
+ `ctrl+enter`:运行单元
+ `shift+enter`:运行并选择下一行
+ `alt+enter`:运行并插入下一行
+ `shift+0`:切换滚动
+ `I,I`打断运行
+ `0,0`:重启内核

## 运行 notebook 服务端

保护 notebook 服务器

您可以使用简单的密码来保护笔记本服务器. 从 notebook 5.0 开始, 这是自动完成的.
要手动设置密码, 可以在`jupyter_notebook_config.py`中配置 `NotebookApp.password` 设置.

### 前提:笔记本配置文件

检查是否有笔记本配置文件`jupyter_notebook_config.py.`.  该文件的默认位置是位于主目录中的 `Jupyter` 文件夹:

```path
Windows: C:\Users\USERNAME\.jupyter\jupyter_notebook_config.py
OS X: /Users/USERNAME/.jupyter/jupyter_notebook_config.py
Linux: /home/USERNAME/.jupyter/jupyter_notebook_config.py
```

如果你还没有 `Jupyter` 文件夹, 或者你的 `Jupyter` 文件夹中没有笔记本配置文件, 请运行以下命令:

```bash
$ jupyter notebook --generate-config
```

如有必要, 此命令将创建 `Jupyter` 文件夹, 并在此文件夹中创建笔记本配置文件`jupyter_notebook_config.py`.

### 自动密码设置

从 `notebook 5.3` 开始, 您首次使用令牌登录时,  `notebook` 服务器应该会提示你从用户界面设置密码.
将会有一个表格, 要求输入`current _token_` 以及`_new_ _password_`,输入之后, 单击登录就设置了新密码.
下次您需要登录时, 将可以使用新密码代替`_token_`, 否则请按照以下步骤在命令行中设置密码.

```bash
$ jupyter notebook password
Enter password:  123 #比如设置为 123
Verify password: 123
[NotebookPasswordApp] Wrote hashed password to /Users/you/.jupyter/jupyter_notebook_config.json
```

这可以用来重置丢失的密码;  或者如果您认为您的凭据已泄漏并且希望更改密码.  重启服务器后, 更改密码将使所有登录的会话无效.

设置`--NotebookApp.allow_password_change=False`可以禁用首次登录时更改密码的功能.

## juypter lab

[jupyterlab-cli](https://nocomplexity.com/documents/jupyterlab/notebooks/jupyterlab-cli.html)

`--notebook-dir=<Unicode>`; The directory to use for notebooks and kernels.
`--preferred-dir`; Preferred starting directory to use for notebooks and kernels.

```bash
#Windows Example
jupyter lab --notebook-dir=E:/ --preferred-dir E:/Documents/Somewhere/Else
#Linux Example
jupyter lab --notebook-dir=/var/ --preferred-dir /var/www/html/example-app/
```

## notebook vscode

`A` 在上方插入cell
`B` 在下方插入cell

`M` 将cell类型改成 Markdown
`Y` 将cell类型改成 code
