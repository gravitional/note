# git book

[新版GitBook安装及使用——不完全避坑指南](https://blog.csdn.net/qq_43528771/article/details/107949010)
[GitBook简明安装教程](https://juejin.cn/post/7063617791043534861)

## Node.js版本问题

问题:  GitBook安装或查看版本信息时出现报错, 报错信息如下:

```js
if (cb) cb.apply(this, arguments)
                 ^
TypeError: cb.apply is not a function
```

或

```js
# Fatal error in , line 0
# Check failed: U_SUCCESS(status).
#
#FailureMessage Object: 00000013F6B1D570
```

使用Windows系统下的Node.js的版本管理器 `nvm-windows`(Node.js Version Manager for Windows).
nvm-windows可以帮助实现在同一台设备上进行多个node版本之间的切换.
主要步骤:

下载nvm: 在 [github地址](https://github.com/coreybutler/nvm-windows),
选择nvm-setup.zip, 下载后直接安装.

使用nvm命令实现多版本node的下载和切换:

```bash
nvm -v     # 查看nvm版本信息:
nvm install 10.21.0  # 安装多版本 node/npm(以v10.21.0为例):
nvm use 10.21.0   #使用特定Node版本(以v10.21.0为例):
nvm ls available #查看远程服务器上的可用Node版本:
nvm ls     3查看本机的可用Node版本
```

一个小tip: 有时使用 `nvm install` 命令下载新的Node版本会比较慢,
这时可以直接从 [nodejs.org](https://nodejs.org/zh-cn/download/releases/) 上下载所需版本,
并安装到nvm的源文件夹下.

## GitBook使用简介

### GitBook创建及预览

创建书籍:
新建一个文件夹, 在文件夹下打开命令窗口(在文件夹地址栏输入cmd后回车或在cmd中用cd命令), 初始化文件夹:

```bash
gitbook init
```

执行上述命令后, 会自动生成两个必要的文件 `README.md` 和 `SUMMARY.md`.

README.md: 书的介绍文字, 如前言, 简介, 在章节中也可做为章节的简介.
SUMMARY.md: 定制书籍的章节结构和顺序.

预览书籍:

```bash
gitbook serve
```

执行命令, GitBook 会启动一个 4000 端口(http://localhost:4000)用于预览.
但由于 GitBook 版本不稳定, 有时运行serve命令会出现报错:

```bash
Error: ENOENT: no such file or directory, stat '~~~.js'
```

解决办法:  在用户目录下找到以下文件 `.gitbook\versions\3.2.3\lib\output\website\copyPluginAssets.js`, 把
所有的 `confirm: true` 替换为 `confirm: false`.

构建书籍:

```bash
gitbook build
```

上述命令默认将生成的静态网站输出到 `_book` 目录.
实际上, 这一步也包含在 `gitbook serve` 里面, 但 `gitbook build` 可以指定路径:

```bash
gitbook build [书籍路径] [输出路径]
```

生成其它格式的电子书:

```bash
gitbook pdf ./ ./mybook.pdf
gitbook epub ./ ./mybook.epub
gitbook mobi ./ ./mybook.mobi
```

## GitBook插件

官方获取插件地址:  https://plugins.gitbook.com/
安装插件只需要在书籍目录下增加 book.json 文件,
例如增加折叠目录的插件, 需要在 book.json 内增加下面代码:

```json
{
    "plugins": ["expandable-chapters-small"],
    "pluginsConfig": {
        "expandable-chapters-small":{}
    }
}
```

然后终端执行 install 来安装插件即可:

```bash
gitbook install
```
