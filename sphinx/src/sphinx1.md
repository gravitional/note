# sphinx

[Sphinx + Read the Docs 从懵逼到入门](https://zhuanlan.zhihu.com/p/264647009)
[Sphinx 网站: ](http://sphinx-doc.org/)
[Sphinx 使用手册: ](https://zh-sphinx-doc.readthedocs.io)
[reStructuredText 网站](https://docutils.sourceforge.io/rst.html)

## 环境搭建

这里以 Ubuntu 为例(其他 Linux 发行版, MacOS 或 Windows 也行),
首先安装 Python3, Git, Make 等基础软件.

```bash
sudo apt install git
sudo apt install make
sudo apt install python3
sudo apt install python3-pip
```

然后安装最新版本的 Sphinx 及依赖.

```bash
pip3 install -U Sphinx
```

为了完成本示例, 还需要安装以下软件包.

```bash
pip3 install sphinx-autobuild
pip3 install sphinx_rtd_theme
pip3 install recommonmark
pip3 install sphinx_markdown_tables
```

安装完成后, 系统会增加一些 sphinx- 开头的命令.

    sphinx-apidoc    sphinx-autobuild    sphinx-autogen    sphinx-build phinx-quickstart
    