# python 包管理

[离线批量安装python包](https://zhuanlan.zhihu.com/p/528753336)

## 批量离线安装包

制作 `requirement.txt`, 导出项目中所有依赖包
比如下载 django 1.8.11版本的包, requirements.txt的内容就是:

django==1.8.11

使用 pip freeze 会输出所有在本地已安装的包(但不包括 pip, wheel, setuptools 等自带包), 若需要输出内容与 pip list 一致, 需使用 pip freeze -all.

使用方法:

pip freeze > requirements.txt

适用场合:

由于 pip freeze 与 pip list 内容区别不大, 所以, 若想要用其作为工程依赖包列表, 需要配合 Python 虚拟环境 virtualenv 使用.

3.2 将所有包下载到目标目录中
例如: 想将包放在\home\packs目录下

pip download -d \home\packs -r requirement.txt

​​​​​​​3.3 将文件打包后放到离线服务器上, 并进行解压缩
pip install --no-index --find-links=\home\packs -r requirements.txt
