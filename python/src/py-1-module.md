# python 模块

[python脚本中的sys.path.append("..")详解](https://www.cnblogs.com/hls-code/p/15337302.html)
[How to prepend a path to a buildout-generated script](https://stackoverflow.com/questions/10197797/how-to-prepend-a-path-to-a-buildout-generated-script)
[pathlib --- 面向对象的文件系统路径](https://docs.python.org/zh-cn/3/library/pathlib.html#basic-use)
[Python获取当前脚本绝对路径](https://blog.csdn.net/junbujianwpl/article/details/75332141)

```python
import sys
sys.path.append('需要引用模块的地址')

# sys.path.append("..")   # 这代表添加当前路径的上一级目录

sys.path.append('你的模块的名称')
sys.path.insert(0,'模块的名称')
```
