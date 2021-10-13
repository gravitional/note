# linxu-ack

[Linux ack命令使用详解 ](https://man.comptechs.cn/ack.html)

`ack` 官网：[ack is a grep-like source code search tool.](https://beyondgrep.com/)

## 安装

ubuntu下要安装`ack-grep`, 因为在`debian`系中, `ack`这个名字被其他的软件占用了.
ubuntu 20 可以直接安装

```re
sudo apt-get install ack
```

参数说明： 这些参数在linux上的使用频率是相当高的, 尤其是你用`vim`做为IDE的话

+ `-c` ; 统计
+ `-i` ; 不区分大小写
+ `-h` ; 不显示名称
+ `-l` ; 只显文件名
+ `-n` ; 加行号
+ `-v` ; 显示不匹配
+ `-r`; 递归搜索子目录

## 实例

在记忆的时候大体上可以分为这几个部分：

+ Searching 代码搜索
+ Search output 搜索结果处理
+ File presentation 文件展示
+ File finding 文件查找
+ File inclusion/exclusion 文件过滤

### grep常用操作

```re
ack -r 'hello_world' # 简单用法
ack '^hello_world' . # 简单正则
ls -l | ack '.py' # 管道用法
```

### Searching

简单的文本搜索, 默认是递归的.

```re
ack hello
ack -i hello
ack -v hello
ack -w hello
ack -Q 'hello*'
```

### 搜索结果,Search File

对搜索结果进行处理, 比如只显示一个文件的一个匹配项, 或者xxx

```re
ack --line=1       # 输出所有文件第二行
ack -l 'hello'     # 包含的文件名
ack -L 'print'     # 非包含文件名
```

### 文件展示,File presentation

输出的结果是以什么方式展示呢, 这个部分有几个参数可以练习下

```re
ack hello --pager='less -R'    # 以less形式展示
ack hello --noheading      # 不在头上显示文件
ack hello --nocolor        # 不对匹配字符着色
```

### 查找文件,File finding

没错, 它可以查找文件, 以省去你要不断的结合 `find` 和 `grep` 的麻烦, 虽然 `linux` 的思想是一个工具做好一件事.

```re
ack -f hello.py     # 查找全匹配文件
ack -g hello.py$    # 查找正则匹配文件
ack -g hello  --sort-files     # 查找然后排序
```

### 文件过滤,File Inclusion/Exclusion

如果你曾经在搜索项目源码是不小心命中日志中的某个关键字的话, 你会觉得这个有用.

```re
ack --python hello       # 查找所有python文件
ack -G hello.py$ hello   # 查找匹配正则的文件
```
