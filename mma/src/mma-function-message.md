# 消息,Messages

tutorial/Messages

消息系统可以用来输出错误和警告: `tutorial/Messages`
常用函数有: `Message`, `Messages`, `Information`

+ `$MessageList` : 当前输入行计算产生的消息列表.
+ `Check[expr,failexpr]`:  如果计算成功, 返回`expr`, 如果计算期间产生消息(一般是因为错误), 就返回`failexpr`

+ `全局信息` tutorial/GlobalSystemInformation

可以使用一些特殊的全局变量, 来查看运行环境, 如:

```mathematica
$Notebooks   记录是否正在使用笔记本前端
$BatchInput   是否以批处理方式给出输入
$BatchOutput   是否应以批处理方式给出输出, 从而不带标签等.
$CommandLine   用于调用Wolfram语言内核的原始命令行
$ParentLink   WSTP LinkObject, 指定调用内核的程序(如果直接调用内核, 则为Null)
$ProcessID   操作系统分配给Wolfram语言内核进程的ID
$ParentProcessID   调用Wolfram语言内核的进程的ID
$Username   运行Wolfram语言内核的用户的登录名
Environment["var"]   操作系统定义的变量的值
```
