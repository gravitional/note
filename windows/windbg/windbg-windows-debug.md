# winDbg

[WinDBG__书](https://blog.csdn.net/weixin_30713953/article/details/95631466)

## [<Windows用户态程序高效排错> 勘误和补充](https://blog.csdn.net/eparg/article/details/1923194)

该书的当前状态请查阅:
http://blog.csdn.net/eparg/archive/2007/12/07/1923193.aspx

书中所引用到的链接列表:
http://blog.csdn.net/eparg/archive/2007/09/19/1792015.aspx

CSDN读书频道预览:
http://book.csdn.net/bookfiles/555/

China-pub, dearbook等网上书店的链接:
http://www.china-pub.com/computers/common/info.asp?id=37008
http://www.dearbook.com.cn/book/230727

下面是勘误.

1. 开篇的博文访谈第二页倒数第二行, Raymond Chen的名字误写为Redmond Chen.  ---- 匿名CSDN网友指出
2. 正文112页第一段把Ngen误写为NGne.  ---- 网友Tom指出, 送书一本
3. 53项最后一行的代码, func_templatefloatstd::string>, 应为func_template<float,std::string>, 少了个左尖括号和逗号.
4. 54页倒数第4行, "奇怪的时候", 是否应为"奇怪的是".
5. 121页第8行, Reflecto, 少了字母r. (错误3-5为网友tankaiha指出, 送书一本)
6. P28,第2行, "调用了getcharBuf函数", 应该为getcharBuffer. (读者fan36 http://fan36.blogspot.com 指出. 没有送书, 因为读者已经买了. 等有了第二版再送)
7. P47, 倒数第11行, "2.第一次触发后不会自动清除端点", 应该是"断点".
8. P36 倒数第二行, 内存占用率应该是cpu占用率 (ting wang)
9. P203 中间 因改为: 通过性能监视器中的Virtual Bytes和Private Bytes, 可以观察这两种内存的使用情况.
10. P29 第二行, "该指令正把十六进制值36(就是4的ASCII)", 应该是34,不是36

下面是补充.

Vista UAC 环境下如何早期加载调试器
http://blog.csdn.net/eparg/archive/2007/11/02/1863641.aspx

简单Access Violation的异常派发, Vista/Longhorn Server
http://blog.csdn.net/eparg/archive/2007/10/16/1826615.aspx

如何设定虚拟机的内核调试, 以及把用户态调试器的输出重定向到内核调试输出
http://blog.csdn.net/eparg/archive/2007/10/11/1820811.aspx

## windbg 用户模式 程序

[开始使用 WinDbg(用户模式)](https://learn.microsoft.com/zh-cn/windows-hardware/drivers/debugger/getting-started-with-windbg?source=recommendations)
[WinDbg教程-[1],调试基础(用户模式)](https://www.cnblogs.com/zhaotianff/p/17956010)
