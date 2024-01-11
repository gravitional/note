# Linux /bin, /sbin, /usr/bin, /usr/sbin, /usr/local/bin, /usr/local/sbin

[Linux文件系统中/bin, /sbin, /usr/bin, /usr/sbin, /usr/local/bin, /usr/local/sbin文件夹的区别是什么?](https://www.zhihu.com/question/21265424)
[linux中系统中 /bin, /sbin, /usr/bin, /usr/sbin, /usr/local/bin, /usr/local/sbin 目录的含义及区别](https://blog.csdn.net/weixin_45649763/article/details/104635869)

传统上的常规做法是:

+ 系统级的组件放在 `/bin`, `/lib`;
+ 根用户才能访问的放在/sbin;
+ 系统repository提供的应用程序放在 `/usr/bin`, `/usr/lib`;
+ 用户自己编译的放在/usr/local/XXX.

现在有一些变化, 在大约两年前, 大量Linux系统都将/bin, /lib弄成/usr/bin, /usr/lib的符号链接.
此外, 不同系统还会有很多的细微区别,
比如Redhat系喜欢把32位的库放在/lib, /usr/lib, 64位的库放在/lib64, /usr/lib64,
而Debian系喜欢把平台相关的那层名字放在/lib, /usr/lib的子目录里, 比如/usr/lib/x86_64-linux-gnu/.

然后, 各种配置文件的文件名, 路径也会有区别,
比如ssh服务器的配置文件可能叫/etc/ssh/sshd.conf,
也可能叫/etc/ssh/sshd_config.

+ 简单地说

/bin: 存放所有用户皆可用的系统程序, 即普通的基本命令, 如: touch ls等.
/sbin: 存放超级用户才能使用的系统程序, 即基本的系统命令, 如: sreboot等.

/usr/bin: 存放所有用户都可用的应用程序, 一般是已安装软件的运行脚本, 如: free, make, wget等.
/usr/sbin: 存放超级用户才能使用的应用程序 , 一般是与服务器软件程序命令相关的, 如: dhcpd,  httpd, samba等.

/usr/local/bin: 存放所有用户都可用的第三方软件程序,如mysql
/usr/local/sbin: 存放超级用户才能使用的第三方软件,如nginx
