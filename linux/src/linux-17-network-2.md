# 网络

### 查看网络配置

例子:

+ `ip addr` Shows addresses assigned to all network interfaces.
+ `ip neigh` Shows the current neighbour table in kernel.
+ `ip link set x up` Bring up interface x.
+ `ip link set x down` Bring down interface x.
+ `ip route` Show table routes.

更多例子:

+ `ip ro` Show all route entries in the kernel.
+ `ip route add default via 192.168.1.1 dev eth0` Adds a default route (for all addresses) via the local gateway 192.168.1.1 that can be reached on device eth0.
+ `ip route add 10.1.1.0/30 encap mpls 200/300 via 10.1.1.1 dev eth0` Adds an ipv4 route with mpls encapsulation attributes attached to it.
+ `ip -6 route add 2001:db8:1::/64 encap seg6 mode encap segs 2001:db8:42::1,2001:db8:ffff::2 dev eth0` Adds an IPv6 route with SRv6 encapsulation and two segments attached.

***

`Ubuntu 18.04 Server` 安装好后,Netplan 的默认描述文件是:`/etc/netplan/50-cloud-init.yaml`.

[Ubuntu18.04的网络配置 netplan]

[Ubuntu18.04的网络配置 netplan]: https://blog.csdn.net/uaniheng/article/details/104233137?utm_medium=distribute.pc_relevant_t0.none-task-blog-BlogCommendFromMachineLearnPai2-1.nonecase&depth_1-utm_source=distribute.pc_relevant_t0.none-task-blog-BlogCommendFromMachineLearnPai2-1.nonecase

### 配置netplan 固定ip

`vim /etc/netplan/50-cloud-init.yaml `

配置如下:

```bash
network:
    ethernets:
        enp3s0:
            addresses: [192.168.0.20/24]  //IP址
            gateway4: 192.168.0.1  // 网关
            nameservers:
             addresses: [114.114.114.114, 192.168.0.1] //DNS
            dhcp4: no
            optional: no
    version: 2
```

或者配置 `dhcp` 自动获取 `ip`

`vim /etc/netplan/50-cloud-init.yaml `

配置如下:

```bash
network:
    ethernets:
        enp3s0:
            dhcp4: true
            optional: yes
    version: 2
```

应用配置文件:

```bash
sudo netplan apply
```

### 查看MAC地址

+ `ifconfig | awk '/eth/{print $1,$5}'`
+ `arp -a | awk '{print $4}`
+ `sudo lshw -C network`
+ `sudo lshw -c network | grep serial`

```bash
wlp2s0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc mq state UP group default qlen 1000
      link/ether 10:5b:ad:df:4c:cd brd ff:ff:ff:ff:ff:ff
    inet 192.168.32.6/24 brd 192.168.32.255 scope global dynamic noprefixroute wlp2s0
       valid_lft 4185sec preferred_lft 4185sec
    inet6 fe80::310a:df04:1f02:d5ac/64 scope link noprefixroute
       valid_lft forever preferred_lft forever
```

`link/ether 10:5b:ad:df:4c:cd brd ff:ff:ff:ff:ff:ff` This is the mac address

### 子网掩码

[为什么每台电脑都要设置子网掩码?](https://www.zhihu.com/question/263438014/answer/278015413)
[24 28 30 位的子网掩码是多少 ](https://zhidao.baidu.com/question/517459209.html)

例如:
IP地址为`130.39.37.100`,
网络地址为`130.39.0.0`,
子网地址为`130.39.37.0`,
子网掩码为`255.255.255.0`,
网络地址部分和子网标识部分对应`1`,host部分对应`0`.
使用CIDR表示为:`130.39.37.100/24`即`IP地址/ 掩码长度`.

`ipv4`是`8bit.8bit.8bit.8bit`的形式,二进制到十六进制是`4`位到`1`位,`8bit`相当于两个`16`进制数字.
所以`ipv4`是四段地址:`0x0x.0x0x.0x0x.0x0x`,每段两个`16`进制数字.
`24`表示掩码开头有`24`个`1`,对应地址中的`3`段,也就是公网地址是`3`段.

`awk`执行按位**与**操作.

```bash
awk 'BEGIN {
awk 'BEGIN {
    num1 = 10
    num2 = 6
    printf "(%d AND %d) = %d\n", num1, num2, and(num1, num2)
}'
```

设

```ip
A:10.1.1.10 /24
B:10.1.1.20 /24
C:50.1.1.80 /24
```

AB在同一局域网,C位于外网.

三个表:

+ `ARP`表:主机维护,存放`IP`地址和`MAC`地址对应关系.
+ `MAC`地址表:交换机维护,存放`MAC`地址和交换机端口对应关系.
+ 路由表:路由器维护,存放`IP`地址和路由器端口对应关系.

首先`AB`通信,
例如`A`要给`B`发送一个数据包,目前`A`知道`B`的`IP`地址,根据掩码规则判定`B`和自己在同一个局域网,同一个广播域.

接下来`A`通过广播方式获取`B`的`MAC`地址,添加到自己的`ARP`表中.
然后把要发送的包封装,然后发送给交换机,交换机收到数据包后解封装得到`B`的`MAC`地址,
根据`MAC`地址表转发到`B`所连接的交换机端口,完成发送.

如果`A`要和`C`通信,发送一个包给`C`的话,也只知道`C`的`IP`地址,然后`A`根据掩码规则发现`C`和自己不是同一个局域网的,
广播不到`C`,所以`A`只能把数据包发给网关,由网关发出去给到`C`.

`A`同样通过广播方式获取网关的`MAC`地址,然后把`C`的`IP`地址和网关的`MAC`地址封装到数据包后发给交换机,
交换机解封装后对比`MAC`地址表,发现是发给网关的包,就转发到网关即路由器所在的交换机端口.
路由器收到包之后再解封装,得到`C`的`IP`地址,然后根据自己的路由表转发到相应的端口.完成通信.

所以如果计算机上不设置子网掩码,从第一步就不能完成,下面就更不能继续了.
如果同一个广播域里有机器设置不同的子网掩码,依然能够通信,只不过有的内网包需要到网关绕一圈.

外网包的话只要网关设置对了就没问题.

## 查看登录用户

[linux查看当前登录用户](https://blog.csdn.net/wanchaopeng/article/details/88425067)

+ `w`命令:显示目前登入系统的用户信息. 选项:

+ `-f`: 开启或关闭显示用户从何处登入系统.
+ `-h`: 不显示各栏位的标题信息列.
+ `-s`: 使用简洁格式列表,不显示用户登入时间,终端机阶段作业和程序所耗费的CPU时间.
+ `-u`: 忽略执行程序的名称,以及该程序耗费CPU时间的信息.
+ `-V`: 显示版本信息.

输出的结果的含义:

+ `USER` 登录的用户名
+ `TTY` 登录终端
+ `FROM` 从哪个IP地址登录
+ `LOGIN`@ 登录时间
+ `IDLE` 用户闲置时间
+ `JCPU` 指的是和该终端连接的所有进程占用的时间,这个时间里并不包括过去的后台作业时间,但却包括当前正在运行的后台作业所占用的时间
+ `PCPU` 当前进程所占用的时间
+ `WHAT` 当前正在运行的命令

+ `who`; 显示当前已登录的用户信息,输出的结果有:用户名,登录终端,登录的时间
+ `last`; 列出目前与过去登入系统的用户相关信息.

+ `-R`: 省略 hostname 的栏位
+ `-n`:指定输出记录的条数.
+ `-f file`:指定用文件`file`作为查询用的`log`文件.
+ `-t time`:显示到指定的时间.
+ `-h `:显示帮助.
+ `-i` or`--ip`:以`数字`and `点`的形式显示`ip`地址.
+ `-x`:显示系统关闭, 用户登录和退出的历史.

命令的输出包含:用户名,登录终端,登录IP,登录时间,退出时间(在线时间)

+ `lastlog`;检查某特定用户上次登录的时间. 选项:

+ `-b`, `--before DAYS`: 仅打印早于 DAYS 的最近登录记录
+ `-h`, `--help`: 显示此帮助信息并推出
+ `-R`, `--root CHROOT_DIR` directory to chroot into
+ `-t`, `--time DAYS` : 仅打印比 DAYS 新近的登录记录
+ `-u`, `--user LOGIN` : 打印 LOGIN 用户的最近登录记录

注意:`lastlog`命令默认读取的是`/var/log/wtmp`这个文件的数据,一定注意这个文件不能用`vi`来查看.
命令输出包括:用户名,登录终端,登录`IP`,最后一次登录时.
