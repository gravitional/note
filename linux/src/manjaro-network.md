# manjaro 网络

[Software access point](https://wiki.archlinux.org/title/Software_access_point)
[Internet sharing](https://wiki.archlinux.org/title/Internet_sharing)

软的接入点(software access point), 也被称为虚拟路由器或虚拟Wi-Fi(virtual router or virtual Wi-Fi),
使一台计算机能够将其无线接口变成一个Wi-Fi接入点. 它省去了购买独立无线路由器的麻烦.

## 要求

Wi-Fi 设备必须支持 `AP模式`

你需要一个兼容 `nl80211` 的无线设备, 它支持 `AP操作模式`.
可以通过运行 `iw list` 命令来检测, 在`Supported interface modes`块下, `AP` 应该被列出:

```bash
$ iw list

Wiphy phy1
...
    Supported interface modes:
         * IBSS
         * managed
         * AP
         * AP/VLAN
         * WDS
         * monitor
         * mesh point
...
```

>注意: 自2019年以来, 大多数英特尔设备将不再提供 `5 GHz` 频段的 `AP服务`, 这
是由于固件(firmware)错误地将`位置感知监管`(Location-Aware Regulatory, LAR)功能启用, 即使处在`AP模式`下.
参见[#在5GHz频段无法启动AP模式](https://wiki.archlinux.org/title/Software_access_point#Cannot_start_AP_mode_in_5_GHz_band).

## 使用单一Wi-Fi设备的无线客户端和软件AP

创建`软件AP` 是独立于你的网络连接(`以太网`, `无线`......). 许多无线设备甚至支持`同时`作为 `AP` 和无线 "客户端 "操作.
利用这种能力, 你可以使用单个无线设备, 创建`软件AP`作为现有网络的 "无线中继器"(wireless repeater). 这种能力在 `iw list` 的输出的以下部分列出:

```bash
$ iw list

Wiphy phy1
...
        valid interface combinations:
                 * #{ managed } <= 2048, #{ AP, mesh point } <= 8, #{ P2P-client, P2P-GO } <= 1,
                   total <= 2048, #channels <= 1, STA/AP BI must match
...
```

限制条件 `#channels <= 1` 意味着你的 `软件AP` 必须在与你的 `Wi-Fi客户端` 连接相同的 `channel` 上运行;
见下面 `hostapd.conf` 中的频道设置.

如果你想使用这个能力/功能, 也许是因为以太网连接不可用, 你需要创建两个单独的虚拟接口.
物理设备 `wlan0` 的虚拟接口可以按如下方式创建.
为网络连接(`wlan0_sta`)本身, 以及`软件AP`/`hostapd` "无线中继器" 创建具有唯一MAC地址的虚拟接口(virtual interfaces):

```bash
# iw dev wlan0 interface add wlan0_sta type managed addr 12:34:56:78:ab:cd
# iw dev wlan0 interface add wlan0_ap type managed addr 12:34:56:78:ab:ce
```

可以使用 [macchanger][] 生成随机 `MAC` 地址.

[macchanger]: https://wiki.archlinux.org/title/MAC_address_spoofing#macchanger

## 配置

设置 `接入点`(access point) 包括两个主要部分:

+ 设置 Wi-Fi 链路层(Wi-Fi link layer), 使`无线客户端`能够连上你的计算机的`软件接入点`, 并与它交换 `IP数据包`.
+ 在你的电脑上设置`网络配置`(network configuration), 这样它就能在 `互联网` 和 `无线客户端` 之间正确地转发 `IP数据包`.

### Wi-Fi链接层

实际的 `Wi-Fi链接` 是通过 `hostapd` 软件包建立的, 它支持 `WPA2`:

如果有必要, 调整 `hostapd` 配置文件中的选项.
特别是, 改变 `ssid` 和 `wpa_passphrase`. 更多信息请参见 [hostapd Linux文档页面](https://wireless.wiki.kernel.org/en/users/documentation/hostapd).

/etc/hostapd/hostapd.conf

```conf
interface=wlan0_ap
bridge=br0

# 在IEEE 802.11管理框架中使用的SSID
ssid=YourWiFiName
# 驱动接口类型(hostap/wired/none/nl80211/bsd)
driver=nl80211
# 国家代码(ISO/IEC 3166-1).
country_code=US

# 操作模式 (a = IEEE 802.11a (5 GHz), b = IEEE 802.11b (2.4 GHz)
hw_mode=g
# Channel 数
channel=7
# 允许的最大站点数 stations
max_num_sta=5

# Bit field: bit0 = WPA, bit1 = WPA2
wpa=2
# Bit field: 1=wpa, 2=wep, 3=both
auth_algs=1

# Set of accepted cipher suites; disabling insecure TKIP
# 可接受的密码套件集; 禁用不安全的 TKIP
wpa_pairwise=CCMP
# 可接受的密钥管理算法集
wpa_key_mgmt=WPA-PSK
wpa_passphrase=Somepassphrase

# hostapd事件记录器配置
logger_stdout=-1
logger_stdout_level=2

# 如果你的设备支持802.11n, 请取消注释并修改以下部分
## 启用802.11n支持
#ieee80211n=1
## 支持QoS
#wmm_enabled=1
## 使用 "iw list" 来显示设备的能力, 相应地修改 ht_capab
#ht_capab=[HT40+][SHORT-GI-40][TX-STBC][RX-STBC1][DSSS_CCK-40]
```

提示: 你可以用 UTF-8 字符设置 `SSID`, 这样国际字符就可以正常显示. 启用它的选项是 `utf8_ssid=1`.
一些客户端在识别正确编码方面可能会有问题(例如 `wpa_supplicant` 或 `Windows 7`).

为了在启动时自动启动 `hostapd`, 请 [systemd 启用][] `hostapd.service`.

如果你要在 boot 时启动 `hostapd`, 请确保 [无线网络接口][] 先被启用(bring up), 否则会失败.
为了确保你的无线接口已经准备好了, [编辑单元配置文件][], 声明它绑定到你的网络接口, 应该在你的网络接口之后启动.

/etc/systemd/system/hostapd.service.d/override.conf

```systemd
[Unit]
BindsTo=sys-subsystem-net-devices-wlan0.device
After=sys-subsystem-net-devices-wlan0.device
```

还要确保该 `接interface口` 没有被其他`network managers`管理.
如果你使用 `NetworkManager`, 请参阅 [NetworkManager#Ignore specific devices](https://wiki.archlinux.org/title/NetworkManager#Ignore_specific_devices).

>警告: 接入点操作所允许的无线信道(channels)因地域而异.
>根据无线固件的不同, 你可能要正确设置地区, 才能使用合法的频道.
>**不要**选择其他地区, 因为你可能会非法干扰网络流量(traffic), 影响你自己的设备和在其范围内的其他人的无线功能!
>要设置区域, 请看[无线网络配置#尊重监管域](https://wiki.archlinux.org/title/Network_configuration/Wireless#Respecting_the_regulatory_domain).

***
>注意: 如果你有一块基于 `RTL8192CU` 芯片组的网卡, 请安装 [hostapd-rtl871xdrv][] 并在`hostapd.conf` 文件中用 `driver=rtl871xdrv` 替换 `driver=nl80211`.

[systemd 启用]: https://wiki.archlinux.org/title/Systemd#Using_units
[无线网络接口]: https://wiki.archlinux.org/title/Network_configuration#Network_interfaces
[编辑单元配置文件]: https://wiki.archlinux.org/title/Systemd#Editing_provided_units
[hostapd-rtl871xdrv]: https://aur.archlinux.org/packages/hostapd-rtl871xdrv/

## 网络配置

有两种基本的实现方式:

+ `网桥`(bridge): 在你的计算机上创建一个网桥, 无线客户端会访问和你的计算机相同的网络接口, 和子网(subnet).
+ `NAT`: 通过 `IP转发/掩码`(IP forwarding/masquerading) 和 `DHCP` 服务,
无线客户端将使用一个专门的子网, 来自/进入该子网的数据被 `NAT` 化.
这时你用来开热点的计算机, 类似于连接到互联网的普通Wi-Fi 路由器.

桥接方式更简单, 但它要求: `无线客户端` 需要的任何服务, 特别是 `DHCP`, 在计算机的外部接口上是可用的.
这意味着, 如果分配 `IP地址` 的外部调制解调器(modem)向不同的客户提供相同的地址, 它将无法工作.

`NAT` 方法的用途更广(versatile), 因为它明确地将 `Wi-Fi客户端` 与你的计算机分开, 对外界完全透明.
它可以与任何类型的网络连接一起工作, 而且(如果需要)可以使用通常的 `iptables` 方法引入流量策略(traffic policies).

可以将这两种方法结合起来:
例如, 创建网桥, 包含 `以太网设备` 和 具有静态ip的`无线设备`,
提供 `DHCP`, 并配置 `NAT` 将流量转发到连接到广域网(WAN)的另一个网络设备.

### 网桥设置

你需要创建一个网桥, 并将你的网络接口(如 `eth0`)加入其中.
你**不应该**把无线设备(如 `wlan0`)添加到网桥中; `hostapd` 会自己添加它.

参见[网络桥接](https://wiki.archlinux.org/title/Network_bridge).

>提示: 如果你有一个现有的网桥, 你可能希望重新使用它(例如, 由虚拟机使用).

### NAT设置

配置细节见 [互联网共享#Configuration](https://wiki.archlinux.org/title/Internet_sharing#Configuration).

在该文章中, 连接到 `LAN` 的设备是 `net0`. 在这种情况下, 该设备就是你的无线设备(例如 `wlan0`).

## 工具

[dnsmasq]: https://wiki.archlinux.org/title/Dnsmasq
[iptables]: https://wiki.archlinux.org/title/Iptables

### linux-wifi-hotspot

[linux-wifi-hotspot][] 软件包提供了一个脚本, 可以创建一个桥接的或NAT的接入点, 用于网络共享.
它结合了 `hostapd`, [dnsmasq][] 和 [iptables][] 来保证接入点的良好运作. 包括命令行和gui.
创建一个 NAT 的虚拟网络的基本语法如下:

```bash
# create_ap wlan0 eth0 MyAccessPoint MyPassPhrase
```

另外, 在 `/etc/create_ap.conf` 中提供的模板配置可以根据自己的需要进行调整, 并使用脚本运行.

```bash
# create_ap --config /etc/create_ap.conf
```

要使用图形用户界面, 在终端运行.

```bash
# wihotspot
```

启用/启动 `create_ap.service`, 以便在启动时按照 `/etc/create_ap.conf `中指定的配置运行脚本.

更多信息见 [GitHub 上的 linux-wifi-hotspot](https://github.com/lakinduakash/linux-wifi-hotspot).

注意: 在桥接模式下, 在 boot time `create_ap` 可能与当前的网络配置冲突.
在这种情况下, 不要配置以太网接口的 `IP` 地址, 既不要配置 `DHCP`, 也不要配置 `statip IP` 地址, 以方便与网桥的绑定.

## RADIUS

[WPA2 Enterprise]: https://wiki.archlinux.org/title/Network_configuration/Wireless#WPA2_Enterprise
[FreeRADIUS]: https://freeradius.org/
[ref1]: https://me.m01.eu/blog/2012/05/wpa-2-enterprise-from-scratch-on-a-raspberry-pi/

参见[ref1][], 了解为 [WPA2 Enterprise][] 运行 [FreeRADIUS][] 服务器的说明.

## 故障处理

### WLAN非常慢

导致吞吐量低于预期的经常性原因包括:

+ `操作模式`选择不当, `hw_mode` 低于支持的模式会人为地限制路由器. 检查是否选择了现代的操作模式.
+ 拥挤或有噪音的频道会严重降低性能, 特别是在人口稠密的地区. 尝试改变到不同的频道, 甚至切换频率.
+ 熵(entropy)太少会导致性能不佳. 考虑安装 [haveged](https://wiki.archlinux.org/title/Haveged).

### NetworkManager有干扰

如果设备是由 `NetworkManager` 管理的, `hostapd` 可能无法工作. 你可以用 `MAC` 屏蔽设备.

/etc/NetworkManager/conf.d/unmanaged.conf

```systemd
[keyfile]
unmanaged-devices=mac:hwaddr
```

或接口名称:

/etc/NetworkManager/conf.d/unmanaged.conf

```systemd
[keyfile]
unmanaged-devices=interface-name:ifname
```

### 无法在5GHz频段启动AP模式

[不启动辐射]: https://wireless.wiki.kernel.org/en/developers/regulatory/processing_rules#post_processing_mechanisms

显然, 由于特殊的国家代码 `00`(全球), `5Ghz` 频段的所有可用频率都会设置 `no-ir`, [不启动辐射][] 标志, 这将阻止 `hostapd` 使用它们.
你需要安装 crda 并设置你的国家代码, 使 `hostapd` 可以使用你所在国家允许的频率.

注意, 最近的英特尔设备有一个位置感知监管(LAR)功能, 它忽略了用户空间的监管数据库(userspace regulatory database),
而是通过监听附近的其他接入点来推断监管区域.
这意味着设备在看到其他5GHz频段的接入点之前, 不会在任何5GHz频率上传输, [在许多情况下根本无法进行任何5GHz传输](https://bugzilla.kernel.org/show_bug.cgi?id=206469).
较早的内核有一个选项可以禁用这个功能, 但在2019年被删除, 因为它导致固件崩溃.
自从这次删除后, 支持 LAR 的英特尔卡不能再作为 5GHz 频段的 access points 使用.

另见

+ [Hostapd : 创建虚拟Wi-Fi接入点的Linux方法](https://nims11.wordpress.com/2012/04/27/hostapd-the-linux-way-to-create-virtual-wifi-access-point/)
+ [用DHCP和DNS配置子网的教程和脚本](https://xyne.dev/notes/network/dhcp_with_dns.html)

## wihotspot

[linux-wifi-hotspot ](https://github.com/lakinduakash/linux-wifi-hotspot)

### 特点

+ 在任何频道创建AP(接入点).
+ 选择以下加密方式之一: WPA, WPA2, WPA/WPA2, 开放(无加密).
+ 隐藏你的SSID.
+ 禁用客户端之间的通信(客户端隔离).
+ 支持IEEE 802.11n & 802.11ac
+ 互联网共享方法. NATed或桥接或无(无互联网共享).
+ 选择AP网关IP(仅适用于 "NATed "和 "None "互联网共享方法).
+ 你可以用你的互联网连接的相同接口创建一个AP.
+ 你可以通过管道或通过参数传递你的SSID和密码(见例子).

![gui](https://github.com/lakinduakash/linux-wifi-hotspot/raw/master/docs/sc4.png)

### 依赖

一般

+ bash (用于运行这个脚本)
+ util-linux (用于getopt)
+ procps 或 procps-ng
+ hostapd
+ iproute2
+ iw
+ iwconfig(只有在 "iw "不能识别你的适配器时才需要这个).
+ haveged (可选)

对于 "NATed "或 "None "互联网共享方法

+ dnsmasq
+ iptables

### 安装

+ 通用的

```bash
git clone https://github.com/lakinduakash/linux-wifi-hotspot
cd linux-wifi-hotspot/src/scripts
make install
```

+ ArchLinux

```bash
pacman -S create_ap
```

+ Gentoo

```bash
emerge layman
layman -f -a jorgicio
emerge net-wireless/create_ap
```

### 例子

```bash
create_ap [options] <wifi-接口>    [<通网的接口>]     [<access-point-name>]   [<passphrase>]
```

+ 没有口令(开放网络):

```bash
create_ap wlan0 eth0 MyAccessPoint
```

+ WPA + WPA2口令:

```bash
create_ap wlan0 eth0 MyAccessPoint MyPassPhrase
```

+ AP, 没有互联网共享:

```bash
create_ap -n wlan0 MyAccessPoint MyPassPhrase
```

+ 桥接式互联网共享:

    create_ap -m bridge wlan0 eth0 MyAccessPoint MyPassPhrase

+ 桥接互联网共享(预先配置的桥接接口):

    create_ap -m bridge wlan0 br0 MyAccessPoint MyPassPhrase

+ 从同一WiFi接口共享互联网.

    create_ap wlan0 wlan0 MyAccessPoint MyPassPhrase

+ 选择一个不同的WiFi适配器驱动程序

    create_ap --driver rtl871xdrv wlan0 eth0 MyAccessPoint MyPassPhrase

+ 没有密码(开放网络)使用管道.

    echo -e "MyAccessPoint" | create_ap wlan0 eth0

+ WPA + WPA2口令使用管道.

    echo -e "MyAccessPoint\nMyPassPhrase" | create_ap wlan0 eth0

+ 启用IEEE 802.11n

    create_ap --ieee80211n --ht_capab '[HT40+]' wlan0 eth0 MyAccessPoint MyPassPhrase

+ 客户端隔离.

    create_ap --isolate-clients wlan0 eth0 MyAccessPoint MyPassPhrase

+ Systemd服务

使用[持久化的systemd][]服务

+ 立即启动服务:

    systemctl start create_ap

+ 启动时启动:

    systemctl enable create_ap

[持久化的systemd]: https://wiki.archlinux.org/index.php/systemd#Basic_systemctl_usage

### 许可证

FreeBSD

- Copyright (c) 2013, oblique
- Copyright (c) 2019, lakinduakash
