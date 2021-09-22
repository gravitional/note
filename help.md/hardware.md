# 硬件

## 内存

To note something on memory stick or memory die

## DP HDMI type-C Thunderbolt

### 接口

[关于DP和HDMI的一些总结](https://zhuanlan.zhihu.com/p/41594596)
[HDMI, DP, DVI, VGA哪个更好](https://zhuanlan.zhihu.com/p/215228683)

`USB`: Universal Serial Bus. 通用串行总线,是一个外部总线标准, 用于规范电脑与外部设备的连接和通讯, 是应用在PC领域的接口技术.
由 Intel, Compaq, Digital, IBM, Microsoft, NEC及Northern Telecom 等计算机公司和通信公司于1995年联合制定, 并逐渐形成了行业标准.

`DP`: DisplayPort, 是一个由PC及芯片制造商联盟开发, 视频电子标准协会(VESA)标准化的数字式视频接口标准.

`HDMI`: High Definition Multimedia Interface, 高清多媒体接口. 是一种全数字化视频和声音发送接口, 可以发送未压缩的音频及视频信号.
HDMI组织的发起者包括各大消费电子产品制造商, 如日立制作所, 松下电器, Quasar, 飞利浦, 索尼, 汤姆生RCA, 东芝, Silicon Image.

分辨率与帧率对带宽的大致需求

|分辨率| 色彩深度| 刷新率(Hz)| 所需传输速率 |
| --- | --- | --- | --- |
| `1920x1080` | `8-bit`     |        `60`      |  `3.20 Gbps` |
| `1920x1080` | `10-bit`    |     `60`      |  `4.00 Gbps` |
| `1920x1080` | `8-bit`     |       `144`    |  `8.00 Gbps` |
| `1920x1080` | `10-bit`    |     `144`   |  `10.00 Gbps` |
| `2560x1440` | `8-bit`     |        `60`     |  `5.63  Gbps` |
| `2560x1440` | `10-bit`    |      `60`     |  `7.04 Gbps` |
| `2560x1440` | `8-bit`     |        `144`   |  `14.08 Gbps` |
| `2560x1440` | `10-bit`    |      `144`  |  `17.60 Gbps` |
| `3840x2160` | `8-bit`     |        `60`    |  `12.54  Gbps` |
| `3840x2160` | `10-bit`    |      `60`    |  `15.68 Gbps` |
| `3840x2160` | `8-bit`     |        `144`   |  `31.35 Gbps` |
| `3840x2160` | `10-bit`    |      `144`  |  `39.19 Gbps` |

DisplayPort

| DisplayPort 版本 | 最大传输速率 | 分辨率和刷新率 |
| --- | --- |---  |
| `1.0-1.1a` |     `8.64 Gbps`           |   `1080p -144 Hz, 4k-30 Hz` |
| `1.2-1.2a` |     `17.28 Gbps`         |   `1080p -240 Hz, 4k-75 Hz, 5k-30 Hz` |
| `1.3`            |    `25.92 Gpbs`         |   `1080p -360 Hz, 4k-120 Hz, 5k-60 Hz, 8k-30 Hz` |
| `1.4-1.4a` |     `25.92 Gbps`         |   `8k-120 Hz w/DSC`  |
| `2`               |      `77.37 Gbps`        |    `4k-240 Hz, 8k-85 Hz` |

HDMI

| HDMI 版本 | 最大传输速率 | 分辨率和刷新率 |
| --- | --- |---  |
| `1.0-1.2a` |     `3.96 Gbps`         |        `1080p -60 Hz`   |
| `1.3-1.4b` |     `8.16 Gbps`         |       `1080p -144 Hz, 1440p-75 Hz, 4k-30 Hz, 4k 4:2:0-60 Hz` |
| `2.0-2.0b`  |    `14.4 Gpbs`         |       `1080p -240 Hz, 4k-60 Hz, 8k 4:2:0-30 Hz` |
| `2.1`             |   `42.6 Gbps`         |       `4k-144 Hz,4k-240 Hz w/DSC` |

DSC, 显示串流压缩技术, 能实现传输`3:1`压缩率的视频信号, 需显示器支持.

### Thunderbolt and type-C

[USB接口详细读解](https://www.bybusa.com/community/usb-interface-detailed-explanation)
[雷电3和USB Type C有什么区别](https://zhuanlan.zhihu.com/p/50034258)

`Thunderbolt 3`(雷电3)和`USB 3.1`是数据传输协议. 不同传输标准区别主要在于传输速率.

`USB3.1`的 `Gen2` 和 `Gen1` 指的是传输速率, `Gen2` 的理论传输速率是`10Gbps`, 而`Gen1`是`5Gbps`, 最新的`USB3.2 Gen2x2`则是`20Gbps`.

就拿`USB3.1`来说, 还分为`USB3.1 Gen1`和`USB3.1 Gen2`, `USB3.1 Gen1`最大传输速率为`5 Gb/s`, `USB3.1 Gen2`最大传输速率为`10 Gb/s`.
而`Type C`是一种接口规格, 比如形状, 此外还有`Type A`, `Type B`等其他接口规格.

雷电3和`USB Type C`虽然物理接口相同, 但对应不同的传输协议, 分别是 `Thunderbolt3` 和 `USB 3.1` .

现在越来越多的设备配备`USB Type C`接口, 但细心的用户会发现部分`USB Type C`仅支持充电和数据传输, 不支持视频信号传输.
那是因为`USB Type C`接口有多种协议, 所以并不是所有的`USB Type C`接口都功能齐全.
如果用户想将配备`USB Type C`接口的笔记本电脑直接连接到带雷电3或者`USB Type C`接口的显示器,
就要确保其笔记本电脑的`USB Type C`接口支持`DisplayPort Alt Mode`, 该`Alt Mode`允许原生的 DisplayPort 高清数字信号通过USB Type C传输.

雷电3提供更高的带宽, 使其能够支持两个4K显示器, 同时提供更快的数据传输速度.
此外, 与USB Type C相比, `Thunderbolt 3`还兼容更多传输协议, 这意味着它可以与更多类型的设备一起使用.

|                       | `USB 3.1 Gen2 Type C` | `Thunderbolt 3` |
| --- | --- |---  |
| 传输速度    |        `10 Gbps`    |    `40 Gbps`      |
| 分辨率        |        支持单`4k`    |    支持双`4k`      |
| 功率             |        `100w`    |    `100w`      |
| 协议             |        `DisplayPort, USB`    |   `DisplayPort, USB, ThunderBolt, PCI Express`      |

### USB-C

[只有5Gbps带宽的USB-C口是如何实现输出4k@60hz的dp信号的](https://www.zhihu.com/question/350209735/answer/1392624525)

`5Gbps`指的是SSTX或者SSRX其中一个的通道的速度. 因为`USB 3.1`数据是发送和接收同时进行的, 一个通道发送, 一个通道接收.
`USB-C`口描述的USB的速度就是发送的速度或者接收的速度, 它们是相等的, 都是`5Gbps`, 也就是一个通道的速度是`5Gbps`.

![USB-C 引脚](https://pic1.zhimg.com/80/v2-aba5b2244f455aed74c0d70ee39a4857_720w.jpg?source=1940ef5c)

`DP`信号`4K 60Hz`需要用到上图标注的`lane0`, `lane1`, `lane2`和`lane 3`这4路通道, 而且DP传输只有发送, 没有接收.
也就是说这4路通道同时传输DP信号, 而上面提到一个通道速度是`5Gbps`, `4`个就能传递`20Gbps`.

再来计算传递`4K 60Hz`的带宽, 分辨率取`4096*2160`, 像素深度取`24bpp`(RGB基本颜色的存储位数取`8bit`),
那么带宽是`4096*2160*24*60=12,740,198,400bps=11.865234375Gbps`, 采用的`8b10b`编码, 那么需要的总带宽为`11.865234375Gbs*10/8 = 14.83154296875Gbps`, 小于`20Gbps`.
单个通道速度为`14.83154296875Gbps/4=3.7078857421875Gbps`, 小于`5Gbps`. 所以`usb3.1 gen1`用上`4`个通道带宽是完全可以传`40K 60Hz`的.
`DP 1.2`的协议开始支持`4K 60Hz`, 总带宽为`21.6Gbps`, 采用的`8b10b`编码, 有效带宽为`21.6*8/10 = 17.28Gbps`.
通过和上面的计算的数值比较, 确实`DP 1.2`需要这样大的带宽. 还剩下部分带宽没有用是为了留有余量.

![DP带宽](https://pic2.zhimg.com/80/v2-09d7bec05e0d58ad53208fcca6514051_720w.jpg?source=1940ef5c)

### 差分线

[差分线(差分互连)基本原理及优缺点](https://www.jianshu.com/p/ae9277187836)

为了更快的传输数据, 我们能想到的办法除了一次多传输几位数据(增加并行总线的数量)之外, 还有一种办法就是提高单通道的数据传输速率, 然而随着单通道速率的提升, 信号完整性问题又会变得越来越突出, 尤其是串扰以及损耗等问题. 为了解决这些问题, 一种全新的数据传输方式应运而生, 就是--差分(差分线, 差分互联). 差分线的主要特点是采样电压取两根电压的差值, 它的优点有：

差分驱动总的$d I/d t$会比单端信号线上大幅降低, 从而减小了潜在的电磁干扰(EMI).
差分信号的值很大程度上与"地"的精确值无关, 能很好的抵抗电源的干扰. 差分信号关注的是两根线之间的电压差值, 与电势零点的高度关系不大.
差分对内每根信号都有自己的返回路径, 能够减轻信号跨分割带来的影响.
