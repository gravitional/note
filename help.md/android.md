# Android.md

## Android 调试桥

[Android 调试桥 (adb)](https://developer.android.google.cn/studio/command-line/adb?hl=zh-cn)

Android 调试桥 (`adb`) 是一种功能多样的命令行工具, 可让您与设备进行通信.
`adb` 命令可用于执行各种设备操作(例如安装和调试应用), 并提供对 `Unix shell`(可用来在设备上运行各种命令)的访问权限.
它是一种客户端-服务器程序, 包括以下三个组件：

+ 客户端：用于发送命令. 客户端在开发机器上运行. 您可以通过发出 `adb` 命令从命令行终端调用客户端.
+ 守护程序 (`adbd`)：用于在设备上运行命令. 守护程序在每个设备上作为后台进程运行.
+ 服务器：用于管理客户端与守护程序之间的通信. 服务器在开发机器上作为后台进程运行.

`adb` 包含在 `Android SDK` 平台工具软件包中. 您可以使用 `SDK 管理器`下载此软件包, 该管理器会将其安装在 `android_sdk/platform-tools/` 下.
或者, 如果您需要独立的 `Android SDK` 平台工具软件包, 也可以[点击此处进行下载](https://developer.android.google.cn/studio/releases/platform-tools?hl=zh-cn).

如需了解如何连接设备以使用 `ADB`, 包括如何使用 Connection Assistant 对常见问题进行排查, 请参阅在[硬件设备上运行应用](https://developer.android.google.cn/studio/run/device?hl=zh-cn).

### 针对开发设置设备

您必须先决定要使用 `USB` 线还是 `WLAN` 连接设备, 才能在设备上开始调试. 然后执行以下操作：

在设备上, 打开设置应用, 选择开发者选项, 然后启用 USB 调试(如果有).
注意：如果您未看到开发者选项, 请按照相关说明启用开发者选项.

设置系统以检测设备.

+ `Chrome` 操作系统：无需其他配置.
+ `macOS`：无需其他配置.
+ `Ubuntu Linux`：需要正确进行两项设置：希望使用 `adb` 的每个用户都需要位于 `plugdev` 组中, 并且需要为系统添加涵盖设备的 `udev` 规则.

+ `plugdev` 组：如果您看到一条错误消息, 指出您不在 `plugdev` 组中, 您需要将自己添加到 `plugdev` 组中.

```bash
sudo usermod -aG plugdev $LOGNAME
```

请注意, 组只会在您登录时更新, 因此您需要退出后重新登录, 此更改才能生效. 当您重新登录后, 可以使用 `id` 检查自己现在是否已在 `plugdev` 组中.

+ `udev` 规则：`android-sdk-platform-tools-common` 软件包中包含一组适用于 `Android` 设备并由社区维护的默认 `udev` 规则. 请使用以下命令添加这些规则：

```bash
apt-get install android-sdk-platform-tools-common
```

+ `Windows`：为 `ADB` 安装 `USB` 驱动程序(如适用). 如需安装指南和 `OEM` 驱动程序的链接, 请参阅安装[OEM USB 驱动程序文档](https://developer.android.google.cn/studio/run/oem-usb?hl=zh-cn).

### adb 的工作原理

当您启动某个 `adb` 客户端时, 该客户端会先检查是否有 `adb` 服务器进程正在运行.
如果没有, 它会启动服务器进程. 服务器在启动后会与本地 `TCP` 端口 `5037` 绑定, 并监听 `adb` 客户端发出的命令 - 所有 `adb` 客户端均通过端口 `5037` 与 `adb` 服务器通信.

然后, 服务器会与所有正在运行的设备建立连接. 它通过扫描 `5555` 到 `5585` 之间(该范围供前 `16` 个模拟器使用)的奇数号端口查找模拟器.
服务器一旦发现 `adb` 守护程序 (`adbd`), 便会与相应的端口建立连接.
请注意, 每个模拟器都使用一对按顺序排列的端口 - 用于控制台连接的`偶数号`端口和用于 `adb` 连接的`奇数号`端口. 例如：

+ 模拟器 1, 控制台：5554
+ 模拟器 1, adb：5555
+ 模拟器 2, 控制台：5556
+ 模拟器 2, adb：5557

依此类推

如上所示, 在端口 `5555` 处与 `adb` 连接的模拟器与控制台监听端口为 `5554` 的模拟器是同一个.

服务器与所有设备均建立连接后, 您便可以使用 `adb` 命令访问这些设备.
由于服务器管理与设备的连接, 并处理来自多个 `adb` 客户端的命令, 因此您可以从任意客户端(或从某个脚本)控制任意设备.

### 在设备上启用 adb 调试

如要在通过 `USB` 连接的设备上使用 `adb`, 您必须在设备的系统设置中启用 `USB` 调试(位于开发者选项下). 如需在通过 `WLAN` 连接的设备上使用 `adb`, 请参阅通过 WLAN 连接到设备.

在搭载 Android 4.2 及更高版本的设备上, `开发者选项`屏幕默认情况下处于隐藏状态. 如需显示开发者选项, 请依次转到`设置 > 关于手机`, 然后点按 `build 号`七次.
返回上一屏幕, 在底部可以找到`开发者选项`.
在某些设备上, `开发者选项`屏幕所在的位置或名称可能有所不同.

现在, 您已经可以通过 `USB` 连接设备. 您可以通过从 `android_sdk/platform-tools/` 目录执行 `adb devices` 验证设备是否已连接. 如果已连接, 您将看到设备名称以`设备`形式列出.

#### 通过 Wi-Fi 连接到设备(Android 11 及更高版本)

[ADB网络调试和常见命令](https://www.jianshu.com/p/2d256f338634)

`Android 11` 及更高版本支持使用 `Android 调试桥 (adb)` 从工作站以无线方式部署和调试应用.
例如, 您可以将可调试应用部署到多台远程设备, 而无需通过 `USB` 实际连接设备. 这样就可以避免常见的 `USB` 连接问题, 例如驱动程序安装方面的问题.

如需使用无线调试, 您需要使用配对码将您的设备与工作站配对. 您的工作站和设备必须连接到同一无线网络. 如需连接到您的设备, 请按以下步骤操作：

+ 在您的工作站上, 更新到最新版本的 `SDK 平台工具`.
+ 在设备上启用`开发者选项`.
+ 启用`无线调试`选项.
+ 在询问`要允许通过此网络进行无线调试吗？`的对话框中, 点击`允许`.
+ 选择使用`配对码配对设备`. 记下设备上显示的`配对码`, `IP 地址`和`端口号`.
+ 在工作站上, 打开一个终端并导航到 `android_sdk/platform-tools`.
+ 运行 `adb pair ipaddr:port`.  使用第 5 步中的 `IP 地址`和`端口号`.
+ 当系统提示时, 输入您在第 5 步中获得的配对码. 系统会显示一条消息, 表明您的设备已成功配对.
+ 仅适用于 `Linux` 或 `Windows`: 运行 `adb connect ipaddr:port`. 使用`无线调试`下的 IP 地址和端口.

*****
重启后依然有效方法

在root权限下执行

```bash
adb shell su -c setprop service.adb.tcp.port 5555
```

*****
修改系统配置文件的方法

在`Android`的`/system/build.prop`文件最后添加`service.adb.tcp.port=5555`,
重启后有效, 需要root权限

*****
输入命令连接到设备

```bash
adb connect 10.0.0.102
```

### 查询设备

在发出 `adb` 命令之前, 了解哪些设备实例已连接到 `adb` 服务器会很有帮助. 您可以使用 `devices` 命令生成已连接设备的列表.

```bash
adb devices -l
```

作为回应, `adb` 会针对每个设备输出以下状态信息：

+ 序列号：由 `adb` 创建的字符串, 用于通过`端口号`唯一标识设备.  下面是一个序列号示例：`emulator-5554`
+ 状态：设备的连接状态可以是以下几项之一：
  + `offline`：设备未连接到 adb 或没有响应.
  + `device`：设备现已连接到 `adb` 服务器. 请注意, 此状态并不表示 `Android` 系统已完全启动并可正常运行, 因为在设备连接到 `adb` 时系统仍在启动. 不过, 在启动后, 这将是设备的正常运行状态.
  + no device：未连接任何设备.
 说明：如果您包含 -l 选项, devices 命令会告知您设备是什么. 当您连接了多个设备时, 此信息很有用, 可帮助您将它们区分开来.

以下示例展示了 `devices` 命令及其输出. 有三个设备正在运行. 列表中的前两行表示模拟器, 第三行表示连接到计算机的硬件设备.

```bash
$ adb devices
List of devices attached
emulator-5556 device product:sdk_google_phone_x86_64 model:Android_SDK_built_for_x86_64 device:generic_x86_64
emulator-5554 device product:sdk_google_phone_x86 model:Android_SDK_built_for_x86 device:generic_x86
0a388e93      device usb:1-1 product:razor model:Nexus_7 device:flo
```

### 模拟器未列出

`adb devices` 命令的极端命令序列会导致正在运行的模拟器不显示在 `adb devices` 输出中(即使在您的桌面上可以看到该模拟器). 当满足以下所有条件时, 就会发生这种情况：

+ adb 服务器未在运行,
+ 您在使用 `emulator` 命令时, 将 `-port` 或 `-ports` 选项的端口值设为 `5554` 到 `5584` 之间的奇数,
+ 您选择的奇数号端口处于空闲状态, 因此可以与指定端口号的端口建立连接, 或者该端口处于忙碌状态, 模拟器切换到了符合第 `2` 条中要求的另一个端口
+ 启动模拟器后才启动 `adb` 服务器.

避免出现这种情况的一种方法是让模拟器自行选择端口, 并且每次运行的模拟器数量不要超过 `16` 个. 另一种方法是始终先启动 `adb` 服务器, 然后再使用 `emulator` 命令, 如下例所示.

+ 示例 1：在下面的命令序列中, `adb devices` 命令启动了 `adb` 服务器, 但是设备列表未显示.

先停止 `adb` 服务器, 然后按照所示顺序输入以下命令. 对于 `avd` 名称, 请提供系统中有效的 `avd` 名称. 如需获取 `avd` 名称列表, 请输入 `emulator -list-avds`.
`emulator` 命令位于 `android_sdk/tools` 目录下.

```bash
$ adb kill-server
$ emulator -avd Nexus_6_API_25 -port 5555
$ adb devices
```

+ 示例 2：在下面的命令序列中, `adb devices` 显示了设备列表, 因为先启动了 `adb` 服务器.

如果想在 `adb devices` 输出中看到模拟器, 请停止 `adb` 服务器, 然后在使用 `emulator` 命令之后, 使用 `adb devices` 命令之前, 重新启动该服务器, 如下所示：

```bash
$ adb kill-server
$ emulator -avd Nexus_6_API_25 -port 5557
$ adb start-server
$ adb devices

List of devices attached
emulator-5557 device
```

如需详细了解模拟器命令行选项, 请参阅使用命令行参数.

### 将命令发送至特定设备

如果有多个设备在运行, 您在发出 `adb` 命令时必须指定目标设备. 为此, 请使用 `devices` 命令获取目标设备的序列号.
获得序列号后, 请结合使用 `-s` 选项与 `adb` 命令来指定序列号.
如果您要发出很多 `adb` 命令, 可以将 `$ANDROID_SERIAL` 环境变量设为包含序列号. 如果您同时使用 `-s` 和 `$ANDROID_SERIAL`, `-s` 会替换 `$ANDROID_SERIAL`.

在以下示例中, 先获得了已连接设备的列表, 然后使用其中一个设备的序列号在该设备上安装了 helloWorld.apk.

```bash
$ adb devices
List of devices attached
emulator-5554 device
emulator-5555 device

$ adb -s emulator-5555 install helloWorld.apk
```

注意：如果您在多个设备可用时发出命令但未指定目标设备, `adb` 会生成错误.

如果有多个可用设备, 但只有一个是模拟器, 请使用 `-e` 选项将命令发送至该模拟器. 同样, 如果有多个设备, 但只连接了一个硬件设备, 请使用 `-d` 选项将命令发送至该硬件设备.

### 安装应用

您可以使用 `adb` 的 `install` 命令在模拟器或连接的设备上安装 `APK`：

```powershell
adb install path_to_apk
```

安装`测试 APK` 时, 必须在 `install` 命令中使用 `-t` 选项. 如需了解详情, 请参阅 `-t`.

要详细了解如何创建可安装在模拟器/设备实例上的 APK 文件, 请参阅构建和运行应用.

请注意, 如果您使用的是 Android Studio, 则无需直接使用 adb 在模拟器/设备上安装您的应用. `Android Studio` 会为您执行应用的打包和安装操作.

### 设置端口转发

您可以使用 `forward` 命令设置任意端口转发, 将特定主机端口上的请求转发到设备上的其他端口. 以下示例设置了主机端口 `6100 `到设备端口 `7100` 的转发：

```bash
adb forward tcp:6100 tcp:7100
```

以下示例设置了主机端口 `6100` 到 `local:logd` 的转发：

```bash
adb forward tcp:6100 local:logd
```

### 将文件复制到设备/从设备复制文件

您可以使用 `pull` 和 `push` 命令将文件复制到设备或从设备复制文件.
与 `install` 命令(仅将 APK 文件复制到特定位置)不同, 使用 `pull` 和 `push` 命令可将任意目录和文件复制到设备中的任何位置.

如需从设备中复制某个文件或目录(及其子目录), 请使用以下命令：

```bash
adb pull remote local
```

如需将某个文件或目录(及其子目录)复制到设备, 请使用以下命令：

```bash
adb push local remote
```

将 `local` 和 `remote` 替换为`开发机器`(本地)和`设备`(远程)上的目标文件/目录的路径. 例如：

```bash
adb push foo.txt /sdcard/foo.txt
```

### 停止 adb 服务器

在某些情况下, 您可能需要终止 `adb` 服务器进程, 然后重启以解决问题(例如, 如果 `adb` 不响应命令).

如需停止 `adb` 服务器, 请使用 `adb kill-server` 命令. 然后, 您可以通过发出其他任何 `adb` 命令来重启服务器.

### 发出 adb 命令

您可以从开发机器上的命令行发出 `adb` 命令, 也可以通过脚本发出. 用法如下：

```bash
adb [-d | -e | -s serial_number] command
```

如果只有一个模拟器在运行或者只连接了一个设备, 系统会默认将 `adb` 命令发送至该设备.
如果有多个模拟器正在运行并且/或者连接了多个设备, 您需要使用 `-d`, `-e` 或 `-s` 选项指定应向其发送命令的目标设备.

您可以使用以下命令来查看所有支持的 `adb` 命令的详细列表：

```bash
adb --help
```

+ `push [--sync] [-z ALGORITHM] [-Z] LOCAL... REMOTE`: copy local files/directories to device
  + `--sync`: 只推送host上比设备新的文件.
  + `-n`: dry run: 模拟推送到设备, 但不实际存储到文件系统
  + `-z`: 使用特定算法压缩 (any, none, brotli)
  + `-Z`: 不使用压缩

+ `pull [-a] [-z ALGORITHM] [-Z] REMOTE... LOCAL`: copy files/dirs from device
  + `-a`: 保持文件的时间戳和mode
  + `-z`: 使用特定算法压缩 (any, none, brotli)
  + `-Z`: 不使用压缩

+ `sync [-l] [-z ALGORITHM] [-Z] [all|data|odm|oem|product|system|system_ext|vendor]`:
sync a local build from `$ANDROID_PRODUCT_OUT` to the device (default all)
  + `-n`: dry run: push files to device without storing to the filesystem
  + `-l`: 列出将要复制的文件, 但不进行复制
  + `-z`: 使用特定算法压缩 (any, none, brotli)
  + `-Z`: 不使用压缩

### 发出 shell 命令

您可以使用 `shell` 命令通过 `adb` 发出设备命令, 也可以启动交互式 `shell`. 如需发出单个命令, 请使用 `shell` 命令, 如下所示：

```bash
adb [-d |-e | -s serial_number] shell shell_command
```

要在设备上启动交互式 `shell`, 请使用 `shell` 命令, 如下所示：

```bash
adb [-d | -e | -s serial_number] shell
```

要退出交互式 `shell`, 请按 `Ctrl+D` 键或输入 `exit`. Android 提供了大多数常见的 `Unix` 命令行工具. 如需查看可用工具的列表, 请使用以下命令：

```bash
adb shell ls /system/bin
```

对于大多数命令, 都可通过 `--help` 参数获得命令帮助. 许多 `shell` 命令都由 `toybox` 提供. 对于所有 `toybox` 命令, 都可通过 `toybox --help` 可获得命令的常规帮助.

另请参阅 `Logcat` 命令行工具, 该工具对监控系统日志很有用.

***
默认情况下`/system`部分是只读的, 若需要读写`/system`, 执行`adb remount`将 `/system` 部分置于可写入的模式, 这个命令只适用于已被 `root` 的设备.

***
查看存储空间

```bash
adb shell df  # 查看存储空间
adb shell du -sh * # 查看文件大小
```

*****
进程详情

+ `VSS`,`Virtual Set Size`: 虚拟耗用内存(包含共享库占用的内存)
+ `RSS`,`Resident Set Size`: 实际使用物理内存(包含共享库占用的内存)
+ `PSS`, `Proportional Set Size`: 实际使用的物理内存(比例分配共享库占用的内存)
+ `USS`, `Unique Set Size`: 进程独自占用的物理内存(不包含共享库占用的内存)

*****
日志相关的命令

```bash
adb logcat | grep 620  #查看进程pid为620的日志信息:
adb logcat | grep "同步数据" # 过滤出有字符串同步数据的日志进行显示
adb logcat -s MainActivity #查看 Tag 为 MainActivity的日志信息
adb logcat MainActivity:V *:S # 查看 Tag 为 MainActivity 的, 日志等级不低于 V的日志信息
```

优先级是下面的字符, 顺序是从低到高：

+ `V`: 明细 verbose(最低优先级)
+ `D`: 调试 debug
+ `I`: 信息 info
+ `W`: 警告 warn
+ `E`: 错误 error
+ `F`: 严重错误 fatal
+ `S`: 无记载 silent

将日志输出到文件, 即重定向,

```bash
adb logcat MainActivity:V *:S  >> ~/Desktop/AtestLog.txt
```

### 调用 Activity 管理器 (am)

在 `adb shell` 中, 您可以使用 `Activity` 管理器 (am) 工具发出命令以执行各种系统操作, 如启动 `Activity`, 强行停止进程, 广播 intent, 修改设备屏幕属性, 等等.
在 `shell` 中, 相应的语法为：

```bash
am command
```

您也可以直接从 `adb` 发出 `Activity` 管理器命令, 无需进入远程 `shell`. 例如：

```bash
adb shell am start -a android.intent.action.VIEW
```

### 调用软件包管理器 (pm)

在 `adb shell` 中, 您可以使用软件包管理器 (pm) 工具发出命令, 以对设备上安装的应用软件包执行操作和查询. 在 `shell` 中, 相应的语法为：

```bash
pm command
```

您也可以直接从 `adb` 发出软件包管理器命令, 无需进入远程 `shell`. 例如：

```bash
adb shell pm uninstall com.example.MyApp
```

+ `adb shell pm list packages`: 查看所有安装包
+ `adb install test.apk`: 安装应用
+ `adb install -r test.apk`: 保留数据和缓存文件, 重新安装, 升级
+ `adb uninstall test.apk`: 卸载应用
+ `adb uninstall -k cnblogs.apk`: 卸载 `app` 但保留数据和缓存文件

### 截取屏幕截图

`screencap` 命令是一个用于对设备显示屏截取屏幕截图的 `shell` 实用程序. 在 `shell` 中, 语法如下：

```bash
screencap filename
```

如需从命令行使用 `screencap`, 请输入以下命令：

```bash
adb shell screencap /sdcard/screen.png
```

以下屏幕截图会话示例展示了如何使用 `adb shell` 截取屏幕截图, 以及如何使用 `pull` 命令从设备下载屏幕截图文件：

```bash
$ adb shell
shell@ $ screencap /sdcard/screen.png
shell@ $ exit
$ adb pull /sdcard/screen.png
```

### 录制视频

`screenrecord` 命令是一个用于录制设备(搭载 `Android 4.4`(API 级别 19)及更高版本)显示屏的 `shell` 实用程序.
该实用程序将屏幕 `Activity` 录制为 `MPEG-4` 文件. 您可以使用此文件创建宣传视频或培训视频, 或将其用于调试或测试.

在 `shell` 中, 使用以下语法：

```bash
screenrecord [options] filename
```

如需从命令行使用 `screenrecord`, 请输入以下命令：

```bash
adb shell screenrecord /sdcard/demo.mp4
```

按 `Ctrl + C` 键(在 `Mac` 上, 按 `Command + C` 键)可停止屏幕录制；如果不手动停止, 到三分钟或 `--time-limit` 设置的时间限制时, 录制将会自动停止.

如需开始录制设备屏幕, 请运行 `screenrecord` 命令以录制视频. 然后, 运行 `pull` 命令以将视频从设备下载到主机. 下面是一个录制会话示例：

```bash
$ adb shell
shell@ $ screenrecord --verbose /sdcard/demo.mp4
(press Control + C to stop)
shell@ $ exit
$ adb pull /sdcard/demo.mp4
```

`screenrecord` 实用程序能以您要求的任何支持的分辨率和比特率进行录制, 同时保持设备显示屏的宽高比. 默认情况下, 该实用程序以本机显示分辨率和屏幕方向进行录制, 时长不超过三分钟.

`screenrecord` 实用程序的局限性：

+ 音频不与视频文件一起录制.
+ 无法在搭载 `Wear OS` 的设备上录制视频.
+ 某些设备可能无法以它们的本机显示分辨率进行录制. 如果在录制屏幕时出现问题, 请尝试使用较低的屏幕分辨率.
+ 不支持在录制时旋转屏幕. 如果在录制期间屏幕发生了旋转, 则部分屏幕内容在录制时将被切断.

表 5. screenrecord 选项
选项   说明

+ `--help`: 显示命令语法和选项
+ `--size widthxheight`: 设置视频大小：`1280x720`. 默认值为设备的本机显示屏分辨率(如果支持)；如果不支持, 则为 `1280x720`. 为获得最佳效果, 请使用设备的 `Advanced Video Coding (AVC)` 编码器支持的大小.
+ `--bit-rate rate`: 设置视频的视频比特率(以 `MB/秒`为单位). 默认值为 `4Mbps`. 您可以增加比特率以提升视频品质, 但这样做会导致视频文件变大. 下面的示例将录制比特率设为 `6Mbps`：

```bash
screenrecord --bit-rate 6000000 /sdcard/demo.mp4
```

+ `--time-limit time`: 设置最大录制时长(以`秒`为单位). 默认值和最大值均为 `180`(`3 分钟`).
+ `--rotate`   将输出旋转 `90` 度. 此功能处于实验阶段.
+ `--verbose`:  在命令行屏幕显示日志信息. 如果您不设置此选项, 则该实用程序在运行时不会显示任何信息.

## fastboot root magisk

[使用fastboot命令刷机流程详解](https://blog.csdn.net/s13383754499/article/details/82755012)

### Root magisk

一些介绍性的文字参考:

[sspai 从零开始安装 Magisk ](https://sspai.com/post/67932)
[Magisk 初识与安装 ](https://sspai.com/post/53043)

安装教程:

[小米11刷机记录 ](https://www.himiku.com/archives/xiaomi-11.html)
[小米11/安卓11安装面具可过银行Root检测面具篇](https://www.coolapk.com/feed/24371534?shareKey=YTA2MmVhZDc2ZGQxNjAzNGM2NGU~&shareUid=481024&shareFrom=com.coolapk.market_11.0.1)

***
刷入`Magisk`的流程:

+ 充电, 备份数据, 安装驱动, 参阅安装[OEM USB 驱动程序文档](https://developer.android.google.cn/studio/run/oem-usb?hl=zh-cn).
+ 解锁`bootloader`, 搜索相关关键词即有官网教程.
+ 用`Magisk`对启动镜像进行修补.首先, 你需要下载手机的`rom`.

对于`MIUI`来说. 如果下载的是线刷包, 可以直接解压, 就能得到系统的各部分镜像.
下载地址比如: [XiaomiROM.com](https://xiaomirom.com/), [小米社区](https://www.xiaomi.cn/board/myBoard)
我们主要使用`boot.img`文件.

如果是卡刷包, 以`.zip`结尾的, 比如从手机系统更新目录, 例如`/storage/0/emulated/Download/downloaded_rom`找到的.
可以用相应的解包程序: [payload_dumper](https://github.com/vm03/payload_dumper) 来获取系统镜像.
配置好依赖之后, 运行下面的命令

``` bash
python payload_dumper.py payload.bin
```

就可以提取出`payload.bin`文件中的镜像到你所在的输出文件夹.

+ 把`boot.img`推送到手机上, 下载并安装最新的`Magisk`应用程序, 打开`Magisk`, 进入`安装>安装>修补启动镜像文件`.
`Magisk`将修补 `boot` 镜像, 并将其存储在你的手机上. 把这个文件复制到电脑上, 重命名为`boot_patched.img`

***

+ 刷入打过补丁的启动镜像: 将你的手机重启到`fastboot`, 连接到电脑上, 从命令行窗口如`powershell`, 输入以下命令.

```bash
# 如果你的手机没有A/B分区系统
fastboot flash boot boot_patched.img
# 如果你的手机有A/B分区系统
fastboot flash boot_a boot_patched.img
fastboot flash boot_b boot_patched.img
```

刷入完成之后, 重启设备.

#### 后续系统更新

之后的更新系统, 就不用像第一次这么繁琐地提取 `boot.img` 打补丁再刷入系统之中了.
只是需要在更新新版本时, 选择`下载完整更新包`. 然后等待更新包写入系统后, 提示`重启完成更新`后,
不要急着更新, 这时候打开 `Magisk`, 选择`安装到未使用的槽位(OTA 后)`, 点击确定.
等待刷入完成. 重启系统, 就更新完了.

或者, 不用下载完整包也可以. 在下载增量包前, 先在 `magisk` 里点击`卸载 Magisk`, 选择`还原原厂镜像`.
然后再`增量更新`. 更新完成后, 不要重启, 和上面一样, 安装 `Magisk`, 选择`安装到未使用的槽位(OTA 后)`, 再重启系统.

#### 一些问题

+ SafetyNet 无法通过

刷了 `Magisk` 后, `Safe­tyNet` 检测就无法通过. 无法通过的话, `Google Play` 保证机制就无法通过, 有些游戏就下载不了.

解决方法是：使用下面这两个模块

+ [safetynet-fix](https://github.com/kdrag0n/safetynet-fix)
+ [XiaomiCTSPass](https://github.com/yanbuyu/XiaomiCTSPass)

在 `magisk` 中刷入, 重启. 就可以欺骗系统通过这一检测, 使证明通过.

Play 商店的话, 清除数据就行. 注意开启 `magisk hide`.

+ `Magisk` 模块出现问题导致手机卡死

[Magisk Frequently Asked Questions](https://topjohnwu.github.io/Magisk/faq.html).
[Magisk 核心功能和翻车自救指南 ](https://sspai.com/post/68071)

安装模块后手机出现应用闪退, 显示错误和无法开机等种种异常, 大概率是使用的某个模块出了问题. 针对不同情况, 目前有五种比较简单的自救方式：

+ 如果还能访问 `Magisk App`, 在模块界面中, `移除` 出问题的模块, 然后重启以完全卸载此模块.
+ 如果已经无法打开 `Magisk App`, 但能使用 `adb`, 用数据线连接电脑, 输入 `adb wait-for-device shell magisk --remove-modules`, 这会删除所有模块并重启.
+ 如果连 `adb` 都不能访问, 那么请尝试进入安全模式. 不同机型安全模式启用方式不同：开机状态下, 比较常见的启用方式是按住电源键呼出的重启按钮；
关机状态下, 一般能通过特定的实体按键激活. 进入安全模式后, `Magisk` 会自动禁用所有模块. 再重启一次, 模块禁用的状态会被保留, 设备应当能恢复正常.
在开机出logo之后,长按 `volume减`(别撒手)可以进入安全模式, 适用于循环引导的情况.
+ 如果装有第三方 `Recovery`, 利用 `Recovey` 的文件管理功能(`高级` > `文件管理`), 定位到`/data/adb/modules`, 将其中的问题模块重命名为`remove`,
Magisk` 会在重启时自动卸载该模块. 更简单粗暴的方式是直接删除该目录下问题模块文件.

一定要注意开启 `USB` 调试, 否则只能靠刷回原厂镜像才能解决.

#### 补充

[秋叶随风](https://www.coolapk.com/feed/21534499?shareKey=OGUxNGU1NDdlNjA4NjAyMjZmNWU~)
[小米11刷机记录 ](https://www.himiku.com/archives/xiaomi-11.html)

在 `Android 11` 版本, Google 使用了名为 `vir­tual A/​B` 的新分区, 并将其率先使用在 Google Pixel 5 上. `vir­tual A/​B` 更新系统的方法与 `A/​B` 类似, 核心功能也相同.
在系统进行 `OTA` 更新时, 如果更新失败会自动回滚到上一个系统, 成功开机进入之后才会将新数据写入到系统中.
且系统更新可在待机状态下完成, 无需重启等待新数据写入, 可谓是极大地改善了更新系统时的用户体验.
只是 `A/​B` 需要准备两个相同的分区, 会占用更多的存储空间, 而 `vir­tual A/​B` 删除了 `cache` 和`re­cov­ery` 分区, `sys­tem` 分区只保留一个, 以解决上述空间占用问题.

小米 11 也是使用的 `vir­tual A/​B`, 可这对我来说并不是什么好消息. 因为`vir­tual A/​B`删除了`re­cov­ery`分区, 没有了`re­cov­ery` 分区就不能刷入第三方`re­cov­ery`.
因此在 `TWRP` 官方解决为这类新分区方式刷入第三方 `re­cov­ery` 之前, 我只能使用其他方法来为小米 11 刷机了.

***
名词解释: [设备端OTA升级](https://help.aliyun.com/document_detail/85700.html).
`OTA`(Over-the-Air Technology)即空中下载技术. 物联网平台支持通过OTA方式进行设备升级.

***
[A/B 无缝系统更新](https://source.android.google.cn/devices/tech/ota/ab?hl=zh-cn)
[分区布局](https://source.android.google.cn/devices/bootloader/system-as-root?hl=zh-cn#sar-partitioning)
[动态分区](https://source.android.google.cn/devices/tech/ota/dynamic_partitions?hl=zh-cn)

动态分区是 `Android` 的用户空间分区系统. 使用此分区系统, 您可以在无线下载 (OTA) 更新期间创建, 销毁分区或者调整分区大小.
借助动态分区, 供应商无需担心各个分区(例如 `system`, `vendor` 和 `product`)的大小.
取而代之的是, 设备分配一个 `super` 分区, 其中的子分区可动态地调整大小. 单个分区映像不再需要为将来的 `OTA` 预留空间. 相反, `super` 中剩余的可用空间还可用于所有动态分区.

+ `hboot` -- 系统开机引导类似电脑, 这块刷错手机就会变成砖
+ `radio` -- 通讯模块, 基带, `WIFI`, `Bluetooth`等衔接硬件的驱动软件
+ `recovery` -- 系统故障时负责恢复
+ `boot` -- Linux嵌入式系统内核
+ `system` -- 系统文件, 应用
+ `cache` -- 系统运行时产生的缓存
+ `userdata` -- 用户使用`APP`产生的缓存数据

### Fastboot是什么

+ 卡刷:
在系统进行定制时, 编译系统会编译出一份`ZIP`的压缩包, 里面是一些系统分区镜像, 提供给客户进行手动升级, 恢复系统.
需要提前将压缩包内置`Sdcard`, 在`Recovery`模式进行. 进入`Recovery`方法：将手机完全关机后, 按住音量键下(上)+电源键, 进入`BootLoader`界面.
用音量加减来控制光标, 电源键来进行确认(有的机器只能用音量下键进行选择, 上键是确认键). 说明：有的机器可能没有预装`Recovery`.

+ 线刷:
在安卓手机中`Fastboot`是一种比`Recovery`更底层的刷机模式. 使用`USB`数据线连接手机的一种刷机模式.
这就是所谓的线刷, 与`Recovery`模式相比`Fastboot`需要掌握一些刷机命令, 对于某些系统来说, 线刷更可靠, 安全.

+ `Fastboot` 环境搭建: 参考上文`Android SDK`工具`platform-tools`.
进入这个目录下就可以使用`adb`, `fastboot`命令了, 也可以先配置环境变量就不用每次这么麻烦.

### adb fastboot

#### adb

全局选项:

+ `-a`: 指示`adb`在所有接口上监听一个连接.
+ `-d`: 将命令引向唯一连接的`USB`设备.  如果有一个以上的`USB`设备, 则返回错误.
+ `-e`: 将命令指向唯一运行的仿真器.  如果有一个以上的仿真器在运行, 返回错误.
+ `-s specific device`:将命令指向具有指定`serial number`(序列号)或`qualifier`(限定符)的设备或仿真器.  覆盖`ANDROID_SERIAL`环境变量.
+ `-p 产品名称或路径`: 简单的产品名称, 如`sooner`, 或相对/绝对路径, 如`out/target/product/sooner`, 指向产品输出目录.
+ 如果没有指定`-p`, 则使用`ANDROID_PRODUCT_OUT`环境变量, 它必须是一个绝对路径.
+ `-H`: `adb`服务器主机的名称(默认：`localhost`).
+ `-P`: `adb`服务器的端口 (默认: `5037`)

***

+ `adb devices`: 查看手机是否连接上
+ `adb start-server`: 确保有一个adb 服务器在运行
+ `adb kill-server`: | 如果adb 服务器在运行, 就停止它
+ `adb reboot bootloader`: 将手机重启到`fastboot`模式
+ `adb reboot [bootloader|recovery]` : 重启设备, 进入 `bootloader` 或者 `recovery`模式
+ `adb root`: 以 `root` 权限重启 `abhd` 守护进程
+ `adb pull 文件路径 保存路径`: 复制手机文件到电脑
+ `adb push 文件路径 保存路径`:  复制文件到手机中

+ `adb wait-for-device` : 阻塞, 直到设备上线
+ `adb get-state`: 打印 `offline` | `bootloader` | `device`
+ `adb get-serialno`: 打印 `<serial-number>`
+ `adb get-devpath`: 打印 `<device-path>`
+ `adb status-window`: 连续打印一个指定设备的设备状态
+ `adb remount`: 重新挂载设备的`/system` 和 `/vendor`(如果存在)分区, 可读写.
+ `adb usb`: 重新启动监听USB的`adbd`守护进程.
+ `adb tcpip <port>` 重新启动`adbd`守护程序, 该程序通过`TCP`方式监听指定端口.

+ `adb ppp <tty> [parameters]`:
通过`USB'运行`PPP'. 注意：你不应该自动启动`PPP`连接. `<tty>`指的是`PPP`流的`tty`. 例如：`dev:/dev/omap_csmi_tty1`.
`[parameters]`: 例如, `defaultroute`, `debug`, `dump`, `local`, `notty`, `usepeerdns`

+ `adb sync`:
`adb sync  [ <directory> ] <localdir>`可以用几种方式解释.
如果没有指定`<directory>`, `/system`, `/vendor`(如果存在)和`/data`分区将被更新.
如果是`system`, `vendor`(供应商), 或`data`, 只有相应的分区被更新.

*****
环境变量.

`ADB_TRACE`: 打印调试信息. 一个逗号分隔的列表, 取值可以为:  `all`, `sockets`, `packets`, `rwx`, `usb`, `sync`, `sysdeps`, `transport`, `jdwp`
`ANDROID_SERIAL`: 要连接的序列号. 如果给定了`-s`, 则优先于此.
`ANDROID_LOG_TAGS`:  当与`logcat`选项一起使用时, 只打印这些 `debug tags`.

#### fastboot

+ `fastboot devices`: 查看`Fastboot`模式下连接的设备
+ `fastboot  flashing  unlock`: 设备解锁 `bootloader`

+ `fastboot flash <partition> [filename]`: 将文件写入到某个分区中.
+ `fastboot  flash  boot  boot.img`:  刷入 `boot` 分区, 如果修改了`kernel` 代码, 则应该刷入此分区以生效
+ `fastboot  flash  system  system.img` | 刷入 `system` 分区. 如果修改的代码会影响 `out/system/` 路径下生成的文件, 则应该刷入此分区以生效
+ `fastboot  flash  recovery  recovery.img` | 刷入 `recovery` 分区
+ `fastboot  flash  country  country.img` | 刷入 `country` 分区. 这个分区是开发组自己划分的, 别的 `Android` 设备上不一定有
+ `fastboot flash radio radio.img`

+ `fastboot boot <kernel> [ramdisk [second]]`: 下载并启动`kernel`.

***
擦除会将该分区恢复到使用前状态

```bash
fastboot erase system
fastboot erase boot
...
```

***
[将 fastboot 移至用户空间](https://source.android.google.cn/devices/bootloader/fastbootd)

```bash
fastboot flash recovery twrp-xxx.img  #将 twrp 刷入到设备上
fastboot boot twrp-xxx.img
fastboot oem reboot-recovery
```

`oem <command>`: 由原始设备制造商 (OEM) 定义的命令.
某些命令具有不同的行为, 具体取决于是`bootloader`(引导加载程序)还是 `fastbootd` 在执行这些命令. 引导加载程序应支持以下命令.

#### 命令执行流程

首先需要准备好刷机包, 可以是自己编译的, 也可以是从别处拷贝的, 但一定要确保刷机包适用于你的 `Android` 设备.
然后解压刷机包, 解压后我们可以得到 `boot.img`, `recovery.img`, `system.img`, `bootloader` 文件, 正是这些文件构成了 `Android` 设备的系统.

线刷需要让设备进入 `fastboot` 环境. 有`2`种方法：

+ 执行命令 `adb  reboot  fastboot`
+ 或者同时按住 `增加音量` 和 `电源` 键开机.

在设备进入到 `fastboot` 环境后, 根据具体需求,执行下面的命令进行刷机：

```bash
fastboot  flashing  unlock # 设备解锁, 开始刷机
fastboot  flash  boot  boot.img  # 刷入 boot 分区, 如果修改了 kernel 代码, 则应该刷入此分区以生效
fastboot  flash  recovery  recovery.img  # 刷入 recovery 分区
fastboot  flash  country  country.img   # 刷入 country 分区. 这个分区是开发组自己划分的, 别的 Android 设备上不一定有
fastboot  flash  system system.img    # 刷入 system 分区. 如果修改的代码会影响  out/system/ 路径下生成的文件, 则应该刷入此分区以生效
fastboot  flash  bootloader  bootloader    # 刷入 bootloader
fastboot  erase  frp    # 擦除 frp 分区, frp 即 Factory Reset Protection, 用于防止用户信息在手机丢失后外泄
fastboot  format  data    # 格式化 data 分区
fastboot  flashing lock    # 设备上锁, 刷机完毕
fastboot  continue    # 自动重启设备
```

### sideload 刷机

[使用adb sideload线刷ROM的方法](https://www.cnblogs.com/godfeer/p/12029476.html)

+ 第一步, 解锁引导程序. 访问小米的官方解锁网站并申请解锁权限. 等待直到获得批准, 这可能需要几天的时间. 强烈建议在进行解锁之前, 在设备上安装最新的官方MIUI开发软件包.
+ 第二步, 安装`TWRP`.

按照上面的教程, 刷入`twrp.img`:

```bash
fastboot flash recovery twrp.img
```

在某些设备上, 可能需要接着执行以下命令:

```bash
fastboot boot twrp.img
```

当显示`OKEY`说明安装成功. 接着输入以下命令进入`TWRP`界面：

```bash
fastboot oem reboot-recovery
```

选择`高级`-`ADB Sideload`-`滑动开始Sideload`--`连接USB数据线到电脑 `, 然后电脑上用`adb`执行

```
adb sideload rom.zip
```

`rom.zip`后面是你的要刷入的`ROM`文件,会出现正在输入的进度. 当出现`Total`, 即刷入成功.
然后拔掉数据线, 操作手机按提示重启即可进入. 只要`ROM`没问题就可以正常进入系统.

## sdkman!

[The Software Development Kit Manager](https://sdkman.io/)
[SDKMAN!使用指南](https://www.jianshu.com/p/8597c22550a6)

`SDKMAN!` 是一款用于在大多数基于`Unix`的系统上管理多个软件开发工具包的并行版本的工具.
它提供了一个方便的命令行界面(CLI)和API, 用于安装, 切换, 删除和列出候选程序.
它的前身是`GVM`, 即`Groovy enVironment Manager`, 其灵感来自于`Ruby`社区广泛使用的非常有用的`RVM`和`Rbenv`工具.

### 安装卸载

在终端中输入以下命令进行安装:

```bash
$ curl -s "https://get.sdkman.io" | bash
```

安装完成后,在终端中输入:

```bash
$ source "$HOME/.sdkman/bin/sdkman-init.sh"
```

输入以下命令查看安装情况:

```bash
$ sdk version
```

#### 安装到自定义位置

`SDKMAN`的默认安装位置为:`$HOME/.sdkman`.你可以通过设置`SDKMAN_DIR`环境变量来修改安装位置:

```bash
$ export SDKMAN_DIR="/usr/local/sdkman" && curl -s "https://get.sdkman.io" | bash
```

#### Beta通道

`SDKMAN`的Bate版,包含一些`cli`的新功能,但是可能会不稳定.如果需要使用`Bate`版本,需要修改`~/.sdkman/etc/config`文件:

```bash
sdkman_beta_channel=true
```

然后打开一个终端执行:

```bash
$ sdk selfupdate force
```

如果不需要使用Bate版本了,将上面的配置修改为`false`, 再执行一次更新即可.

#### 卸载

`SDKMAN!`没有提供自动化的卸载方法,可以通过以下命令进行卸载:

```bash
tar zcvf ~/sdkman-backup_$(date +%F-%kh%M).tar.gz -C ~/ .sdkman
$ rm -rf ~/.sdkman
```

然后从`.bashrc`, `.bash_profile` 或 `.profile`文件中编辑和删除初始化代码片段. 如果您使用`ZSH`, 请将其从`.zshrc`文件中删除. 要删除的代码片段如下所示：

```bash
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
[[ -s "/home/dudette/.sdkman/bin/sdkman-init.sh" ]] && source "/home/dudette/.sdkman/bin/sdkman-init.sh"
```

### 使用

***
列出支持的软件

```bash
sdk list
# 列出软件的版本
sdk list gradle
# 安装gradle
sdk install gradle
# 安装指定版本软件, 后面跟上版本号即可
sdk install gradle 4.4.1
# 安装本地包
sdk install groovy 3.0.0-SNAPSHOT /path/to/groovy-3.0.0-SNAPSHOT
# 卸载包
sdk uninstall scala 2.11.6
# 选择一个版本用于当前终端
sdk use scala 2.12.1
#  设置默认版本
sdk default scala 2.11.6
# 查看当前使用的版本
sdk current java
#查看所有本地包的当前版本
sdk current
# 升级 sdk版本
sdk upgrade springboot
# 本地所有sdk全部升级
sdk upgrade
# 离线模式, 当电脑没有网的时候,离线模式会进行自动切换.
sdk offline enable
sdk offline disable
# SDKMAN!版本升级
sdk selfupdate
# 强制重新安装
sdk selfupdate force
```

## Magisk 官方安装教程

[Magisk 官方安装教程](https://topjohnwu.github.io/Magisk/install.html#huawei)

### 安装

如果你已经安装了`Magisk`, 强烈建议你通过`Magisk`应用程序直接升级, 使用它的`直接安装`方法. 下面的教程只针对第一次安装.

### 准备

在你开始之前.

+ 本教程假定你了解如何使用 `adb` 和 `fastboot` .
+ 你的设备必须解锁`bootloader`.
+ 如果你还打算安装自定义`内核`, 请在 `Magisk` 之后安装.

下载并安装最新的[Magisk应用程序](https://github.com/topjohnwu/Magisk/releases). 程序会收集设备的一些信息. 在主屏幕上, 应该能看到`magisk`和`app`的安装信息.

![img](https://topjohnwu.github.io/Magisk/images/device_info.png)

请特别注意 `Ramdisk` 信息, 它决定了你的设备在`boot`分区中是否有`Ramdisk`.

> 不幸的是, 也有例外, 因为有些设备的引导程序接受 `ramdisk`, 即使它做不到. 在这种情况下, 你需要假装启动分区确实有`ramdisk`, 继续按照指示进行操作.
> 除了实际尝试, 没有办法检测到这一点. 幸运的是, 据我们所知, 只有一些小米设备有这个属性, 所以大多数人可以简单地忽略这条信息).

+ 如果你的设备没有 `boot ramdisk`, 请在安装前阅读`恢复分区的Magisk`部分. 该部分的信息是非常重要的!
+ 如果你使用的是三星的设备, 并且`SAR`的结果是`Yes`, 请查看[Samsung 的部分](https://topjohnwu.github.io/Magisk/install.html#samsung-system-as-root).
+ 否则, 请继续查看`Patching Images`, 修补镜像.

1. 如果你的设备有`boot ramdisk`, 你也可以用`custom recovery,`安装`Magisk`, 但不建议这样做
2. 华为设备不被支持, 该说明已从本页面删除)

### 修补镜像

+ 如果你的设备有`boot ramdisk`, 你需要一份`boot.img`的副本.
+ 如果你的设备没有`boot ramdisk`, 你需要一份`recovery.img`的副本.

你应该能够从官方固件包或你的定制`ROM` zip 中提取你需要的文件. 如果你仍然有问题, 去`XDA-Developers`寻找资源, 指南, 讨论, 或者在你设备的论坛上寻求帮助.

+ 将 `boot.img`或`recovery.img` 镜像复制到你的设备上
+ 在`Magisk`应用中, 点击`Magisk`卡片右边的安装按钮
+ 如果你正在修补一个`recovery`镜像, 确保在选项中勾选`恢复模式`, 在大多数情况下, 它应该已经被自动选中.
+ 在`方式`中选择`选择并修补一个文件`, 然后选择刚刚存到手机上的`boot/recovery`镜像.
+ `Magisk`应用程序会修补镜像,  并保存到`[内部存储器]/Download/magisk_patched_[random_strings].img`. 例如`/sdcard/Download/...`
+ 用`ADB`把打好补丁的镜像拷贝到你的电脑上. `adb pull /sdcard/Download/magisk_patched_[random_strings].img`
+ 将打过补丁的`boot/recovery`镜像刷写到你的设备上. 对于大多数设备, 重启到`fastboot`模式, 然后用下列命令刷入:

```bash
fastboot flash boot "magisk_patched.img的路径" # 或者是
fastboot flash recovery "magisk_patched.img的路径" # 如果是刷入 recovery 镜像
```

+ 重新启动, 就可以了!

### 卸载

卸载`Magisk`最简单的方法是直接通过`Magisk`应用程序. 如果你非得用自定义`recovery`, 把`Magisk APK`重命名为`uninstall.zip`, 然后像刷入其他普通的`zip`刷机就可以了.

### 恢复分区的Magisk

如果你的设备在启动镜像中没有`ramdisk`, `Magisk`没得办法, 只能劫持(hijack)恢复分区. 对于这些设备, 每次你想启用`Magisk`的时候, 都必须重启到`recovery`.

由于`Magisk`现在劫持了恢复区, 有一个特殊的机制让你实际启动到`recovery`模式.
每个设备型号都有自己重启到`recovery`的组合键, 以`Galaxy S10`为例, 它是`电源+Bixby+音量增大`.
在网上快速搜索应该很容易得到这个信息. 一旦你按下组合键, 设备就会振动并出现`splash`屏幕, 释放所有按钮, 将启动到`Magisk`.
如果你决定启动到实际的恢复模式, 长按`音量加`直到你看到`recovery`界面.

作为总结, 在恢复模式下安装Magisk后, 从关机开始：

+ 正常开机 ->(没有Magisk的系统)
+ `recovery`组合键 -> 闪屏 -> 释放所有按键 -> 有Magisk的系统
+ `recovery`-> 闪屏 -> 长按`音量加` -> 恢复模式

注意：在这种情况下, 你不能使用 custom 的`recovery`来安装或升级Magisk !!

### 三星System-as-root

>如果你的三星设备不是用安卓9.0或更高版本启动的, 那就别往下看了.

## 在安装Magisk之前

+ 安装`Magisk WILL trip KNOX`
+ 第一次安装`Magisk`需要`完全清除数据`, 这不包括解锁`bootloader`时的数据清除, 在继续之前备份你的数据.
+ 下载支持你设备的`Odin`(只在Windows上运行). 你可以通过在线搜索轻松找到它.

### 解锁bootloader

在现今的三星设备上解锁引导程序有一些注意事项. 在某些情况下, 新引入的`VaultKeeper`服务将使`bootloader`(启动加载器)拒绝任何非官方的分区.

+ 在`开发者选项`中允许`bootloader`解锁->`OEM unlocking`
+ 重新启动到`下载模式`：关闭设备并按下设备的下载模式组合键
+ 长按`音量加`来解锁`bootloader`. **这将擦除你的数据并自动重新启动**.
+ 通过初始设置. 跳过所有的步骤, 因为数据将在后面的步骤中再次被擦除. **在设置过程中, 将设备连接到互联网**.
+ 启用开发者选项, **确认OEM解锁选项存在, 并且是灰色的**. 这意味着`VaultKeeper`服务已经释放了`bootloader`.
+ 在`下载模式`下, 您的`bootloader`现在可以接受的非官方镜像.

说明:

+ 使用[samfirm.js](https://github.com/jesec/samfirm.js), [Frija](https://forum.xda-developers.com/t/tool-frija-samsung-firmware-downloader-checker.3910594/)或[Samloader](https://forum.xda-developers.com/t/tool-samloader-samfirm-frija-replacement.4105929/), 直接从三星服务器下载你的设备的最新固件压缩包.
+ 解压固件并复制`AP` tar 文件到你的设备. 它通常被命名为`AP_[device_model_sw_ver].tar.md5`.
+ 按下`Magisk`卡片上的安装按钮
+ 如果你的设备没有`boot ramdisk`, 确保`恢复模式`在选项中被选中. 在大多数情况下, 它应该已经被自动选中.
+ 在`方式`中选择`Select and Patch a File`, 然后选择`AP` tar文件.
+ Magisk应用程序将修补整个固件文件到`[Internal Storage]/Download/magisk_patched_[random_strings].tar`.
+ 用`ADB`把修补好的`tar`文件复制到你的电脑上.

```bash
adb pull /sdcard/Download/magisk_patched_[random_strings].tar
```

不要使用`MTP`, 因为它会损坏大文件.

+ 重新启动到`下载模式`. 在你的电脑上打开`Odin`, 将`Magisk_patched.tar`作为`AP`刷入, 同时刷入原始固件中的`BL`, `CP`, `CSC`(不是`HOME_CSC`, 因为我们要擦除数据). 这可能需要一些时间(>10分钟).
+ 一旦`Odin`完成刷入, 你的设备应该自动重启. 如果有提示, 请同意进行出厂重置.
+ 如果你的设备没有`boot ramdisk`, 现在重启到`recovery`, 启用`Magisk`(原因在`恢复区的Magisk中`说明).
+ 安装最新的`Magisk`应用程序并启动. 它应该会显示对话框, 要求进行额外设置. 让它继续完成工作, 应用程序将自动重启你的设备.

走你！享受Magisk吧 😃

### 升级操作系统

一旦你`root`了你的三星设备, 你就不能再通过`OTA`升级你的`Android`操作系统.
要升级你的设备的操作系统, 你必须手动下载新的固件压缩文件, 并通过上一节中相同的给`AP`打补丁的过程.
这里唯一的区别是在`Odin`刷机步骤：我们不使用`CSC` tar, 而是使用`HOME_CSC` tar. 因为我们正在进行升级, 而不是初始安装.

重要说明

> 永远不要试图将`boot`或`recovery`分区恢复到原始状态！你可能会得到一块板砖.唯一的方法是做一个完整的`Odin`恢复, 并且要擦除数据.
> 当用新的固件升级你的设备时, 千万不要直接使用固件中的`AP` tar文件, 原因如上所述. 一定要在Magisk应用程序中修补`AP`, 并使用它来代替.
> 不要只刷入`AP`, 否则`Odin`可能会缩小你的`/data`文件系统. 在升级时, 要刷入`AP+BL+CP+HOME_CSC`.

### 自定义Recovery

>这种安装方法已被废弃, 用最小的努力来维护. 出事儿我可不管啊!

在自定义`Recovery`环境中, 准确检测设备的信息是非常困难的. 由于这个原因, 不再推荐在现代设备上通过自定义`Recovery`安装`Magisk`.
如果你遇到任何问题, 请使用`修补镜像`的方法, 因为它保证100%能行.

+ 下载`Magisk APK`
+ 将`.apk`文件的扩展名改为`.zip`. 例如`Magisk-v22.0.apk -> Magisk-v22.0.zip`. 如果你在重命名文件扩展名时遇到困难(比如在Windows上), 可以使用`Android`上的文件管理器或`TWRP`中的文件管理器来重命名文件.
+ 像使用其他普通的刷机文件一样, 将该压缩文件刷进手机.
+ 重新启动, 检查`Magisk`应用程序是否已经安装. 如果没有自动安装, 手动安装`APK`.

>警告：模块的`sepolicy.rule`文件可能保存在`cache`分区里. 不要手动擦除`CACHE`分区.
