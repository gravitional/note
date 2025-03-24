# manjaro daily 2

## 已请求远程控制

在 Wayland 桌面环境中, 出现一个名为"系统门户"的程序弹窗, 询问"已请求远程控制", 这通常与 Flatpak 或 Snap 等沙盒化应用程序框架有关.
在 Wayland 桌面环境中, 出现一个名为"系统门户"的程序弹窗, 询问"已请求远程控制", 这通常与 **Flatpak** 或 **Snap** 等沙盒化应用程序框架有关.

### 1. **Flatpak 的"系统门户"**

Flatpak 应用程序在沙盒中运行, 当它们需要访问系统资源(如摄像头, 麦克风, 文件系统等)时,
会通过 **xdg-desktop-portal** 向用户请求权限. 这个弹窗可能是由 `xdg-desktop-portal` 触发的.

- **xdg-desktop-portal** 是一个实现 [XDG Desktop Portal 规范](https://flatpak.github.io/xdg-desktop-portal/) 的服务, 允许沙盒应用程序与宿主系统交互.
- 当 Flatpak 应用程序请求远程控制(例如屏幕共享, 远程桌面等)时, `xdg-desktop-portal` 会弹窗询问用户是否允许.

#### 解决方法:

- 如果你信任该应用程序, 可以点击"允许".
- 如果你不确定, 可以点击"拒绝", 并检查是哪个应用程序触发了该请求.
- 你可以通过以下命令查看正在运行的 Flatpak 应用程序:

  ```bash
  flatpak list --app
  ```

### 2. **Snap 的权限请求**

Snap 是另一种沙盒化应用程序框架, 类似于 Flatpak. 当 Snap 应用程序需要访问系统资源时, 也会通过类似的机制请求权限.

- Snap 使用 **snapd** 和 **xdg-desktop-portal** 来处理权限请求.
- 弹窗可能是由 Snap 应用程序触发的.

#### 解决方法:

- 检查正在运行的 Snap 应用程序:

  ```bash
  snap list
  ```

- 如果你不确定是哪个应用程序触发了请求, 可以查看日志:

  ```bash
  journalctl -f
  ```

  然后在弹窗出现时观察日志输出.

### 3. **其他可能的原因**

- **远程桌面工具**: 如果你使用了远程桌面工具(如 GNOME Remote Desktop, VNC 或 RDP), 这些工具可能会请求远程控制权限.
- **浏览器或应用程序的屏幕共享**: 某些应用程序(如浏览器)在请求屏幕共享时也会触发此类弹窗.

### 如何进一步排查

1. **查看日志**:
   使用 `journalctl` 查看系统日志, 找到弹窗出现时的相关记录:

   ```bash
   journalctl -f
   ```

2. **检查正在运行的应用程序**:
   使用 `ps` 或 `htop` 查看当前运行的应用程序:

   ```bash
   ps aux | grep -i portal
   ```

3. **检查 Flatpak/Snap 应用程序**:
   列出已安装的 Flatpak 和 Snap 应用程序:

   ```bash
   flatpak list
   snap list
   ```

## [ArchLinux配置登录密码错误次数和锁定时间](https://cloud.tencent.com/developer/article/1959542)

不小心多次输入错误密码, 造成锁定.

### 简介

在 Arch Linux 的默认配置下, 用户在登录系统时如果在 15 分钟内输错密码 3 次, 则会被锁定 10 分钟.
对于个人电脑来说, 只有 3 次输错机会有点少, 因为有时候总是会突然多按或少按一个键,
或者由于 Capslock 开启的缘故, 导致连续几次都出错, 很容易被锁定.
而一旦被锁定, 就要等 10 分钟, 除非重启, 对于个人用户来说实在太长了.

### 2. 配置

#### 2.1 相邻两次登录间隔

Arch Linux 默认在一次登录失败后, 需要等待一段时间的延迟才能进行下一次的登录, 默认设置下个人感觉还可以接受. 如果需要修改, 则可以在配置文件 /etc/pam.d/system-login 中增加以下一行设定:

```conf
auth optional pam_faildelay.so delay=4000000
```

上述设定是 4 秒, 如果需要其它时间, 只需修改 delay 字段, 它的单位是毫秒.

#### 2.2 登录失败次数和锁定时间

Arch Linux 默认在 15 分钟内登录失败 3 次就锁定 10 分钟, 可以修改 /etc/security/faillock.conf 来更改默认设定, 主要修改其中三个字段:

+ `deny`: 登录失败次数;
+ `fail_interval`: 计数周期, 单位秒;
+ `unlock_time`: 锁定时间, 单位秒.

### 3. 解除锁定

如果被锁定了, 除了重启, 其实也可以通过以下方法来解除锁定:

>如果你除了当前登录窗口还有其它登录窗口, 比如 tty2, tty3 等, 且至少有一个已经成功登录了当前账号,
>然后已登录的 tty 终端解除当前账号的限制; 或者没有其它已登录窗口,
> 仍可以通过 root 帐号登录 tty 终端, 然后用 root 帐号解除当前账号的限制:

```bash
faillock --reset --user username
```

或者直接置空 /run/faillock 目录下被锁定的用户对应的锁定文件:

```bash
dd if=/dev/null of=/run/faillock/username
```

### okular 更新背景颜色

背景颜色

```bash
绿豆沙 #C7EDCC
青草绿 #E3EDCD RGB(227, 237, 205)
电脑管家 #CCE8CF RGB(204, 232, 207)
WPS护眼色 #6E7B6C RGB(110, 123, 108)
Settings -> Configure Okular -> Accessibility(辅助功能) -> Change colors -> Color mode: Change Paper Color -> Paper color -> HTML: #d6e7cb
```
