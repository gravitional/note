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
