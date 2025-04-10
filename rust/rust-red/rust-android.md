# 在 Android 上运行 Rust 程序

## 1. 开发环境准备

### 1.1 安装 Rust Android 目标

```bash
rustup target add aarch64-linux-android
```

下载 [Android NDK][def], 解压到合适的目录, 将 
`.../ndk/android-ndk-r27c/toolchains/llvm/prebuilt/windows-x86_64/bin` 添加到环境变量 `PATH`.
`...` 表示根据你的安装位置而定.

crate 项目的 `.cargo/config.toml` 要写上

```toml
[target.aarch64-linux-android]
linker = ".../ndk/android-ndk-r27c/toolchains/llvm/prebuilt/windows-x86_64/bin/aarch64-linux-android35-clang.cmd"
ar = ".../ndk/android-ndk-r27c/toolchains/llvm/prebuilt/windows-x86_64/bin/llvm-ar.exe"
```

注意修改 `...` 为你的安装位置.

### cargo.toml 添加程序二进制路径 和 优化选项

```toml
[[bin]]
name = "auto_video"
path = "src/bin/auto_video.rs"

[[bin]]
name = "auto_manga"
path = "src/bin/auto_manga.rs"

[profile.release]
opt-level = 'z'   # 优化体积
lto = true        # 启用链接时优化
codegen-units = 1 # 降低并行度以提高优化
panic = 'abort'   # 移除 panic 展开代码
strip = true      # 移除符号信息
```

### 1.2 安装 Android 终端模拟器

1. 在 Android 设备上安装 Termux（从 F-Droid 或 Google Play 下载）
2. 打开 Termux，创建程序目录：
```bash
mkdir -p ~/bin
```

## 2. 程序编译

### 2.1 编译 Rust 程序

```bash
cargo build --target aarch64-linux-android --release
```

### 2.2 程序文件准备

编译后的文件位于：
- `target/aarch64-linux-android/release/auto_video`
- `target/aarch64-linux-android/release/auto_manga`

## 3. 部署到设备

### 3.1 复制程序到设备

```bash
adb push target/aarch64-linux-android/release/auto_video /storage/emulated/0/Download/
adb push target/aarch64-linux-android/release/auto_manga /storage/emulated/0/Download/
```

### 3.2 在 Termux 中设置

1. 打开 Termux
2. 移动程序到 bin 目录：
```bash
mv /storage/emulated/0/Download/auto_* ~/bin/
```

### 3.3 设置权限

```bash
chmod +x ~/bin/auto_video ~/bin/auto_manga
```

## 4. 运行程序

### 4.1 直接运行

在 Termux 中直接输入程序名即可运行：

```bash
~/bin/auto_video
# 或
~/bin/auto_manga
```

### 4.2 注意事项

- 确保 Termux 有存储权限
- 程序会自动处理 Download 目录下的文件
- 处理后的文件会移动到 `/storage/emulated/0/1rs` 目录

## 5. 故障排除

### 5.1 常见问题

- 如果提示权限错误，检查 Termux 的存储权限
- 如果程序无法运行，确保文件有执行权限
- 如果找不到文件，检查 PATH 环境变量是否包含 `~/bin`

### 5.2 权限设置

在 Termux 中运行：
```bash
termux-setup-storage
```

[def]: https://developer.android.google.cn/ndk/downloads?hl=zh-cn