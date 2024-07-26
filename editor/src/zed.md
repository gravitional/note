# zed 编辑器

## 为 Windows 构建 Zed

>注意
>以下命令可在任何 shell 中执行.

### 版本库

克隆 [Zed 仓库](https://github.com/zed-industries/zed).

### 依赖

安装 Rust.
如果已经安装, 请确保是最新的:

```bash
rustup update
```

安装 Rust wasm 工具链:

```bash
rustup target add wasm32-wasi
```

安装 [Visual Studio](https://visualstudio.microsoft.com/downloads/) 和
可选组件 MSVC v*** - VS YYYY C++ x64/x86 构建工具, 并根据系统安装 Windows 11 或 10 SDK

>注意
>v*** 是您的 VS 版本, YYYY 是您的 VS 发布年份.

### 后端依赖

警告
本部分仍在开发中.
说明尚未完成.

如果您要开发 Zed 的协作功能, 则需要安装 zed 协作服务器的依赖项:

+ 安装 [Postgres](https://www.postgresql.org/download/windows/)
+ 安装 [Livekit](https://github.com/livekit/livekit-cli) 和 [Foreman](https://theforeman.org/manuals/3.9/quickstart_guide.html)

或者, 如果你安装了 Docker, 可以使用 Docker Compose 调用所有 collab 依赖项:

```bash
docker compose up -d
```

### 从源代码构建

安装好依赖项后, 就可以使用 [Cargo](https://doc.rust-lang.org/cargo/) 构建 Zed 了.

+ debug build

```bash
cargo run
```

+ release build

```bash
cargo run --release
```

+ 运行测试

```bash
cargo test --workspace
```
