# 有用的 crates

总有一些新的 crate 被发布出来, 这些板块在命令行应用程序的开发中是很有用的.

## 本书中提到的板块

+ `anyhow` ; 提供 `anyhow::Error` 以方便错误处理.
+ `assert_cmd` ; 简化了 `CLI` 的集成测试
+ `assert_fs` ; 设置 `输入文件`, 测试输出文件
+ `atty` ; 检测应用程序是否在一个 `tty` 中运行
+ `clap-verbosity-flag`; 为 `structopt CLI` 增加一个 `--verbose` 标志.
+ `clap` ; 命令行参数解析器(parser)
+ `confy` ; boilerplate-free 的配置管理
+ `crossbeam-channel`; 为消息传递提供多 `生产者多消费者`通道
+ `ctrlc` - 简单的 `ctrl-c` 处理程序
+ `env_logger` ; 实现一个可通过环境变量配置的记录器
+ `exitcode` ; system exit code 常量
+ `human-panic`; `panic` 信息处理程序
+ `indicatif` ; 进度条和旋转器 (progress bars and spinners)
+ `log` ; 提供抽象化的日志实现; provides logging abstracted over implementation
+ `predicates` ; 实现布尔值的谓词函数; boolean-valued predicate functions
+ `proptest` ; 属性测试框架
+ `serde_json` ; 对JSON进行序列化/反序列化
+ `signal-hook` ; 处理 UNIX 信号
+ `structopt` ; 将命令行参数解析为结构体
+ `tokio` ; 异步运行时; asynchronous runtime
+ `wasm-pack` ; 用于构建 `WebAssembly` 的工具

## 其他crates

[lib.rs]: https://lib.rs

由于 `Rust crates` 的情况不断变化, 寻找 `crates` 的一个好地方是 [lib.rs][] crate index.
这里有几个特定的类别, 可能对构建 CLI 有用.

+ [命令行界面](https://lib.rs/command-line-interface)
+ [配置](https://lib.rs/config)
+ [数据库接口](https://lib.rs/database)
+ [编码](https://lib.rs/encoding)
+ [文件系统](https://lib.rs/filesystem)
+ [HTTP客户端](https://lib.rs/web-programming/http-client)
+ [操作系统](https://lib.rs/os)
