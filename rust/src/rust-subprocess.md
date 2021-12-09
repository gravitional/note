# subprocess

[subprocess ](https://lib.rs/crates/subprocess)

`subprocess` 库实现了与 `外部进程`(external processes)的执行和交互, 以及管道(pipelines),其灵感来自于Python的subprocess模块.
`subprocess` 托管在 [crates.io](https://crates.io/crates/subprocess),
API 文档在 [docs.rs](https://docs.rs/subprocess/latest/subprocess/).

## 特点

[communicate]: https://docs.rs/subprocess/latest/subprocess/struct.Popen.html#method.communicate_start

这个库用于 `启动外部进程`, 可以重定向标准`输入`, `输出`和`错误`.
它涵盖了与 [std::process](https://doc.rust-lang.org/std/process/index.html) 标准库模块类似的领域, 但有额外的功能.

+ 用于 `无死锁`(deadlock-free) 捕获子进程输出/错误到内存的 [communicate][] 方法系列,
同时向其标准输入提供数据. `捕获`(Capturing) 支持可选的 `超时`(timeout) 和 读取大小限制.
+ 将多个命令连接到操作系统级别的管道
+ 灵活的重定向选项, 如连接 `标准流` 到任意文件, 或像 `shell` 的 `2>&1` 和`1>&2` 操作符那样合并输出流.
+ 用于等待进程的`非阻塞`(non-block)和超时(timeout)方法: `poll`, `wait` 和 `wait_timeout`.

该 crate 对第三方 crate 的依赖性最小, 只需要 `Unix` 上的 `libc`, 或者 `Windows` 上的 `winapi`.
它的目的是在类 `Unix` 平台以及合理的最新 `Windows` 上工作.
它经常在Linux, MacOS和Windows上进行测试.

## API概述

[subprocess.Popen]: https://docs.python.org/3/library/subprocess.html#subprocess.Popen
[subprocess module]: https://docs.python.org/3/library/subprocess.html#popen-constructor

`API` 分为两部分: 底层的 `Popen` API, 类似于 `Python` 的 [subprocess.Popen][], 以及方便创建命令和管道的上层 `API`.
两者可以混合使用, 因此可以使用`构建器`(builder)来创建 `Popen` 实例, 然后直接继续使用它们.

虽然 `Popen` 松散地遵循了 `Python` 的 [subprocess module][], 但它并不是逐字的翻译.
有些变化是为了适应语言之间的差异, 比如 `Rust` 中缺少默认参数和关键字参数,
其他的变化则是利用了 `Rust` 更先进的类型系统, 或者额外的能力, 比如 `所有权系统` 和 `Drop` 特性.
`Python` 的实用函数如 `subprocess.run` 没有被包括在内, 因为它们可以用 builder pattern 更好地表达.

上层 `API` 提供了一个优雅的进程和管道创建接口, 以及用于捕获其输出和退出状态的方便方法.

## 实例

### 生成和重定向, spawing and redirection

+ 执行命令并等待它完成:

```rust
let exit_status = Exec::cmd("umount").arg(dirname).join()?;
assert!(exit_status.success());
```

+ 为了防止 `quoting` 问题和 `注入攻击`(injection attacks), 除非明确要求, 否则子进程不会 spawn 一个 `shell`.
要使用操作系统 `shell` 来执行一个命令, 像C的 `system` 一样, 使用 `Exec::shell`:

```rust
Exec::shell("shutdown -h now").join()?;
```

+ 启动`子进程`, 并以 `Read` trait对象 的形式获得其输出, 就像C语言的 `popen`.

```rust
let stream = Exec::cmd("find /").stream_stdout()?;
// 调用stream.read_to_string, 构造io::BufReader(stream)并对其按行进行遍历.
```

+ 捕获命令的`输出`:

```rust
let out = Exec::cmd("ls")
  .stdout(Redirection::Pipe)
  .capture()?
  .stdout_str();
```

+ 将 `标准错误` 重定向到`标准输出`, 并在字符串中捕获它们.

```rust
let out_and_err = Exec::cmd("ls")
  .stdout(Redirection::Pipe)
  .stderr(Redirection::Merge)
  .capture()?
  .stdout_str();
```

+ 为命令提供一些`输入`, 并读取其`输出`:

```rust
let out = Exec::cmd("sort")
  .stdin("b\nc\na\n")
  .stdout(Redirection::Pipe)
  .capture()?
  .stdout_str();
assert_eq!(out, "a\nb\nc\n");
```

将 `stdin` 连接到一个开放的文件也会起作用.

### 管道,Pipelines

`Popen` 对象支持将 `输入` 和 `输出` 连接到任意的 open file, 包括其他 `Popen` 对象.
这可以用来形成`进程`的`管道`. builder API 将自动完成, 通过作用在 `Exec` 对象上的 `|` 操作符.

+ 执行一个管道并返回最后一条命令的退出状态:

```rust
let exit_status =
  (Exec::shell("ls *.bak") | Exec::cmd("xargs").arg("rm")).join()?;
```

+ 捕获管道的输出:

```rust
let dir_checksum = {
    Exec::shell("find . -type f") | Exec::cmd("sort") | Exec::cmd("sha1sum")
}.capture()?.stdout_str();
```

### 底层`Popen`类型

```rust
let mut p = Popen::create(&["command", "arg1", "arg2"], PopenConfig {
    stdout: Redirection::Pipe, ..Default::default()
})?;

// 因为我们要求 stdout 被重定向到管道,
// 所以父级的管道的末端可以作为 p.stdout 使用.
// 它既可以被直接读取, 或者使用communication()方法处理.
let (out, err) = p.communicate(None)?;

// 检查该进程是否还活着
if let Some(exit_status) = p.poll() {
  //该进程已经结束
} else {
  //它还在运行, 终止它
  p.terminate()?;
}
```

### 查询和终止

+ 检查先前启动的进程是否仍在运行:

```rust
let mut p = Exec::cmd("sleep").arg("2").popen()?;
thread::sleep(Duration::new(1, 0));
if p.poll().is_none() {
    // 如果进程已经完成, poll() 返回 Some(exit_status)
    println!("process is still running");
}
```

给进程 `1秒钟` 运行, 如果没有完成就 `kill` 它:

```rust
let mut p = Exec::cmd("sleep").arg("2").popen()?;
if let Some(status) = p.wait_timeout(Duration::new(1, 0))? {
    println!("process finished as {:?}", status);
} else {
    p.kill()?;
    p.wait()?;
    println!("process killed");
}
```
