# rust 常见问题

[解决 Blocking waiting for file lock on package cache](https://zhuanlan.zhihu.com/p/543720330)
[Cargo build hangs](https://stackoverflow.com/questions/47565203/cargo-build-hangs-with-blocking-waiting-for-file-lock-on-the-registry-index-a)

出现问题的场景:

下载create出现timeout,
之后更换了国内源下载 `create` 成功后,再次build或run时出现这个问题.

解决办法:

删除 `~/.cargo/.package-cache`, 然后再次 `build` 或 `run`
