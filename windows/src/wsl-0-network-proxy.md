# wsl 设置代理, 使用 host 科学上网设置

[为 WSL2 一键设置代理](https://zhuanlan.zhihu.com/p/153124468)
[Accessing network applications with WSL](https://learn.microsoft.com/en-us/windows/wsl/networking)

为了让 WSL 使用和 host 一样的科学上网设置,
编辑 `~/.wslconfig` 文件, 添加如下设置; 使用 `#` 表示注释

```bash
# Settings apply across all Linux distros running on WSL 2
[wsl2]
networkingMode=mirrored
# nestedVirtualization=true
ipv6=true
# dnsTunneling=true
# firewall=true
autoProxy=true

[experimental]
# autoMemoryReclaim=gradual # gradual | dropcache | disabled
```