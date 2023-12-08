# git 换行符问题

[Git 中源代码的换行符问题](https://zhuanlan.zhihu.com/p/605019769)
[git在windows和linux之间换行符问题](https://zhuanlan.zhihu.com/p/605019769)

git 中的源码可能包含 Windows 下的 CRLF 字符,
或者 Linux 下的是 `LF` 字符,
设置为 `git config --global core.autoCRLF true` 后,
在提交时候 转换为 `LF`, 检出时根据当前系统决定转换为 CRLF, 或者不转换.

## AutoCRLF

关于配置的作用域, systemwide > global > local.
local 如果没有配置, global 的设置为 "autoCRLF = false",
systemwide 的设置为 "autoCRLF = true", 则实际生效的设置是 global,
即 "autoCRLF = false";如果 global 中也没有配置,
则实际生效的设置是 systemwide, 即 "autoCRLF = true".

```bash
# 提交时转换为 LF, 检出时根据当前系统决定转换为 CRLF, 或者不转换
git config --global core.autoCRLF true

# 提交时转换为 LF, 检出时不转换
git config --global core.autoCRLF input

# 提交检出时均不转换, 保持原样
git config --global core.autoCRLF false
```

默认值为 "core.autoCRLF input",
在 TortoiseGit 中的该选项名称为: "自动换行符转化",
勾选为"true", 不勾选为 "false", 半勾选为 "input",
可以自己手动编辑全局 .git/config 文件.

## SafeCRLF

```bash
# 拒绝提交包含混合换行符的文件
git config --global core.safeCRLF true

# 允许提交包含混合换行符的文件
git config --global core.safeCRLF false

# 提交包含混合换行符的文件时给出警告
git config --global core.safeCRLF warn
```

默认值为 "core.safeCRLF false", 在 TortoiseGit 中的该选项名称为: "检查换行", 这三个选项都有.

## 推荐设置

在 Windows 系统上推荐使用:

```bash
# 提交时转换为 LF, 检出时根据当前系统决定转换为 CRLF, 或者不转换
git config --global core.autoCRLF true

# 拒绝提交包含混合换行符的文件
git config --global core.safeCRLF true
```
