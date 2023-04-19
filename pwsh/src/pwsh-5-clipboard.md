# powershell 剪贴板

[剪切板操作](https://blog.csdn.net/xuchaoxin1375/article/details/122274078)

reference link
[Get-Clipboard](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-clipboard)
[Set-Clipboard](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/set-clipboard)

本地查看剪切板相关功能的命令,

```powershell
gcm *clipboard*

gal -Definition Get-Clipboard
gal -Definition Set-Clipboard
```

别名分别为:

get-clipboard -> gcb
set-clipboard -> scb

直接复制文本文件中的内容内容到剪切板

```powershell
cat $PROFILE | scb
gcb
```
