# windows 驱动

## dism 备份还原驱动

[Win10使用Dism备份还原驱动](https://blog.csdn.net/u012075442/article/details/109044119)

## 备份Win10驱动的命令:

以管理员身份运行命令提示符, 输入并运行以下命令:

```pwsh
dism /online /export-driver /destination:D:\DriversBackup
```

`D:\DriversBackup` 为备份的驱动程序保存的目录

还原Win10驱动的命令:

```pwsh
Dism /online /Add-Driver /Driver:D:\DriversBackup /Recurse
```
