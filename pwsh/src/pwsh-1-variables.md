# powershell 变量

[Powershell 脚本简单入门(二)——变量和赋值](https://blog.csdn.net/DoomHush/article/details/103208961)
[PowerShell 语法入门之变量](https://zhuanlan.zhihu.com/p/219702648)

如刚开始的示例[DataType]$var = value, 除了可以不直接申明变量的类型, 还可以显示的指定变量的类型.

[int]$myPrice = 128
[string]$myDescription = "Barbecue grill"
[string]$myDescription = 123
[string]$myDate = (get-date).ToString("yyyyMM")
$([DateTime] "12/30/2009")
$([DateTime]::Now)
[datetime]$start_date=[datetime]::now.date.addDays(-5)

## $null

[关于 $null 的各项须知内容](https://learn.microsoft.com/zh-cn/powershell/scripting/learn/deep-dives/everything-about-null)
