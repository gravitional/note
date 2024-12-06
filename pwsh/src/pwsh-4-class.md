# pwsh class 类

[about_Classes_Constructors](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_classes_constructors?view=powershell-7.4)
[Learn How to Use .NET Framework Commands inside Windows PowerShell](https://devblogs.microsoft.com/scripting/learn-how-to-use-net-framework-commands-inside-windows-powershell/)

## class, new, constructor

+ Example 2 - Overriding the default constructor

```pwsh
class ExampleBook2 {
    [string]   $Name
    [string]   $Author
    [int]      $Pages
    [datetime] $PublishedOn

ExampleBook2() {
        $this.PublishedOn = (Get-Date).Date
        $this.Pages       = 1
    }
}

[ExampleBook2]::new()

# Name Author Pages PublishedOn
# ---- ------ ----- -----------
#                 1 11/1/2023 12:00:00 AM
```

+ Example 3 - Defining constructor overloads

```pwsh
class ExampleBook3 {
    [string]   $Name
    [string]   $Author
    [int]      $Pages
    [datetime] $PublishedOn

ExampleBook3([hashtable]$Info) {
        switch ($Info.Keys) {
            'Name'        { $this.Name        = $Info.Name }
            'Author'      { $this.Author      = $Info.Author }
            'Pages'       { $this.Pages       = $Info.Pages }
            'PublishedOn' { $this.PublishedOn = $Info.PublishedOn }
        }
    }

ExampleBook3(
        [string]   $Name,
        [string]   $Author,
        [int]      $Pages,
        [datetime] $PublishedOn
    ) {
        $this.Name        = $Name
        $this.Author      = $Author
        $this.Pages       = $Pages
        $this.PublishedOn = $PublishedOn
    }

ExampleBook3([string]$Name, [string]$Author) {
        $this.Name   = $Name
        $this.Author = $Author
    }
}

[ExampleBook3]::new(@{
    Name        = 'The Hobbit'
    Author      = 'J.R.R. Tolkien'
    Pages       = 310
    PublishedOn = '1937-09-21'
})
[ExampleBook3]::new('The Hobbit', 'J.R.R. Tolkien', 310, '1937-09-21')
[ExampleBook3]::new('The Hobbit', 'J.R.R. Tolkien')
[ExampleBook3]::new()
```

## New-Object

[New-Object](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/new-object?view=powershell-7.4)
[Version Class](https://learn.microsoft.com/en-us/dotnet/api/system.version?view=net-9.0)

+ Example 1: Create a System.Version object

```pwsh
New-Object -TypeName System.Version -ArgumentList "1.2.3.4"

# Major  Minor  Build  Revision
# -----  -----  -----  --------
# 1      2      3      4
```

+ Example 2: Create an Internet Explorer COM object

```pwsh
$IE1 = New-Object -COMObject InternetExplorer.Application -Property @{Navigate2="www.microsoft.com"; Visible = $True}

# The following command gets the same results as the example above.
$IE2 = New-Object -COMObject InternetExplorer.Application $IE2.Navigate2("www.microsoft.com") $IE2.Visible = $True
```

+ Example 3: Use the Strict parameter to generate a non-terminating error

```pwsh
$A = New-Object -COMObject Word.Application -Strict -Property @{Visible = $True}

# New-Object : The object written to the pipeline is an instance of the type...
# At line:1 char:14
# + $A = New-Object  <<<< -COM Word.Application -Strict; $a.visible=$true
```

+ Example 4: Create a COM object to manage Windows desktop

本例演示了如何创建和使用 `COM` 对象来管理 Windows 桌面.

第一条命令使用 `New-Object` cmdlet 的 `ComObject` 参数
创建带有 `Shell.Application ProgID` 的 COM 对象.
它将生成的对象存储在 `$ObjShel`l 变量中.
第二条命令将 `$ObjShell` 变量导入 `Get-Member` cmdlet,
该 cmdlet 将显示 `COM` 对象的属性和方法. 其中包括 `ToggleDesktop` 方法.
第三条命令调用对象的 `ToggleDesktop` 方法, 以最小化桌面上打开的窗口.

```pwsh
$Objshell = New-Object -COMObject "Shell.Application"
$objshell | Get-Member
$objshell.ToggleDesktop()
```

## 调用静态方法

已知有 `System.Diagnostics.Process` .NET Framework class.
默认已导入 `System` 命名空间, 因此可以省略 `System`
调用静态方法:

```pwsh
[System.Diagnostics.Process]::GetProcesses()
[Diagnostics.Process]::GetProcesses()
# 等价于 cmdlet, 但 Get-Process 默认按照 precessName 排序
Get-Process
```

查看 `[diagnostics.process]` 类的成员, 给出了函数签名

```pwsh
# Get-Member; format-list
[diagnostics.process] | gm -s getProcessesbyname | fl *

# TypeName   : System.Diagnostics.Process
# Name       : GetProcessesByName
# MemberType : Method
# Definition :
# static System.Diagnostics.Process[] GetProcessesByName(string processName),
# static System.Diagnostics.Process[] GetProcessesByName(string processName, string machineName)

# --- 因此可以这样调用
[diagnostics.process]::GetProcessesByName("explorer")
[diagnostics.process]::GetProcessesByName("explorer", "localhost")
# 等价的 Get-Process cmdlet
Get-Process -Name explorer -ComputerName localhost
```

另一个例子, 修改进程的 `ProcessorAffinity`

```pwsh
notepad
Get-Process notepad

# Handles  NPM(K)    PM(K)      WS(K) VM(M)   CPU(s)     Id ProcessName
# ——-  ——    —–      —– —–   ——     — ———–
#      67       7     1524       5416    71     0.03   6236 notepad

$a = Get-Process notepad
$a.ProcessorAffinity # 处理器亲和性
# 3
$a.ProcessorAffinity = 0x0002
$a.ProcessorAffinity
# 2
```
