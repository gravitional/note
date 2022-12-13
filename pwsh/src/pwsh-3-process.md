# powershell 进程管理

## Start-Process

[Start-Process](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.management/start-process)

Example 1: Start a process that uses default values
This example starts a process that uses the Sort.exe file in the current folder.
The command uses all the default values, including the default window style, working folder, and credentials.

```PowerShell
Start-Process -FilePath "sort.exe"
```

### Example 2: Print a text file

This example starts a process that prints the C:\PS-Test\MyFile.txt file.

```PowerShell
Start-Process -FilePath "myfile.txt" -WorkingDirectory "C:\PS-Test" -Verb Print
```

## Start-Job

[Start-Job](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/start-job?view=powershell-7.2)

Start-Job cmdlet在本地计算机上启动一个PowerShell后台作业.

PowerShell后台作业在不与当前会话交互的情况下运行一个命令. 
当你启动一个后台作业时, 一个作业对象会立即返回, 即使该作业需要很长时间才能完成. 
在作业运行时, 你可以继续在会话中工作而不被打断.

作业对象包含关于作业的有用信息, 但它不包含作业结果. 当作业完成后, 
使用 Receive-Job cmdlet 来获取作业的结果. 关于后台作业的更多信息, 请参阅 about_Jobs.

要在远程计算机上运行一个后台作业, 可以使用许多cmdlet上的AsJob参数,
 或者使用Invoke-Command cmdlet在远程计算机上运行一个Start-Job命令. 更多信息, 请看 about_Remote_Jobs.

从PowerShell 3.0开始, Start-Job可以启动自定义作业类型的实例, 
如计划作业. 关于如何使用Start-Job来启动自定义类型的作业, 请参见作业类型功能的帮助文档.

从PowerShell 6.0开始, 你可以使用安培尔(&)背景操作符来启动作业. 
背景操作符的功能与Start-Job类似. 这两种启动作业的方法都会创建一个PSRemotingJob作业对象. 
关于使用安培尔(&)的更多信息, 请参见about_Operators.

PowerShell 7引入了WorkingDirectory参数, 指定了背景作业的初始工作目录. 
如果没有指定该参数, Start-Job默认为启动该作业的调用者的当前工作目录.
