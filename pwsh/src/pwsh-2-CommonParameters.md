# powershell 公共参数

[about_CommonParameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_commonparameters).

The following list displays the common parameters.
Their aliases are listed in parentheses.

+ `Debug` (db)
+ `ErrorAction` (ea)
+ `ErrorVariable` (ev)
+ `InformationAction` (infa)
+ `InformationVariable` (iv)
+ `OutVariable` (ov)
+ `OutBuffer` (ob)
+ `PipelineVariable` (pv)
+ `Verbose` (vb)
+ `WarningAction` (wa)
+ `WarningVariable` (wv)

## -PipelineVariable

`PipelineVariable` 分配一个临时变量 `$temp`, 保存管道中的值.
`$temp` 名字可以自定义
这使得 `PipelineVariable` 比特定的临时变量更容易使用,
因为临时变量可能需要在多个位置进行分配.

与 `$_` 或 `$PSItem` 不同,
使用 `PipelineVariable` 允许相隔多个 `|` 的命令访问 `$temp`
这允许一个命令将其输出 `反馈` 给前一个命令(或其本身).

`PipelineVariable` 的 `scope` 是它被调用的流水线.
管道外的变量, 如果使用相同的名称, 在管道执行前会被清除掉.
当管道终止时, `$temp` 就会超出范围.
如果管道内的多个命令指定了相同的PipelineVariable, 则只有一个共享变量.
该变量会根据指定该变量的命令的最新管道输出进行更新.

一些阻塞命令在产生任何输出之前收集所有的管道项目,
例如Sort-Object或Select-Object -Last.
在这种阻塞命令之前的命令中指定的任何PipelineVariable,
在阻塞命令之后的命令中使时, 总是包含前一个命令的最后一个管道项目.

### 例子

```powershell
# Create a variable named $temp
$temp=8
Get-Variable temp
# Note that the variable just created is not available on the
# pipeline when -PipelineVariable creates the same variable name
1..5 | ForEach-Object -PipelineVariable temp -Begin {
    Write-Host "Step1[BEGIN]:`$temp=$temp"
} -Process {
  Write-Host "Step1[PROCESS]:`$temp=$temp - `$_=$_"
  Write-Output $_
} | #此处仍然可以访问到 $temp
ForEach-Object {
  Write-Host "`tStep2[PROCESS]:`$temp=$temp - `$_=$_"
}
# $temp 变量将在此处失效, 包括全局变量 $temp
Get-Variable temp
```

## -OutVariable

除了沿管道发送输出外, 还会在 `指定变量` 中存储命令的输出对象.
选项的参数是字符串类型的 `变量名称`.

```YAML
Type: String
Aliases: ov

Required: False
Position: Named
Default value: None
Accept pipeline input: False
Accept wildcard characters: False
```

若要将输出 `附加` 到变量,
而不是 `替换` 变量中之前的输出, 请在 `变量名称` 之前键入 `加号`(`+`) .

例如, 以下命令创建 `$out` 变量并将进程对象存储在其中:

```PowerShell
Get-Process PowerShell -OutVariable out
```

以下命令将进程对象添加到 `$out` 变量:

```PowerShell
Get-Process iexplore -OutVariable +out
```

以下命令显示变量 `$out` 的内容:

```PowerShell
$out
```

### 备注

`OutVariable` 参数创建的变量, 类型是 `[System.Collections.ArrayList]`.
