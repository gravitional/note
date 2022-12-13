# powershell 重定向

## tee-object

[Tee-Object](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/tee-object)

```powershell
Tee-Object
   [-InputObject <PSObject>]
   [-FilePath] <String>
   [-Append]
   [[-Encoding] <Encoding>]
   [<CommonParameters>]
```

### 参数

`-InputObject`; 指定要保存和显示的对象. 
输入包含对象的 `变量` 或输入 `获得对象的命令或表达式`. 
你也可以用管道将 `对象` 输送到 `Tee-Object`.

当你将 `InputObject` 参数与 `Tee-Object` 一起使用时, 
而不是将命令结果输送到 `Tee-Object`, 
`InputObject` 值被视为 `单一的对象`, 即使该值是一个集合.

类型: PSObject
位置: 命名参数
默认值: None
接受管道输入: True
接受通配符: False

#### 例子

例如脚本文件 `te.ps1` 内容如下

```powershell
echo 1
sleep -s 2
echo 2
```

```powershell
# 可以运行成功, 实时输出 1,2
.\te.ps1 | Tee-Object -FilePath 'tee.txt' -Encoding utf8 
.\te.ps1 | Tee-Object -FilePath 'tee.txt' -Encoding utf8 -InputObject  #运行失败
## 会等待脚本结束，再一起输出 1,2 
Tee-Object -FilePath 'tee.txt' -Encoding utf8 -InputObject (./te.ps1)
```
