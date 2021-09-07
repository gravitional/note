# x.vscode.md

## 按文件名搜索文件

按快捷键`ctrl+p`可以弹出一个小窗, 在上面的输入框输入文件名, 下拉框点击一个文件

## latex 编译调试

```powershell
$temp=latexmk -f -xelatex; Write-Output "++++++++++++" ;$temp | Where-Object {$_ -like "*tex:*"}
```

```powershell
$temp | Where-Object {$_ -like "*tex:*"}
```
