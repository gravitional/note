# windows bugs

## microsoft edge 中快捷键失效

[如何解决复制快捷键ctrl+c在浏览器中的失灵现象?](https://www.zhihu.com/question/498251854)
[edge 可以右键复制但ctrl+c 复制经常不起作用](https://zhuanlan.zhihu.com/p/480911247)

设置 -> 外观 -> 往下滑, 上下文菜单 -> 选择文本时的微型菜单 -> 关闭

## win11 右键默认展开

[有没有什么办法可以让win11右键默认显示更多选项?](https://www.zhihu.com/question/480356710/answer/2279799895)

管理员运行命令:

```powershell
reg.exe add "HKCU\Software\Classes\CLSID\{86ca1aa0-34aa-4e8b-a509-50c905bae2a2}\InprocServer32" /f /ve
```

重启就恢复win10右键了

```powershell
reg.exe delete "HKCU\Software\Classes\CLSID\{86ca1aa0-34aa-4e8b-a509-50c905bae2a2}\InprocServer32" /va /f
```

这个是恢复win11右键
