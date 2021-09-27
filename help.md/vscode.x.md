# vscode.x

[code.visualstudio.com/docs](https://code.visualstudio.com/docs)
[codebasics](https://code.visualstudio.com/docs/editor/codebasics)

## 按文件名搜索文件

按快捷键`ctrl+p`可以弹出一个小窗, 在上面的输入框输入文件名, 下拉框点击一个文件.

## latex 编译调试

```powershell
$temp=latexmk -f -xelatex; Write-Output "++++++++++++" ;$temp | Where-Object {$_ -like "*tex:*"}
```

```powershell
$temp | Where-Object {$_ -like "*tex:*"}
```

## 快速打开文件

Keyboard Shortcut: `Ctrl+P`

### Open multiple files from Quick Open

你可以通过按 `右箭头` 从快速打开中打开多个文件.
这将在后台打开当前选定的文件, 而你可以继续从 "快速打开 "中选择文件.

### code 环境变量

[Variables Reference](https://code.visualstudio.com/docs/editor/variables-reference)

以下是预定义的变量: 

+ `${workspaceFolder}` - the path of the folder opened in VS Code
+ `${workspaceFolderBasename}` - the name of the folder opened in VS Code without any slashes (/)
+ `${file}` - the current opened file
+ `${relativeFile}` - the current opened file relative to workspaceFolder
+ `${relativeFileDirname}` - the current opened file's dirname relative to workspaceFolder
+ `${fileBasename}` - the current opened file's basename
+ `${fileBasenameNoExtension}` - the current opened file's basename with no file extension
+ `${fileDirname}` - the current opened file's dirname
+ `${fileExtname}` - the current opened file's extension
+ `${cwd}` - the task runner's current working directory on startup
+ `${lineNumber}` - the current selected line number in the active file
+ `${selectedText}` - the current selected text in the active file
+ `${execPath}` - the path to the running VS Code executable
+ `${defaultBuildTask}` - the name of the default build task
