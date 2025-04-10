# jetbrains resharper c++

调试.自动窗口, Ctrl+Alt+V,A 全局
调试.局部变量, Ctrl+Alt+V,L 全局
调试.调用堆栈, Ctrl+Alt+C 全局
调试.监视1, Ctrl+Alt+W 1 全局
快速监视,计算表达式; shift+F9, 

`Ctrl+B` Declaration/Definition 切换
`Ctrl+Alt+Shift+B`; Go to Implementation
`Ctrl+Alt+B`; Derived Symbols

`Ctrl+[` containing Declaration
`Alt+Up`; Previous Member
`Alt+Down`; Next  Member
`Ctrl+Shift+Backspace`; Previous Edit
`Ctrl+Shift+Alt+G`; Related Files

在有 Resharper hint 的地方, 例如变量下面有绿色虚线,
`Ctrl+.` == `Alt+Enter`, 打开建议窗口
在普通变量上, `Ctrl+.` 没有效果, `Ctrl+K, Ctrl+i` 打开 VS 的悬浮提示

## Navigate 导航

Search EveryWhere/Go to Type; `Ctrl+N`
Go to File...; 打开文件; Ctrl+Shift+N
File.NewProject `Ct+Al+N`; 全局
View.ASP.NETNonvisualControls; `Ct+Al+N`; HTML编辑器设计视图

Go to Symbol...; 打开符号; Ctrl+Shift+Alt+N

Go to Text ...
Go to File Member...; 到文件中的成员; `Ctrl+F12`
Edit.GoToDeclaration; 全局
Edit.GoToImplementation; `Ctrl+F12`; C#编辑器
Edit.GoToImplementation; `Ctrl+F12`; 带编码功能的C#编辑器
Edit.GoToImplementation; `Ctrl+F12`; Visual Basic 编辑器
Edit.GoToImplementation; `Ctrl+F12`; 带编码功能的 Visual Basic 编辑器



Go to Actions... Ctrl+Shift+A

Navigate To ... Ctrl+Shift+G
Derived Symbols Ctrl+Alt+B

Next Member; `Alt+DArr`; GoToNextMember; 下一个成员;ov
Edit.MoveSelectedLinesDown; `Alt+DArr`; 文本编辑器
Next Member; `Alt+UArr`; GoToPrevMember, 上一个成员;ov
Edit.MoveSelectedLinesUp; `Alt+UArr`; 文本编辑器

Previous Member Alt+Up
containing Declaration `Ctrl+[`

Recent Files... Ctrl+E
Recent Edits... Ctrl+Shift+Alt+Backspace
Previous Edit Ctrl+Shift+Backspace
Related Files Ctrl+Shift+Alt+G
Switch Header/Source Alt+O

Go to File Nearby
Bookmarks `` Ctrl+` ``
Clear All Bookmarks
Clear All Bookmarks in Documents
Breakpoints... Ctrl+Alt+F9

Nevigate Backward; 向后导航, `Ctrl+-`
Nevigate Forward; 向前导航; `Ctrl+Shift+-`

### Find

Optimize Reference ... Ctrl+Alt+Y
Search with Pattern...

Go to Previous Location; `Ctrl+Alt+PgUp`, `Ctrl+Alt+UpArrow`; ReSharper.ReSharper_ResultListGoToPrevLocation; ov;
Window.PreviousTab; Ctrl+Alt+PgUp; 全局

Go to Next Location Ctrl+Alt+Down; ReSharper.ReSharper_ResultListGoToNextLocation; ov;
Window.ShowEzMDIFileList; `Ctrl+Alt+DownArrow`; 全局
InteractiveConsole.SearchHistroyNext; Ctrl+Alt+Down; 交互窗口
Table.RowBelow; Ctrl+Alt+Down; HTML编辑器设计视图

## Edit

Complete Symbol
Import Symbol; Ctrl+Alt+Space
Smart Code Completion; Ctrl+Shift+Space
Complete Statement
parameter Information; Ctrl+P
Show Quick Documentation...; Ctrl+Q

Generate Code... Alt+Ins
Create Ne File... Ctrl+Alt+Ins
Insert Live Template... Ctrl+J
Surround with Template... Ctrl+Alt+J

Extend Selection; Ctrl+W
Duplicate Text; Ctrl+D
Comment With Line Commnet; Ctrl+/
Comment With Block Commnet; Ctrl+Shift/

Rearrange Code; 移动代码位置; 
`C+S+A+UArr`; `C+S+A+DArr`; `C+S+A+LArr`; `C+S+A+RArr`
另外使用 vim 键位, J 向下, K向上,

`Ctrl+Shift+Alt+LArw`; ReSharper.ReSharper_MoveLeft; 移动定义的整体位置, 使用上下左右箭头
Edit.PreviousSubwordExtend; `C+S+A+LArw`; 文本编辑器
`C+S+A+J`; ReSharper.ReSharper_MoveRight;
Edit.NextSubwordExtend; `C+S+A+LArw`; 文本编辑器


Cleanup Code... Ctrl+Alt+F
Silent Cleanup Code; Ctrl+Shift+Alt+F
Reformat Code; Ctrl+Alt+Enter
Apply Syntax Style; Ctrl+Alt+S
Detect Code Style Settings

Paste... Ctrl+Shift+V
Join Lines; Ctrl+Shift+J
Edit Project Item Properties
Copy Code Reference... Ctrl+Shift+Alt+C
Document; 添加注释文档

## Refactor

Refactor This...; Ctrl+Shift+R
Renmae...; Shift+F6
Safe Delete...; Alt+Del
Change Signature.. Ctrl+F6

Extract
Inline; ;就是把函数的定义直接粘贴过来
Convert

`Ct+Al+N` inline variable; 内联变量, 将变量的定义直接填入使用的地方; 移除多余变量定义;
`Ct+Al+M`; Extract Method; 提取方法, 将代码段提取为 成员函数
`Ct+Al+V`; Introduce Variable; 提取变量; 将代码段赋值给变量, 方便重复访问值;
`Ct+Al+D`; Introduce Field; 引入字段(成员变量); 将代码段的值赋给成员变量.

## Inspect

Inspect This...; Ctrl+Shift+Alt+A

Outgoing Calls;
Incoming Calls;
Hierarchies; Ctrl+Alt+H
Show Project Hierarchies

Code issues in Solution
Code issues in Current Project
Load Saved Report...
Save issues Report...

Next Issue in File; 文件中的下一个问题 F12
Previous Issue in File; Shift+F12
Next Error/Waring; Shift+Alt+F12

Solution Errors;
Analyze includes in Solution
Analyze includes in Current Project

## Unit Tests

## Tools

Validate Regular Expression...
To-do Explorer Ctrl+Alt+.
Locate in Solution Explorer Shift+Alt+L
Create Live Template from Selection...

Edit .editorconfig
Browse Stack Trace...; Ctrl+Shift+E
Export Settings to .editorconfig
Run Configurations... Ctrl+Shift+Alt+R
Template Explorer...
Build & Run

## Options...

## Manager Options...

## Extension Manager...

## Windows

Unit Test Explorer; Ctrl+Alt+T
Unit Test Sessions; Ctrl+Alt+R
Unit Test Exploreration Results; Ctrl+T, Ctrl+O
To-do Explorer; Ctrl+Alt+.
Stack Traces
Process Explorer

Inspections `Ctrl+Alt+.`
Hierarchies
Find Results; `Ctrl+Alt+U`
Solution Errors
File Formatting Info
File Struture; `Ctrl+F11`
BreakPoints;
Build Results
NuGet Browser
Build & Run


`Ctrl+Alt+U`; ReSharper.ReSharper_ShowFindResults; ov;
Debug.Modules; `Ctrl+Alt+U`; 全局

`Alt+F7`; ReSharper.ReSharper_FindUsages; ov
Window.NextToolWindowNav; Alt+F7; 全局


`Ctrl+Alt+F9`; ReSharper.ReSharper_GotoBreakpoints; ov
View.Publish; Ctrl+Alt+F9; 全局
`Ctrl+Shift+F9`; 删除所有断点.


`Ctrl+F11`; ReSharper.ReSharper_ShowCodesStructure; ov
Debug.ToggleDisassembly; Ctrl+F11; 全局


`Ctrl+Q`; ReSharper.ReSharper_QuickDoc, ov 
Window.ActivateQuickLaunch; 全局

`Ctrl+Shift+T` ReSharper.ReSharper_GotoTypeDeclaration; ov 
Edit.GoToFile; 全局

+ 重定义; 编辑器上下文菜单.导航.与活动文档同步, `ctrl+, S`, control+逗号 然后按 s
+ 重定义; 编辑.转到所有, `ctrl+, ctrl+.`, control+逗号, 句号, 弹出 VS 的代码搜索页面

`F6`; ReSharper.ReSharper_Move; ov; 重命名文件
Window.NextSplitPane; F6 全局
FocusAllEnvironmentsComboBox; F6; Rest API 编辑器

`F12`; ReSharper.ReSharper_GotoNextHighlight; ov
Edit.GoToDefinition,全局
TestExplorer.OpenTest, 测试资源管理器
TestExplorer.OpenTest, Live Unit Testing 选项卡
TestExplorer.OpenTest, "测试资源管理器播放列表"选项卡

`Shift+F12`; ReSharper.ReSharper_GotoPrevHighlight; ov 
Edit.FindAllReferences; 全局

`Ctrl+Enter`; ReSharper.ReSharper_ForceCompleteItem; ov 
Edit.LineOpenAbove; 文本编辑器

`Alt+Enter`; ReSharper.ReSharper_AltEnter; ov
View.QuickActions; 文本编辑器

`Ctrl+E`; ReSharper.ReSharper_GotoRecentFiles; ov 
Edit.ToggleWordWrap; Ctrl+E,Ctrl+W; 文本编辑器
Edit.Duplicate; Ctrl+E,V; 文本编辑器
Edit.Duplicate; Ctrl+E,Ctrl+V; 文本编辑器
View.ShowTaskStatusCenter; Ctrl+E,Ctrl+T; 全局
...

`Ctrl+R, Ctrl+R`; ReSharper.ReSharper_Rename; 重命名，重构

`Ctrl+T, Ctrl+T`; ReSharper.ReSharper_UnitTestSessionRepeatPreviousRun;
Format.TestDialog; Ctrl+T; VC对话框编辑器
Image.TextTool; Ctrl+T; VC图像编辑器
Edit.NavigateTo; Ctrl+T; 全局
QueryDesigner.CancelRetrievingData; Ctrl+T; 查询设计器
QueryDesigner.CancelRetrievingData; Ctrl+T; 查看设计器

`Ctrl+U`; ReSharper.ReSharper_GotoBase; 跳转到基类; ov
Format.Underline; Ctrl+U; HTML 编辑器设计视图
Edit.MakeLowercase; Ctrl+U; 文本编辑器
Image.UseSelectionBrush; Ctrl+U; VC图像编辑器

`Ctrl+D`; ReSharper.ReSharper_DuplicateText; 复制文字; ov
Edit.Duplicate; Ctrl+D; 文本编辑器

`Ctrl+J`; ReSharper.ReSharper_LiveTemplatesInsert; 
Edit.ListMembers; Ctrl+J; 文本编辑器

`Ctrl+W`; ReSharper.ReSharper_ExtendSeclection; ov
Edit.SelectCurrentWord; 文本编辑器

`Ctrl+B`; ReSharper.ReSharper_GotoDeclaration; ov
Format.Bold Ctrl+B; HTML编辑器设计视图
Build.BuildSelecton; Ctrl+B; 全局
Format.ButtonBottom; Ctrl+B; VC对话框编辑器
Image.BrushTool; Ctrl+B; VC图像编辑器
生成.生成选定内容, `Ctrl+, B`

`Ctrl+N`; ReSharper.ReSharper_GotoType; ov
File.NewFile; Ctrl+N; 全局

`Ctrl+/`; ReSharper.ReSharper_LineComment; ov
Edit.ToggleLineComment; Ctrl+/; 文本编辑器.

`Ctrl+Alt+B`; ReSharper.ReSharper_GoToInheritors; ov
Debug.BreakPoints; `Ctrl+Alt+B`; 全局

`Ctrl+Alt+M`; ReSharper.ReSharper_ExtractMethod; ov
Debug.Memory1 Ctrl+Alt+M 1; 全局
Debug.Memory2 Ctrl+Alt+M 2; 全局
Debug.Memory3 Ctrl+Alt+M 3; 全局
Debug.Memory4 Ctrl+Alt+M 4; 全局