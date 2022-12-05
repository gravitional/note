# matlab 搜索路径

[什么是 MATLAB 搜索路径?](https://ww2.mathworks.cn/help/matlab/matlab_env/what-is-the-matlab-search-path.html)

## 检查文件或文件夹是否处于搜索路径上

要确定文件是否处于搜索路径上, 请运行 which filename. 如果文件处于搜索路径上, MATLAB 将返回文件的完整路径.

要确定文件或文件夹是否处于搜索路径上, 请使用 `当前文件夹浏览器`.
不在路径上的文件和文件夹将灰显.
要确保已对当前文件夹浏览器进行设置, 使之指示文件或文件夹是否在搜索路径上,
请右键点击任一文件或文件夹, 然后选择 `指示不在路径中的文件` 选项.

将指针悬停在当前文件夹浏览器中任何灰显的文件或文件夹之上以查明其灰显原因.
此时会打开一个包含说明的工具提示. 工具提示往往会指示文件或文件夹不在 MATLAB 路径上.
如果没有显示工具提示, 则它可能已被禁用.
要启用工具提示, 请进入 `主页` 选项卡, 并在环境部分点击  `预设`.
然后, 选择 `MATLAB > 当前文件夹`.
选择 `显示工具提示`, `说明文件无法访问的原因` 以显示工具提示.

## 查看整个搜索路径

运行 path 命令以查看位于 MATLAB 搜索路径上的所有文件夹.

此外, 可以使用"设置路径"对话框来查看整个 MATLAB 搜索路径. 在主页选项卡上的环境部分中, 点击设置路径.

## 搜索路径不是系统路径

`搜索路径` 与 `系统路径` 不同. 后者是指 windows 系统的 `path`.
此外, MATLAB 搜索路径与系统路径之间没有任何显式关系.
但是, 两个路径都有助于找到文件, 具体如下所述:

+ MATLAB 使用搜索路径更高效地找到 MATLAB 文件.
+ 操作系统可使用系统路径高效地找到操作系统文件.

## MATLAB 如何存储搜索路径

MATLAB 将搜索路径信息保存在 `pathdef.m` 文件中.
此文件包含一系列完整路径名称, 搜索路径上的每个文件夹对应其中一个.

默认情况下, `pathdef.m` 位于 `matlabroot/toolbox/local` 中.

当您更改搜索路径时, `MATLAB` 会在当前会话中使用该路径, 但不会更新 `pathdef.m`.
要在当前和以后的会话中使用修改后的搜索路径, 可以使用 `savepath`,
或`设置路径` 对话框中的 `保存按钮` 来保存所做的更改, 这将更新 `pathdef.m`.

## userpath

查看或更改 `默认用户工作文件夹`, 就是每次启动 matlab 所在的默认 `工作目录`.

```matlab
userpath
userpath(newpath)
userpath('reset')
userpath('clear')
```

### 说明

+ `userpath` 返回用户特定的文件夹(MATLAB 在启动时将其添加到搜索路径中), 指定为字符向量.

+ userpath(newpath) 将搜索路径中的用户特定文件夹设置为 `newpath`.
使用 `userpath` 指定的文件夹不仅立即, 而且在 `以后的会话中` 启动时都将显示在搜索路径中.
MATLAB 将从搜索路径中删除之前由 `userpath` 指定的文件夹.

## path

```matlab
path
path(newpath)
path(oldpath,newfolder)
path(newfolder,oldpath)
p = path(___)
```

### 说明

path 显示 MATLAB 搜索路径, 该路径存储在 `pathdef.m` 中.

+ path(newpath) 将搜索路径更改为 newpath.

+ path(oldpath,newfolder) 将 newfolder 文件夹添加到搜索路径的末尾.
如果 newfolder 已存在于搜索路径中, 则 path(oldpath,newfolder) 将 newfolder 移至搜索路径的底层.
要添加多个文件夹, 请使用 addpath 函数.

+ path(newfolder,oldpath) 将 newfolder 文件夹添加到搜索路径的开头.
如果 newfolder 已经在搜索路径中, 则 path(oldpath,newfolder) 将 newfolder 移到搜索路径的开头.

`p = path(___)` 以字符向量形式返回 MATLAB 搜索路径.
您可以将此语法与上述语法中的任何输入参数结合使用.

## addpath

向 `搜索路径` 中添加文件夹

```matlab
addpath(folderName1,...,folderNameN)
addpath(folderName1,...,folderNameN,position)
addpath(___,'-frozen')
oldpath = addpath(___)
```

### 说明

+ `addpath(folderName1,...,folderNameN)` 将指定的文件夹添加到当前 MATLAB® 会话的搜索路径的顶层.
+ `addpath(folderName1,...,folderNameN,position)`; 将指定的文件夹添加到 position 指定的搜索路径的最前面或最后面.
+ `addpath(___,'-frozen')` 还会为所添加的文件夹禁用文件夹更改检测.
为文件夹禁用文件夹更改检测后, MATLAB 将不会检测从 MATLAB 以外的地方对文件夹所做的更改.
请将此语法与上述语法中的任何参数结合使用. 可以按任一顺序指定 `'-frozen'` 和 `position`.

此外, `oldpath = addpath(___)` 返回在添加指定文件夹之前的路径.

+ 创建一个文件夹, 将其添加到您的搜索路径的最前面,
然后保存搜索路径以用于将来的 MATLAB® 会话.

```matlab
mkdir('matlab/myfiles')
addpath('matlab/myfiles')
savepath matlab/myfiles/pathdef.m
```

+ 将 matlab/myfiles 及其子文件夹添加到搜索路径.
创建文件夹 matlab/myfiles 并在 addpath 内调用 genpath,

```matlab
mkdir('matlab/myfiles')
addpath(genpath('matlab/myfiles'))
```

## fullfile

从各个部分构建完整文件名全页折叠

```matlab
f = fullfile(filepart1,...,filepartN)
```

### 说明

`f = fullfile(filepart1,...,filepartN)` 根据指定的文件夹和文件名构建完整的文件设定.
fullfile 在必要情况下插入依平台而定的文件分隔符, 但不添加尾随的文件分隔符.
在 Windows 平台上, 文件分隔符为反斜杠 (`\`).
在其他平台上, 文件分隔符可能为不同字符.

在 Windows 上, fullfile 将所有正斜杠 (`/`) 替换为反斜杠 (`\`).
在 UNIX 平台上, 反斜杠 (`\`) 字符在文件名中是有效字符, 不会被替换.

fullfile 不裁剪前导或尾随的分隔符.
fullfile 折叠内部重复的文件分隔符, 除非它们出现在完整文件设定的开头.
fullfile 还将折叠由圆点符号指示的相对目录, 除非它们出现在完整文件设定的末尾.
由双圆点符号指示的相对目录不会折叠.

### 示例

+ 在 Windows 上创建完整文件路径. 在 Windows 平台上, 文件分隔符为反斜杠 (`\`).
返回字符向量

```matlab
f = fullfile('myfolder','mysubfolder','myfile.m')
f =

    'myfolder\mysubfolder\myfile.m'
```

+ 在 UNIX 上创建完整文件路径. 在 UNIX 平台上, 文件分隔符为正斜杠 (`/`).

```matlab
f = fullfile('myfolder','mysubfolder','myfile.m')
f =
'myfolder/mysubfolder/myfile.m'
```

+ 在 Windows 上创建多个文件的路径
fullfile 返回一个元胞数组, 其中包含文件 `myfile1.m` 和 `myfile2.m` 的路径.

```matlab
f = fullfile('c:\','myfiles','matlab',{'myfile1.m';'myfile2.m'})
f =

  2×1 cell array

    'c:\myfiles\matlab\myfile1.m'
    'c:\myfiles\matlab\myfile2.m'
```

## Matlab如何获取当前运行.m文件的位置以及文件名称

[当前运行.m文件的位置](https://blog.csdn.net/tsingke/article/details/124030888)

在 `matlab` 程序运行过程中, 有时需要获取 `当前运行程序所在的位置` 以及调用的函数的名称,

下面的方法可以实现:

```matlab
fullpath = mfilename('fullpath');
[fpath,fname]=fileparts(fullpath);
```

得到的 `fpath` 变量就是当前 `.m` 文件所在的目录,
`fname` 是 `.m` 文件的名称(不带 `.m` 后缀)

例如下面语句中关于 `mfilename` 的用法:

```matlab
%获取当前脚本的名称,mfilename本身就是其名称, 合成 Performance\xxx 文件夹
SavePath=strcat('Performance\',mfilename);
if exist(SavePath,'file')==0
　　mkdir(SavePath);
end
```
