# SQLiteStudio

## 备份视频

[如何将学堂在线(安卓)APP视频保存到本地并重命名](https://www.cnblogs.com/XenonZinc/p/6771198.html)

### 环境

+ 魅族 PRO 6 Plus (flyme 6.0, 未root)
+ Excel 2016
+ SQLiteStudio
+ Notepad++

主要步骤

1. 准备工作
1. 视频拷贝到电脑
    视频存储的路径为: `xuetangx/mobilev1/videocache`, 全部拷贝出来, 到电脑即可.
    你会发现视频都是类似于 `D4E7D501976F810F9C33DC5901307461.mp4` 的文件.

### 查找视频信息命名

由于手机没有 `Root`, `APP` 的文件是不可以直接操作的,
因此需要用到 `APP` 备份功能.
`设置-存储与备份-备份手机数据-立即备份`
找到备份文件: `backup/时间戳/App/com.xuetangx.mobile.zip`.

`root` 用户可以在以下路径寻找:

    /data/data/com.xuetangx.mobile/databases

把这个 `zip` 文件拷贝到电脑, 并解压缩后, 找到 `databases/xuetangx.db` 文件. 
这是 `SQLite` 数据库文件, 用 `SQLiteStudio` 打开.

找到表 `T_DOWNLOAD` , 导出成 `csv` 文件, 这个文件是可以用 `Excel` 打开的.

|字段名|含义|
|---|---|
|cc_id|文件名|
|course_name|课程名|
|chapter_num|章节编号|
|sequence_num|课程编号|
|_id|序列号|

一切其他的字段就不一一列举了, 在Excel中可以自由组合出自己想要的文件名.
有些课程是同一章节同一课程下多个视频组成. 此时视频的排序由 `_id` 字段完成.

### 批量重命名

批量对文件进行重命名主要涉及两个命令

`PowerShell` 环境下的 `mv` 命令

```powershell
Move-Item [-Path] <string[]> [[-Destination] <string>]  [<CommonParameters>]
Move-Item [[-Destination] <string>]  [<CommonParameters>]
```

可以简单的理解为 `命令 源文件 目标文件` 的三段格式.
