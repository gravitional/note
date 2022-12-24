## 搜索当前目录下的 *.mp4, 使用 ffmpeg 合并成一个文件
#--- 设置默认格式, 默认输出名称
$format = "*.mp4"
$outName = 'output.mp4'
## 新建结果文件
New-Item files.txt
Clear-Content files.txt
$files = (Get-ChildItem -Recurse $format)
$lines = @(1..$files.Length); 
$num = 0
foreach ($f in $files) {
    $lines[$num] = "file '{0}'" -f $f.FullName
    $num += 1
}
$lines > files.txt
# 执行ffmpeg 合并命令; safe 0 选项用于忽略不安全的文件名
ffmpeg -f concat -safe 0 -i files.txt -codec copy $outName


##---遍历当前目录下的子目录, 执行 mp4 文件合并
$format = "*.mp4"
$outName = 'output.mp4'
$root = (Get-ChildItem .);
$dirs = (Get-ChildItem -Directory);
foreach ($d in $dirs) {
    Set-Location $d;
    New-Item -Force files.txt;
    Clear-Content files.txt;
    $files = (Get-ChildItem -Recurse $format)
    foreach ($f in $files) {
        "file '{0}'" -f $f.FullName >> files.txt
    };
    ffmpeg -f concat -safe 0 -i files.txt -codec copy $outName
}
Set-Location $root


##--- 将生成的 mp4 文件, 移动到同一个目录下
Set-Location $root
New-Item -Path 'videos' -ItemType "directory"
$num = 0
$files = (Get-ChildItem -Recurse $outName)
foreach ($f in $files) {
    Move-Item $f ("videos/{0}_{1}{2}" -f $f.BaseName, $num, $f.Extension) -WhatIf
    $num += 1
}


##--- 截取视频片段
ffmpeg  -ss     00:33:34    -t    00:32:43  -i  `
    input.mp4 `
    -codec copy -avoid_negative_ts 1 `
    output.mp4

## 左右翻转视频
ffmpeg -i `
    input.mp4 `
    -avoid_negative_ts 1 `
    -vf 'hflip' `
    output.mp4
