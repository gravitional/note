## 搜索当前目录下的 *.mp4, 使用 ffmpeg 合并成一个文件
#--- 设置默认格式, 默认输出名称
$format = "*.mp4"
$outName = 'output.mp4'
## 新建结果文件
New-Item files.txt
Clear-Content files.txt
$files = (Get-ChildItem -Recurse $format)
$lines = @(1..$files.Length)
$num = 0
foreach ($f in $files) {
    $lines[$num] = "file '{0}'" -f $f.FullName
    $num += 1
}
$lines > files.txt
# 执行ffmpeg 合并命令; safe 0 选项用于忽略不安全的文件名
ffmpeg -y -f concat -safe 0 -i files.txt -codec copy $outName


##---遍历当前目录下的子目录, 执行 mp4 文件合并
$format = "*.mp4"
$outName = 'output.mp4'
$root = (Get-Location)
$dirs = (Get-ChildItem -Directory  -LiteralPath $root )
foreach ($d in $dirs) {
    Set-Location -LiteralPath $d
    New-Item -Force files.txt
    Clear-Content files.txt
    $files = (Get-ChildItem  -LiteralPath $d -Recurse $format )
    foreach ($f in $files) {
        "file '{0}'" -f $f.FullName >> files.txt
    }
    ffmpeg -y -f concat -safe 0 -i files.txt -codec copy $outName
}
Set-Location $root


##--- 将生成的 mp4 文件, 移动到同一个目录下
$root = (Get-Location)
$outName = 'output.mp4'
$dir = "{0}-v" -f (Split-Path -Leaf $root) || 'video'
New-Item -Path $dir -ItemType "directory"
$num = 22
$files = (Get-ChildItem -LiteralPath $root -Recurse $outName )
foreach ($f in $files) {
    Move-Item -LiteralPath $f  -Destination ("{0}/{1}_{2}{3}" -f $dir, $f.BaseName, $num, $f.Extension)
    $num += 1
}


##--- 截取视频片段
ffmpeg  -y -ss     00:33:34    -t    00:32:43  -i  `
    input.mp4 `
    -codec copy -avoid_negative_ts 1 `
    output.mp4

## 左右翻转视频
ffmpeg -y -i `
    input.mp4 `
    -avoid_negative_ts 1 `
    -vf 'hflip' `
    output.mp4
