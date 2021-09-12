#!/usr/bin/env pwsh
# 编译目录中的 .tex 文件
if ($(Get-ExecutionPolicy) -ne 'Unrestricted') {
    @'
++++++++++++++++++++++++++++++
PowerShell 的默认脚本执行策略不是 'Unrestricted', 这样可以防止执行互联网上的恶意脚本。
但是也没法执行自己编写的脚本。请按下 Win+X, 打开 PowerShell管理员 或者 Windows 终端管理员, 执行:
Set-ExecutionPolicy -Scope CurrentUser,Process Unrestricted
允许本地脚本执行.
-----------------------------------------
'@    
}
if ( -not $( latexmk -v)) {
    @'
    ++++++++++++++++++++++++++++++
       此脚本主要使用 TeXLive 自带的 latexmk 自动化工具, 没有检测到 latexmk 工具,
       请安装最新版 TeXLive 或者 检查环境变量的配置.
       TeX Live: http://tug.org/texlive/
       TeXLive 镜像: https://mirrors.tuna.tsinghua.edu.cn/CTAN/systems/texlive/Images/
    -----------------------------------------
'@
}
if ( -not $(Test-Path ~\.latexmkrc)) {
    @'
++++++++++++++++++++++++++++++
此脚本主要使用 TeXLive 自带的 latexmk 自动化工具, 检测到不存在 ~/.latexmkrc 配置文件，Windows 下此路径一般为, 
    C:\Users\你的名字\.latexmkrc
在其中添加 pdf 浏览器的路径即可,  例如 
    $pdf_previewer = '"E:\tools\sumatrapdf\SumatraPDF.exe" %O %S';
将内层双引号中的""路径替换成你电脑上的安装位置，如果路径没有空格，也可以省略内层的双引号. 
Adobe Reader 可能会加文件锁，造成 LaTeX 没法动态更新PDF.
建议使用SumatraPDF:  https://www.sumatrapdfreader.org/download-free-pdf-viewer
-----------------------------------------
'@
}

# 设置格式化相关的部分
$nameis = "name is :"
# 定义一个打印的函数,并且加上颜色输出
function echo2() {
    $delimiter1 = '-------------------------------------------------------'
    $delimiter2 = '-------------------------------------------------------'
    Write-Host $delimiter1;
    Write-Host -BackgroundColor DarkBlue -ForegroundColor White $args ;
    Write-Host  $delimiter2;
}
# 打印当前目录
echo2 "current directory is ", (Get-Location)
# tex主文档的默认文件名是 main.tex
$tex_usual = "main.tex"
echo2 "tex_usual", $nameis, $tex_usual
# 当前tex文件列表
$tex_here = (Get-ChildItem -path . *.tex)
echo2 "tex_here $nameis,$tex_here"

# 如果有命令行参数，优先使用
if ($args) {
    $tex_here = $(Get-ChildItem $args)
    echo2 "We just compile the, ", $tex_here
}
# 如果无命令行参数, 判断当前文件列表中是否包含 main.tex
elseif ($tex_here -contains 'main.tex') {
    $tex_here = $tex_usual
    echo2 "We just compile the, ", $tex_here
}
# 如果当前目录中没有tex文件，提示检查
elseif ($null -eq $tex_here) {
    echo2 "I can not see any tex file here, check the file names or whatever. "
}
else {
    echo2 "There isn't a tex file named 'main.tex', so we just compile the, ", $tex_here
}
# 开始循环，对每一个tex文件编译，并寻找错误
# 可增加输出文件夹选项 -auxdir=temp -outdir=temp
# 还有 -shell-escape 选项
# 把下面这行加入到 ~/.latexmkrc，指定 pdf 查看程序
# $pdf_previewer = 'evince %O %S';
# -silent 选项可以抑制输出
foreach ($t in $tex_here) {
    # 打印正在处理的tex 文件名字
    echo2 "the tex processed is $t"
    # 用latexmk 逐个编译 *.tex
    latexmk -silent -xelatex -pv -view=pdf -bibtex -cd -recorder -file-line-error -interaction=nonstopmode -synctex=1  ($t.BaseName)
    ## 输出错误记录
    echo2 'echo message'
    $logFile = $t.BaseName + ".log"
    # 之前用 tail 减少输出数量
    # Select-String -Context 0, 8 "\[\d+\]" "$t.log" | tail -n 50
    # 输出警告
    Select-String -Context 0, 8 "\[\d+\]" -Path $logFile
    # 输出各种形式的错误
    echo2 'echo errors 1'
    Select-String -Context 1, 6 "! LaTeX Error:" -Path $logFile
    echo2 'echo errors 2'
    Select-String -Context 1, 6 "\? x" -Path $logFile
    echo2 'echo errors 3'
    Select-String -Context 0, 6 "\.tex:\d+:" -Path $logFile
    #echo2 'echo errors 4'
    # Select-String -Context 0, 6 "! Package tikz Error:" $logFile
    # 给出错误的具体位置 l.123
    echo2 'the error line postion'
    Select-String -Context 1, 1 "l\.\d+" -Path $logFile
}
