# bash-script-exmaples

[shell脚本中一些特殊符号](https://www.cnblogs.com/xuxm2007/archive/2011/10/20/2218846.html)

## perl-rename, 替换文件名不规范字符

```perl
perl -C32 -S rename -v 's/[\(\)\[\]\(\)\<\> <>[]\=, \&\^\$\#\@\!\*\+]/-/g'    * -n
```

## 合并子文件到上层

```bash
SAVEIFS=$IFS
IFS=$(echo -en "\n\b")
# find all files
declare -a fList=($(find . -mindepth 1 -type f -print))
# loop rename and move
for f in ${fList[@]}; do
    mv $f "$(dirname $f)-$(basename $f)"
done
# delete empty directorys
find . -mindepth 1 -type d -print0 | xargs --null rmdir
IFS=$SAVEIFS
```

## 解压压缩文件

```bash
SAVEIFS=$IFS
IFS=$(echo -en "\n\b")
declare -a archs=($(find . -mindepth 1 -maxdepth 1 -type f -iname '*.zip' -print0 | xargs --null basename -s '.zip'))
declare -p archs
totalNum=${#archs[@]}
teNum=1
for i in ${archs[@]}; do
    echo -e "\033[1;44m\033[1;37m Processing number $((teNum++)) of ${totalNum} \033[0;0m"
    7z x "$i.zip" -o$i
done
IFS=$SAVEIFS
```

## formfactor 脚本

复制结果的脚本

```bash
#!/usr/bin/env python3
import os,shutil,time,gfm
# 复制到论文中的都是 ci==1.50 的结果
user_name='tom'
# 配置计算结果目录,论文目录,论文压缩文件目录
originpath=os.getcwd()
result_path=os.path.join(originpath,'expression-results/')
paper_path=os.path.join('/home',user_name,'private','paper-2.prd/')
desk_path=os.path.join('/home',user_name,'Desktop','paper.ff/')
# 复制计算结果到论文目录
shutil.copy(os.path.join(result_path,'fig.baryons.ge.charge.L-0.90.ci-1.50.pdf'),os.path.join(paper_path,'fig4.pdf'))
shutil.copy(os.path.join(result_path,'fig.baryons.ge.neutral.L-0.90.ci-1.50.pdf'),os.path.join(paper_path,'fig5.pdf'))
shutil.copy(os.path.join(result_path,'fig.baryons.gm.charge.L-0.90.ci-1.50.pdf'),os.path.join(paper_path,'fig2.pdf'))
shutil.copy(os.path.join(result_path,'fig.baryons.gm.neutral.L-0.90.ci-1.50.pdf'),os.path.join(paper_path,'fig3.pdf'))
# cd 到论文目录,重新编译论文
os.chdir(paper_path)
# 清除之前的编译结果,重新编译
os.system('latexmk -C')
os.system('./build.sh')
# 如果桌面有压缩文件目录,就删除,shutil.copytree需要目标不存在
src_list=['fig1.pdf','fig2.pdf','fig3.pdf','fig4.pdf','fig5.pdf','octetFF.tex','octetFF.pdf']
# 把论文目录的东西复制到桌面目录中
if  os.path.isdir(desk_path):
    for src in src_list:
        shutil.copy2(src,desk_path)
else:
    os.mkdir(desk_path)
    for src in src_list:
        shutil.copy2(src,desk_path)

## 切换到桌面整理目录
os.chdir(desk_path)

print("+++++++\nthe file left in",os.getcwd(),"\n+++++++")
os.listdir(desk_path)

# 产生论文压缩文件
os.system('rm ../paper.7z; 7z a ../paper.7z '+desk_path)
# 回到原来的文件夹
os.listdir(originpath)
```

## xelatex 编译脚本

[shell 参数换行 & shell 输出换行的方法](https://blog.csdn.net/donaldsy/article/details/99938408)

首先测试一下括号的用法:

```bash
tex_list=1; echo $tex_list; tex_list=$( { ls -x *.tex } ); echo $tex_list;
tex_list=1; echo $tex_list; tex_list=$( ( ls -x *.tex ) ); echo $tex_list;
tex_list=$(ls -x *.tex; ls -x *.log); echo $tex_list;
tex_list=$( (ls -x *.tex; ls -x *.log) ); echo $tex_list;
```

```bash
#!/usr/bin/env bash

texPath=$1

if [[ $2 != "" ]]; then
 texEngine=$2
else
 texEngine="xelatex"
fi
# 设置格式化相关的部分
# 定义一个打印的函数,并且加上颜色输出
function echo2() {
 delimiter1="-------------------------------------------------------"
 delimiter2="-------------------------------------------------------"
 echo -e "${delimiter1}\n\033[1;44m\033[1;37m$*\033[0;0m\n${delimiter2}"
}
# 打印当前目录
echo2 "current directory is: "
pwd
# tex主文档的默认文件名是 main.tex
texUsual="main.tex"
echo2 "texUsual name is: $texUsual"
# 当前tex文件列表
declare -a texArr=($(find . -mindepth 1 -maxdepth 1 -type f -iname '*.tex' -print0 | xargs --null echo))
texHere=${texArr[@]:0:5}
echo2 "The first 5 .tex files here are:"
ls --color=always -h $texHere

# 如果有命令行参数, 优先使用, 后面会去掉后缀名.
result=$(echo $texHere | grep -iP "main.tex")
if [[ $texPath != "" ]]; then
 texHere=$texPath
 echo2 "We just compile the, $texHere"
 # 如果无命令行参数, 判断当前文件列表中是否包含 main.tex
elif [[ $result != "" ]]; then
 texHere=$texUsual
 echo2 "We just compile the, $texHere"
elif [[ $texHere == "" ]]; then
 echo2 "I can not see any tex file here, check the file names or whatever. "
else
 echo2 "because there isn't a tex file named 'main.tex', so we just compile the, $texHere"
fi
# 开始循环, 对每一个tex文件编译, 并寻找错误
# 可增加输出文件夹选项 -auxdir=temp -outdir=temp
# 还有 -shell-escape 选项
# 把下面这行加入到 ~/.latexmkrc, 指定 pdf 查看程序
# $pdf_previewer = 'evince %O %S';
# -silent 选项可以抑制输出
for t in $texHere; do
 # 打印正在处理的tex 文件名字
 echo2 "the tex processed is $t"
 tBase=$(basename -s '.tex' $t)
 # 用latexmk 逐个编译 *.tex
 latexmk -silent -$texEngine -pv -view=pdf -bibtex -cd -recorder -file-line-error -interaction=nonstopmode -synctex=1 $tBase
 ## 输出错误记录
 echo2 'echo message'
 # 输出警告
 grep -m 10 -Pi -n --color -B 0 -A 8 "\[\d+\]" "$tBase.log"
 # 输出各种形式的错误
 echo2 'echo errors 1'
 grep -Pi -m 15 -n --color -B 1 -A 6 -e "! LaTeX Error:" "$tBase.log"
 echo2 'echo errors 2'
 grep -Pi -m 15 -n --color -B 1 -A 6 -e "\? x" "$tBase.log"
 echo2 'echo errors 3'
 grep -Pi -m 15 -n --color -B 0 -A 6 -e "\.tex:\d+:" "$tBase.log"
 #echo2 'echo errors 4'
 #grep -Pi -m 15 -n --color -B 0 -A 6 -e "! Package tikz Error:" "${t}.log"
 # 给出错误的具体位置 l.123
 echo2 'the error line postion'
 grep -Pi -m 15 -n --color -B 1 -A 1 "l\.\d+" "$tBase.log"
done
```

>默认情况下,`echo`关闭了对转义字符的解释, 添加 `-e `参数可打开`echo`对转义字符的解释功能.
`-E`关闭转义字符, 是默认值.

```bash
echo -e "hello\n wrold" #换行输出 hello world
echo -E "hello\n wrold" #输出 hello\n world, 默认情况
```
