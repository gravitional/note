#!/usr/bin/env python3
import os,shutil,time,gfm
# 复制到论文中的都是 ci==1.50 的结果
user_name='tom'
# 配置计算结果目录，论文目录，论文压缩文件目录
originpath=os.getcwd()
paper_path=os.path.join('/home',user_name,'private','paper-2.prd/')
desk_path=os.path.join('/home',user_name,'Desktop','paper.ff/')
# 复制计算结果到论文目录
shutil.copy('fig.baryons.ge.charge.L-0.90.ci-1.50.pdf', paper_path+'fig4.pdf') 
shutil.copy('fig.baryons.ge.neutral.L-0.90.ci-1.50.pdf', paper_path+'fig5.pdf') 
shutil.copy('fig.baryons.gm.charge.L-0.90.ci-1.50.pdf', paper_path+'fig2.pdf') 
shutil.copy('fig.baryons.gm.neutral.L-0.90.ci-1.50.pdf', paper_path+'fig3.pdf') 
# cd 到论文目录，重新编译论文
os.chdir(paper_path)
os.system('./build.sh')
# 如果桌面有压缩文件目录，就删除，shutil.copytree需要目标不存在
if  os.path.isdir(desk_path):
    shutil.rmtree(desk_path)
    # 把论文目录的东西复制到桌面目录中
    shutil.copytree('.',desk_path)
else:
    shutil.copytree('.',desk_path)

print("+++++++\nthe file copied from paper_path\n+++++++")
os.listdir(desk_path)

## 切换到桌面整理目录
os.chdir(desk_path )
print("+++++++\ndelete auxilary files\n +++++++")

rm_list=['*.aux','*.lof','*.log','*.lot','*.fls','*.out',
'*.toc', '*.fmt','*.fot','*.cb','*.cb2','*.ptc','*.xdv','*.fdb_latexmk',
'*.synctex.gz','*.swp','*.ps1','*.sh','*.bib','*.bbl','*.blg',
'*.py','*.pyc','__pycache__'
]

for aux in rm_list:
    gfm.rma('.',aux)

print("+++++++\nthe file left in",os.getcwd(),"\n+++++++")
os.listdir(desk_path)

# 产生论文压缩文件
os.system(('7z a ../paper.7z '+desk_path))
# 回到原来的文件夹
os.listdir(originpath)
