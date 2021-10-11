#!/bin/bash
# program to output a axo script which can draw one loop diagrams

fig_size=( 400 400 )
background=true

cat>_tex_chpt_1loop.tex<<EOF
\documentclass[a4paper]{article}
\usepackage{axodraw2}
\usepackage{pstricks}
\usepackage{color}

\begin{document}
EOF

#  定义图像的尺寸

cat>>_tex_chpt_1loop.tex<<EOF
\begin{center}
    \begin{axopicture}(${fig_size[0]},${fig_size[1]})
EOF

# 画背景的格子
if ${background}; then
    cat >>_tex_chpt_1loop.tex<<EOF
        % ++++++++++++++++++++格子   
        \AxoGrid(0,0)(10,10)(52,52){LightGray}{0.5}
        % ++++++++++++++++++++
EOF
fi

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++开始编辑图 a
## 箭头的样式
arrow='arrow,arrowpos=0.5,arrowlength=6,arrowwidth=3,arrowinset=0.1'
#介子圈的半径,起始角度和终止角度,虚线尺寸
radius=30
arc_deg1=0
arc_deg2=180
dash=4
#光子传播子的长度，振幅,波浪数目
pho_len=40
pho_amp=3
pho_wig=8
# 文字的偏移量，旋转角度
tex_ofs=10
tex_ang=0
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些植
#这个图的名字
fig_name='a'
# 图像的左下角
fig_x1=0
fig_x2=100
# 图像的右下角
fig_y1=400
fig_y2=470
##++++++++++++++++++++++++++++++++++++++++
# 计算图的各个关键点位置
mid_x=$(((fig_x1 + fig_x2) / 2))
mid_y=$(((fig_y1 + fig_y2) / 2))
# 文字的位置
tex_x1=$((fig_x2 - tex_ofs))
tex_y1=$((fig_y2 - tex_ofs))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些植
# 光子传播子的位置
pho_x1=$(( mid_x )); pho_x2=$(( pho_x1 ));
pho_y1=$(( fig_y1 + radius ));pho_y2=$(( pho_y1 + pho_len))
# 费米子线的位置
line_x1=$(( fig_x1 ));line_x2=$(( fig_x2 ));
line_y1=$(( fig_y1 ));line_y2=$(( line_y1 ));
# 介子圈的圆心
arc_x1=$(( mid_x  ))
arc_y1=$(( fig_y1 ))
##++++++++++++++++++++++++++++

cat>>_tex_chpt_1loop.tex<<EOF
        % +++++++++++++++++++++++++++++ 图 a(0,400),(100,470)
        \Line[${arrow}](${line_x1},${line_y1})(${line_x2},${line_y2})
        % (起点)，(终点)
        \DashArc(${arc_x1},${arc_y1})(${radius},${arc_deg1},${arc_deg2}){${dash}}
        % (center)(radius,start_angle,end_angle){dash_size}
        \Photon(${pho_x1},${pho_y1})(${pho_x2},${pho_y2}){${pho_amp}}{${pho_wig}}
        %(start)(end){amplitude}{numbers}
        \Text(${tex_x1},${tex_y1})(${tex_ang}){\$${fig_name}\$}
        % \Text(x,y)(旋转)[对齐]{文字}
        % +++++++++++++++++++++++++++++ 

EOF

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++开始编辑图 b
## 箭头的样式
arrow='arrow,arrowpos=0.5,arrowlength=6,arrowwidth=3,arrowinset=0.1'
#介子圈的半径,起始角度和终止角度,虚线尺寸
radius=30
arc_deg1=0
arc_deg2=180
dash=4
#光子传播子的长度，振幅,波浪数目
pho_len=40
pho_amp=3
pho_wig=8
# 文字的偏移量，旋转角度
tex_ofs=10
tex_ang=0
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些植
#这个图的名字
fig_name='b'
# 图像的左下角
fig_x1=110
fig_x2=210
# 图像的右下角
fig_y1=400
fig_y2=470
##+++++++++++++++++++++++++++
# 计算图的各个关键点位置
mid_x=$(((fig_x1 + fig_x2) / 2))
mid_y=$(((fig_y1 + fig_y2) / 2))
# 文字的位置
tex_x1=$((fig_x2 - tex_ofs))
tex_y1=$((fig_y2 - tex_ofs))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些植
# 光子传播子的位置
pho_x1=$(( mid_x )); pho_x2=$(( pho_x1 ));
pho_y1=$(( fig_y1 + radius ));pho_y2=$(( pho_y1 + pho_len))
# 费米子线的位置
line_x1=$(( fig_x1 ));line_x2=$(( fig_x2 ));
line_y1=$(( pho_y1 ));line_y2=$(( line_y1 ));
# 介子圈的圆心
arc_x1=$(( mid_x  ))
arc_y1=$(( pho_y1 ))
##++++++++++++++++++++++++++++

cat>>_tex_chpt_1loop.tex<<EOF
        % +++++++++++++++++++++++++++++ 图 a(0,400),(100,470)
        \Line[${arrow}](${line_x1},${line_y1})(${line_x2},${line_y2})
        % (起点)，(终点)
        \DashArc(${arc_x1},${arc_y1})(${radius},${arc_deg1},${arc_deg2}){${dash}}
        % (center)(radius,start_angle,end_angle){dash_size}
        \Photon(${pho_x1},${pho_y1})(${pho_x2},${pho_y2}){${pho_amp}}{${pho_wig}}
        %(start)(end){amplitude}{numbers}
        \Text(${tex_x1},${tex_y1})(${tex_ang}){\$${fig_name}\$}
        % \Text(x,y)(旋转)[对齐]{文字}
        % +++++++++++++++++++++++++++++ 

EOF



cat >>_tex_chpt_1loop.tex<<EOF
\end{axopicture}
\end{center}
\end{document}
EOF
