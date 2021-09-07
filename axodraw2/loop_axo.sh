#!/bin/bash
# program to output a axo script which can draw one loop diagrams

tex_name="tex_loop_sh"

fig_size=(350 350)
background=false

## 覆盖之前的文件，重新开始输入
cat >"${tex_name}.tex" <<EOF
\documentclass[a4paper]{article}
\usepackage{axodraw2}
\usepackage{pstricks}
\usepackage{color}

\begin{document}
EOF

#  定义图像的尺寸

cat >>"${tex_name}.tex" <<EOF
\begin{center}
    \begin{axopicture}(${fig_size[0]},${fig_size[1]})
EOF

# 画背景的格子
if ${background}; then
    cat >>"${tex_name}.tex" <<EOF
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
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
#这个图的名字
fig_name='a'
# 图像的左右边界
fig_x1=0
fig_x2=100
# 图像的下上边界
fig_y1=400
fig_y2=470
##++++++++++++++++++++++++++++++++++++++++
# 计算图的各个关键点位置
mid_x=$(((fig_x1 + fig_x2) / 2))
mid_y=$(((fig_y1 + fig_y2) / 2))
# 文字的位置
tex_x1=$((fig_x2 - tex_ofs))
tex_y1=$((fig_y2 - tex_ofs))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
# 光子传播子的位置
pho_x1=$((mid_x))
pho_x2=$((pho_x1))
pho_y1=$((fig_y1 + radius))
pho_y2=$((pho_y1 + pho_len))
# 单实线的位置
line_x1=$((fig_x1))
line_x2=$((fig_x2))
line_y1=$((fig_y1))
line_y2=$((line_y1))
# 介子圈的圆心
arc_x1=$((mid_x))
arc_y1=$((fig_y1))
##++++++++++++++++++++++++++++

cat >>"${tex_name}.tex" <<EOF
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
#光子传播子的长度，振幅,波浪数目
pho_len=40
pho_amp=3
pho_wig=8
# 文字的偏移量，旋转角度
tex_ofs=10
tex_ang=0
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
#介子圈的半径,起始角度和终止角度,虚线尺寸
radius=30
arc_deg1=180
arc_deg2=360
dash=4
#这个图的名字
fig_name='b'
# 图像的左右边界
fig_x1=110
fig_x2=210
# 图像的上下边界
fig_y1=400
fig_y2=470
##+++++++++++++++++++++++++++
# 计算图的各个关键点位置
mid_x=$(((fig_x1 + fig_x2) / 2))
mid_y=$(((fig_y1 + fig_y2) / 2))
# 文字的位置
tex_x1=$((fig_x2 - tex_ofs))
tex_y1=$((fig_y2 - tex_ofs))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
# 介子圈的圆心
arc_x1=$((mid_x))
arc_y1=$((fig_y1 + radius))
# 光子传播子的位置
pho_x1=$((arc_x1))
pho_y1=$((arc_y1))
pho_x2=$((arc_x1))
pho_y2=$((arc_y1 + pho_len))
# 单实线的位置,x1,y1,x2,y2
line1=( 
    $((fig_x1))
    $((arc_y1))
    $((arc_x1-radius))
    $((arc_y1))
)

line2=( 
    $((arc_x1-radius))
    $((arc_y1))
    $((arc_x1))
    $((arc_y1))
)

line3=( 
    $((arc_x1))
    $((arc_y1))
    $((arc_x1+radius))
    $((arc_y1))
)

line4=( 
    $((arc_x1+radius))
    $((arc_y1))
    $((fig_x2))
    $((arc_y1))
)
##++++++++++++++++++++++++++++

cat >>"${tex_name}.tex" <<EOF
        % +++++++++++++++++++++++++++++ 图 a(0,400),(100,470)
        \Line[](${line1[0]},${line1[1]})(${line1[2]},${line1[3]})
        \Line[${arrow}](${line2[0]},${line2[1]})(${line2[2]},${line2[3]})
         \Line[${arrow}](${line3[0]},${line3[1]})(${line3[2]},${line3[3]})
        \Line[](${line4[0]},${line4[1]})(${line4[2]},${line4[3]})
        % (起点)，(终点)
        \DashArc(${arc_x1},${arc_y1})(${radius},${arc_deg1},${arc_deg2}){${dash}}
        % (center)(radius,start_angle,end_angle){dash_size}
        \Photon(${pho_x1},${pho_y1})(${pho_x2},${pho_y2}){${pho_amp}}{${pho_wig}}
        %(start)(end){amplitude}{numbers}
        \Text(${tex_x1},${tex_y1})(${tex_ang}){\$${fig_name}\$}
        % \Text(x,y)(旋转)[对齐]{文字}
        % +++++++++++++++++++++++++++++ 

EOF

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++开始编辑图 c
## 箭头的样式
arrow='arrow,arrowpos=0.5,arrowlength=6,arrowwidth=3,arrowinset=0.1'
#光子传播子的长度，振幅,波浪数目
pho_len=40
pho_amp=3
pho_wig=8
# 文字的偏移量，旋转角度
tex_ofs=10
tex_ang=0
### 方框顶点的宽度和高度
box_w=4
box_h=4
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
#介子圈的半径,起始角度和终止角度,虚线尺寸
radius=30
arc_deg1=180
arc_deg2=360
dash=4
#这个图的名字
fig_name='c'
# 图像的左右边界
fig_x1=0
fig_x2=100
# 图像的上下边界
fig_y1=310
fig_y2=380
##+++++++++++++++++++++++++++
# 计算图的各个关键点位置
mid_x=$(((fig_x1 + fig_x2) / 2))
mid_y=$(((fig_y1 + fig_y2) / 2))
# 文字的位置
tex_x1=$((fig_x2 - tex_ofs))
tex_y1=$((fig_y2 - tex_ofs))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
# 介子圈的圆心
arc_x1=$((mid_x))
arc_y1=$((fig_y1 + radius))
# 光子传播子的位置
pho_x1=$((arc_x1))
pho_y1=$((arc_y1))
pho_x2=$((arc_x1))
pho_y2=$((pho_y1 + pho_len))
# 单实线的位置,x1,y1,x2,y2
line1=( 
    $((fig_x1))
    $((arc_y1))
    $((arc_x1-radius))
    $((arc_y1))
)

line2=( 
    $((arc_x1-radius))
    $((arc_y1))
    $((arc_x1))
    $((arc_y1))
)

line3=( 
    $((arc_x1))
    $((arc_y1))
    $((arc_x1+radius))
    $((arc_y1))
)

line4=( 
    $((arc_x1+radius))
    $((arc_y1))
    $((fig_x2))
    $((arc_y1))
)
# 方块顶点的位置
box_x1=$((pho_x1))
box_y1=$((pho_y1))
##++++++++++++++++++++++++++++

cat >>"${tex_name}.tex" <<EOF
        % +++++++++++++++++++++++++++++ c
        \Line[](${line1[0]},${line1[1]})(${line1[2]},${line1[3]})
        \Line[${arrow}](${line2[0]},${line2[1]})(${line2[2]},${line2[3]})
         \Line[${arrow}](${line3[0]},${line3[1]})(${line3[2]},${line3[3]})
        \Line[](${line4[0]},${line4[1]})(${line4[2]},${line4[3]})
        % (起点)，(终点)
        \DashArc(${arc_x1},${arc_y1})(${radius},${arc_deg1},${arc_deg2}){${dash}}
        % (center)(radius,start_angle,end_angle){dash_size}
        \Photon(${pho_x1},${pho_y1})(${pho_x2},${pho_y2}){${pho_amp}}{${pho_wig}}
        %(start)(end){amplitude}{numbers}
        \Text(${tex_x1},${tex_y1})(${tex_ang}){\$${fig_name}\$}
        % \Text(x,y)(旋转)[对齐]{文字}
        \BBoxc(${box_x1},${box_y1})(${box_w},${box_h})
        %(center)(width,height)
        % +++++++++++++++++++++++++++++ 

EOF

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++开始编辑图 d
## 箭头的样式
arrow='arrow,arrowpos=0.5,arrowlength=6,arrowwidth=3,arrowinset=0.1'
#光子传播子的长度，振幅,波浪数目
pho_len=40
pho_amp=3
pho_wig=8
# 文字的偏移量，旋转角度
tex_ofs=10
tex_ang=0
### 方框顶点的宽度和高度
box_w=4
box_h=4
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
#介子圈的半径,起始角度和终止角度,虚线尺寸
radius=30
arc_deg1=180
arc_deg2=360
dash=4
#这个图的名字
fig_name='d'
# 图像的左右边界
fig_x1=110
fig_x2=210
# 图像的上下边界
fig_y1=310
fig_y2=380
##+++++++++++++++++++++++++++
# 计算图的各个关键点位置
mid_x=$(((fig_x1 + fig_x2) / 2))
mid_y=$(((fig_y1 + fig_y2) / 2))
# 文字的位置
tex_x1=$((fig_x2 - tex_ofs))
tex_y1=$((fig_y2 - tex_ofs))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
# 光子传播子的位置
pho_x1=$((mid_x - radius))
pho_x2=$((pho_x1))
pho_y1=$((fig_y1 + radius))
pho_y2=$((pho_y1 + pho_len))
# 单实线的位置
line_x1=$((fig_x1))
line_x2=$((fig_x2))
line_y1=$((pho_y1))
line_y2=$((line_y1))
# 介子圈的圆心
arc_x1=$((mid_x))
arc_y1=$((pho_y1))
##++++++++++++++++++++++++++++

cat >>"${tex_name}.tex" <<EOF
        % +++++++++++++++++++++++++++++ c
        \Line[${arrow}](${line_x1},${line_y1})(${line_x2},${line_y2})
        % (起点)，(终点)
        \DashArc(${arc_x1},${arc_y1})(${radius},${arc_deg1},${arc_deg2}){${dash}}
        % (center)(radius,start_angle,end_angle){dash_size}
        \Photon(${pho_x1},${pho_y1})(${pho_x2},${pho_y2}){${pho_amp}}{${pho_wig}}
        %(start)(end){amplitude}{numbers}
        \Text(${tex_x1},${tex_y1})(${tex_ang}){\$${fig_name}\$}
        % \Text(x,y)(旋转)[对齐]{文字}
        %\BBoxc(${pho_x1},${pho_y1})(${box_w},${box_h})
        %(center)(width,height)
        % +++++++++++++++++++++++++++++ 

EOF

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++开始编辑图 e
## 箭头的样式
arrow='arrow,arrowpos=0.5,arrowlength=6,arrowwidth=3,arrowinset=0.1'
#光子传播子的长度，振幅,波浪数目
pho_len=40
pho_amp=3
pho_wig=8
# 文字的偏移量，旋转角度
tex_ofs=10
tex_ang=0
### 方框顶点的宽度和高度
box_w=4
box_h=4
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
#介子圈的半径,起始角度和终止角度,虚线尺寸
radius=30
arc_deg1=180
arc_deg2=360
dash=4
#这个图的名字
fig_name='e'
# 图像的左右边界
fig_x1=0
fig_x2=100
# 图像的上下边界
fig_y1=220
fig_y2=290
##+++++++++++++++++++++++++++
# 计算图的各个关键点位置
mid_x=$(((fig_x1 + fig_x2) / 2))
mid_y=$(((fig_y1 + fig_y2) / 2))
# 文字的位置
tex_x1=$((fig_x2 - tex_ofs))
tex_y1=$((fig_y2 - tex_ofs))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
# 光子传播子的位置
pho_x1=$((mid_x + radius))
pho_x2=$((pho_x1))
pho_y1=$((fig_y1 + radius))
pho_y2=$((pho_y1 + pho_len))
# 单实线的位置
line_x1=$((fig_x1))
line_x2=$((fig_x2))
line_y1=$((pho_y1))
line_y2=$((line_y1))
# 介子圈的圆心
arc_x1=$((mid_x))
arc_y1=$((pho_y1))
##++++++++++++++++++++++++++++

cat >>"${tex_name}.tex" <<EOF
        % +++++++++++++++++++++++++++++ c
        \Line[${arrow}](${line_x1},${line_y1})(${line_x2},${line_y2})
        % (起点)，(终点)
        \DashArc(${arc_x1},${arc_y1})(${radius},${arc_deg1},${arc_deg2}){${dash}}
        % (center)(radius,start_angle,end_angle){dash_size}
        \Photon(${pho_x1},${pho_y1})(${pho_x2},${pho_y2}){${pho_amp}}{${pho_wig}}
        %(start)(end){amplitude}{numbers}
        \Text(${tex_x1},${tex_y1})(${tex_ang}){\$${fig_name}\$}
        % \Text(x,y)(旋转)[对齐]{文字}
        %\BBoxc(${pho_x1},${pho_y1})(${box_w},${box_h})
        %(center)(width,height)
        % +++++++++++++++++++++++++++++ 

EOF

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++开始编辑图 f
## 箭头的样式
arrow='arrow,arrowpos=0.5,arrowlength=6,arrowwidth=3,arrowinset=0.1'
#光子传播子的长度，振幅,波浪数目
pho_len=40
pho_amp=3
pho_wig=8
# 文字的偏移量，旋转角度
tex_ofs=10
tex_ang=0
### 方框顶点的宽度和高度
box_w=4
box_h=4
### 实心圆点顶点的半径和灰度
circ_r=3
circ_gs=0
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
#介子圈的半径,起始角度和终止角度,虚线尺寸
radius=30
arc_deg1=180
arc_deg2=360
dash=4
#这个图的名字
fig_name='f'
# 图像的左右边界
fig_x1=110
fig_x2=210
# 图像的上下边界
fig_y1=220
fig_y2=290
##+++++++++++++++++++++++++++
# 计算图的各个关键点位置
mid_x=$(((fig_x1 + fig_x2) / 2))
mid_y=$(((fig_y1 + fig_y2) / 2))
# 文字的位置
tex_x1=$((fig_x2 - tex_ofs))
tex_y1=$((fig_y2 - tex_ofs))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
# 光子传播子的位置
pho_x1=$((mid_x - radius))
pho_x2=$((pho_x1))
pho_y1=$((fig_y1 + radius))
pho_y2=$((pho_y1 + pho_len))
# 单实线的位置
line_x1=$((fig_x1))
line_x2=$((fig_x2))
line_y1=$((pho_y1))
line_y2=$((line_y1))
# 介子圈的圆心
arc_x1=$((mid_x))
arc_y1=$((pho_y1))
# 实心圆点顶点的位置
circ_x1=$((pho_x1))
circ_y1=$((pho_y1))
##++++++++++++++++++++++++++++

cat >>"${tex_name}.tex" <<EOF
        % +++++++++++++++++++++++++++++ c
        \Line[${arrow}](${line_x1},${line_y1})(${line_x2},${line_y2})
        % (起点)，(终点)
        \DashArc(${arc_x1},${arc_y1})(${radius},${arc_deg1},${arc_deg2}){${dash}}
        % (center)(radius,start_angle,end_angle){dash_size}
        \Photon(${pho_x1},${pho_y1})(${pho_x2},${pho_y2}){${pho_amp}}{${pho_wig}}
        %(start)(end){amplitude}{numbers}
        \Text(${tex_x1},${tex_y1})(${tex_ang}){\$${fig_name}\$}
        % \Text(x,y)(旋转)[对齐]{文字}
        %\BBoxc(${pho_x1},${pho_y1})(${box_w},${box_h})
        %(center)(width,height)
        \GCirc(${circ_x1},${circ_y1}){${circ_r}}{${circ_gs}}
        % \GCirc(center){radius}{grayscale}
        % +++++++++++++++++++++++++++++ 

EOF

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++开始编辑图 g
## 箭头的样式
arrow='arrow,arrowpos=0.5,arrowlength=6,arrowwidth=3,arrowinset=0.1'
#光子传播子的长度，振幅,波浪数目
pho_len=40
pho_amp=3
pho_wig=8
# 文字的偏移量，旋转角度
tex_ofs=10
tex_ang=0
### 方框顶点的宽度和高度
box_w=4
box_h=4
### 实心圆点顶点的半径和灰度
circ_r=3
circ_gs=0
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
#介子圈的半径,起始角度和终止角度,虚线尺寸
radius=30
arc_deg1=180
arc_deg2=360
dash=4
#这个图的名字
fig_name='g'
# 图像的左右边界
fig_x1=0
fig_x2=100
# 图像的上下边界
fig_y1=130
fig_y2=200
##+++++++++++++++++++++++++++
# 计算图的各个关键点位置
mid_x=$(((fig_x1 + fig_x2) / 2))
mid_y=$(((fig_y1 + fig_y2) / 2))
# 文字的位置
tex_x1=$((fig_x2 - tex_ofs))
tex_y1=$((fig_y2 - tex_ofs))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
# 光子传播子的位置
pho_x1=$((mid_x + radius))
pho_x2=$((pho_x1))
pho_y1=$((fig_y1 + radius))
pho_y2=$((pho_y1 + pho_len))
# 单实线的位置
line_x1=$((fig_x1))
line_x2=$((fig_x2))
line_y1=$((pho_y1))
line_y2=$((line_y1))
# 介子圈的圆心
arc_x1=$((mid_x))
arc_y1=$((pho_y1))
# 实心圆点顶点的位置
circ_x1=$((pho_x1))
circ_y1=$((pho_y1))
##++++++++++++++++++++++++++++

cat >>"${tex_name}.tex" <<EOF
        % +++++++++++++++++++++++++++++ c
        \Line[${arrow}](${line_x1},${line_y1})(${line_x2},${line_y2})
        % (起点)，(终点)
        \DashArc(${arc_x1},${arc_y1})(${radius},${arc_deg1},${arc_deg2}){${dash}}
        % (center)(radius,start_angle,end_angle){dash_size}
        \Photon(${pho_x1},${pho_y1})(${pho_x2},${pho_y2}){${pho_amp}}{${pho_wig}}
        %(start)(end){amplitude}{numbers}
        \Text(${tex_x1},${tex_y1})(${tex_ang}){\$${fig_name}\$}
        % \Text(x,y)(旋转)[对齐]{文字}
        %\BBoxc(${pho_x1},${pho_y1})(${box_w},${box_h})
        %(center)(width,height)
        \GCirc(${circ_x1},${circ_y1}){${circ_r}}{${circ_gs}}
        % \GCirc(center){radius}{grayscale}
        % +++++++++++++++++++++++++++++ 

EOF

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++开始编辑图 h
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
## 重子双实线的间隔
dbl_sp=1.5
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
#这个图的名字
fig_name='h'
# 图像的左右边界
fig_x1=110
fig_x2=210
# 图像的下上边界
fig_y1=130
fig_y2=200
##++++++++++++++++++++++++++++++++++++++++
# 计算图的各个关键点位置
mid_x=$(((fig_x1 + fig_x2) / 2))
mid_y=$(((fig_y1 + fig_y2) / 2))
# 文字的位置
tex_x1=$((fig_x2 - tex_ofs))
tex_y1=$((fig_y2 - tex_ofs))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
# 光子传播子的位置
pho_x1=$((mid_x))
pho_x2=$((pho_x1))
pho_y1=$((fig_y1 + radius))
pho_y2=$((pho_y1 + pho_len))
# 单实线的位置
line_x1=$((fig_x1))
line_x2=$((fig_x2))
line_y1=$((fig_y1))
line_y2=$((line_y1))
# 介子圈的圆心
arc_x1=$((mid_x))
arc_y1=$((fig_y1))
## 重子双实线的位置
dbl_x1=$((arc_x1 - radius))
dbl_x2=$((arc_x1 + radius))
dbl_y1=$((arc_y1))
dbl_y2=$((dbl_y1))
##++++++++++++++++++++++++++++

cat >>"${tex_name}.tex" <<EOF
        % +++++++++++++++++++++++++++++ 图 a(0,400),(100,470)
        \Line[${arrow}](${line_x1},${line_y1})(${line_x2},${line_y2})
        % (起点)，(终点)
        \DashArc(${arc_x1},${arc_y1})(${radius},${arc_deg1},${arc_deg2}){${dash}}
        % (center)(radius,start_angle,end_angle){dash_size}
        \Photon(${pho_x1},${pho_y1})(${pho_x2},${pho_y2}){${pho_amp}}{${pho_wig}}
        %(start)(end){amplitude}{numbers}
        \Text(${tex_x1},${tex_y1})(${tex_ang}){\$${fig_name}\$}
        % \Text(x,y)(旋转)[对齐]{文字}
        \DoubleLine[${arrow}](${dbl_x1},${dbl_y1})(${dbl_x2},${dbl_y2}){${dbl_sp}}
        % +++++++++++++++++++++++++++++ 

EOF

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++开始编辑图 i
## 箭头的样式
arrow='arrow,arrowpos=0.5,arrowlength=6,arrowwidth=3,arrowinset=0.1'
#介子圈的半径,起始角度和终止角度,虚线尺寸
radius=30
arc_deg1=180
arc_deg2=360
dash=4
#光子传播子的长度，振幅,波浪数目
pho_len=40
pho_amp=3
pho_wig=8
# 文字的偏移量，旋转角度
tex_ofs=10
tex_ang=0
## 重子双实线的间隔
dbl_sp=1.5
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
#这个图的名字
fig_name='i'
# 图像的左右边界
fig_x1=220
fig_x2=320
# 图像的下上边界
fig_y1=400
fig_y2=470
##++++++++++++++++++++++++++++++++++++++++
# 计算图的各个关键点位置
mid_x=$(((fig_x1 + fig_x2) / 2))
mid_y=$(((fig_y1 + fig_y2) / 2))
# 文字的位置
tex_x1=$((fig_x2 - tex_ofs))
tex_y1=$((fig_y2 - tex_ofs))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
# 介子圈的圆心
arc_x1=$((mid_x))
arc_y1=$((fig_y1 + radius))
# 光子传播子的位置
pho_x1=$((arc_x1))
pho_y1=$((arc_y1))
pho_x2=$((arc_x1))
pho_y2=$((pho_y1 + pho_len))
# 单实线的位置x1,y1,x2,y2
line1=( 
    $((fig_x1))
    $((arc_y1))
    $((fig_x2))
    $((arc_y1))
)

line2=( 
    $((arc_x1-radius))
    $((arc_y1))
    $((arc_x1))
    $((arc_y1))
)

line3=( 
    $((arc_x1))
    $((arc_y1))
    $((arc_x1+radius))
    $((arc_y1))
)

line4=( 
    $((arc_x1+radius))
    $((arc_y1))
    $((fig_x2))
    $((arc_y1))
)
## 重子双实线的位置
dbl1=(
$((arc_x1 - radius)) 
$((arc_y1)) 
$((arc_x1 )) 
$((arc_y1)) 
)
dbl2=(
$((arc_x1 )) 
$((arc_y1)) 
$((arc_x1 + radius)) 
$((arc_y1)) 
)
##++++++++++++++++++++++++++++

cat >>"${tex_name}.tex" <<EOF
        % +++++++++++++++++++++++++++++ 图 a(0,400),(100,470)
        \Line[](${line1[0]},${line1[1]})(${line1[2]},${line1[3]})
        % (起点)，(终点)
        \DashArc(${arc_x1},${arc_y1})(${radius},${arc_deg1},${arc_deg2}){${dash}}
        % (center)(radius,start_angle,end_angle){dash_size}
        \Photon(${pho_x1},${pho_y1})(${pho_x2},${pho_y2}){${pho_amp}}{${pho_wig}}
        %(start)(end){amplitude}{numbers}
        \Text(${tex_x1},${tex_y1})(${tex_ang}){\$${fig_name}\$}
        % \Text(x,y)(旋转)[对齐]{文字}
        \DoubleLine[${arrow}](${dbl1[0]},${dbl1[1]})(${dbl1[2]},${dbl1[3]}){${dbl_sp}}
        \DoubleLine[${arrow}](${dbl2[0]},${dbl2[1]})(${dbl2[2]},${dbl2[3]}){${dbl_sp}}
        % +++++++++++++++++++++++++++++ 

EOF

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++开始编辑图 j
## 箭头的样式
arrow='arrow,arrowpos=0.5,arrowlength=6,arrowwidth=3,arrowinset=0.1'
#介子圈的半径,起始角度和终止角度,虚线尺寸
radius=30
arc_deg1=180
arc_deg2=360
dash=4
#光子传播子的长度，振幅,波浪数目
pho_len=40
pho_amp=3
pho_wig=8
# 文字的偏移量，旋转角度
tex_ofs=10
tex_ang=0
## 重子双实线的间隔
dbl_sp=1.5
### 方框顶点的宽度和高度
box_w=4
box_h=4
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
#这个图的名字
fig_name='j'
# 图像的左右边界
fig_x1=340
fig_x2=440
# 图像的下上边界
fig_y1=400
fig_y2=470
##++++++++++++++++++++++++++++++++++++++++
# 计算图的各个关键点位置
mid_x=$(((fig_x1 + fig_x2) / 2))
mid_y=$(((fig_y1 + fig_y2) / 2))
# 文字的位置
tex_x1=$((fig_x2 - tex_ofs))
tex_y1=$((fig_y2 - tex_ofs))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
# 介子圈的圆心
arc_x1=$((mid_x))
arc_y1=$((fig_y1 + radius))
# 光子传播子的位置
pho_x1=$((arc_x1))
pho_y1=$((arc_y1))
pho_x2=$((arc_x1))
pho_y2=$((pho_y1 + pho_len))
# 单实线的位置x1,y1,x2,y2
line1=( 
    $((fig_x1))
    $((arc_y1))
    $((fig_x2))
    $((arc_y1))
)

line2=( 
    $((arc_x1-radius))
    $((arc_y1))
    $((arc_x1))
    $((arc_y1))
)

line3=( 
    $((arc_x1))
    $((arc_y1))
    $((arc_x1+radius))
    $((arc_y1))
)

line4=( 
    $((arc_x1+radius))
    $((arc_y1))
    $((fig_x2))
    $((arc_y1))
)
## 重子双实线的位置
dbl1=(
$((arc_x1 - radius)) 
$((arc_y1)) 
$((arc_x1 )) 
$((arc_y1)) 
)
dbl2=(
$((arc_x1 )) 
$((arc_y1)) 
$((arc_x1 + radius)) 
$((arc_y1)) 
)
# 方块顶点的位置
box_x1=$((arc_x1))
box_y1=$((arc_y1))
##++++++++++++++++++++++++++++

cat >>"${tex_name}.tex" <<EOF
        % +++++++++++++++++++++++++++++ 图 a(0,400),(100,470)
        \Line[](${line1[0]},${line1[1]})(${line1[2]},${line1[3]})
        % (起点)，(终点)
        \DashArc(${arc_x1},${arc_y1})(${radius},${arc_deg1},${arc_deg2}){${dash}}
        % (center)(radius,start_angle,end_angle){dash_size}
        \Photon(${pho_x1},${pho_y1})(${pho_x2},${pho_y2}){${pho_amp}}{${pho_wig}}
        %(start)(end){amplitude}{numbers}
        \Text(${tex_x1},${tex_y1})(${tex_ang}){\$${fig_name}\$}
        % \Text(x,y)(旋转)[对齐]{文字}
        \DoubleLine[${arrow}](${dbl1[0]},${dbl1[1]})(${dbl1[2]},${dbl1[3]}){${dbl_sp}}
        \DoubleLine[${arrow}](${dbl2[0]},${dbl2[1]})(${dbl2[2]},${dbl2[3]}){${dbl_sp}}
         \BBoxc(${box_x1},${box_y1})(${box_w},${box_h})
        %(center)(width,height)
        % +++++++++++++++++++++++++++++ 

EOF

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++开始编辑图 k
#介子圈的半径,起始角度和终止角度,虚线尺寸
radius=30
arc_deg1=180
arc_deg2=360
dash=4
#光子传播子的长度，振幅,波浪数目
pho_len=40
pho_amp=3
pho_wig=8
# 文字的偏移量，旋转角度
tex_ofs=10
tex_ang=0
## 重子双实线的间隔
dbl_sp=1.5
### 方框顶点的宽度和高度
box_w=4
box_h=4
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
#这个图的名字
fig_name='k'
# 图像的左右边界
fig_x1=220
fig_x2=320
# 图像的下上边界
fig_y1=310
fig_y2=380
##++++++++++++++++++++++++++++++++++++++++
# 计算图的各个关键点位置
mid_x=$(((fig_x1 + fig_x2) / 2))
mid_y=$(((fig_y1 + fig_y2) / 2))
# 文字的位置
tex_x1=$((fig_x2 - tex_ofs))
tex_y1=$((fig_y2 - tex_ofs))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
# 光子传播子的位置
pho_x1=$((mid_x))
pho_x2=$((pho_x1))
pho_y1=$((fig_y1 + radius))
pho_y2=$((pho_y1 + pho_len))
# 介子圈的圆心
arc_x1=$((mid_x))
arc_y1=$((pho_y1))
# 单实线的位置
line_x1=$((fig_x1))
line_x2=$((fig_x2))
line_y1=$((arc_y1))
line_y2=$((line_y1))
## 单实线箭头的样式
arr_line='arrow,arrowpos=0.66,arrowlength=6,arrowwidth=3,arrowinset=0.1'
## 重子双实线的位置
dbl_x1=$((arc_x1 - radius))
dbl_x2=$((arc_x1))
dbl_y1=$((arc_y1))
dbl_y2=$((dbl_y1))
##双实线箭头的样式
arr_dbl='arrow,arrowpos=0.5,arrowlength=6,arrowwidth=3,arrowinset=0.1'
# 方块顶点的位置
box_x1=$((arc_x1))
box_y1=$((arc_y1))
##++++++++++++++++++++++++++++

cat >>"${tex_name}.tex" <<EOF
        % +++++++++++++++++++++++++++++ 图 a(0,400),(100,470)
        \Line[${arr_line}](${line_x1},${line_y1})(${line_x2},${line_y2})
        % (起点)，(终点)
        \DashArc(${arc_x1},${arc_y1})(${radius},${arc_deg1},${arc_deg2}){${dash}}
        % (center)(radius,start_angle,end_angle){dash_size}
        \Photon(${pho_x1},${pho_y1})(${pho_x2},${pho_y2}){${pho_amp}}{${pho_wig}}
        %(start)(end){amplitude}{numbers}
        \Text(${tex_x1},${tex_y1})(${tex_ang}){\$${fig_name}\$}
        % \Text(x,y)(旋转)[对齐]{文字}
        \DoubleLine[${arr_dbl}](${dbl_x1},${dbl_y1})(${dbl_x2},${dbl_y2}){${dbl_sp}}
        \BBoxc(${box_x1},${box_y1})(${box_w},${box_h})
        %(center)(width,height)
        % +++++++++++++++++++++++++++++ 

EOF

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++开始编辑图 l
#介子圈的半径,起始角度和终止角度,虚线尺寸
radius=30
arc_deg1=180
arc_deg2=360
dash=4
#光子传播子的长度，振幅,波浪数目
pho_len=40
pho_amp=3
pho_wig=8
# 文字的偏移量，旋转角度
tex_ofs=10
tex_ang=0
## 重子双实线的间隔
dbl_sp=1.5
### 方框顶点的宽度和高度
box_w=4
box_h=4
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
#这个图的名字
fig_name='l'
# 图像的左右边界
fig_x1=340
fig_x2=440
# 图像的下上边界
fig_y1=310
fig_y2=380
##++++++++++++++++++++++++++++++++++++++++
# 计算图的各个关键点位置
mid_x=$(((fig_x1 + fig_x2) / 2))
mid_y=$(((fig_y1 + fig_y2) / 2))
# 文字的位置
tex_x1=$((fig_x2 - tex_ofs))
tex_y1=$((fig_y2 - tex_ofs))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
# 光子传播子的位置
pho_x1=$((mid_x))
pho_x2=$((pho_x1))
pho_y1=$((fig_y1 + radius))
pho_y2=$((pho_y1 + pho_len))
# 介子圈的圆心
arc_x1=$((mid_x))
arc_y1=$((pho_y1))
# 单实线的位置
line_x1=$((fig_x1))
line_x2=$((fig_x2))
line_y1=$((arc_y1))
line_y2=$((line_y1))
## 单实线箭头的样式
arr_line='arrow,arrowpos=0.34,arrowlength=6,arrowwidth=3,arrowinset=0.1'
## 重子双实线的位置
dbl_x1=$((arc_x1))
dbl_x2=$((arc_x1 + radius))
dbl_y1=$((arc_y1))
dbl_y2=$((dbl_y1))
##双实线箭头的样式
arr_dbl='arrow,arrowpos=0.5,arrowlength=6,arrowwidth=3,arrowinset=0.1'
# 方块顶点的位置
box_x1=$((arc_x1))
box_y1=$((arc_y1))
##++++++++++++++++++++++++++++

cat >>"${tex_name}.tex" <<EOF
        % +++++++++++++++++++++++++++++ 图 a(0,400),(100,470)
        \Line[${arr_line}](${line_x1},${line_y1})(${line_x2},${line_y2})
        % (起点)，(终点)
        \DashArc(${arc_x1},${arc_y1})(${radius},${arc_deg1},${arc_deg2}){${dash}}
        % (center)(radius,start_angle,end_angle){dash_size}
        \Photon(${pho_x1},${pho_y1})(${pho_x2},${pho_y2}){${pho_amp}}{${pho_wig}}
        %(start)(end){amplitude}{numbers}
        \Text(${tex_x1},${tex_y1})(${tex_ang}){\$${fig_name}\$}
        % \Text(x,y)(旋转)[对齐]{文字}
        \DoubleLine[${arr_dbl}](${dbl_x1},${dbl_y1})(${dbl_x2},${dbl_y2}){${dbl_sp}}
        \BBoxc(${box_x1},${box_y1})(${box_w},${box_h})
        %(center)(width,height)
        % +++++++++++++++++++++++++++++ 

EOF

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++开始编辑图 m
#介子圈的半径,起始角度和终止角度,虚线尺寸
radius=30
arc_deg1=180
arc_deg2=360
dash=4
#光子传播子的长度，振幅,波浪数目
pho_len=40
pho_amp=3
pho_wig=8
# 文字的偏移量，旋转角度
tex_ofs=10
tex_ang=0
## 重子双实线的间隔
dbl_sp=1.5
### 方框顶点的宽度和高度
box_w=4
box_h=4
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
#这个图的名字
fig_name='m'
# 图像的左右边界
fig_x1=220
fig_x2=320
# 图像的下上边界
fig_y1=220
fig_y2=290
##++++++++++++++++++++++++++++++++++++++++
# 计算图的各个关键点位置
mid_x=$(((fig_x1 + fig_x2) / 2))
mid_y=$(((fig_y1 + fig_y2) / 2))
# 文字的位置
tex_x1=$((fig_x2 - tex_ofs))
tex_y1=$((fig_y2 - tex_ofs))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
# 光子传播子的位置
pho_x1=$((mid_x - radius))
pho_x2=$((pho_x1))
pho_y1=$((fig_y1 + radius))
pho_y2=$((pho_y1 + pho_len))
# 介子圈的圆心
arc_x1=$((mid_x))
arc_y1=$((pho_y1))
# 单实线的位置
line_x1=$((fig_x1))
line_x2=$((fig_x2))
line_y1=$((arc_y1))
line_y2=$((line_y1))
## 单实线箭头的样式
arr_line='arrow,arrowpos=0.5,arrowlength=6,arrowwidth=3,arrowinset=0.1'
## 重子双实线的位置
dbl_x1=$((arc_x1 - radius))
dbl_x2=$((arc_x1 + radius))
dbl_y1=$((arc_y1))
dbl_y2=$((dbl_y1))
##双实线箭头的样式
arr_dbl='arrow,arrowpos=0.5,arrowlength=6,arrowwidth=3,arrowinset=0.1'
# 方块顶点的位置
box_x1=$((arc_x1))
box_y1=$((arc_y1))
##++++++++++++++++++++++++++++

cat >>"${tex_name}.tex" <<EOF
        % +++++++++++++++++++++++++++++ 图 a(0,400),(100,470)
        \Line[${arr_line}](${line_x1},${line_y1})(${line_x2},${line_y2})
        % (起点)，(终点)
        \DashArc(${arc_x1},${arc_y1})(${radius},${arc_deg1},${arc_deg2}){${dash}}
        % (center)(radius,start_angle,end_angle){dash_size}
        \Photon(${pho_x1},${pho_y1})(${pho_x2},${pho_y2}){${pho_amp}}{${pho_wig}}
        %(start)(end){amplitude}{numbers}
        \Text(${tex_x1},${tex_y1})(${tex_ang}){\$${fig_name}\$}
        % \Text(x,y)(旋转)[对齐]{文字}
        \DoubleLine[${arr_dbl}](${dbl_x1},${dbl_y1})(${dbl_x2},${dbl_y2}){${dbl_sp}}
        %\BBoxc(${box_x1},${box_y1})(${box_w},${box_h})
        %(center)(width,height)
        % +++++++++++++++++++++++++++++ 

EOF

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++开始编辑图 n
#介子圈的半径,起始角度和终止角度,虚线尺寸
radius=30
arc_deg1=180
arc_deg2=360
dash=4
#光子传播子的长度，振幅,波浪数目
pho_len=40
pho_amp=3
pho_wig=8
# 文字的偏移量，旋转角度
tex_ofs=10
tex_ang=0
## 重子双实线的间隔
dbl_sp=1.5
### 方框顶点的宽度和高度
box_w=4
box_h=4
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
#这个图的名字
fig_name='n'
# 图像的左右边界
fig_x1=340
fig_x2=440
# 图像的下上边界
fig_y1=220
fig_y2=290
##++++++++++++++++++++++++++++++++++++++++
# 计算图的各个关键点位置
mid_x=$(((fig_x1 + fig_x2) / 2))
mid_y=$(((fig_y1 + fig_y2) / 2))
# 文字的位置
tex_x1=$((fig_x2 - tex_ofs))
tex_y1=$((fig_y2 - tex_ofs))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
# 光子传播子的位置
pho_x1=$((mid_x + radius))
pho_x2=$((pho_x1))
pho_y1=$((fig_y1 + radius))
pho_y2=$((pho_y1 + pho_len))
# 介子圈的圆心
arc_x1=$((mid_x))
arc_y1=$((pho_y1))
# 单实线的位置
line_x1=$((fig_x1))
line_x2=$((fig_x2))
line_y1=$((arc_y1))
line_y2=$((line_y1))
## 单实线箭头的样式
arr_line='arrow,arrowpos=0.5,arrowlength=6,arrowwidth=3,arrowinset=0.1'
## 重子双实线的位置
dbl_x1=$((arc_x1 - radius))
dbl_x2=$((arc_x1 + radius))
dbl_y1=$((arc_y1))
dbl_y2=$((dbl_y1))
##双实线箭头的样式
arr_dbl='arrow,arrowpos=0.5,arrowlength=6,arrowwidth=3,arrowinset=0.1'
# 方块顶点的位置
box_x1=$((arc_x1))
box_y1=$((arc_y1))
##++++++++++++++++++++++++++++

cat >>"${tex_name}.tex" <<EOF
        % +++++++++++++++++++++++++++++ 图 a(0,400),(100,470)
        \Line[${arr_line}](${line_x1},${line_y1})(${line_x2},${line_y2})
        % (起点)，(终点)
        \DashArc(${arc_x1},${arc_y1})(${radius},${arc_deg1},${arc_deg2}){${dash}}
        % (center)(radius,start_angle,end_angle){dash_size}
        \Photon(${pho_x1},${pho_y1})(${pho_x2},${pho_y2}){${pho_amp}}{${pho_wig}}
        %(start)(end){amplitude}{numbers}
        \Text(${tex_x1},${tex_y1})(${tex_ang}){\$${fig_name}\$}
        % \Text(x,y)(旋转)[对齐]{文字}
        \DoubleLine[${arr_dbl}](${dbl_x1},${dbl_y1})(${dbl_x2},${dbl_y2}){${dbl_sp}}
        %\BBoxc(${box_x1},${box_y1})(${box_w},${box_h})
        %(center)(width,height)
        % +++++++++++++++++++++++++++++ 

EOF

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++开始编辑图 o
#介子圈的半径,起始角度和终止角度,虚线尺寸
radius=30
arc_deg1=180
arc_deg2=360
dash=4
#光子传播子的长度，振幅,波浪数目
pho_len=40
pho_amp=3
pho_wig=8
# 文字的偏移量，旋转角度
tex_ofs=10
tex_ang=0
## 重子双实线的间隔
dbl_sp=1.5
### 方框顶点的宽度和高度
box_w=4
box_h=4
### 实心圆点顶点的半径和灰度
circ_r=3
circ_gs=0
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
#这个图的名字
fig_name='o'
# 图像的左右边界
fig_x1=220
fig_x2=320
# 图像的下上边界
fig_y1=130
fig_y2=200
##++++++++++++++++++++++++++++++++++++++++
# 计算图的各个关键点位置
mid_x=$(((fig_x1 + fig_x2) / 2))
mid_y=$(((fig_y1 + fig_y2) / 2))
# 文字的位置
tex_x1=$((fig_x2 - tex_ofs))
tex_y1=$((fig_y2 - tex_ofs))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
# 光子传播子的位置
pho_x1=$((mid_x - radius))
pho_x2=$((pho_x1))
pho_y1=$((fig_y1 + radius))
pho_y2=$((pho_y1 + pho_len))
# 介子圈的圆心
arc_x1=$((mid_x))
arc_y1=$((pho_y1))
# 单实线的位置
line_x1=$((fig_x1))
line_x2=$((fig_x2))
line_y1=$((arc_y1))
line_y2=$((line_y1))
## 单实线箭头的样式
arr_line='arrow,arrowpos=0.5,arrowlength=6,arrowwidth=3,arrowinset=0.1'
## 重子双实线的位置
dbl_x1=$((arc_x1 - radius))
dbl_x2=$((arc_x1 + radius))
dbl_y1=$((arc_y1))
dbl_y2=$((dbl_y1))
##双实线箭头的样式
arr_dbl='arrow,arrowpos=0.5,arrowlength=6,arrowwidth=3,arrowinset=0.1'
# 方块顶点的位置
box_x1=$((arc_x1))
box_y1=$((arc_y1))
# 实心圆点顶点的位置
circ_x1=$((arc_x1 - radius))
circ_y1=$((arc_y1))
##++++++++++++++++++++++++++++

cat >>"${tex_name}.tex" <<EOF
        % +++++++++++++++++++++++++++++ 图 a(0,400),(100,470)
        \Line[${arr_line}](${line_x1},${line_y1})(${line_x2},${line_y2})
        % (起点)，(终点)
        \DashArc(${arc_x1},${arc_y1})(${radius},${arc_deg1},${arc_deg2}){${dash}}
        % (center)(radius,start_angle,end_angle){dash_size}
        \Photon(${pho_x1},${pho_y1})(${pho_x2},${pho_y2}){${pho_amp}}{${pho_wig}}
        %(start)(end){amplitude}{numbers}
        \Text(${tex_x1},${tex_y1})(${tex_ang}){\$${fig_name}\$}
        % \Text(x,y)(旋转)[对齐]{文字}
        \DoubleLine[${arr_dbl}](${dbl_x1},${dbl_y1})(${dbl_x2},${dbl_y2}){${dbl_sp}}
        %\BBoxc(${box_x1},${box_y1})(${box_w},${box_h})
        %(center)(width,height)
        \GCirc(${circ_x1},${circ_y1}){${circ_r}}{${circ_gs}}
        % \GCirc(center){radius}{grayscale}
        % +++++++++++++++++++++++++++++ 

EOF

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++开始编辑图 p
#介子圈的半径,起始角度和终止角度,虚线尺寸
radius=30
arc_deg1=180
arc_deg2=360
dash=4
#光子传播子的长度，振幅,波浪数目
pho_len=40
pho_amp=3
pho_wig=8
# 文字的偏移量，旋转角度
tex_ofs=10
tex_ang=0
## 重子双实线的间隔
dbl_sp=1.5
### 方框顶点的宽度和高度
box_w=4
box_h=4
### 实心圆点顶点的半径和灰度
circ_r=3
circ_gs=0
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
#这个图的名字
fig_name='p'
# 图像的左右边界
fig_x1=340
fig_x2=440
# 图像的下上边界
fig_y1=130
fig_y2=200
##++++++++++++++++++++++++++++++++++++++++
# 计算图的各个关键点位置
mid_x=$(((fig_x1 + fig_x2) / 2))
mid_y=$(((fig_y1 + fig_y2) / 2))
# 文字的位置
tex_x1=$((fig_x2 - tex_ofs))
tex_y1=$((fig_y2 - tex_ofs))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++修改这些值
# 光子传播子的位置
pho_x1=$((mid_x + radius))
pho_x2=$((pho_x1))
pho_y1=$((fig_y1 + radius))
pho_y2=$((pho_y1 + pho_len))
# 介子圈的圆心
arc_x1=$((mid_x))
arc_y1=$((pho_y1))
# 单实线的位置
line_x1=$((fig_x1))
line_x2=$((fig_x2))
line_y1=$((arc_y1))
line_y2=$((line_y1))
## 单实线箭头的样式
arr_line='arrow,arrowpos=0.5,arrowlength=6,arrowwidth=3,arrowinset=0.1'
## 重子双实线的位置
dbl_x1=$((arc_x1 - radius))
dbl_x2=$((arc_x1 + radius))
dbl_y1=$((arc_y1))
dbl_y2=$((dbl_y1))
##双实线箭头的样式
arr_dbl='arrow,arrowpos=0.5,arrowlength=6,arrowwidth=3,arrowinset=0.1'
# 方块顶点的位置
box_x1=$((arc_x1))
box_y1=$((arc_y1))
# 实心圆点顶点的位置
circ_x1=$((arc_x1 + radius))
circ_y1=$((arc_y1))
##++++++++++++++++++++++++++++

cat >>"${tex_name}.tex" <<EOF
        % +++++++++++++++++++++++++++++ 图 a(0,400),(100,470)
        \Line[${arr_line}](${line_x1},${line_y1})(${line_x2},${line_y2})
        % (起点)，(终点)
        \DashArc(${arc_x1},${arc_y1})(${radius},${arc_deg1},${arc_deg2}){${dash}}
        % (center)(radius,start_angle,end_angle){dash_size}
        \Photon(${pho_x1},${pho_y1})(${pho_x2},${pho_y2}){${pho_amp}}{${pho_wig}}
        %(start)(end){amplitude}{numbers}
        \Text(${tex_x1},${tex_y1})(${tex_ang}){\$${fig_name}\$}
        % \Text(x,y)(旋转)[对齐]{文字}
        \DoubleLine[${arr_dbl}](${dbl_x1},${dbl_y1})(${dbl_x2},${dbl_y2}){${dbl_sp}}
        %\BBoxc(${box_x1},${box_y1})(${box_w},${box_h})
        %(center)(width,height)
        \GCirc(${circ_x1},${circ_y1}){${circ_r}}{${circ_gs}}
        % \GCirc(center){radius}{grayscale}
        % +++++++++++++++++++++++++++++ 

EOF

cat >>"${tex_name}.tex" <<EOF
\end{axopicture}
\end{center}
\end{document}
EOF

# 运行latex编译程序

xelatex ${tex_name}
axohelp ${tex_name}
xelatex ${tex_name}

if [ -f "${tex_name}.pdf" ]; then
    evince "${tex_name}.pdf" &
fi

# pdfcrop

if [ -f "${tex_name}.pdf" ]; then
    pdfcrop --margins 3 --clip --bbox '120 480 570 830' "${tex_name}.pdf" "${tex_name}_cropped.pdf"
fi

if [ -f "${tex_name}_cropped.pdf" ]; then
    evince "${tex_name}_cropped.pdf" &
fi
