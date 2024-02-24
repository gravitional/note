"""
画出内部 Edges上 的面电荷分布, 需要先运行 my_mesh_h5.py, 生成 python 版本的输入
"""
import math
import numpy as np
from numpy import ndarray
import matplotlib.pyplot as plt
import matplotlib.figure as mpfig
import matplotlib.patches as mpatches
#  导入 节点list 和 单元 list
from _nodes import in_nodes_coord
from _Velements import in_Velements
from _Selements import in_Selements
from _facesCharge import in_faces_charge
from my_conf import *

# -----------------------
myText = plt.text  # 控制是否显示 文字标注; 空函数为: myText=my_void

a_color_V = 'k'  # 体单元 line 颜色
a_color_Nd = 'k'  # 节点ID text 颜色
a_color_S = 'k'  # 面单元 line arrow 颜色
a_color_in = 'b'  # inner Edge line, text 颜色


#=============== 画出体单元; 输入 node 坐标list; 体单元构成list
def plot_Veles(nodeData: ndarray, Velements: ndarray):
    VeleLen = Velements.shape[0]  # 体单元list 长度
    nodeN = Velements.shape[1] - VndOS  # ele 拥有的节点数
    xlst = np.empty(1 + nodeN)
    ylst = np.empty(1 + nodeN)
    # 遍历体单元列表
    for eleID in range(VeleLen):
        for nodeIdx in range(nodeN):
            # 获取节点获取节点的 index
            nodeID = Velements[eleID, nodeIdx + VndOS]
            # 获取节点的 x,y 坐标
            xlst[nodeIdx] = nodeData[nodeID, ndPosX]  # nodex是三个横坐标
            ylst[nodeIdx] = nodeData[nodeID, ndPosY]  # ylst 是三个纵坐标
            center_x = np.average(xlst[0:-1])
            center_y = np.average(ylst[0:-1])
        VndRaffle(xlst)
        VndRaffle(ylst)
        xlst[nodeN] = xlst[0]  # 首尾相接
        ylst[nodeN] = ylst[0]
        plt.plot(list(xlst),
                 list(ylst),
                 linestyle='-',
                 linewidth=a_lw_VS,
                 color=a_color_V,
                 alpha=a_alpha_VS)
        myText(center_x,
               center_y,
               str(eleID),
               fontsize=a_ftsz_VS,
               color=a_color_V)


#=============== 画出节点
def plot_nodes(nodeData: ndarray):
    maxe = plt.gca()
    nodeLen = nodeData.shape[0]
    for ndID in range(nodeLen):
        cx = nodeData[ndID, ndPosX]  # nodex是三个横坐标
        cy = nodeData[ndID, ndPosY]  # ylst 是三个纵坐标
        myText(cx + a_dlt_nd,
               cy + a_dlt_nd,
               f'({ndID}',
               fontsize=a_ftsz_VS,
               color=a_color_Nd)


#=============== 画出 外表面单元;输入 node 坐标list; 面单元构成list
def plot_Seles(nodeData: ndarray, Selements: ndarray):
    SeleLen = Selements.shape[0]  # surf 单元list 长度
    nodeN = Selements.shape[1] - SndOS  # surf ele 拥有的节点数
    xlst = np.empty(1 + nodeN)
    ylst = np.empty(1 + nodeN)
    maxe = plt.gca()
    for eleID in range(SeleLen):
        for nodeIdx in range(nodeN):
            nodeID = Selements[eleID, nodeIdx + SndOS]
            xlst[nodeIdx] = nodeData[nodeID, ndPosX]
            ylst[nodeIdx] = nodeData[nodeID, ndPosY]
        SndRaffle(xlst)
        SndRaffle(ylst)
        for ai in range(nodeN - 1):
            x_tail = xlst[ai]
            y_tail = ylst[ai]
            dx = xlst[ai + 1]
            dy = ylst[ai + 1]
            # plt.plot(xlst, ylst, linestyle='-', color='r')
            arrow = mpatches.FancyArrowPatch((x_tail, y_tail), (dx, dy),
                                             mutation_scale=12,
                                             linewidth=a_lw_VS,
                                             color=a_color_S,
                                             alpha=a_alpha_VS)
            maxe.add_patch(arrow)
    maxe.autoscale(tight=True)


#======================= 画出面电荷分布
def plot_faces_charge(nodeData: ndarray, Velements: ndarray,
                      fchaArray: ndarray):
    """
    fchaArray: face charge array, 名称为 in_faces_charge
    """
    fchLen = fchaArray.shape[0]  # error list 长度
    face_charge = fchaArray[:, 0]  # face charge list
    xlst = fchaArray[:, 1]
    ylst = fchaArray[:, 2]
    # print(f'face charge array: {face_charge}')
    fchMin = np.amin(face_charge, axis=0)
    fchMax = np.amax(face_charge, axis=0)
    # note: face charge 可能为负数
    face_charge = (face_charge * (a_face_marker_size / fchMax))
    maxe = plt.gca()
    mfig = plt.gcf()
    myp2 = maxe.scatter(xlst, ylst, c=face_charge, s=abs(face_charge))
    mfig.colorbar(myp2)  # color bar


def get_range(start: list, end: list, ratio: float = 0.01):
    dist = [a - b for a, b in zip(end, start)]
    delta = [a * ratio for a in dist]
    start = [a - b for a, b in zip(start, delta)]
    end = [a + b for a, b in zip(end, delta)]
    return start[0], start[1], end[0], end[1]


def plot_region():
    plt.figure()
    # plot title
    plt.title("Mesh Info & inner face charges")
    plt.rcParams['axes.unicode_minus'] = False
    # plt.rcParams['font.sans-serif'] = ['SimHei'] #中文
    plt.xlabel('X')  # x轴标签
    plt.ylabel('Y')  # y轴标签

    nodeData = in_nodes_coord
    x1, y1, _ = np.amin(nodeData[:, 1:], axis=0)
    x2, y2, _ = np.amax(nodeData[:, 1:], axis=0)
    # -----------
    plot_Veles(nodeData, in_Velements)
    # plot_nodes(nodeData)
    plot_Seles(nodeData, in_Selements)
    plot_faces_charge(nodeData, in_Velements, in_faces_charge)

    x1, y1, x2, y2 = get_range([x1, y1], [x2, y2], 0.015)
    plt.gca().set_xlim(x1, x2)
    plt.gca().set_ylim(y1, y2)
    plt.show()


if __name__ == '__main__':
    plot_region()
