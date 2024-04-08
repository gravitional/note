"""
根据网格信息, 画出单元, 节点, 边界，需要先运行 my_mesh_h5.py, 生成 python 版本的输入
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
from _inFaces_0 import in_inner_faces
from my_conf import VndOS, a_color_V, a_ftsz_VS, a_color_Nd, ndPosX, ndPosY, a_alpha_VS, a_lw_VS, a_lw_in, myText, a_dlt_nd, SndOS, a_color_S, IFndOS, a_in_text_ws, a_color_in, a_alpha_in, a_ftsz_in, VndRaffle, SndRaffle, a_list_spec, a_color_V_spec, a_ftsz_VS_spec


#=============== 画出体单元; 输入 node 坐标list; 体单元构成list
def plot_Veles(nodeData: ndarray, Velements: ndarray):
    VeleLen = Velements.shape[0]  # 体单元list 长度
    nodeN = Velements.shape[1] - VndOS  # ele 拥有的节点数
    xlst = np.empty(1 + nodeN)
    ylst = np.empty(1 + nodeN)
    # 遍历体单元列表
    for eleID in range(VeleLen):
        _color_V = a_color_V
        _ftsz_VS = a_ftsz_VS
        # 突出显示特定单元
        if eleID in a_list_spec:
            _color_V = a_color_V_spec
            _ftsz_VS = a_ftsz_VS_spec
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
                 color=_color_V,
                 alpha=a_alpha_VS)
        myText(center_x,
               center_y,
               str(eleID),
               fontsize=_ftsz_VS,
               color=_color_V)


#=============== 画出节点, 二阶单元包括中间节点
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


#==================== 画出内部面，或者内部 Edge, 适用于 2D
def plot_Facets(nodeData: ndarray, inFaces: ndarray):
    SeleLen = inFaces.shape[0]  # surf 单元list 长度
    nodeN = inFaces.shape[1] - IFndOS  # surf ele 拥有的节点数
    print(f'inner face node num: {nodeN}')
    xlst = np.empty(nodeN)
    ylst = np.empty(nodeN)
    maxe = plt.gca()
    for eleID in range(SeleLen):
        ownID = inFaces[eleID, 0]
        neibID = inFaces[eleID, 1]
        for nodeIdx in range(nodeN):
            nodeID = inFaces[eleID, nodeIdx + IFndOS]
            xlst[nodeIdx] = nodeData[nodeID, ndPosX]
            ylst[nodeIdx] = nodeData[nodeID, ndPosY]
        SndRaffle(xlst)
        SndRaffle(ylst)
        maxe.plot(xlst,
                  ylst,
                  linestyle='-',
                  linewidth=a_lw_in,
                  marker=None,
                  color=a_color_in,
                  alpha=a_alpha_in)
        nodeIDs = inFaces[eleID, IFndOS:]
        pos_text1 = [
            np.average(xlst, weights=a_in_text_ws(nodeN)),
            np.average(ylst, weights=a_in_text_ws(nodeN))
        ]
        # 标注信息为: ownerID, neighborID, node1, node2
        myText(*pos_text1,
               f'({ownID},{neibID}, {nodeIDs}',
               fontsize=a_ftsz_in,
               color=a_color_in,
               alpha=a_alpha_in)


def get_range(start: list, end: list, ratio: float = 0.01):
    dist = [a - b for a, b in zip(end, start)]
    delta = [a * ratio for a in dist]
    start = [a - b for a, b in zip(start, delta)]
    end = [a + b for a, b in zip(end, delta)]
    return start[0], start[1], end[0], end[1]


def plot_region():
    plt.figure()
    # plot title
    plt.title("Mesh Info. Text on Edge: OwnerID, NeighborID, [node1,node2]")
    plt.rcParams['axes.unicode_minus'] = False
    # plt.rcParams['font.sans-serif'] = ['SimHei'] #中文
    plt.xlabel('X')  # x轴标签
    plt.ylabel('Y')  # y轴标签

    nodeData = in_nodes_coord
    x1, y1, _ = np.amin(nodeData[:, 1:], axis=0)
    x2, y2, _ = np.amax(nodeData[:, 1:], axis=0)
    # -----------
    plot_Veles(nodeData, in_Velements)
    plot_nodes(nodeData)
    plot_Seles(nodeData, in_Selements)
    plot_Facets(nodeData, in_inner_faces)

    x1, y1, x2, y2 = get_range([x1, y1], [x2, y2], 0.015)
    plt.gca().set_xlim(x1, x2)
    plt.gca().set_ylim(y1, y2)
    plt.show()


if __name__ == '__main__':
    plot_region()
