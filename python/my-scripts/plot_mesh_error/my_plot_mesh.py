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
from my_conf import CFG_data_spec, my_text_fun, VndRaffle, SndRaffle, fct_text_weights

cfg = CFG_data_spec()
if cfg.plot_in_facet:
    from _inFaces_0 import in_inner_faces


#=============== 画出体单元; 输入 node 坐标list; 体单元构成list
def plot_Veles(nodeData: ndarray, Velements: ndarray):
    VeleLen: int = Velements.shape[0]  # 体单元list 长度
    nodeN: int = Velements.shape[1] - cfg.vol_node_ofst  # ele 拥有的节点数
    xlst = np.empty(1 + nodeN)
    ylst = np.empty(1 + nodeN)
    # ----
    center_x: float = 0.0
    center_y: float = 0.0
    # 遍历体单元列表
    for eleID in range(VeleLen):
        vol_clr = cfg.vol_color
        vs_ftsz = cfg.VS_fontsize
        # 突出显示特定单元
        if eleID in cfg.high_id_list:
            vol_clr = cfg.high_color
            vs_ftsz = cfg.high_fontsize
        #---
        for nodeIdx in range(nodeN):
            # 获取节点获取节点的 index
            nodeID = Velements[eleID, nodeIdx + cfg.vol_node_ofst]
            # 获取节点的 x,y 坐标
            xlst[nodeIdx] = nodeData[nodeID, cfg.crd_x_seq]  # nodex是三个横坐标
            ylst[nodeIdx] = nodeData[nodeID, cfg.crd_y_seq]  # ylst 是三个纵坐标
            center_x = np.average(xlst[0:-1])
            center_y = np.average(ylst[0:-1])
        VndRaffle(xlst)  # 二阶节点顺序重排
        VndRaffle(ylst)
        xlst[nodeN] = xlst[0]  # 首尾相接
        ylst[nodeN] = ylst[0]
        plt.plot(xlst,
                 ylst,
                 linestyle='-',
                 linewidth=cfg.VS_line_width,
                 color=cfg.vol_color,
                 alpha=cfg.VS_alpha)
        my_text_fun(center_x,
                    center_y,
                    str(eleID),
                    fontsize=vs_ftsz,
                    color=vol_clr)
    return VeleLen


#=============== 画出节点, 二阶单元包括中间节点
def plot_nodes(nodeData: ndarray):
    maxe = plt.gca()
    nodeLen = nodeData.shape[0]
    for ndID in range(nodeLen):
        node_color = cfg.node_color
        node_fontsz = cfg.VS_fontsize
        # 突出显示特定 节点
        if ndID in cfg.high_nodeID_list:
            node_color = cfg.high_nodeID_color
            node_fontsz = cfg.high_nodeID_fontsize
        #---
        cx = nodeData[ndID, cfg.crd_x_seq]  # nodex是三个横坐标
        cy = nodeData[ndID, cfg.crd_y_seq]  # ylst 是三个纵坐标
        my_text_fun(cx + cfg.node_text_ofst,
                    cy + cfg.node_text_ofst,
                    f'({ndID}',
                    fontsize=node_fontsz,
                    color=node_color)


#=============== 画出 外表面单元;; 输入 node 坐标list; 面单元构成list; surf 编号偏移
def plot_Seles(nodeData: ndarray, Selements: ndarray, idOfst: int):
    SeleLen = Selements.shape[0]  # surf 单元list 长度
    nodeN = Selements.shape[1] - cfg.surf_node_ofst  # surf ele 拥有的节点数
    xlst = np.empty(1 + nodeN)
    ylst = np.empty(1 + nodeN)

    center_x: float = 0.0
    center_y: float = 0.0
    vol_clr = cfg.surf_txt_color
    vs_ftsz = cfg.surf_txt_ftsz

    maxe = plt.gca()
    for eleID in range(SeleLen):
        center_x: float = 0.0
        center_y: float = 0.0
        for nodeIdx in range(nodeN):
            nodeID = Selements[eleID, nodeIdx + cfg.surf_node_ofst]
            xlst[nodeIdx] = nodeData[nodeID, cfg.crd_x_seq]
            ylst[nodeIdx] = nodeData[nodeID, cfg.crd_y_seq]
        center_x = np.average(xlst[0:-1])
        center_y = np.average(ylst[0:-1])
        my_text_fun(center_x,
                    center_y,
                    str(eleID + idOfst),
                    fontsize=vs_ftsz,
                    color=vol_clr)

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
                                             linewidth=cfg.VS_line_width,
                                             color=cfg.surf_color,
                                             alpha=cfg.VS_alpha)
            maxe.add_patch(arrow)
    maxe.autoscale(tight=True)


#==================== 画出内部面，或者内部 Edge, 适用于 2D
def plot_Facets(nodeData: ndarray, inFaces: ndarray):
    SeleLen = inFaces.shape[0]  # surf 单元list 长度
    nodeN = inFaces.shape[1] - cfg.fct_node_ofst  # surf ele 拥有的节点数
    print(f'inner face node num: {nodeN}')
    xlst = np.empty(nodeN)
    ylst = np.empty(nodeN)
    maxe = plt.gca()
    for eleID in range(SeleLen):
        ownID = inFaces[eleID, 0]
        neibID = inFaces[eleID, 1]
        tmpID = inFaces[eleID, 2]  # facet tmp ID
        genSeq = inFaces[eleID, 3]  # facet gen seq
        for nodeIdx in range(nodeN):
            nodeID = inFaces[eleID, nodeIdx + cfg.fct_node_ofst]
            xlst[nodeIdx] = nodeData[nodeID, cfg.crd_x_seq]
            ylst[nodeIdx] = nodeData[nodeID, cfg.crd_y_seq]
        SndRaffle(xlst)
        SndRaffle(ylst)
        maxe.plot(xlst,
                  ylst,
                  linestyle='-',
                  linewidth=cfg.fct_line_width,
                  marker=None,
                  color=cfg.fct_color,
                  alpha=cfg.fct_alpha)
        nodeIDs = inFaces[eleID, cfg.fct_node_ofst:]
        pos_text1 = [
            np.average(xlst, weights=fct_text_weights(nodeN)),
            np.average(ylst, weights=fct_text_weights(nodeN))
        ]
        # 标注信息为: ownerID, neighborID, node1, node2
        my_text_fun(*pos_text1,
                    f'({ownID},{neibID},{tmpID}, {nodeIDs}',
                    fontsize=cfg.fct_fontsize,
                    color=cfg.fct_color,
                    alpha=cfg.fct_alpha)


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
    VeleLen = plot_Veles(nodeData, in_Velements)
    plot_nodes(nodeData)
    plot_Seles(nodeData, in_Selements, VeleLen)
    if cfg.plot_in_facet:
        plot_Facets(nodeData, in_inner_faces)

    x1, y1, x2, y2 = get_range([x1, y1], [x2, y2], 0.015)
    plt.gca().set_xlim(x1, x2)
    plt.gca().set_ylim(y1, y2)
    plt.show()


if __name__ == '__main__':
    plot_region()
