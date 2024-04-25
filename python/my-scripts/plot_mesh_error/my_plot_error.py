#!/usr/bin/env python3
"""
画出2维案例的误差分布, 需要先运行 my_mesh_h5.py 生成 python 版本的 mesh 输入
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
from _eleErrors import in_ele_errors
from my_conf import CFG_data_spec, my_text_fun, VndRaffle, SndRaffle

cfg = CFG_data_spec()


#=============== 画出体单元; 输入 node 坐标list; 体单元构成list
def plot_Veles(nodeData: ndarray, Velements: ndarray):
    VeleLen = Velements.shape[0]  # 体单元list 长度
    nodeN = Velements.shape[1] - cfg.vol_node_ofst  # ele 拥有的节点数
    xlst = np.empty(1 + nodeN)
    ylst = np.empty(1 + nodeN)
    # print(f'#ele nodes: {nodeN}')
    # 遍历体单元列表
    for eleID in range(VeleLen):
        vol_clr = cfg.vol_color
        vs_ftsz = cfg.VS_fontsize
        # 突出显示特定单元
        if eleID in cfg.high_id_list:
            vol_clr = cfg.high_color
            vs_ftsz = cfg.high_fontsize
        for nodeIdx in range(nodeN):
            # 获取节点获取节点的 index
            nodeID = Velements[eleID, nodeIdx + cfg.vol_node_ofst]
            # 获取节点的 x,y 坐标
            xlst[nodeIdx] = nodeData[nodeID, cfg.crd_x_seq]  # nodex是三个横坐标
            ylst[nodeIdx] = nodeData[nodeID, cfg.crd_y_seq]  # ylst 是三个纵坐标
            center_x = np.average(xlst[0:-1])
            center_y = np.average(ylst[0:-1])
        VndRaffle(xlst)
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


#=============== 画出节点
def plot_nodes(nodeData: ndarray):
    maxe = plt.gca()
    nodeLen = nodeData.shape[0]
    for ndID in range(nodeLen):
        cx = nodeData[ndID, cfg.crd_x_seq]  # nodex是三个横坐标
        cy = nodeData[ndID, cfg.crd_y_seq]  # ylst 是三个纵坐标
        my_text_fun(cx + cfg.node_text_ofst,
                    cy + cfg.node_text_ofst,
                    f'({ndID}',
                    fontsize=cfg.VS_fontsize,
                    color=cfg.node_color)


#=============== 画出 外表面单元;输入 node 坐标list; 面单元构成list
def plot_Seles(nodeData: ndarray, Selements: ndarray):
    SeleLen = Selements.shape[0]  # surf 单元list 长度
    nodeN = Selements.shape[1] - cfg.surf_node_ofst  # surf ele 拥有的节点数
    xlst = np.empty(1 + nodeN)
    ylst = np.empty(1 + nodeN)
    maxe = plt.gca()
    for eleID in range(SeleLen):
        for nodeIdx in range(nodeN):
            nodeID = Selements[eleID, nodeIdx + cfg.surf_node_ofst]
            xlst[nodeIdx] = nodeData[nodeID, cfg.crd_x_seq]
            ylst[nodeIdx] = nodeData[nodeID, cfg.crd_y_seq]
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


def plot_error(nodeData: ndarray, Velements: ndarray, errArray: ndarray):
    errLen = errArray.shape[0]  # error list 长度
    Vtriangles = np.empty((errLen, cfg.tri_ele_vert_Num), dtype=int)  # 体单元节点构成
    eleErrs = np.empty(errLen, dtype=float)  # 误差向量
    # print(f'ele array: {errArray}')
    ii = 0
    xlst = nodeData[:, cfg.crd_x_seq]
    ylst = nodeData[:, cfg.crd_y_seq]
    for errPair in errArray:
        eleID = int(errPair[cfg.err_eleID_ofst])
        # 对于二阶单元, 只取前三个 node GID, 即只取顶点
        Vtriangles[ii] = Velements[eleID][cfg.tri_ele_vert_seq]
        eleErrs[ii] = errPair[cfg.err_val_ofst] * cfg.err_val_scale
        ii += 1
    # print(f'ele errors: {eleErrs}')
    maxe = plt.gca()
    mfig = plt.gcf()
    tpc = maxe.tripcolor(
        xlst,
        ylst,
        Vtriangles,
        #  vmin=0.0, vmax=1.0E-38,
        facecolors=eleErrs,
        edgecolors='k')
    mfig.colorbar(tpc)  # color bar


def get_range(start: list, end: list, ratio: float = 0.01):
    dist = [a - b for a, b in zip(end, start)]
    delta = [a * ratio for a in dist]
    start = [a - b for a, b in zip(start, delta)]
    end = [a + b for a, b in zip(end, delta)]
    return start[0], start[1], end[0], end[1]


def plot_region():
    plt.figure()
    # plot title
    plt.title("Mesh Info & Element errors")
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
    plot_error(nodeData, in_Velements, in_ele_errors)

    x1, y1, x2, y2 = get_range([x1, y1], [x2, y2], 0.015)
    plt.gca().set_xlim(x1, x2)
    plt.gca().set_ylim(y1, y2)
    plt.show()


if __name__ == '__main__':
    plot_region()
