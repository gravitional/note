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

#  真空介电常数  和 真空磁导率
er0 = 8.854187817e-12
mu0 = 4 * math.pi * 1e-7

_face_charge_norm: float = 1.0E0  # 面电荷密度的放大系数
_face_marker_size: float = 5.0E3  # face charge 记号系数

# -----------------------
_dlt = 0.004  # 节点文字 偏移比率
_irange = 0.006  # 图形范围
_dlt_nd: float = _dlt * _irange  # 节点文字偏移
_ftsz_VS: float = 10  # 体,面单元 文字标注大小
_ftsz_in: float = 7  # inner边 文字标注大小
_in_text_ws = [0.60, 0.40]  # inner边 文字位置, 节点坐标的权重

_color_V = 'k'  # 体单元 line 颜色
_color_Nd = 'k'  # 节点ID text 颜色
_color_S = 'k'  # 面单元 line arrow 颜色
_color_in = 'b'  # inner Edge line, text 颜色

_alpha_VS = 0.20  # 体单元和面单元的透明度
_alpha_in = 0.50  # 内部边界的透明度
_lw_VS = 1.0  # 体单元, 表面单元 线宽度
_lw_in = 1.0  # 内部边 线宽度

#--------- node 坐标 array中, X Y Z数据列的位置
nodePosX = 1
nodePosY = 2
nodePosZ = 3
VnodeOFST: int = 1  # volume ele 构成节点的 列偏移
SnodeOFST: int = 3  # surface ele 构成节点的 列偏移
IFnodeOFST: int = 2  # inner faces 构成节点的 列偏移


#=============== 画出体单元; 输入 node 坐标list; 体单元构成list
def plot_Veles(nodeData: ndarray, Velements: ndarray):

    VeleLen = Velements.shape[0]  # 体单元list 长度
    nodeN = Velements.shape[1] - VnodeOFST  # ele 拥有的节点数
    xlst = np.empty(1 + nodeN)
    ylst = np.empty(1 + nodeN)
    # 遍历体单元列表
    for eleID in range(VeleLen):
        for nodeIdx in range(nodeN):
            # 获取节点获取节点的 index
            nodeID = Velements[eleID, nodeIdx + VnodeOFST]
            # 获取节点的 x,y 坐标
            xlst[nodeIdx] = nodeData[nodeID, nodePosX]  # nodex是三个横坐标
            ylst[nodeIdx] = nodeData[nodeID, nodePosY]  # ylst 是三个纵坐标
            center_x = np.average(xlst[0:-1])
            center_y = np.average(ylst[0:-1])
        xlst[nodeN] = xlst[0]  # 首尾相接
        ylst[nodeN] = ylst[0]
        plt.plot(list(xlst),
                 list(ylst),
                 linestyle='-',
                 linewidth=_lw_VS,
                 color=_color_V,
                 alpha=_alpha_VS)
        plt.text(center_x,
                 center_y,
                 str(eleID),
                 fontsize=_ftsz_VS,
                 color=_color_V)


#=============== 画出节点
def plot_nodes(nodeData: ndarray):
    maxe = plt.gca()
    nodeLen = nodeData.shape[0]
    for ndID in range(nodeLen):
        cx = nodeData[ndID, nodePosX]  # nodex是三个横坐标
        cy = nodeData[ndID, nodePosY]  # ylst 是三个纵坐标
        maxe.text(cx + _dlt_nd,
                  cy + _dlt_nd,
                  f'({ndID}',
                  fontsize=_ftsz_VS,
                  color=_color_Nd)


#=============== 画出 外表面单元;输入 node 坐标list; 面单元构成list
def plot_Seles(nodeData: ndarray, Selements: ndarray):
    SeleLen = Selements.shape[0]  # surf 单元list 长度
    nodeN = Selements.shape[1] - SnodeOFST  # surf ele 拥有的节点数
    xlst = np.empty(1 + nodeN)
    ylst = np.empty(1 + nodeN)

    maxe = plt.gca()
    for eleID in range(SeleLen):
        for nodeIdx in range(nodeN):
            nodeID = Selements[eleID, nodeIdx + SnodeOFST]
            xlst[nodeIdx] = nodeData[nodeID, nodePosX]
            ylst[nodeIdx] = nodeData[nodeID, nodePosY]
            xlst[nodeN] = xlst[0]
            ylst[nodeN] = ylst[0]
        x_tail = xlst[0]
        y_tail = ylst[0]
        dx = xlst[1]
        dy = ylst[1]
        # plt.plot(xlst, ylst, linestyle='-', color='r')
        arrow = mpatches.FancyArrowPatch((x_tail, y_tail), (dx, dy),
                                         mutation_scale=12,
                                         linewidth=_lw_VS,
                                         color=_color_S,
                                         alpha=_alpha_VS)
        maxe.add_patch(arrow)
    maxe.autoscale(tight=True)


def plot_faces_charge(nodeData: ndarray, Velements: ndarray,
                      fchaArray: ndarray):
    """
    fchaArray: face charge array, 名称为 in_faces_charge
    """
    fchLen = fchaArray.shape[0]  # error list 长度
    nodeN = Velements.shape[1] - VnodeOFST  # ele 拥有的节点数
    Vtriangles = np.empty((fchLen, nodeN), dtype=int)  # 体单元节点构成
    fchData = np.empty(fchLen, dtype=float)  # face charge list
    # print(f'face charge ndarray: {fchaArray}')
    face_charge = fchaArray[:, 0]
    xlst = fchaArray[:, 1]
    ylst = fchaArray[:, 2]
    # print(f'face charge array: {face_charge}')
    fchMin = np.amin(face_charge, axis=0)
    fchMax = np.amax(face_charge, axis=0)
    face_charge *= (_face_marker_size / fchMax)
    maxe = plt.gca()
    mfig = plt.gcf()
    myp2 = maxe.scatter(xlst, ylst, c=face_charge, s=face_charge)
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
