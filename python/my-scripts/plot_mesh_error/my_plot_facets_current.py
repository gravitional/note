"""
画出内部 Edges上 的面电荷分布, 需要先运行 my_mesh_h5.py, 生成 python 版本的输入
"""
import numpy as np
from numpy import ndarray
import matplotlib.pyplot as plt
from functools import reduce

from _facetsCurrent import facets_current
from my_conf import CFG_data_spec, my_text_fun, VndRaffle, SndRaffle, fct_text_weights

cfg = CFG_data_spec()

# -----------------------
a_color_V = 'k'  # 体单元 line 颜色
a_color_Nd = 'k'  # 节点ID text 颜色
a_color_S = 'k'  # 面单元 line arrow 颜色
a_color_in = 'b'  # inner Edge line, text 颜色


#======================= 画出面电荷分布
def plot_faces_charge(fchaArray: ndarray):
    """
    fchaArray: facet charge array, 名称为 facets_current
    """
    fchLen = fchaArray.shape[0]  # error list 长度
    facet_curr_arr = fchaArray[:, 0:3]  # face charge list
    facet_curr = np.linalg.norm(facet_curr_arr, axis=1)
    xlst = fchaArray[:, 3]
    ylst = fchaArray[:, 4]
    zlst = fchaArray[:, 5]

    # print(f'facet current array: {facet_curr}')
    fchMin = np.amin(facet_curr, axis=0)
    fchMax = np.amax(facet_curr, axis=0)
    # note: face charge 可能为负数
    facet_curr = (facet_curr * (cfg.fct_charge_marker_size / fchMax))
    maxe = plt.gca()
    mfig = plt.gcf()
    myp2 = maxe.scatter(xs=xlst,
                        ys=ylst,
                        zs=zlst,
                        c=facet_curr,
                        s=np.absolute(facet_curr))
    mfig.colorbar(myp2)  # color bar
    return (xlst, ylst, zlst)


# --------- 输入 [x,y,z] 起始点, 结束点
def get_range(start: list, end: list, ratio: float = 0.01):
    dist = [a - b for a, b in zip(end, start)]
    delta = [a * ratio for a in dist]
    start = [a - b for a, b in zip(start, delta)]
    end = [a + b for a, b in zip(end, delta)]
    return start + end


def plot_region():
    fig, axe = plt.subplots(subplot_kw={"projection": "3d"})
    # plot title
    plt.title("Mesh Info & inner face charges")
    plt.rcParams['axes.unicode_minus'] = False
    # plt.rcParams['font.sans-serif'] = ['SimHei'] #中文
    plt.xlabel('X')  # x轴标签
    plt.ylabel('Y')
    # plt.zlabel('Z')

    # -----------
    xlst, ylst, zlst = plot_faces_charge(facets_current)

    xyzList = [
        [np.min(ll, axis=0), np.max(ll, axis=0)] for ll in [xlst, ylst, zlst]
    ]
    xyzL2 = np.transpose(xyzList)
    x1, y1, z1, x2, y2, z2 = get_range(*xyzL2, 0.015)
    plt.gca().set_xlim(x1, x2)
    plt.gca().set_ylim(y1, y2)
    plt.gca().set_zlim(z1, z2)
    plt.show()


if __name__ == '__main__':
    plot_region()
