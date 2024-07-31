import math
import matplotlib.pyplot as plt
from dataclasses import dataclass, field


def my_void(*args, **kwargs):
    pass


# 控制是否显示 单元文字标注;
my_text_fun = None
if 1:
    my_text_fun = plt.text  # 显示单元编号
else:
    my_text_fun = my_void  #空函数


#------- facet 上文字位置, 节点坐标的权重
def fct_text_weights(nodeN):
    if 2 == nodeN:
        return [0.60, 0.40]
    elif 3 == nodeN:
        return [0.60, 0.40, 0.0]


# 二阶 vol单元节点重排序
def VndRaffle(il):
    n = len(il) - 1  # len= 单元节点数 +1, +1用于画图封闭轮廓
    if n == 3:
        pass
    elif n == 6:
        (il[1], il[2], il[3], il[4]) = (il[3], il[1], il[4], il[2])


# 二阶 surf单元 节点重排序
def SndRaffle(il):
    n = len(il) - 1
    if n == 2:
        pass
    elif n == 3:
        il[1], il[2] = (il[2], il[1])


#----------------  真空介电常数  和 真空磁导率
er0: float = 8.854187817e-12
mu0: float = 4 * math.pi * 1e-7

# 缩写含义::
# ofst: offset;  vol: volume;  surf: surface;
# fct: facet;  tri: triangle;  ele: element
# vert: vertex;  indi: indicator; high: highlight


#---------------- volume, surface, facet 数据特征指定
# @dataclass
class CFG_data_spec:
    plot_in_facet: bool = False  # 是否画出 内部 facets

    crd_x_seq: int = 1  # node 坐标 array中, X Y Z数据列的位置
    crd_y_seq: int = 2
    crd_z_seq: int = 3
    vol_node_ofst: int = 1  # volume ele 构成节点的 列偏移
    surf_node_ofst: int = 3  # surface ele 构成节点的 列偏移
    fct_node_ofst: int = 4  # inner facets 构成节点的 列偏移

    tri_ele_vert_Num: int = 3  # 三角形单元顶点数; 对一二阶单元相同
    tri_ele_vert_seq: list[int] = [
        vol_node_ofst + 0, vol_node_ofst + 1, vol_node_ofst + 2
    ]  # 一阶或二阶单元中, 三角形顶点的 loc 索引

    # ---- 误差分析数据
    err_eleID_ofst: int = 0  # error data 中 ele Global ID 的列偏移
    err_val_ofst: int = 1  # error data 中 error value 的 列偏移
    err_refn_indi_ofst: int = 2  # error data 中 Refine Indicator 的列偏移

    # ----------------------- 误差 归一化
    err_val_scale: float = 1.0E0  # 误差的放大系数
    err_indi_scale: float = 1.0E0  # indicator 的放大系数
    fct_charge_scale: float = 1.0E0  # 面电荷密度的放大系数
    fct_charge_marker_size: float = 1.0E3  # face charge 记号系数

    # --------------------- 画图的 text 和 color 配置
    node_text_ofst_ratio: float = -0.001  # 节点文字 偏移比率
    axe_range: float = 0.006  # 图形范围
    node_text_ofst: float = node_text_ofst_ratio * axe_range  # 节点文字偏移
    VS_fontsize: float = 10  # 体,面单元 文字标注大小
    fct_fontsize: float = 7  # facet 文字标注大小

    vol_color: str = 'olive'  # 体单元 line 颜色
    node_color: str = 'green'  # 节点ID text 颜色
    surf_color: str = 'red'  # 面单元 line arrow 颜色
    fct_color: str = 'blue'  # inner Edge line, text 颜色

    surf_txt_color: str = 'red'  # 面单元文字标注颜色
    surf_txt_ftsz: float = 9  # edge 文字标注大小

    VS_alpha: float = 0.20  # 体单元和面单元的透明度
    fct_alpha: float = 0.50  # 内部边界的透明度
    VS_line_width: float = 3.0  # 体单元, 表面单元 线宽度
    fct_line_width: float = 1.0  # 内部边 线宽度

    # --------------------------- 特殊上色区域, element
    high_id_list: set[int] = {1, 2, 3, 4}
    high_color: str = 'magenta'
    high_fontsize: int = 12

    # --------------------------- 特殊上色区域, node
    high_nodeID_list: set[int] = {1, 2, 3, 4}
    high_nodeID_color: str = 'black'
    high_nodeID_fontsize: int = 14
