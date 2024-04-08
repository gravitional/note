import math
import matplotlib.pyplot as plt


def my_void(*args, **kwargs):
    pass


# 控制是否显示 单元文字标注;
myText = None
if 1:
    myText = plt.text  # 显示单元编号
else:
    myText = my_void  #空函数


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
er0 = 8.854187817e-12
mu0 = 4 * math.pi * 1e-7

#---------------- node 坐标 array中, X Y Z数据列的位置
ndPosX = 1
ndPosY = 2
ndPosZ = 3
VndOS: int = 1  # volume ele 构成节点的 列偏移
SndOS: int = 3  # surface ele 构成节点的 列偏移
IFndOS: int = 4  # inner facets 构成节点的 列偏移
ErrID_OS: int = 0  # error data 中 ele Global ID 的列偏移
ErrV_OS: int = 1  # error data 中 error value 的 列偏移
ErrRI_OS: int = 2  # error data 中 Refine Indicator 的列偏移

tri_ndN: int = 3  # 三角形单元顶点数; 对一二阶单元相同
tri_LocLst = [VndOS + 0, VndOS + 1, VndOS + 2]  # 一阶或二阶单元中, 三角形顶点的 loc 索引

# ----------------------- 误差 归一化
a_error_norm = 1.0E0  # 误差的放大系数
a_face_charge_norm: float = 1.0E0  # 面电荷密度的放大系数
a_face_marker_size: float = 1.0E3  # face charge 记号系数

# -----------------------
a_dlt = -0.001  # 节点文字 偏移比率
a_irange = 0.006  # 图形范围
a_dlt_nd: float = a_dlt * a_irange  # 节点文字偏移
a_ftsz_VS: float = 10  # 体,面单元 文字标注大小
a_ftsz_in: float = 7  # inner边 文字标注大小


# inner边 文字位置, 节点坐标的权重
def a_in_text_ws(nodeN):
    if 2 == nodeN:
        return [0.60, 0.40]
    elif 3 == nodeN:
        return [0.60, 0.0, 0.40]


a_color_V = 'olive'  # 体单元 line 颜色
a_color_Nd = 'green'  # 节点ID text 颜色
a_color_S = 'red'  # 面单元 line arrow 颜色
a_color_in = 'blue'  # inner Edge line, text 颜色

a_alpha_VS = 0.20  # 体单元和面单元的透明度
a_alpha_in = 0.50  # 内部边界的透明度
a_lw_VS = 3.0  # 体单元, 表面单元 线宽度
a_lw_in = 1.0  # 内部边 线宽度

# --------------------------- 特殊上色区域
a_list_spec = [1, 2, 3, 4]
a_color_V_spec = 'magenta'
a_ftsz_VS_spec = '12'
