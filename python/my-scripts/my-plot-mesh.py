import csv
import math
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.figure as mpfig
import matplotlib.patches as mpatches

#  真空介电常数  和 真空磁导率
er0 = 8.854187817e-12
mu0 = 4 * math.pi * 1e-7

# 文件名称
filename_node = 'Node-1.txt'
filename_plane = 'Tri3-3.txt'
filename_bound = 'Line2-5.txt'


#  读取数据
def readData2(name: str):
    with open(name, newline='') as csvfile:
        spamreader = csv.reader(csvfile, delimiter=' ',
                                skipinitialspace=True, quotechar='|')
        for row in spamreader:
            print(', '.join(row))


def readData(name: str, cols, skip=0, /, dtype=float):
    with open(name, newline='') as csvfile:
        mdata = np.loadtxt(csvfile, delimiter=" ",
                           usecols=cols, skiprows=skip, dtype=dtype)
        return mdata


def plotPlane(nodeData):
    # 读取数据
    planeRef = readData(filename_plane, [1, 2, 3], dtype=int)
    # 节点 X Y 数据列的位置
    nodePosX = 0
    nodePosY = 1
    # txt 文件中数据列的偏移量
    planeOffset = 0
    # 边界节点数据从第二列开始
    surfStart = 0
    surfEnd = 1
    # 节点 和 平面 的维数
    nodeLen = len(nodeData)
    planeLen = len(planeRef)
    nodeN = 3

    xlst = np.empty(1 + nodeN)
    ylst = np.empty(1 + nodeN)

    for eleid in range(planeLen):
        for nodeid in range(nodeN):
            # 获取节点的 x ，y 坐标;
            row = planeRef[eleid, nodeid + planeOffset]
            xlst[nodeid] = nodeData[row, nodePosX]  # nodex是三个横坐标
            ylst[nodeid] = nodeData[row, nodePosY]  # ylst 是三个纵坐标
            xlst[nodeN] = xlst[0]
            ylst[nodeN] = ylst[0]
        plt.plot(list(xlst), list(ylst), linestyle='-', color='b')


def plotSurf(nodeData):
    surfRef = readData(filename_bound, [1, 2], dtype=int)
    surfLen = len(surfRef)
    nodeN = 2
    # 节点 X Y 数据列的位置
    nodePosX = 0
    nodePosY = 1
    surfOffset = 0

    xlst = np.empty(1 + nodeN)
    ylst = np.empty(1 + nodeN)

    maxe = plt.gca()
    for eleid in range(surfLen):
        for nodeid in range(nodeN):
            row = surfRef[eleid, nodeid + surfOffset]
            xlst[nodeid] = nodeData[row, nodePosX]
            ylst[nodeid] = nodeData[row, nodePosY]
            xlst[nodeN] = xlst[0]
            ylst[nodeN] = ylst[0]
        x_tail = xlst[0]
        y_tail = ylst[0]
        dx = xlst[1]
        dy = ylst[1]
        # plt.plot(xlst, ylst, linestyle='-', color='r')
        arrow = mpatches.FancyArrowPatch(
            (x_tail, y_tail), (dx, dy), mutation_scale=12, color='r')
        maxe.add_patch(arrow)
    maxe.autoscale(tight=True)


def getRange(start: list, end: list, ratio: float = 0.01):
    dist = [a - b for a, b in zip(end, start)]
    delta = [a * ratio for a in dist]
    start = [a - b for a, b in zip(start, delta)]
    end = [a + b for a, b in zip(end, delta)]
    return start[0], start[1], end[0], end[1]


def plotRegion():
    plt.figure()

    plt.title("Mesh")  # 括号当中输入标题的名称
    plt.rcParams['axes.unicode_minus'] = False
    # plt.rcParams['font.sans-serif'] = ['SimHei'] #中文
    plt.xlabel('X')  # x轴标签
    plt.ylabel('Y')  # y轴标签

    nodeData = readData(filename_node, [1, 2, 3])
    x1, y1, _ = np.amin(nodeData, axis=0)
    x2, y2, _ = np.amax(nodeData, axis=0)
    plotPlane(nodeData)
    plotSurf(nodeData)

    x1, y1, x2, y2 = getRange([x1, y1], [x2, y2], 0.015)
    plt.gca().set_xlim(x1, x2)
    plt.gca().set_ylim(y1, y2)
    plt.show()


if __name__ == '__main__':
    plotRegion()
