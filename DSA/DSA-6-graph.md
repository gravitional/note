# graph

平面图: planar graph, 可以嵌入平面的图, 不相邻的边不能相交.

欧拉公式(1750), for any PG
$$v-e+f-c=1$$

其中 `v`, `e` 和 `f` 分别是 `点`, `边` 和 `面` 的个数,
`c` 是连通域的个数.

对于树图有:
$$v-e=1$$
即顶点比边多`1`.

## 1

在含 `20` 个顶点的简单无向图中, 边的数量最多为: 190
此时度最小的顶点的度为: `19`

解析: 即组合数 `C(20,2)$`

## 2

某宴会一共有7个人参加, 与会者之间进行了亲切的握手.
已知他们中的每个人进行握手的次数分别为:

3, 1, 2, 2, 3, 1, 2

请问宴会上总共发生了多少次握手?
`7次`

解析: 等于 `求和/2`

## 3

在人类的历史长河中, 每个人都可能要与其他人握手.
如果某人在他的一生中进行握手的次数为奇数, 则称他为A类人, 否则称为B类人.
试问从古至今A类人的个数是: (假设人类只能和人类握手)
`偶数`

解析: `无向图` 的边数等于各顶点度数之和的一半
如果边是奇数个, 则顶点是偶数个

## 5

对于包含n个顶点e条边的简单无向图, 以下关于它的邻接矩阵A的说法中错误的是:

A有n行e列, 其中元素取值于{0, 1}
A的第k行中1的个数等于顶点k的度
$A=A^T$
A中位于第u行v列的元素为1当且仅当顶点u和顶点v邻接

## 6

`G` 是简单无向图, `A` 为 `G` 的邻接矩阵, `M` 为 `G` 的关联矩阵,
`D`是对角线上第 `i` 个元素为顶点i的度的对角矩阵,它们的关系是:

$A+D = M M^T$

`邻接+度矩阵 = 关联* 关联的转置`

## 7

用 `邻接矩阵` 实现含n个顶点e条边的图:
Space complexity 空间复杂度:
$O(n^2)$

## 8

删除边 `(i, j)` 的时间复杂度是 $O(1)$

## 9

遍历顶点v的所有邻居的时间复杂度: $O(n)$

## 10

访问顶点v中存储的数据的时间复杂度: $O(1)$

## 11

G是有向无环图, (u, v)是G中的一条由u指向v的边. 对G进行DFS的结果是:

`fTime(u) > fTime(v)`

解析: G不含环路, (u, v)不可能是 `BACKWARD`, 对 `u` 的访问结束时, 对 `v`的访问必然已经结束
访问结束 `visited` 类似于二叉树的后序遍历.
