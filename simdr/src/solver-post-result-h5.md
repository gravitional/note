# 求解器后处理

+ `自然序`: 某组数据的固有次序, 即 `0, 1, 2... N-1`,
简记为 `NS` (natural sequence), `h5` dataset 左边总有一个 `NS`;

+ 如果数组 A, B 中的数据依照`NS` 一一对应,
则称为 `ONSM`(One-one Natural Sequence Mapping), 简记为 `A <-> B`

## result.h5 格式

+ `CellNode`; `Cell` 和 `Node`; 有限元 `体单元` 和 `构成节点`;
+ `FaceCell`; `Face` 和 `Cell`

+ 以下 `Dyna` 和 `Mesh` 表示两个 顶层 `Group`.
+ 以 `.` 开头的表示属性, 例如 `C_1.Type` 表示 `C_1` 的属性 `.Type`.

+ `result.h5/Mesh/Node/Coord` 有自己的排列顺序, 记作 `PGID`(post global id);
和 `mesh.h5/Node/Coord` 下面的顺序不一样, 记作 `GID` (global id);

`reuslt.h5` 里面的节点场, 是按照 `result.h5` 自己的节点顺序给出的.
如果场存在 mapping, 仍然是从 `data <--> result的PGID`.

```bash
# ---------------------------------
Dyna/
    1.20000000/ # 时刻 Tag
    Field/   # 场数据 Group
        C1/ # 场数据名称
          .Location # 例如 Node, 表明是 节点场
          .Suffix # 例如 xyz,
          .Type # 例如 Scalar, 表示标量场
          .Unit # 例如 m^-3*mol, 单位
          .Value.Mapping # Mapping Array 的名称, 例如 C1.map; C1.map <-> C1.Value
            Dimension # 量纲, 如 V, A, V/m 等等
            Value  # 场数据 Array;

# ---------------------------------
Mesh/
    .Type # String, 例如 "CellNode"
    Node/
      .Count   # 节点总数 N
        Coord/ # 节点坐标 {x,y,z} Array; 长度N*3; NS/3 给出 节点PGID
    Cell/
        .Count # 单元总数 M; 包括 体单元 和 面单元
        Type/  # 单元类型; 例如 203 表示 2维, 3个节点; 此NS即 体单元PGID
        Nodes.Index # 见下面 index 和 value; 包括体单元和面单元
        Nodes.Value # 元素为节点的 PGID

Mapping/  # 某些场数据 和 网格间 的特殊映射关系
    xxx.CellNode.1  # 按单元存储的场; 场data <-> 单元PGID
    xxx.Node.1      # 按节点存储的场; 场data <-> 节点PGID
```

### Index 和 Value

>`Nodes.Index` 和 `Nodes.Value` 是一对数据, 组合起来相当于一个 JagArray.
>给出了单元的构成节点. 例子如下

| Nodes.Value | Nodes.Index |
| ----------- | ----------- |
| 0 1 2       | 0           |
| 1 3 4       | 3           |
| 3 5 6       | 6           |

左边是 `Node` 的 `PGID Array`, 0-based.
右边是 `PGID Array` 的截断点, 0-based, 左闭右开.

### 场数据排列

+ 如果是节点上的 `标量场`, 那么自然对应一维 Array.
data <-> Node 的 PGID.

+ 如果是 单元上的矢量场,
则按照

```bash
单元 # 例如 tri3
    节点 # 例如 共3个
        场分量 # 例如共 3个分量.
```

的层级压缩成一维数组.
则Array 的`0~8` 元素 存储 `单元0` 的3个节点上的3个场分量;
Array 的`9~17`元素 存储 `单元1` 的3个节点上的3个场分量. 以此类推.
也就是 Fortran 多维数组的存储方式, `场分量`指标 变化最快.
