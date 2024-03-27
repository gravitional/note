# 求解器后处理

`自然序`: 某组数据的固有次序, 表示为 `0, 1, 2... N-1`

## result.h5 格式

以下 `Dyna` 和 `Mesh` 表示两个 顶层 `Group`.
以 `.` 开头的表示属性, 例如 `C_1.Type` 表示 `C_1` 的属性 `.Type`.

```bash
# ---------------------------------
Dyna/
    1.20000000/ #时间
    Field/   # 单元的节点上的场数据
        C_1/ # 具体场数据
          .Location #例如 Node, 表明是节点上的场
          .Suffix # 例如 xyz
          .Type # 例如 Scalar
          .Unit # 例如 m^-3*mol
          .Value.Mapping #
            Dimension # 量纲, 如 V, A, V/m 等等
            Value  # 场数据自然序 {单元, 节点, 场分量} -> 数据;
            # 场数据原本是多维数组, 压缩到一维


# ---------------------------------
Mesh/
    .Type "CellNode"
    Node/
      .Count   # 节点总数 N; 对应的节点索引是 0,1,...,N-1
        Coord/ # 节点自然序1 -> 节点坐标 {x,y,z}
    Cell/
        .Count # 单元总数 M
        Type/  # 单元类型, 例如 203 表示 2维, 3个节点
        Nodes.Index # 单元自然序1 -> 每个单元的 起止 节点自然序2, 左闭右开; 包括 实体单元 和 边界单元
        Nodes.Value # 节点自然序2 -> 节点自然序1 (上面的 Node.Coord)

Mapping/
    xxx.CellNode.1  # 场数据自然序 -> 单元自然序1 (上面的 Cell/Nodes.Index)
    xxx.Node.1      # 场数据自然序 -> 节点自然序1 (上面的 Cell/Nodes.Index)
```

## 节点顺序

result.h5/Mesh/Node/Coord 有自己的排列顺序,
和 mesh.h5/Node/Coord 下面的顺序不一样.

reuslt.h5 里面的节点场, 是按照 result.h5 自己的节点顺序给出的.
如果场存在 mapping, 仍然是从 `自然序 -> result 节点编号`.
