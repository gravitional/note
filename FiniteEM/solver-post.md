# 求解器后处理

自然序: 某组数据的固有次序, 表示为 0, 1, 2... N-1

Result.h5
    
Dyna/
    1.20000000/ 时间
    Field/   单元的节点上的场数据
        C_1/ 具体场数据
            Value  场数据自然序 {单元, 节点, 场分量} -> 数据; 
            场数据原本是多维数组, 压缩到一维

Mesh/
    .Type "CellNode"
    Node/
        .Count  节点总数 N, 节点索引 0,1,2..., N-1
        Coord/  节点自然序1 -> 节点坐标 {x,y,z}
    Cell/
        .Count 单元总数 M
        Type/  单元类型, 例如 203 表示 2维, 3个节点
        Nodes.Index 单元自然序1 -> 每个单元的 起止 节点自然序2, 左闭右开; 包括 实体单元 和 边界单元
        Nodes.Value 节点自然序2 -> 节点自然序1 (上面的 Node.Coord)

Mapping/
    xxx.CellNode.1  场数据自然序 -> 单元自然序1 (上面的 Cell/Nodes.Index)