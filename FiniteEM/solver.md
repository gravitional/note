# Solver

`model()`; 单例模式

```cpp
//通过id 获取某类存储组件的指针.
FieldConstraintEF* GetComponent<FieldConstraintEF>(int id);
// 获取单元并转换类型
CompElementEF* GetElement<CompElementEF>(int id);
// 取构件 T 及子类的指针向量, 存储到 ptrs 中
void GetCompPtrs<PostAnalysisBaseEF>(std::vector<PostAnalysisBaseEF*> &ptrs)
```

## 全局函数

这些函数作为全局函数, 对应的类使用单例模式,
返回一个指向唯一实例的指针.

### model()

持有模型数据,
构件,
节点, 边,  单元, 单元组,
材料, 自定义集合

`GetAllNodeIDs()`; 获取所有 Node ID
`GetAllEdgeIDs()`; 获取所有 Edge ID
`GetAllElementIDs()`; 获取所有 Ele ID

### fieldInfo()

物理场信息类,包括量纲和角标
在各顶层模块中统一赋值, 输出时自动识别.

### equation()

线性方程组管理类
方程排序功能, 管理方程编号.

### globalInfo()

全局信息类;

+ 从文件读取信息
+ 设置工程路径

+ 静态信息: 求解器名称, 空间类型, 空间维度
    + 空间类型: DIM3,DIM2,AXIS2

+ 动态信息(存储少量的全局常量)

### idObjects()

ID对象管理类, 0-base

## Util 类

一个工具类, 具有
array, algebra, geometry, quick sort, allocate,
class type 等函数模板.

## VarValue

常数或函数参数类,
保存用户自定义 py 函数
