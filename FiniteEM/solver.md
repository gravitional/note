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

## Singleton.h

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

### resultManip()

结果操纵类

### controlReader()

读取 control 文件的类

### args()

Argument 类, 命令行参数解析类

### anlsCtrl()

AnalysisControl, 分析管理类

+ 许可证
+ 分析
+ 当前分析信息
+ 计时器
+ 输出变量控制
+ temp 分析步名称

## Util 类

一个工具类, 具有
array, algebra, geometry, quick sort, allocate,
class type 等函数模板.

## VarValue

常数或函数参数类,
保存用户自定义 py 函数

## AnalysisBase 类

运行分析: `Run()`
取可输出变量;

+ 设置分析步序号,
+ 设置工况号,取工况号
+ 设置分析名称, 取分析名

## 12.5 各种自由度记号

+ `AXIS2`: 表示轴对称系统, 取柱坐标系计算.
+ `NdNum()`;节点数目.
+ `DofNum()`: 自由度数目, 取决于体系, 例如每个单元中, 自由度就是节点上的电势.
+ `GSNum()`, `GsNumKe()`, `GsNumFe()`, `GsNumVe()`: 高斯点数目;对于阶数不同的多项式, 需要的采样点数目不同, 因此 Gauss 点数目可能不同.

`GsNumVe()`: 用于计算 `ElementFin()`, `MutualMatrix()`, `NodeVirtualForce()`, `SubJobFv()`
`GsNumKe()`; 用于计算 `ElementMatrixKe()`,
`GsNumFe()`; 用于计算 `AssembleFVByDisCharge()`; `GetGeometryFeature3D()`,也就是体积.

## C++编码风格

+ 命名空间: 小驼峰(受单词首字母小写,其余单词首字母大写,不用下划线),尽量用工程名, 各个单词或缩写
+ 类: 大驼峰(全部单词首字母大写, 不用下划线)
+ 类函数: 大驼峰
+ 类数据: 下划线+小驼峰
+ 全局函数: 小驼峰
+ 全局变量: 小驼峰, 尽量少使用
+ 常量: 全大写, 下划线连接
+ 枚举值: 全大写, 下划线连接

## exec

```cpp
main(int argc, char* argv[])
->common::Control().Execute(argc, argv); // 总流程控制类, 管理 Impl 资源
->

