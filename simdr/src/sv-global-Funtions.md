# solver中的全局函数

这些函数作为全局函数, 对应的类使用 `单例模式`, 返回指向唯一实例的指针.

## fieldInfo()

物理场信息类,包括量纲和角标
在各顶层模块中统一赋值, 输出时自动识别.

## equation()

线性方程组管理类
方程排序功能, 管理方程编号.

## globalInfo()

全局信息类;

+ 从文件读取信息
+ 设置工程路径

+ 静态信息: 求解器名称, 空间类型, 空间维度
    + 空间类型: DIM3,DIM2,AXIS2

+ 动态信息(存储少量的全局常量)

## idObjects()

ID对象管理类, 0-base

## resultManip()

结果操纵类

## controlReader()

读取 control 文件的类

## args()

Argument 类, 命令行参数解析类

## anlsCtrl()

AnalysisControl, 分析管理类

+ 许可证
+ 分析
+ 当前分析信息
+ 计时器
+ 输出变量控制
+ temp 分析步名称

## lib()

Library, 动态库管理类
Load(...); 加载动态库

## getIDbySet()

Common/Common/Misc/Set.h

```cpp
//获得某集合的ID数组
COMMON_API std::vector<int> getIDbySet(int setID);
//获得某些集合的ID数组
COMMON_API std::vector<int> getIDbySet(const std::vector<int> &setIDs);
```
