# Solver

```cpp
//通过id 获取某类存储组件的指针.
FieldConstraintEF* GetComponent<FieldConstraintEF>(int id);
// 获取单元并转换类型
CompElementEF* GetElement<CompElementEF>(int id);
// 取构件 T 及子类的指针向量, 存储到 ptrs 中
void GetCompPtrs<PostAnalysisBaseEF>(std::vector<PostAnalysisBaseEF*> &ptrs)
```

## 设计模式

单例模式

工厂模式

```cpp
// FiniteElement/FeModelCreator.cpp
AnalysisBase *analysis=NewAnalysis(type);
```

## 工具类

### Util 类

一个工具类, 具有
array, algebra, geometry, quick sort, allocate,
class type 等函数模板.

### VarValue

常数或函数参数类,
保存用户自定义 py 函数

### AnalysisBase 类

运行分析: `Run()`
取可输出变量;

+ 设置分析步序号,
+ 设置工况号,取工况号
+ 设置分析名称, 取分析名

### ModelCtreator

模型创建类:

+ 生成网格和模型
+ 生成全局信息
+ 生成函数
+ 生成坐标系

### Singleton.h

单例模式

### ResultIO

相关类; DataIO, Hdf5IO

结果 input, output 类,
读写 resutl.h5 文件

### AnalysisFe

```cpp
AnalysisFe:public AnalysisBase{...}
```

创建 FiniteElement 分析任务,
成员函数 Read() 分配任务号, `_jobID`.

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

## 重要的数字标记

```cpp
int AnalysisBase::_jobID // 工况号.
int AnalysisBase::_stepID // 分析步序号
std::string AnalysisBase::_name // 分析名
bool AnalysisBase::_restart // 是否续算
```

## 命令习惯

函数命名, 计算代价从小到大

```cpp
get(), set()
search()
find()
calc()
```

## 场数据类型, FieldData, SingleFieldData

在 `GetResultManip` 类中, `GetEleNdData()` 函数返回 `FieldData` 对象的引用,

```cpp
FieldData& GetEleNdData() const { return getFieldData("EleNd"); }
GetEleNdData().SetData(
    storeNo, filmID, ele->getID(), nComp * ele->NdNum());
```

其中:
`storeNo` : STP_ACCUM, STP_SEARCH, jobID 等等.
`ele->getID()` : 单元编号.
`nComp*ele->NdNum()` : 衍生场分量数 * 此单元中的节点数

后两个指标 `单元编号, 衍生场分量*节点数` 对应 `SingleFieldData` 对象,
`SingleFieldData` 底层是 `JagArray`,
由于单元分成体单元, 表面单元等, 不同单元具有的 `衍生场分量*节点数` 可能不同,
所以是 `不定长二维数组`.
