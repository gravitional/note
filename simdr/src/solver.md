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

## 单元法向

Comsol 二维模型中, 边界的法向正向, 指向区域内部,
可以通过电流密度矢量的约定判断.

## 材料节点平均

```cpp
void FieldWriterEC::SetTestFieldData()
```

有限元计算的 `自由度` 是 `节点`,
但在计算场的分布时, 是遍历 模型的单元列表,

### Test Node

在输出场时 `SetTestFieldData` 中使用
`SetTestNodeField(std::string&name, int nComp, vector<double>& data)` 输出,
此函数按 `节点值` 输出, 由于 节点 可能被单元共享(公共节点),
所以需要对 单元场值做平均, 才能得到节点场值,
在这个过程中, 也可能用到材料性质, 所以把 `全局节点编号` 按照 `材料id` 分组.

### Cell Node

而另一个函数 `WriteField()` 中调用的
`WriteCellNodeField(const string& name, int nComp, const vector<double>& data)`
则直接输出 单元场数据, 因此无需作平均.

## 高斯积分

[Numerical quadrature](https://finite-element.github.io/1_quadrature.html#extending-legendre-gausz-quadrature-to-two-dimensions)
[Gaussian Quadrature](https://mathworld.wolfram.com/GaussianQuadrature.html)
[Gaussian quadrature wiki](https://en.wikipedia.org/wiki/Gaussian_quadrature)

## Echemistry

电场和浓度场在时间步上交错计算,
在 time 0: 先计算电场分布, 然后使用 time0电场 计算 time0浓度场分布.
在 time 1: 使用 time0 浓度场分布计算 time1 电场分布. 再使用 time1 电场计算 time1 浓度场分布.

计算 time0 电场时, film 厚度为零. 计算完成后更新 film 厚度.
计算 time1 电场时, 使用 time0-1 积累的 film 厚度.
计算 time2 电场时, 使用 time0-2 积累的 film 厚度.
...

## solver 字符串操作, 路径操作

见 `Str.cpp`
