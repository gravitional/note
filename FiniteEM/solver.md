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

## 全局函数

这些函数作为全局函数, 对应的类使用单例模式,
返回一个指向唯一实例的指针.

### model()

单例模式

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

### lib()

Library, 动态库管理类
Load(...); 加载动态库

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

## 求解器调用栈,静电场

```cpp
//solver/main.cpp
main(int argc, char* argv[]);
    common::Control().Execute(argc, argv); // 总流程控制类, 管理 Impl 资源, /Common/Control/Control.cpp
        args(); //解析命令行参数
        _pIml->Initialize(argc, argv); // 初始化 Blas, mpi, cuda, thread, hypre, petsc, profiler(分析器)
        anlsCtrl()->TestLicense(); //检查许可证
        _pIml->SetWorkingPath(); //设置工作路径
        _pIml->PrintHeader(); //打印日志头, 成刷版本, 进程数目等.
        _pIml->CreateModel(); //创建模型
            reader=controlReader(); // read control and solver, 从 control.json
            anlsCtrl()->TestLicense(solver); //
            lib()->Load(Str::ToLower(solver)); // load solver dll

            memTracker()->SnapShot("CreateModel"); //分析内存
            unique_ptr<ModelCreator> creator(ModelCreator::New(solver));// 创建 solver model
            creator->Create(); // 创建网格和模型, /Common/Model/ModelCreator.cpp
                CreateMesh(); // 创建网格
                CreateModel(); //创建模型 /FiniteElement/Model/FeModelCreator.cpp
                    CreateFunction(); //自定义函数
                    CreateCoordSys(); //自定义坐标系
                    CreateMonitor(); //
                    CreateNode();
                    CreateElement(data);
                    CreateMaterial(data);
                    CreateInitialField(data);
                    CreateMisc(data); //创建各场的特有数据, /Electrics/Model/ModelCreatorEF.cpp
                        CreateInfo(data);
                            data.ReadValue("Thickness2D"...) //读取厚度
                            data.ReadValue("Sector"...) //读取模型分数
                        CreateConstraint(data) // 创建约束, 即边界条件
                    CreateAnalysis(); // 创建分析, /FiniteElement/Model/FeModelCreator.cpp
                        for(分析类型){
                            AnalysisBase *analysis =NewAnalysis(type);
                            analysis->AnalysisFe::Read(); // 动态多态, 转向 AnalysisFe::Read()
                                auto reader=controlReader(); // control.json 单例
                                auto job =NewJob(); //任务工厂方法
                                _jobID=model()->AddComponentGenID(job); //分配 jobID, 即 工况号
                                _name=reader->GetCurrentNode(); // 获取有限元分析名称
                                // 设置 jobID 对应的名称, 存入 unordered_map<string, int> _nameMap, 不同 reader 共享
                                reader->SetIDByName(_name,_jobID); 
                                job->Create(*reader); //virtual, 创建 new job
                                    非线性迭代 // /Electrics/Analysis/LoadJobEF.cpp
                                    载荷
                                    int id=model()->AddComponentGenID(_PostAnalysis); // 添加后处理分析任务
                            anlsCtrl()->AddAnalysis(analysis) // append 分析到分析队列末尾
                        }

                    Initialize(); // 各个场自己的特定初始化, /Electrics/Model/ModelCreatorEF.cpp
                        model()->CopyThreadsMaterial(); //复制线程私有材料对象
                        ApplyConstraint(); // 施加约束
                        ArrangeEquationNo(); // 数据自由度排序
                        PartitionMesh(); // 剖分网格
                        AdjustRankComponent(); // 调整构件所属MPI 节点
                        CreateElementList(); // 生成单元列表
                        RemoveOtherRankComponent(); //删除其他MPI 节点上的对象
                        globalInfo()->SetDouble("Penalty",1e60)// 罚系数确定
                        WriteGeoInfo(); //写出网格信息

            memTracker()->SnapShot("CreateModel"); //分析内存,

        _pIml->AnalyzeModel(); //运行模型分析, /Common/Control/Control.cpp
            //迭代每个分析
            while(true){
                auto analysis = anlsCtrl()->GetNextAnalysis();
                memTracker();timeTracker(); // 记录内存, 时间占用
                analysis->Run(); // 运行每个分析. /Common/Analysis/AnalysisBase.cpp
                    Analyze(); // virtual, /Electrics/Analysis/AnalysisStaticEF.cpp
                        InitBeforeJobLoop(); // 静态电场分析初始化
                        analyze_EM_Field_Nonlinear(); //非线性迭代
                memTracker();timeTracker(); // 记录内存, 时间占用
            }
        _pIml->Finalize(); // 计算收尾, /Common/Control/Control.cpp
            ThreadUtil::WaitAll(); // async
            timeTracker();memTracker(); //profiler
            objects()->Clear(); // objects
            Pyutil::Finalize(); //python
            PetscUtil::Finalize(); //petsc
            HypreUtil::Finalize(); //hypre
            AmgxUtil::Finalize(); //amgx
            MPIUtil::Finalize(); //mpi
            CUDAUtil::Finalize(); //gpu
```

### 重要的数字标记

```cpp
int AnalysisBase::_jobID // 工况号. 
int AnalysisBase::_stepID // 分析步序号
std::string AnalysisBase::_name // 分析名
bool AnalysisBase::_restart // 是否续算
```
