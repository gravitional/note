# solver 宏

## RuntimeSelection.h

在运行时, 根据 字符串 选择 对象类型 创建
`std::string` -> `DerivedType`

```cpp
//======================================= 基类协议
//>在基类 New函数(工厂函数) 的实现中添加; 从 _constructorMap 寻找 子类构造函数
//> 运行时构建新对象, 和 New 配合使用,
// 协议: New() 的第一个参数必须为 const string& type
#define RuntimeSelectionConstructionWithArg(ParList)                                  // ParList 构造函数参数
#define RuntimeSelectionConstruction(ParList) RuntimeSelectionConstructionWithArg(()) // 无参数版本

//> 声明运行时选择机制, 在基类定义中(.h) 注入
// 通过添加 RtsNames() 函数, 和 AddConstructorToMap 内嵌类, 实现 运行时选择功能.
// protocol: ClassName 基类名称; ArgList 构造函数形参类型; ParList 构造函数 形参名称 列表
// e.g. RuntimeSelectionDeclarationWithArg(Optimizer, (OptFunc*, int), (func,dim))
#define RuntimeSelectionDeclarationWithArg(ClassName, ArgList, ParList)
// 无参数版本
#define RuntimeSelectionDeclaration(ClassName, (), ())

//> 在基类 实现(.cpp) 中注入
//> 定义运行时选择构造表, _constructorMap:unordered_map
#define RuntimeSelectionDefinition(ClassName)
// 双参数模板类 版本
#define RuntimeSelectionDefinitionTemplate2(ClassName, Type1, Type2)
template <>
ClassName<Type1, Type2>::ConstructorMap *ClassName<Type1, Type2>::_constructorMap = nullptr;

//======================================= 派生类协议
//> 派生类类型名, 在派生类定义中(.h) 注入, 定义 key; RTSName(), GetRTSName() 函数
#define RuntimeSelectionTypeName(TypeName)

//> 注册派生类, 在派生类实现中(.cpp) 注入,  注册到构造表
#define RuntimeSelectionRegister(BaseClassName, ClassName)
// 单参数 模板类 版本
#define RuntimeSelectionRegisterTemplate(BaseClassName, ClassName, Type)
// 双参数 模板类 版本
#define RuntimeSelectionRegisterTemplate2(BaseClassName, ClassName, Type1, Type2)
```

使用例子

```cpp
//====== ModelCreator.h
static ModelCreator* New(const std::string& type) // 运行时 New 工厂函数
{
    RuntimeSelectionConstruction // 在 构造表 寻找 子类构造函数
};
RuntimeSelectionDeclaration(ModelCreator); // 运行时选择机制的实现
///--- ModelCreator.cpp
RuntimeSelectionDefinition(ModelCreator); // 构造表实体创建

//======  ModelCreatorEF.h
RuntimeSelectionTypeName(Electrics); // 子类名称 RTSName
///--- ModelCreatorEF.cpp
RuntimeSelectionRegister(ModelCreator, ModelCreatorEF); // 注册子类构造关系 {key->构造函数} 到构造表
```
