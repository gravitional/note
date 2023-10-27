# solver 宏

## RuntimeSelection.h

在运行时, 根据 字符串 选择 对象类型 创建
`std::string` -> `DerivedType`

```cpp
//======================================= 基类协议
// 创建新对象, 在基类 New函数(工厂函数) 的实现中添加; 协议: New() 的第一个参数必须为 const string& type
#define RuntimeSelectionConstructionWithArg(ParList)                                  // ParList 构造函数参数
#define RuntimeSelectionConstruction(ParList) RuntimeSelectionConstructionWithArg(()) // 无参数版本

// 声明运行时选择机制, 在基类定义中(.h) 添加
// protocol: ClassName 基类名称; ArgList 构造函数形参类型; ParList 构造函数 形参变量列表
// e.g. RuntimeSelectionDeclarationWithArg(Optimizer, (OptFunc*, int), (func,dim))
#define RuntimeSelectionDeclarationWithArg(ClassName, ArgList, ParList)
// 无参数版本
#define RuntimeSelectionDeclaration(ClassName, (), ())

// 定义运行时选择构造表, 在基类 实现(.cpp) 中添加
#define RuntimeSelectionDefinition(ClassName)
// 双参数模板类 版本
#define RuntimeSelectionDefinitionTemplate2(ClassName, Type1, Type2)
template <>
ClassName<Type1, Type2>::ConstructorMap *ClassName<Type1, Type2>::_constructorMap = nullptr;

//======================================= 派生类协议
// 派生类类型名, 在派生类定义中(.h) 添加, 定义 key
#define RuntimeSelectionTypeName(TypeName)

// 注册派生类, 在派生类实现中(.cpp) 添加,  注册到构造表
#define RuntimeSelectionRegister(BaseClassName, ClassName)
// 单参数 模板类 版本
#define RuntimeSelectionRegisterTemplate(BaseClassName, ClassName, Type)
// 双参数 模板类 版本
#define RuntimeSelectionRegisterTemplate2(BaseClassName, ClassName, Type1, Type2)
```
