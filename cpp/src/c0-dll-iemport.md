# DLL 导入和导出

[导入和导出](https://docs.microsoft.com/zh-cn/cpp/build/importing-and-exporting?view=msvc-170)

可以使用两种方法将公共符号导入应用程序或从 DLL 导出函数:

+ 生成 DLL 时使用模块定义 (.def) 文件
+ 在主应用程序的函数定义中使用关键字 __declspec(dllimport) 或 __declspec(dllexport)
