# Compiler options listed by category

[Compiler options listed by category](https://learn.microsoft.com/en-us/cpp/build/reference/compiler-options-listed-by-category)

## Optimization

Option   Purpose

+ `/favor:<blend|AMD64|INTEL64|ATOM>`; 产生针对指定架构或一系列架构的优化代码.
+ `/O1`; 产生小代码.
+ `/O2`; 创建快速代码.
+ `/Ob<n>`; 控制内联扩展.
+ `/Od`; 禁用优化.
+ `/Og`; Deprecated. 使用全局优化.
+ `/Oi[-]` 生成内在函数.
+ `/O`; 偏向于小代码.
+ `/Ot`; 倾向于快速代码.
+ `/Ox`; /O2的一个子集, 不包括/GF或/Gy.
+ `/Oy`; 省略帧指针(rame pointer). (仅X86)

## Code generation

Option   Purpose

+ `/arch:<IA32|SSE|SSE2|AVX|AVX2|AVX512>`; 最低CPU架构要求. IA32, SSE和SSE2只有X86.
+ `/clr` 产生一个输出文件, 在通用语言运行时运行.
+ `/clr:implicitKeepAlive-` 关闭System::GC::KeepAlive(this)的隐式发射.
+ `/clr:initialAppDomain` 启用Visual C++ 2002的初始AppDomain行为.
+ `/clr:netcore` 产生针对.NET核心运行时间的程序集.
+ `/clr:noAssembly` 不产生程序集.
+ `/clr:nostdimport` 不要隐含地导入任何需要的程序集.
+ `/clr:nostdlib` 在搜索程序集时忽略系统.NET框架目录.
+ `/clr:pure` 生成一个只有IL的输出文件(没有本地可执行代码).
+ `/clr:safe` 产生一个仅有IL的可验证的输出文件.
+ `/EHa` 启用 C++ 异常处理(使用 SEH 异常).
+ `/EHc` ; `extern "C"` 默认为 `nothrow`.
+ `/EHr `总是生成noexcept运行时终止检查.
+ `/EHs` 启用C++异常处理(无SEH异常).
+ `/fp:contract` 生成代码时考虑浮点缩略语.
+ `/fp:except[-]` 生成代码时考虑浮点异常.
+ `/fp:fast` "快速 "浮点模型;结果不容易预测.
+ `/fp:precision` "精确的 "浮点模型;结果是可预测的.
+ `/fp:strict` "严格的 "浮点模型(意味着/fp:except).
+ `/fpcvt:BC` 向后兼容的浮点到无符号整数的转换.
+ `/fpcvt:IA` 英特尔本地浮点到无符号整数的转换行为.
+ `/fsanitize` 启用Sanitizer工具的编译, 如AddressSanitizer.
+ `/fsanitize-coverage` 启用LibFuzzer等库的代码覆盖仪的编译.

+ `/GA` 为Windows应用程序进行优化.
+ `/Gd` 使用__cdecl调用约定. (仅X86)
+ `/Ge` 已废弃. 激活堆栈探测.
+ `/GF` 启用字符串池.
+ `/Gh` 调用钩子函数_penter.
+ `/GH` 调用钩子函数_pexit.
+ `/GL[-]` 启用整个程序优化.
+ `/Gm[-]` Deprecated. 启用最小化重建.
+ `/Gr` 使用 __fastcall 调用惯例. (仅X86)
+ `/GR[-]` 启用运行时类型信息(RTTI).
+ `/GS[-]` 检查缓冲区安全.
+ `/G[n]` 控制堆栈探测.
+ `/GT` 支持通过使用静态线程本地存储分配的数据的光纤安全.
+ `/Gu[-]` 确保不同的函数有不同的地址.
+ `/guard:cf[-]` 增加控制流防护安全检查.
+ `/guard:ehcont[-]` 启用EH延续性元数据.
+ `/Gv` 使用 __vectorcall 调用惯例. (仅x86和x64)
+ `/Gw[-]` 启用整个程序的全局数据优化.
+ `/GX[-]` 废弃. 启用同步异常处理. 使用/EH代替.
+ `/Gy[-]` 启用函数级链接.
+ `/Gz` 使用__stdcall调用约定. (仅X86)
+ `/GZ` 被弃用. 启用快速检查. (与/RTC1相同)
+ `/homeparams` 强制在寄存器中传递的参数在函数进入时被写到堆栈中的位置. 这个编译器选项只适用于x64编译器(本地和交叉编译).
+ `/hotpatch` 创建一个可热补丁的图像.
+ `/Qfast_transcendentals` 产生快速超越.
+ `/QIfist` 已被弃用. 当需要从浮点类型转换到积分类型时, 抑制对辅助函数_ftol的调用. (仅适用于x86)

+ `/Qimprecise_fwaits` 移除try块内的fwait命令.
+ `/QIntel-jcc-erratum` 减轻Intel JCC erratum微代码更新的性能影响.
+ `/Qpar` 启用循环的自动并行化.
+ `/Qpar-report:n` 启用自动并行化的报告级别.
+ `/Qsafe_fp_loads` 对浮点值使用整数移动指令, 并禁用某些浮点加载优化.
+ `/Qspectre[-]` 启用CVE 2017-5753的缓解措施, 用于一类Spectre攻击.
+ `/Qspectre-load` 为每个加载指令生成序列化指令.
+ `/Qspectre-load-cf` 为每个加载内存的控制流指令生成序列化指令.
+ `/Qvec-report:n` 启用自动矢量化的报告级别.
+ `/RTC1` 启用快速运行时检查(相当于/RTCsu).
+ `/RTCc` 在运行时转换为较小的类型检查.
+ `/RTCs` 启用堆栈帧运行时检查.
+ `/RTCu` 启用未初始化的本地使用检查.
+ `/volatile:iso` 在volatile访问中不保证获取/释放语义.
+ `/volatile:ms`  在易失性访问中保证获取/释放语义.

## Output files

Option   Purpose

+ `/doc` 将文档注释处理到一个 XML 文件.
+ `/FA` 配置一个汇编列表文件.
+ `/Fa` 创建一个汇编列表文件.
+ `/Fd` 重命名程序数据库文件.
+ `/Fe` 重命名可执行文件.
+ `/Fi` 指定预处理的输出文件名.
+ `/Fm` 创建一个地图文件.
+ `/Fo` 创建一个对象文件.
+ `/Fp` 指定一个预编译的头文件名称.
+ `/FR, /Fr` 命名生成的`.sbr`浏览器文件. `/Fr`已被弃用.
+ `/Ft<dir>` 为`#import`生成的头文件的位置.

## Preprocessor

Option   Purpose

+ `/AI<dir>` 指定一个要搜索的目录以解决传递给#using指令的文件引用.
+ `/C` 在预处理过程中保留注释.
+ `/D<name>{=|#}<text>` 定义常量和宏.
+ `/E` 将预处理程序的输出复制到标准输出.
+ `/EP` 将预处理程序的输出复制到标准输出.
+ `/FI<file>` 预处理指定的包含文件.
+ `/FU<file>` 强制使用一个文件名, 就像它被传递给 `#using` 指令一样.
+ `/Fx` 将注入的代码与源文件合并.
+ `/I<dir>` 在 `<dir>` 中搜索包含文件(include).
+ `/P` 将预处理程序的输出写入文件.
+ `/PD` 打印所有宏定义.
+ `/PH` 在预处理时生成#pragma file_hash.
+ `/U<name>` 移除一个预定义的宏.
+ `/u` 删除所有预定义的宏.
+ `/X` 忽略标准include目录.

## Header units/modules

Option   Purpose

+ `/exportHeader` 创建由输入参数指定的头单元文件(.ifc).
+ `/headerUnit` 指定在哪里可以找到指定头的头单元文件(.ifc).
+ `/headerName` 从指定的头文件建立一个头单元.
+ `/ifcOutput` 为.ifc文件指定输出文件或目录.
+ `/interface` 将输入文件作为一个模块接口单元.
+ `/internalPartition` 将输入文件视为内部分区单元.
+ `/reference` 使用命名的模块IFC.
+ `/scanDependencies` 以C++标准JSON形式列出模块和头文件单元的依赖关系.
+ `/sourceDependencies` 列出所有源码级的依赖关系.
+ `/sourceDependencies:directives`; 列出模块和头文件单元的依赖.
+ `/translateInclude`; 将 `#include` 视为 `import`.

## Language

Option   Purpose

+ `/await` 启用coroutines(可恢复函数)扩展.
+ `/await:strict` 启用对早期语言版本的标准C++20 coroutine支持.
+ `/constexpr:backtrace<N>` 在诊断中显示N个constexpr的计算(默认: 10).
+ `/constexpr:depth<N>`; constexpr计算的递归深度限制(默认: 512).
+ `/constexpr:step<N>` 在N步之后终止constexpr计算(默认: 100000).
+ `/openmp` 在源代码中启用#pragma omp.
+ `/openmp:experimental` 启用OpenMP 2.0语言扩展, 并选择OpenMP 3.0+语言扩展.
+ `/openmp:llvm` 使用LLVM运行时间的OpenMP语言扩展.
+ `/permissive[-]` 设置标准一致性模式.
+ `/std:c++14` C++14标准 `ISO/IEC` 14882:2014(默认).
+ `/std:c++17` C++17标准 `ISO/IEC` 14882:2017.
+ `/std:c++20` C++20标准 `ISO/IEC` 14882:2020.
+ `/std:c++latest` 最新的C++标准草案预览功能.
+ `/std:c11` C11标准 `ISO/IEC` 9899:2011.
+ `/std:c17` C17标准 `ISO/IEC` 9899:2018.
+ `/vd{0|1|2}`   抑制或启用隐藏的vtordisp类成员.
+ `/vmb` 对成员的指针使用最佳基础.
+ `/vmg` 对成员的指针使用完全通用性.
+ `/vmm` 声明多继承性.
+ `/vms` 声明单一继承.
+ `/vmv` 声明虚拟继承.

+ `/Z7` 产生与C7.0兼容的调试信息.
+ `/Za` 在C代码中禁用一些C89语言扩展.
+ `/Zc:alignedNew[-]` 启用C++17的超对齐动态分配(在C++17中默认为打开).
+ `/Zc:auto[-]` 执行新的标准C++对auto的含义(默认打开).
+ `/Zc:char8_t[-]` 启用或禁用C++20的本地u8字头支持为const char8_t(默认为关闭, 除非在/std:c++20下).
+ `/Zc:__cplusplus[-]` 启用__cplusplus 宏来报告支持的标准 (默认为关闭).
+ `/Zc:externC[-]` 对extern "C" 函数执行标准C++规则 (由 /permissive- 暗示).
+ `/Zc:externConstexpr[-]` 为 constexpr 变量启用外部链接(默认为关闭).
+ `/Zc:forScope[-]` 强制执行标准C++的范围规则(默认为打开).
+ `/Zc:hiddenFriend[-]` 执行标准C++的隐藏好友规则(由/permissive-隐含).
+ `/Zc:implicitNoexcept[-]` 在需要的函数上启用隐式noexcept(默认为开启).
+ `/Zc:inline[-]` 删除未引用的函数或数据, 如果它们是COMDAT或只有内部链接(默认为关闭).
+ `/Zc:lambda[-]` 启用新的lambda处理器, 用于通用lambdas中的一致性模式语法检查.
+ `/Zc:noexceptTypes[-]` 执行C++17的noexcept规则(在C++17或更高版本中默认为打开).
+ `/Zc:preprocessor[-]` 使用新的符合要求的预处理程序(默认为关闭, C11/C17除外).
+ `/Zc:referenceBinding[-]` 一个UDT临时不会绑定到一个非const的lvalue引用(默认关闭).
+ `/Zc:rvalueCast[-]` 执行标准C++显式类型转换规则(默认为关闭).

+ `/Zc:sizedDealloc[-]` 启用C++14全局大小的去分配函数(默认为开启).
+ `/Zc:strictStrings[-]` 禁用字符串字头到char*或wchar_t*的转换(默认关闭).
+ `/Zc:ternary[-]` 强制执行操作数类型的条件运算符规则(默认为关闭).
+ `/Zc:threadSafeInit[-]` 启用线程安全的本地静态初始化(默认为打开).
+ `/Zc:throwingNew[-]` 假设操作符new在失败时抛出(默认为关闭).
+ `/Zc:trigraphs` 启用三段式(过时了, 默认为关闭).
+ `/Zc:tlsGuards[-]` 为TLS变量的初始化生成运行时检查(默认为打开).
+ `/Zc:twoPhase[-]` 使用不符合要求的模板解析行为(默认为符合).
+ `/Zc:wchar_t[-]` wchar_t是一个本地类型, 而不是一个类型定义(默认开启).
+ `/Zc:zeroSizeArrayNew[-]` 为0大小的对象数组调用成员new/delete(默认开启).
+ `/Ze` 已废止. 启用C89语言扩展.
+ `/Zf` 改进并行构建中的PDB生成时间.
+ `/ZH:[MD5|SHA1|SHA_256]` 在调试信息中指定 MD5, SHA-1, 或 SHA-256 的校验值.
+ `/ZI` 包括与编辑和继续兼容的程序数据库中的调试信息. (仅X86)
+ `/Zi` 产生完整的调试信息.
+ `/Zl` 移除.obj文件中的默认库名.
+ `/Zo[-]` 为优化代码生成更丰富的调试信息.
+ `/Zp[n] n`  打包结构成员(Packs structure members).
+ `/Zs` 只检查语法.
+ `/ZW` 产生一个输出文件, 在Windows Runtime上运行.

## Linking

Option   Purpose

+ `/F` 设置堆栈大小.
+ `/LD` 创建一个动态链接库.
+ `/LDd` 创建一个调试动态链接库.
+ `/link` 将指定的选项传递给LINK.
+ `/LN` 创建一个MSIL .netmodule.
+ `/MD` 通过使用MSVCRT.lib, 编译创建一个多线程的DLL.
+ `/MDd` 通过使用 MSVCRTD.lib 编译创建一个调试的多线程 DLL.
+ `/MT` 通过使用LIBCMT.lib, 编译创建一个多线程的可执行文件.
+ `/MTd` 通过使用LIBCMTD.lib编译创建一个调试的多线程可执行文件.

## Miscellaneous

Option   Purpose

+ `/?`   列出了编译器选项.
+ `@` 指定一个响应文件.
+ `/analyze` 启用代码分析.
+ `/bigobj` 增加.obj文件中可寻址部分的数量.
+ `/c` 在不连接的情况下进行编译.
+ `/cgthreads` 指定用于优化和代码生成的cl.exe线程的数量.
+ `/errorReport` 已废弃. 错误报告由Windows错误报告(WER)设置控制.
+ `/execution-charset` 设置执行字符集.
+ `/fastfail` 启用快速失败模式.
+ `/FC` 在诊断文本中显示传递给cl.exe的源代码文件的完整路径.
+ `/FS` 强制通过MSPDBSRV.EXE对PDB文件进行写入序列化.
+ `/H` 已废弃. 限制外部(公共)名称的长度.
+ `/HELP` 列出编译器选项.
+ `/J` 改变默认的char类型.
+ `/JMC` 支持本地C++ Just My Code 调试.
+ `/kernel` 编译器和链接器将创建一个可以在Windows内核中执行的二进制文件.
+ `/MP` 同时构建多个源文件.
+ `/nologo` 抑制显示登录标语.
+ `/presetPadding` 为基于堆栈的类类型实现零初始化填充.
+ `/showIncludes` 在编译过程中显示所有包含文件的列表.
+ `/source-charset` 设置源字符集.
+ `/Tc` 指定一个C源文件.
+ `/TC` 指定所有源文件为C.
+ `/Tp` 指定一个C++源文件.
+ `/TP` 指定所有的源文件都是C++.
+ `/utf-8` 将 `源文件` 和 `执行文件` 的字符集设为 `UTF-8`.
+ `/V` 废弃的. 设置版本字符串.
+ `/validate-charset`; 验证 `UTF-8` 文件, 只验证兼容的字符.
+ `/volatileMetadata`; 在易失性内存访问中生成元数据.
+ `/Yc` 创建 `.PCH` 文件.
+ `/Yd` 已弃用. 在所有对象文件中放置完整的调试信息. 使用/Zi代替.
+ `/Yl` 在创建调试库时注入一个PCH引用.
+ `/Yu` 在建立过程中使用 `预编译的头文件`.
+ `/Y-` 忽略当前构建中所有其他预编译头的编译器选项.
+ `/Zm` 指定预编译头的内存分配限制.

## Diagnostics

Option  Purpose

+ `/diagnostics:caret[-]` 诊断格式: 打印列和指示的源行.
+ `/diagnostics:classic` 使用传统的诊断格式.
+ `/diagnostics` 诊断格式: 打印列的信息.
+ `/external:anglebrackets` 将所有通过<>包含的头信息视为外部信息.
+ `/external:env:<var>` 指定一个带有外部头文件(headers)位置的 `环境变量`.
+ `/external:I <path>` 指定外部 `头文件`(headers)的位置.
+ `/external:templates[-]` 计算整个模板实例化链(template instantiation chain)的警告级别.

+ `/external:W<n>` 设置外部头文件的警告级别.
+ `/options:strict` 未识别的编译器选项是错误的.
+ `/sdl` 启用更多的安全功能和警告.
+ `/w` 禁用所有警告.
+ `/W0, /W1, /W2, /W3, /W4` 设置输出警告级别.
+ `/w1<n>, /w2<n>, /w3<n>, /w4<n> 为指定的 `警告` 设置警告级别.
+ `/Wall` 启用所有警告, 包括默认禁用的警告.
+ `/wd<n>` 禁用指定的警告.
+ `/we<n>` 将指定的警告视为错误.
+ `/WL` 在从命令行编译C++源代码时, 启用错误和警告信息的单行诊断.
+ `/wo<n>` 只显示指定的警告一次.
+ `/Wv:xx[.yy[.zzzzz]]` 禁用指定版本的编译器之后引入的警告.
+ `/WX` 将警告视为错误. Treat warnings as errors.

## Experimental options

实验性选项可能只被某些版本的编译器所支持.
它们在不同的编译器版本中的表现也可能不同.
通常情况下, 最好的或者说唯一的实验性选项的文档是在 [微软C++团队博客中](https://devblogs.microsoft.com/cppblog/).

Option   Purpose

/experimental:module   启用实验性模块支持.

[/experimental:module](https://learn.microsoft.com/en-us/cpp/build/reference/experimental-module)

## Deprecated and removed compiler options

Option   Purpose

+ `/clr:noAssembly` 已废弃. 使用/LN(创建MSIL模块)代替.
+ `/errorReport` 已废弃. 错误报告由Windows错误报告(WER)设置控制.
+ `/experimental:preprocessor` 已废弃. 启用实验性的符合要求的预处理程序支持. 使用/Zc:preprocessor.
+ `/Fr` 已弃用. 创建一个没有局部变量的浏览信息文件.
+ `/Ge` 已弃用. 激活堆栈探测. 默认开启.
+ `/Gm` 弃用. 启用最小重建.
+ `/GX` 弃用. 启用同步异常处理. 使用/EH代替.
+ `/GZ` 已弃用. 启用快速检查. 使用/RTC1代替.
+ `/H` 弃用. 限制外部(公共)名称的长度.
+ `/Og` 已弃用. 使用全局优化.
+ `/QIfist` 已废弃. 曾经用于指定如何从浮点类型转换为积分类型.
+ `/V` 已废弃. 设置.obj文件的版本字符串.
+ `/Wp64` 已过时. 检测64位的可移植性问题.
+ `/Yd` 已废弃. 在所有对象文件中放置完整的调试信息. 使用/Zi代替.
+ `/Zc:forScope-` 已废弃. 禁用for循环范围内的一致性.
+ `/Ze` 已弃用. 启用语言扩展.
+ `/Zg` 在Visual Studio 2015中被删除. 生成函数原型.
