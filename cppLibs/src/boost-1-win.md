# Getting Started on Windows

## 获取Boost

获得Boost拷贝的最可靠的方法是下载 [boost_1_80_0.7z](https://www.boost.org/users/history/version_1_80_0.html)
或 [boost_1_80_0.zip](https://www.boost.org/users/history/version_1_80_0.html),
然后解压, 安装一个完整的Boost发行版.

## Boost发行版

这是编译结果的目录结构的草图:

```bash
boost_1_80_0 ................. boost根目录
   index.htm ......... www.boost.org的副本从这里开始.
   boost......................... 所有的Boost头文件
   lib.....................预编译的二进制库
   libs\............测试, .cpps, 文档等, 按库分类.
     index.html ........ 库的文档从这里开始
     algorithm\
     any\
     array\
                     ...更多的库...
   status\ ......................... 整个Boost测试套件
   tools\...........Utilities, e.g. Boost.Build, quickbook, bcp
   more\.......................... Policy 文件, 等等.
   doc\............... 所有Boost库文档的一个子集
```

### 头部组织

`Boost库` 的头文件组织并不完全统一, 但大多数库都遵循一些模式.

一些老的库, 和大多数非常小的库把所有的公共头文件直接放在 `boost/` 中.
大多数库的公共头文件都在 `boost/` 的一个子目录中, 以库的名字命名.
例如, 你会发现 `Python` 库的 `def.hpp` 头文件在

    boost\python\def.hpp.

有些库在 `boost/` 中拥有一个 `聚合 header`, 它#包含了该库的所有其他头.
例如, `Boost.Python` 的聚合头是

    boost\python.hpp.

大多数库将私有头文件放在一个叫做 `detail\` 或 `aux_\` 的子目录中.
不要指望在这些目录中找到你能用的东西, 它们是实现的私有细节.

### 注意以下几点是很重要的

+ 在文档和邮件列表中, boost根目录(通常是 `C:\Program Files\boost\boost_1_80_0` )
的路径有时被称为 `$BOOST_ROOT`.
+ 要在Boost中编译任何东西, 你需要在你的 `#include` 路径中包含某目录, 此目录需含有 `boost/`.
在Microsoft Visual Studio中设置 `#include` 路径的具体步骤见本文后面;
如果你使用其他IDE, 请查阅你的产品文档以获得指导.

+ 由于 `Boost` 的所有头文件的扩展名都是 `.hpp`, 并且都在 `boost根目录` 下的 `boost/subdirectory`中,
所以你的Boost `#include` 指令看起来就像这样.

```cpp
#include <boost/whatever.hpp>
```

或

```cpp
#include "boost/whatever.hpp"
```

这取决于你对 `<>` 的使用的偏好.
即使是Windows用户也可以(而且, 出于可移植性的考虑, 也许应该)在 `#include` 指令中使用 `正斜杠`(`/`);
你的编译器并不关心啥系统, 它喜欢 `/`.

+ 不要被 `doc/` 子目录所干扰, 它只包含 `Boost` 文档的一个子集.
如果你想了解全部的内容, 可以从 `libs\index.html` 开始.
