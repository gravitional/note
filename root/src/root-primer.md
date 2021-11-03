# ROOT Primer

## intro

ROOT是一个用于数据分析和I/O的软件框架: 一个强大的工具, 以应对最先进的科学数据分析的典型的苛刻任务.
它的突出特点包括一个先进的图形用户界面, 是交互式分析的理想选择;
一个C++编程语言的解释器, 用于快速和有效的原型设计;
一个C++对象的持久化机制, 也用于编写每年由大型强子对撞机实验记录的PB级数据.
这个介绍性指南说明了ROOT的主要特点, 这些特点与数据分析的典型问题有关: 测量数据的输入和绘图以及分析函数的拟合.

将测量数据与理论模型进行比较是实验物理学的标准任务之一.
在最简单的情况下, 一个 "模型 "只是一个提供测量数据预测的函数. 很多时候, 该模型取决于参数.
这样的模型可以简单地说明 "电流I与电压U成正比", 而实验者的任务包括从一组测量数据中确定电阻, R.

作为第一步, 需要对数据进行可视化处理. 接下来, 通常需要进行一些操作, 例如修正或参数转换.
很多时候, 这些操作是复杂的, 应该提供一个强大的数学函数和程序库
--例如, 考虑到应用于输入光谱的积分或峰值搜索或傅里叶变换, 以获得模型描述的实际测量.

实验物理学的一个特点是影响每个测量的不可避免的不确定性, 而可视化工具必须包括这些.
在随后的分析中, 必须正确处理误差的统计性质.

作为最后一步, 测量结果要与模型进行比较, 在这个过程中需要确定自由模型参数.
参见图1.1, 这是一个函数(模型)拟合数据点的例子. 有几种标准方法可供选择, 数据分析工具应提供对其中一种以上方法的方便使用.
还必须有量化测量和模型之间一致程度的方法.

![examplefit](https://root.cern/primer/examplefit.png)

Figure 1.1: 测量的数据点与误差条和拟合的二次函数.

很多时候, 要分析的数据量很大--想想借助计算机积累的细粒度测量(fine-granular).
因此, 一个可用的工具必须包含易于使用, 和有效的方法来存储和处理数据.

在量子力学中, 模型通常只预测, 取决于一些参数的测量的概率密度函数("pdf"),
而实验分析的目的是从观察到的测量的某些值的频率分布中提取参数.
这类测量需要生成和可视化频率分布的手段, 即所谓的直方图(histograms),
以及严格的统计处理, 以便从纯统计分布中提取模型参数.

对预期数据的模拟是数据分析的另一个重要方面.
通过重复生成 "伪数据", 以与真实数据相同的方式进行分析, 可以验证或比较分析程序.
在许多情况下, 测量误差的分布并不精确, 而模拟提供了测试不同假设的影响的可能性.

ROOT是一个强大的软件框架, 它满足了上述所有的要求, 是一个由欧洲核研究组织(CERN)在日内瓦协调的开源项目.
(European Organisation for Nuclear Research, CERN)

ROOT非常灵活, 既提供了用于自定义应用的编程接口, 又提供了用于交互式数据分析的图形用户界面.
本文件的目的是作为初学者的指南, 并根据学生实验室中的典型问题, 为你自己的使用案例提供可扩展的例子.
希望本指南能够为您在未来的科学工作中建立一个现代化的, 最先进的数据分析工具奠定更复杂的应用基础.

本指南以教程的形式, 旨在向你快速介绍ROOT软件包.
根据 "边做边学 "的原则, 我们将通过具体的例子来实现这一目标.
也正因为如此, 本指南不能涵盖ROOT包的所有复杂性.
尽管如此, 一旦你对以下各章所介绍的概念感到有信心, 你将能够通过参考指南来找到你可能感兴趣的所有细节.
你甚至可以看一下代码本身, 因为ROOT是一个免费的, 开源的产品.
在使用本教程的同时, 还可以使用这些文档!

ROOT数据分析框架本身是用C++编程语言编写的, 并在很大程度上依赖于这种语言: 需要一些关于C++的知识.
如果你对C++语言没有任何概念, 就从大量的关于C++的文献中获取 power.

ROOT适用于许多平台(Linux, Mac OS X, Windows......), 但在本指南中我们将假设你使用的是 Linux.
使用ROOT的第一件事是安装它, 获得最新的ROOT版本是很简单的.
只要在[安装页面](https://root.cern/install)上选择你喜欢的安装方法.
你可以找到不同架构的预编译版本, 或者ROOT的源代码来自己编译.
只要采用你喜欢的风格, 并按照安装说明进行操作即可.

让我们深入了解一下ROOT!

>注意事项
>本文件中介绍的宏和数据文件可以在ROOT的[GitHub仓库](https://github.com/root-project/root/tree/master/documentation/primer/macros)中找到.

## ROOT基础知识

现在你已经安装了ROOT, 你正在运行的这个交互式 shell 是什么呢? 我们来研究一下.
ROOT 有双重生活.
它有一个`宏`(macros)的解释器(Cling), 你可以从命令行运行, 或像应用程序一样运行.
但它也是一个交互式 shell, 可以计算任意的`语句`和`表达式`.
这对于 调试, 快速 hacking 和 测试 是非常有用的.
让我们首先看一下一些非常简单的例子.

### 作为计算器的ROOT

你甚至可以用ROOT交互式 shell 来代替计算器! 用以下命令启动 `ROOT` 交互式 `shell`

```bash
# conda 需要先激活环境: conda activate tom
$ root
```

在你的Linux盒子上. 很快就会出现提示:

```cpp
root [0]
```

然后让我们按照这里的步骤进行操作.

```cpp
root [0] 1+1
(int) 2
root [1] 2*(4+2)/12.
(double) 1.000000
root [2] sqrt(3.)
(double) 1.732051
root [3] 1 > 2
(bool) false
root [4] TMath::Pi()
(double) 3.141593
root [5] TMath::Erf(.2)
(double) 0.222703
```

还不错. 你可以看到, ROOT不仅为你提供了输入C++语句的可能性,
而且还提供了高级数学函数的可能性, 这些函数存在于 `TMath` 命名空间.

现在让我们来做一些更详细的事情. 用众所周知的geometrical series 列举一个数字的例子.

```cpp
root [6] double x=.5
(double) 0.500000
root [7] int N=30
(int) 30
root [8] double geom_series=0
(double) 0.000000
root [9] for (int i=0;i<N;++i)geom_series+=TMath::Power(x,i)
root [10]  cout << TMath::Abs(geom_series - (1-TMath::Power(x,N-1))/(1-x)) <<endl;
1.86265e-09
```

在这里, 我们向前迈出了一步. 我们甚至声明了变量并使用了一个`for`控制结构.
请注意, Cling和标准C++语言之间有一些微妙的区别.
在交互式模式下, 你不需要在行尾添加`;`--体会这种区别, 例如使用行`root [6]`的命令.

### 在ROOT提示下学习C++

在 ROOT prompt 的背后, 有一个基于真正的`compiler toolkit`的解释器(interpreter): LLVM.
因此, 可以行使 C++ 和标准库的许多功能.
例如, 在下面的片段中, 我们定义了一个 `lambda` 函数, 一个 `矢量`, 并以不同的方式对其进行排序.

```cpp
root [0] using doubles = std::vector<double>;
root [1] auto pVec = [](const doubles& v){for (auto&& x:v) cout << x << endl;};
root [2] doubles v{0,3,5,4,1,2};
root [3] pVec(v);
0
...
root [4] std::sort(v.begin(),v.end());
root [5] pVec(v);
0
...
5
root [6] std::sort(v.begin(),v.end(),[](double a, double b){return a>b;});
root [7] pVec(v);
5
...
0
```

或者, 如果你喜欢生成随机数

```cpp
root [0] std::default_random_engine generator;
root [1] std::normal_distribution<double> distribution(0.,1.);
root [2] distribution(generator)
(std::normal_distribution<double>::result_type) -1.219658e-01
root [3] distribution(generator)
(std::normal_distribution<double>::result_type) -1.086818e+00
root [4] distribution(generator)
(std::normal_distribution<double>::result_type) 6.842899e-01
```

令人印象深刻, 不是吗?

### ROOT作为函数 plotter

使用ROOT的一个强大的类, 在这里是`TF1`, 将允许我们 display 单变量 `x` 的函数.
尝试一下:

```cpp
root [11] TF1 f1("f1","sin(x)/x",0.,10.);
root [12] f1.Draw();
```

`f1` 是 `TF1` 类的一个实例, 我们将参数用于构造函数;
第一个字符串类型的参数是, 要输入内部ROOT内存管理系统的名称,
第二个字符串类型的参数定义了函数, 这里是 `sin(x)/x`, 两个`double`类型的参数定义了变量x的范围.
`Draw()`方法, 这里没有任何参数, 在你打完上面两行后, 应该弹出一个窗口显示这个函数.

一个稍加扩展的版本是定义带参数的函数, 在 `ROOT formula` 语法中称为`[0], [1]`等等.
我们现在需要一种为这些参数赋值的方法;
这可以通过 `TF1` 类的 `SetParameter(<parameter_number>,<parameter_value>)`方法实现.
下面是一个例子.

```cpp
root [13] TF1 f2("f2","[0]*sin([1]*x)/x",0.,10.);
root [14] f2.SetParameter(0,1);
root [15] f2.SetParameter(1,1);
root [16] f2.Draw();
```

当然, 这个版本显示的结果与最初的版本相同. 试着玩玩参数, 并再次绘制函数.
TF1类有大量非常有用的方法, 包括 `积分` 和 `微分`.
要充分利用这个`类`和其他 `ROOT` 类, 请访问互联网上[https://root.cern/doc/master/ ](https://root.cern/doc/master/)下的文档.
ROOT中的公式是用TFormula类来计算的, 所以也要查阅相关的类文档, 了解例子, 和它实现的函数和语法.

你一定要把这个指南下载到你自己的系统中, 以便在你需要的时候可以随时使用.

在上面的例子上做一点延伸, 考虑一下你想定义的更复杂的函数.
你也可以用标准的C或C++代码来做这件事.

考虑一下下面的例子, 它计算并显示光线落在多条狭缝上产生的干涉图案.(interference pattern)
请不要在ROOT命令行中键入下面的例子, 有一个更简单的方法.
在你在磁盘新建 `slits.C` 文件, 贴入这些代码, 并在shell中输入 `root slits.C`.
这将启动root并使其读取 "宏 " `slits.C`, 即该文件中的所有行将被一个接一个地执行.

```cpp
// Example drawing the interference pattern of light
// falling on a grid with n slits and ratio r of slit
// width over distance between slits.

auto pi = TMath::Pi();

// function code in C
double single(double *x, double *par) {
  return pow(sin(pi*par[0]*x[0])/(pi*par[0]*x[0]),2);
}

double nslit0(double *x,double *par){
  return pow(sin(pi*par[1]*x[0])/sin(pi*x[0]),2);
}

double nslit(double *x, double *par){
  return single(x,par) * nslit0(x,par);
}

// This is the main program
void slits() {
  float r,ns;

  // request user input
  cout << "slit width / g ? ";
  scanf("%f",&r);
  cout << "# of slits? ";
  scanf("%f",&ns);
  cout <<"interference pattern for "<< ns
       <<" slits, width/distance: "<<r<<endl;

  // define function and set options
  TF1 *Fnslit  = new TF1("Fnslit",nslit,-5.001,5.,2);
  Fnslit->SetNpx(500);

  // set parameters, as read in above
  Fnslit->SetParameter(0,r);
  Fnslit->SetParameter(1,ns);

  // draw the interference pattern for a grid with n slits
  Fnslit->Draw();
}
```

这个例子首先要求用户输入, 即`狭缝宽度`与`狭缝距离`之比, 以及狭缝的`数量`.
输入这些信息后, 你应该看到如图2.1所示的图形输出.

这是个比我们之前看到的更复杂的例子, 所以要花些时间仔细分析, 在继续之前你应该已经明白了.
让我们详细了解一下.

第`7-18`行在C++代码中定义了必要的函数, 按照所考虑的问题的特点, 分成了三个独立的函数.
完整的干涉模式是由一个取决于狭缝的宽度和距离之比的函数,  和第二个取决于狭缝数量的函数的乘积给出的.
对我们来说, 更重要的是定义这些函数的接口, 使它们可以用于ROOT类TF1:
第一个参数是`x`的指针, 第二个参数指向参数阵列.

主程序从第21行开始, 定义了一个 `void` 类型的函数`slits()`.
在要求用户输入后, 使用开头给出的C型函数定义了一个ROOT函数.
现在我们可以使用 `TF1` 类的所有方法来控制我们函数的行为--很好, 不是吗?

如果你愿意, 你可以很容易地扩展这个例子,
在TF1实例中使用函数 `double single`, 或者使用函数 `double nslit0`,
来绘制单个狭缝的干涉模式, 或者绘制带有狭缝的网格.

在这里, 我们使用了`宏`, 即某种轻量级的程序. 与ROOT一起分发的Cling 解释器能够执行这个宏.
这是一个很特别的情况, 因为C++本身并不是一种`解释型`语言, 要说的还有很多: 有章节的确是专门讨论宏的.

### 控制ROOT

目前还要说明一下: 由于你输入 `ROOT` 的每一条命令通常都会被 `Cling` 解释,
所以需要一个 "转义字符"(escape character)来直接向 `ROOT` 传递命令.
这个字符就是位于行首的点`.`

```cpp
root [1] .<command>
```

这是最常见的命令的集合:

+ 退出root, 只需输入`.q`
+ 获得一个命令列表, 使用 `.?`
+ 访问操作系统的 shell, 输入 `.!<OS_command>`; 尝试, 例如 `.!ls` 或 `.!pwd`
+ 执行一个宏, 输入 `.x <file_name>`; 在上面的例子中, 你可能在 `ROOT` 提示下使用了 `.x slits.C`;
+ 加载宏, 输入 `.L <file_name>`; 在上面的例子中, 你可能使用了 `.L slits.C` 命令, 后面是函数调用 `slits();`.
注意, 加载一个宏后, 其中定义的所有函数和程序都可以在ROOT提示符下使用.
+ 编译一个宏, 输入 `.L <file_name>+`; ROOT能够在幕后为你管理C++编译器, 并从你的`宏`开始产生机器代码.
人们可以决定编译一个宏, 以获得更好的性能或更接近生产环境.

在提示符下使用 `.help` 来查看完整的列表
