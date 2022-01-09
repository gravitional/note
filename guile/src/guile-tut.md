# Guile教程简介

本文是关于使用 `GNU扩展语言和系统 Guile` 的介绍.
Guile 是 Scheme 编程语言的一种方言, 我假定你至少对Scheme或LISP的基本知识有了解信心.
然而, Guile 也允许应用程序开发人员将其集成到他们的代码中, 为其提供脚本能力,
并通过 `此应用程序` 特有的 `原语` 来扩展 `Guile 主语言`.
确切地说, 这就是我想简单介绍的内容.

如果你想在阅读本文的同时开发和测试这里介绍的代码
(这肯定是个好主意, 如果你想玩玩中间没有字面介绍的某些东西, 那就更好了),
你当然需要安装 `GNU Guile`(包括适当的开发包, 包括头文件, 如果你的系统需要的话)
以及 `Gnuplot`, 并且有类似 `UNIX` 的环境和一些C编译器(用 gcc 准行).
我在 `GNU/Linux` 机器上工作, 使用 `Guile 1.8.7`, `Gnuplot 4.2` 和 `gcc 4.3.1`,
但任何最新的 `Guile` 和 `Gnuplot` 都应该做到.

[基础知识][]: 关于Guile的基础知识.
[Tortoise][]: 我们要实现的Tortoise包.
[Guiling][]: Guile与它有什么关系.
[进一步][]: 你可以采取的下一步措施.

[基础知识]: https://www.gnu.org/software/guile/docs/guile-tut/tutorial.html#Fundamentals
[Tortoise]: https://www.gnu.org/software/guile/docs/guile-tut/tutorial.html#Tortoise
[Guiling]: https://www.gnu.org/software/guile/docs/guile-tut/tutorial.html#Guiling
[进一步]: https://www.gnu.org/software/guile/docs/guile-tut/tutorial.html#Further

## 基本原理

`Guile` 是 `Scheme` 编程语言的一种实现和方言(`Scheme` 本身就是 `LISP` 的一种方言).
它的目的是作为 `GNU项目` 的扩展语言来使用.

这意味着 `Guile` 被设计成 `库`, 你可以把它包含在你自己的项目中, 使解释器运行其中的代码;
另外, 你可以在此 `Guile环境` 中提供特殊的 `procedures`, 与你的 `application` 的核心对接(interface to).
这样一来, 用户编写的 `Scheme` 脚本, 就可以操作你的 `application` 中的东西了.

这一切的意义在于为你提供一种简单的方法, 使你的应用程序具有可扩展性和脚本性.(extensible and scriptable)
例如, 如果你的应用程序需要某种 `配置文件`, 甚至是真正的 `脚本支持`, 你可以使用 `Guile`,
而不是推出你自己的 "小"  `配置或脚本语言和解释器`;
它已经存在了, 所以不要把时间浪费在另一个上, 而是继续研究你项目中新的, 令人兴奋的部分.
将 `Guile` 作为某种 "通用" 的脚本语言(至少Guile人希望它是这样的, 但我认为它适合这个目标),
也意味着用户不必为她想配置或编写脚本的每个应用程序学习不同的语言.
相反, 她只需学习Scheme就可以在她喜欢的所有软件上这样做.
此外, 至少在我个人看来, `Scheme` 是非常好的, 有趣的编程方式, 而且非常适用于像脚本这样的小段代码.

最重要的是: 目前 `Guile` 的开发, 允许核心Guile "平台" 除了运行Scheme代码, 还能在Guile之上支持其他语言.
因此, 你可以把 `Guile` 作为脚本解释器集成到你的代码中, 让它同时运行 `ECMAScript`, `Emacs Lisp` 或 `Guile` 将来要实现的任何其他语言
(如 `Perl`, `Tcl`, `Python` 或其他语言)的脚本--而且你不需要对此作任何考虑.

具体来说, 在你的系统上安装 `Guile` 基本上为你提供两件事.

+ 你可以使用 `guile` 命令行工具作为 `Scheme` 解释器来编写程序(如果你愿意, 也可以作为桌面计算器).
+ 使用 `libguile` 编程库来运行使用 `Guile` 作为 `脚本扩展` 的应用程序;
或者自己编写可以 `访问解释器` 的程序, 并利用它来编写你的代码.

## The Tortoise

>"如果他不是乌龟, 你为什么叫他乌龟? " 爱丽丝问道.
>"我们叫他乌龟, 因为他教我们, "模拟乌龟生气地说.
>(刘易斯-卡罗尔, <爱丽丝梦游仙境>)

作为演示 `Guile` 的项目, 我们将开发一个非常简单的 "乌龟"(Tortoise) 图形程序.
它将使用 `Gnuplot` 进行图形输出, 但应该很容易适应其他任何图形系统.

它将通过假设有一只乌龟坐在屏幕中间来产生图形输出; 这只乌龟能够执行用户给出的一些基本指令.

你可以要求它向左转一定的度数(或向右转, 提供负数), 或者你可以指示它向前走一定的步数.
它有一支笔, 你可以要求它把笔放在爪子里或耳朵后面, 这样当它移动时就会在地上留下痕迹或不留下痕迹.

最后, 如果你把自己和这只可怜的乌龟完全弄糊涂了, 你可以要求它走到一个新的(空的)地面上,
坐在中间, 面向右边, 就像一开始那样.

+ [后台][]: 核心的乌龟系统.
+ [第一次测试][]: 测试后端.

[后台]: https://www.gnu.org/software/guile/docs/guile-tut/tutorial.html#Backend
[第一次测试]: https://www.gnu.org/software/guile/docs/guile-tut/tutorial.html#First-Test

## 核心系统

让我们最终开始并实现核心程序, 它将记录乌龟的轨迹, 它的运动和图形输出.
如前所述, 我将使用 `Gnuplot` 进行图形输出(起初我想使用 `Gtk+`工具包, 但这种方法有一些问题, 我将在后面再谈).

我们的想法是在后台启动一个 `Gnuplot` 进程, 通过管道向它发送命令, 在屏幕上画出我们想要的线条.
下面是程序的代码, 它将启动 `Gnuplot` 进程, 并通过管道与它保持连接, 以传输进一步的绘图命令:

```cpp
/* Simple backend for a Logo like tortoise drawer.  */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

static const int WIDTH = 10;
static const int HEIGHT = 10;

static FILE*
start_gnuplot ()
{
  FILE* output;
  int pipes[2];
  pid_t pid;

  pipe (pipes);
  pid = fork ();

  if (!pid)
    {
      dup2 (pipes[0], STDIN_FILENO);
      execlp ("gnuplot", NULL);
      return; /* Not reached.  */
    }

  output = fdopen (pipes[1], "w");

  fprintf (output, "set multiplot\n");
  fprintf (output, "set parametric\n");
  fprintf (output, "set xrange [-%d:%d]\n", WIDTH, WIDTH);
  fprintf (output, "set yrange [-%d:%d]\n", HEIGHT, HEIGHT);
  fprintf (output, "set size ratio -1\n");
  fprintf (output, "unset xtics\n");
  fprintf (output, "unset ytics\n");
  fflush (output);

  return output;
}

static FILE* global_output;

int
main (int argc, char* argv[])
{
  global_output = start_gnuplot ();

  return EXIT_SUCCESS;
}
```

注意, 这里我们没有对 `system routines` 做任何错误检查;
正常情况下应该要做检查, 但暂时忽略它使这段代码尽可能的简单.
由于到目前为止, 这与你想在本教程中阅读的内容无关, 我认为这应该是最好的方式.

我们在这里做的是用 `start_gnuplot` 例程启动 `Gnuplot` 进程,
并打开管道, 可以通过它命令送入 `global_output`, 这样我们以后就能绘制线条.

`Gnuplot` 是以固定的 `坐标范围` (X和Y方向的-10到10)启动的.
我们将使用参数(parametric)模式, 这样我们在绘制垂直线时就不会有任何问题,
同时使用 `多重绘图`(multiplot)模式, 以便通过逐次添加 `线条` 逐步建立图形.

现在, 这段代码添加了绘制从 `(x1, y1)` 到 `(x2, y2)` routine:

```cpp
static void
draw_line (FILE* output, double x1, double y1, double x2, double y2)
{
  fprintf (output, "plot [0:1] %f + %f * t, %f + %f * t notitle\n",
           x1, x2 - x1, y1, y2 - y1);
  fflush (output);
}
```

如果你不确定这里发生了什么, 你可能想读一下Gnuplot中的 `参数化绘图` 或 `线的参数化方程`.
或者相信我, 这能满足我们目前的需要.

最后, 我们可以编写控制乌龟的程序; 这里涉及一些三角学(trigonometry), 你需要 `#include <math.h>`:

```cpp
static double x, y;
static double direction;
static int pendown;

static void
tortoise_reset ()
{
  x = y = 0.0;
  direction = 0.0;
  pendown = 1;

  fprintf (global_output, "clear\n");
  fflush (global_output);
}

static void
tortoise_pendown ()
{
  pendown = 1;
}

static void
tortoise_penup ()
{
  pendown = 0;
}

static void
tortoise_turn (double degrees)
{
  direction += M_PI / 180.0 * degrees;
}

static void
tortoise_move (double length)
{
  double newX, newY;

  newX = x + length * cos (direction);
  newY = y + length * sin (direction);

  if (pendown)
    draw_line (global_output, x, y, newX, newY);

  x = newX;
  y = newY;
}
```

就是这样, 只要在启动 `Gnuplot` 后, 在主程序中添加 `tortoise_reset();` 调用,
这样乌龟就会在合适的位置开始.

### 测试它

你现在当然想试试了, 不是吗? 至少我已经兴奋得发烫了......
所以, 我们要给乌龟实际一些指令, 直接通过 `main` routine 中的C代码:

```cpp
{
  int i;
  tortoise_pendown (); /* This is unnecessary, but makes it clearer.  */
  for (i = 1; i <= 4; ++i)
    {
      tortoise_move (3.0);
      tortoise_turn (90.0);
    }
}
```

顺便提一下: 这个程序在终止时让 `Gnuplot` 进程保持活动;
我们可以在结束前给它发送`quit` 命令, 但稍后我们就不能这样做了, 与其做更复杂的事情, 我不如让它这样.

它基本上是这样工作的, 甚至还有一个好处,
那就是尽管我们的乌龟程序已经运行结束, 但Gnuplot窗口会一直打开直到你关闭它.
如果你担心这些进程, 只需在事后做一个 `killall gnuplot` 的动作......

我不知道你怎么想的, 但我自己喜欢使用 `Makefile`;
所以把我们制定的完整代码保存到 `tortoise.c` 中, 并编写如下 `Makefile`:

```Makefile
# Basic Makefile for the tortoise package.

CFLAGS =
LIBS =

.PHONY: clean build run

build: tortoise

clean:
    rm -f tortoise tortoise.o

run: tortoise
    ./tortoise

tortoise: tortoise.o
    gcc $< -o $@ $(LIBS)

tortoise.o: tortoise.c
    gcc -c $< -o $@ $(CFLAGS)
```

当然, `CFLAGS` 和 `LIBS` 目前还没有什么用处, 但它们以后会有一些用处, 所以我只是马上把它们包括进去.
现在, 运行 `make` 应该可以编译和运行你的代码, 并打开 `Gnuplot` 窗口, 显示出结果的图形;
对我来说, 它看起来像这样.

![Tortoise1](https://www.gnu.org/software/guile/docs/guile-tut/plot1.png)

恭喜你, 你刚刚完成了你的第一个 `乌龟画图`!
不过, 我们还是不想像刚才那样, 直接用 C代码来修复和编译指令;
相反, 用户应该能够互动地控制乌龟.
