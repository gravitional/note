# cpp error

## Run-Time Check Failure #3

[Run-Time Check Failure #3](https://blog.csdn.net/qq_29894329/article/details/51184920)

类似下面这种报错:

    Run-Time Check Failure #3 - The variable 'p' is being used without being initialized.

代码示例:

```cpp
calc::CoordSysUtil *cs;
cs->SetCartesianCS(axisX,axisY,axisZ)
```

通过分析发现这与RTC(Run-Time Check, 运行时检查)机制有关(以下都是以VS2012为标准).
首先普及一下RTC(Run-Time Check)机制, 包括:
`堆栈帧`(RTCS), `未初始化变量`(RTCu), `两者都有`, 以及 `默认值` 四种.
在 VS2022 编译器中, `项目`->`属性`->`配置属性`->`C/C++` ->`代码生成`->`基本运行时检查`

`Type *var` 声明的变量, 必须用指针类型初始化, 也就是需要用 `new` 或其他方式返回的指针.
