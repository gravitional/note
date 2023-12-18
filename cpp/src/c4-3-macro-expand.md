# C语言 宏嵌套的展开规则

C语言中, 宏是在预编译时, 用宏体内容 `文本替换` 代码中的 `宏名`.

先讲一些宏嵌套的展开规则:

+ 一般的展开规律像函数的参数一样: 先展开参数, 再分析函数, 即由内向外展开
+ 当宏中有 `#` 运算符的时候, 不展开参数
+ 当宏中有 `##` 运算符的时候, 先展开函数, 再分析参数
+ `##` 运算符用于将参数连接到一起, 预处理过程把出现在 `##` 运算符两侧的参数合并成一个 `符号`, 注意不是字符串

## `#`和`##`的使用规则

`#` 是将宏参数转换为字符串.
不管该参数宏什么, 即 `原貌` 用字符串显示出来.
即将宏参数用双引号 `""` 包裹起来形成一个字符串.
例如

```c
#define T(x) #x
int temp = 10;
cout<<T(temp)<<endl;//输出 temp 而不是 10
// T(temp) -> "temp"  (将宏参数用双引号包含起来形成一个字符串)
```

`##` 被称为连接符(concatenation),
把宏参数与之前的token(参数／字符串空格等)连接起来.
例如

```c
#define T(x) x##[2]
int a[5] = {1,2,3,4,5};
cout<<T(a)<<endl; //输出 3 即 a[2]
```

## 宏的常见展开错误

### 宏参数在宏体中未加括号包裹起来

```c
/// worng
#define T(a)  a*10
int a  = 1;
cout<<T(a+1)<<endl; //输出 11 而非20
```

### 整个宏体内容未加括号包裹起来

```c
/// worng
#define T(x)  x+1
cout<<10*T(1)<<endl; //输出 11 而非20
```

解决办法:

+ 在宏定义中, 将参数加上括号, 这样在替换时保证括号内的表达式优先运算.
+ 利用括号将整个宏定义的内容括起来, 保证整个宏定义中的表达式优先运算.

## 宏嵌套

宏嵌套是宏使用的难点, 也是易错点.
下面我将宏嵌套的展开规则用流程图来说明一下:

![宏嵌套的展开规则流程图](https://pic1.zhimg.com/80/v2-91787722dc1c06fb829743c307f0574c_720w.webp)

注意: 上图中的 2 和 3 是条件或, 只要满足一个条件就会进入流程5.

下面举例说明:

```c
// example 1
#include <cstdio>
#define TO_STRING2(x) #x
#define TO_STRING1(x) #x
#define TO_STRING(x) TO_STRING1(x)

#define PARAM(x) #x
#define ADDPARAM(x) INT_##x

int main()
{
    const char *str = TO_STRING(PARAM(ADDPARAM(1)));
    printf("%s\n",str); //输出: "ADDPARAM(1)"

    str = TO_STRING2(PARAM(ADDPARAM(1)));
    printf("%s\n",str); //输出: PARAM(ADDPARAM(1))

    return 0;
}
```

上例中两个嵌套宏的展开流程如下:

+ `TO_STRING(PARAM(ADDPARAM(1)))` // TO_STRING 宏体不包含 # or ##
-> `PARAM: TO_STRING("ADDPARAM(1)")` // 先展开内层, #x 变成字符串
-> 展开 `TO_STRING: TO_STRING1("ADDPARAM(1)")` // 代入 TO_STRING
-> 展开 `TO_STRING1: "\"ADDPARAM(1)\""` // 再次 变成字符串

+ `TO_STRING2(PARAM(ADDPARAM(1)))` //TO_STRING2 宏体不包含 #
-> 展开 `TO_STRING2: "PARAM(ADDPARAM(1))"`

```c
// example 2
#include <cstdio>
#define TO_STRING2(x) a_##x
#define TO_STRING1(x) #x
#define TO_STRING(x) TO_STRING1(x)

#define PARAM(x) #x
#define ADDPARAM(x) INT_##x

int main()
{
    const char *str = TO_STRING(TO_STRING2(PARAM(ADDPARAM(1))));
    printf("%s\n",str); //输出: a_PARAM(INT_1)
    return 0;
}
```

上例中嵌套宏的展开流程如下:

`TO_STRING(TO_STRING2(PARAM(ADDPARAM(1))))`
-> 展开 `TO_STRING2`: `TO_STRING(a_PARAM(ADDPARAM(1)))`
//注意此次展开后, PARAM宏名被破坏了, 变成了a_PARAM不再是有效的宏名了
-> 展开 `ADDPARAM: TO_STRING(a_PARAM(INT_1))`
-> 展开 `TO_STRING: TO_STRING1(a_PARAM(INT_1))`
-> 展开 `TO_STRING1: "a_PARAM(INT_1)"`

注意: 嵌套宏的展开规则与编译器有关, 不同的编译器可能对同一个嵌套宏展开不同.
以上测试都是在 VS2010(x86)上
