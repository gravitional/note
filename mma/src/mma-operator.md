# 算符优先级

tutorial/OperatorInputForms.  
具有内置定义的运算符. 下面是常用算符的优先级, 同一优先级的放到一组:

大部分算符序列从`右边往左边`依次结合, 再作用到参数上.

```mathematica
f@g@h@x==f[g[h[x]]]
```

也有一些从左边开始结合, 例如:

```mathematica
exp1[exp2],   (e[e])[e]
exp1[[exp2]],   (e[[e]])[[e]]
exp1/*exp2,     e/*e/*e
exp1/exp2,    (e/e)/e
exp1/;exp2,    (e/;e)/;e
```

***
数字
符号`Symbol`, 上下文
字符串
***
矩阵, 二维列表, List
分段函数,PieceWise
***
`exp::string`, 信息名, MessageName
`exp::string1::String2`, 消息, 语言
***
包含`#`的格式: 函数变量
包含`%`的格式: `%n`,输出历史
包含`_`的格式: 模式匹配
***
`<<`,Get,导入文件
***
`\!boxes` ,(诠释框符)
***
`exp1?exp2`, PatternTest, 模式检测
***
`exp1[exp2]`, 函数应用, 左结合
`exp1[[exp2]]`, Part,切片
***
`exp1@*exp2`, Compositon, 复合函数,右边先作用
`exp1/*exp2`, RightCompositon, 右复合函数, 左边先作用
***
`exp1@exp2`, 前缀, 右结合
***
`exp1~exp2~exp3`, exp2[exp1,exp3], 中缀
***
`exp1/@exp2`, Map, 映射, 右结合
`exp1//@exp2`, MapAll, 右结合
`exp1@@exp2`, Apply, 右结合
`exp1@@@exp2`, Apply,1, 右结合
***
`exp!`,Factorial
`exp!!`,Factorial2
***
`exp1<>exp2<>exp3`,StringJoin, 字符串衔接
***
`exp1^exp2`,Power, 右结合
***
`Sqrt[exp]`,Sqrt, 右结合
***
`exp1.exp2.exp3`,Dot,
***
`-exp`,Times[-1,exp]
`+exp`,exp
`PlusMinus`
`MinusPlus`
***
`exp1/exp2`, exp1 (exp2)^-1, 除法, 左结合
***
`exp1 exp2 exp3`, Times, 乘法
`exp1*exp2*exp3`, Times
***
`exp1+exp2+exp3`,Plus, 加法
`exp1-exp2`,exp1+(-exp2), 减法
***
`i;;j;;k`,Span
***
`exp1==exp2`,Equal
`exp1!=exp2`,Unequal
`exp1>=exp2`,GreaterEqual
***
`exp1===exp2`,SameQ
`exp1=!=exp2`,UnSameQ
***
`!expr`,Not
***
`exp1&&exp2&&exp3`,And
***
`exp1||exp2`,Or
***
`expr..`, Repeated
`expr...` RepeatedNull
***
`exp1 | exp2` , Alternatives, 模式并集
***
`symb:expr`, Pattern, 模式命名
`patt:expr`, Optional, 可选
***
`exp1~~exp2~~exp3`, StringExpression, 字符串表达式
***
`exp1/;exp2`, Condition, 条件, 左结合,
***
`exp1->exp2`,Rule,规则, 右结合
`exp1->exp2`, Rule, 右结合
`exp1:>exp2`, RuleDelayed, 右结合
`exp1:> exp2`,RuleDelayed, 右结合
***
`exp1/.exp2`, ReplaceAll, 替换规则, 左结合
`exp1//.exp2`, ReplaceRepeated, 左结合
***
`expr&`, Function, 函数
***
`exp1//exp2`, 后缀, 后作用
***
`exp1=exp2`,  Set[exp1,exp2],赋值, 右结合
`exp1:=exp2`,  SetDelayed[exp1,exp2],延迟赋值, 右结合
`exp1^=exp2`,  UpSet[exp1,exp2], 右结合
`exp1^:=exp2`,  UpSetDelayed[exp1,exp2], 右结合
`symb/:exp1=exp2`,  TagSet[symb,exp1,exp2], 右结合
`symb/:exp1:=exp2`,  TagSetDelayed[symb,exp1,exp2], 右结合
`expr=.` ,  Unset[expr]
`symb/:expr=.` ,  TagUnset[symb,expr]
`exp1|->exp2`,  Function[{exp1},exp2], 右结合
***
`expr>>filename`,Put, 导出
`expr>>>filename`,PutAppend
***
`exp1;exp2;exp3`,CompoundExpression, 复合表达式
`exp1;exp2;`,CompoundExpression
***
`` exp1\`exp2 ``, FormBox[exp1,exp2], 格式框符,右结合

## 特殊形式

`#`,Slot[1]
`#n`, Slot[n]
`#string`Slot["string"], 第一个参数是关联的时候, 提取`Key["string"]`对应的值.
`##`,SlotSequence[1], 提供给纯函数的参数序列
`##n`,SlotSequence[n],从第`n`个参数开始的参数序列
***
`%`,Out
`%%`,
`%n`,
***
`_`, Blank
`_expr`,Blank[expr]
`__`,BlankSequence[ ]
`__expr`,BlankSequence[expr]
`___`,BlankNullSequence[ ]
`___expr`,BlankNullSequence[expr]
`_.`,Optional[Blank[ ]]
`symb_`,Pattern[symb,Blank[ ]]
`symb_expr`,Pattern[symb,Blank[expr]]
`symb__`,Pattern[symb,BlankSequence[ ]]
`symb__expr`,Pattern[symb,BlankSequence[expr]]
`symb___`,Pattern[symb,BlankNullSequence[ ]]
`symb___expr`,Pattern[symb,BlankNullSequence[expr]]
`symb_.`,Optional[Pattern[symb,Blank[ ]]]
`_ :expr`, Optional[Blank[],expr], 带有默认值的可选参数
`symb_:expr`, Optional[Pattern[symb,Blank[]],expr], 命名的带有默认值的可选参数,
`_head :expr`, Optional[Blank[head],expr], 带有头部和默认值的可选参数
`symb_head:expr`, Optional[Pattern[symb,Blank[head]],expr], 命名的带有头部和默认值的可选参数

## 算符复合形式

guide/FunctionCompositionAndOperatorForms
