# 字符串模式

`字符串处理`: `字符串模式` tutorial/WorkingWithStringPatterns

StringExpression objects can be used in many string manipulation functions, including StringReplace, StringCases, StringSplit, and StringMatchQ.

`MatchQ`不能识别`StringExpression`, 即字符串表达式. 需要用`StringMatchQ`才可以识别. 例如:

```mathematica
MatchQ["adbasdd", __ ~~ "dd"]
StringMatchQ["adbasdd", __ ~~ "dd"]
```

`DeleteCases`不能删除字符串模式,可能是由于它使用`MatchQ`做匹配.

+ `LetterQ`

比较字符串时忽略大小写, 这个功能在`StringMatchQ`的选项中:
`StringMatchQ["acggtATTCaagc", __ ~~ "aT" ~~ __, IgnoreCase -> True]`

在字符串匹配中, `x_`只匹配单个字符, `characters`,`StringExpression[pattern...]`可以用来表示模式序列, 有各种各样对应正则表达式功能的函数.

比如

+ `StartOfString`   字符串开头
+ `EndOfString`   字符串结尾
+ `StartOfLine`   行的开始
+ `EndOfLine`   行的结束
+ `WordBoundary`   boundary between word characters and others
+ `Except[WordBoundary]`   anywhere except a word boundary

***

`字符串模式`: tutorial/StringPatterns
`文本标准化` guide/TextNormalization
`StringDelete`
`StringReplace`:替换字符串
`字符串运算`: guide/StringOperations
`正则表达式`:RegularExpression
