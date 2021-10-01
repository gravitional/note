# javascript

[JavaScript 教程](https://www.w3school.com.cn/js/index.asp)
[JavaScript教程-liaoxuefeng](https://www.liaoxuefeng.com/wiki/1022910821149312)

## intro

`JavaScript` 是属于 `HTML` 和 `Web` 的编程语言. 为何学习 `JavaScript`?
`JavaScript` 是 web 开发者必学的三种语言之一:

+ `HTML` 定义网页的内容
+ `CSS` 规定网页的布局
+ `JavaScript` 对网页行为进行编程

本教程提供关于 `JavaScript`, 以及 `JavaScript` 如何与 `HTML` 和 `CSS` 协同工作的知识.

+ `JavaScript` 和 Java 是完全不同的语言, 不论是概念还是设计.
+ `JavaScript` 在 1995 年由 Brendan Eich 发明, 并于 1997 年成为一部 ECMA 标准.
+ ECMA-262 是其官方名称. ECMAScript 6 (发布于 2015 年)是最新的 `JavaScript` 版本.

`JavaScript`是世界上最流行的脚本语言, 因为你在电脑, 手机, 平板上浏览的所有的网页, 以及无数基于 `HTML5` 的手机App, 交互逻辑都是由`JavaScript`驱动的.

简单地说, `JavaScript`是一种运行在浏览器中的解释型的编程语言.

那么问题来了, 为什么我们要学`JavaScript`?尤其是当你已经掌握了某些其他编程语言如Java, C++的情况下.

简单粗暴的回答就是: 因为你没有选择. 在Web世界里, 只有`JavaScript`能跨平台, 跨浏览器驱动网页, 与用户交互.

### JavaScript简介

#### JavaScript历史

要了解`JavaScript`, 我们首先要回顾一下`JavaScript`的诞生.

在上个世纪的1995年, 当时的网景公司正凭借其Navigator浏览器成为Web时代开启时最著名的第一代互联网公司.
由于网景公司希望能在静态`HTML`页面上添加一些动态效果, 于是叫Brendan Eich这哥们在两周之内设计出了`JavaScript`语言.
你没看错, 这哥们只用了10天时间.

为什么起名叫`JavaScript`?原因是当时Java语言非常红火, 所以网景公司希望借Java的名气来推广, 但事实上`JavaScript`除了语法上有点像Java, 其他部分基本上没啥关系.

#### ECMAScript

因为网景开发了`JavaScript`, 一年后微软又模仿`JavaScript`开发了JScript, 为了让`JavaScript`成为全球标准, 几个公司联合ECMA(European Computer Manufacturers Association)组织定制了`JavaScript`语言的标准, 被称为ECMAScript标准.

所以简单说来就是, ECMAScript是一种语言标准, 而`JavaScript`是网景公司对ECMAScript标准的一种实现.
那为什么不直接把`JavaScript`定为标准呢?因为`JavaScript`是网景的注册商标.
不过大多数时候, 我们还是用`JavaScript`这个词. 如果你遇到`ECMAScript`这个词, 简单把它替换为`JavaScript`就行了.

#### JavaScript版本

`JavaScript`语言是在10天时间内设计出来的, 虽然语言的设计者水平非常NB, 但谁也架不住"时间紧, 任务重",
所以, `JavaScript`有很多设计缺陷, 我们后面会慢慢讲到.

此外, 由于`JavaScript`的标准——`ECMAScript`在不断发展, 最新版`ECMAScript` 6标准(简称ES6)已经在2015年6月正式发布了, 所以, 讲到`JavaScript`的版本, 实际上就是说它实现了`ECMAScript`标准的哪个版本.

由于浏览器在发布时就确定了`JavaScript`的版本, 加上很多用户还在使用`IE6`这种古老的浏览器, 这就导致你在写`JavaScript`的时候, 要照顾一下老用户, 不能一上来就用最新的ES6标准写, 否则, 老用户的浏览器是无法运行新版本的`JavaScript`代码的.

不过, `JavaScript`的核心语法并没有多大变化. 我们的教程会先讲`JavaScript`最核心的用法, 然后, 针对ES6讲解新增特性.

### 快速入门

`JavaScript`代码可以直接嵌在网页的任何地方, 不过通常我们都把`JavaScript`代码放到

```js
<head>中:

<html>
<head>
  <script>
    alert('Hello, world');
  </script>
</head>
<body>
  ...
</body>
</html>
```

由`<script>...</script>`包含的代码就是JavaScript代码, 它将直接被浏览器执行.

第二种方法是把JavaScript代码放到一个单独的.js文件,
然后在HTML中通过`<script src="..."></script>`引入这个文件:

```js
<html>
<head>
  <script src="/static/js/abc.js"></script>
</head>
<body>
  ...
</body>
</html>
```

这样, `/static/js/abc.js`就会被浏览器执行.

把JavaScript代码放入一个单独的.js文件中更利于维护代码, 并且多个页面可以各自引用同一份`.js`文件.

可以在同一个页面中引入多个`.js`文件,
还可以在页面中多次编写`<script> js代码... </script>`, 浏览器按照顺序依次执行.

有些时候你会看到`<script>`标签还设置了一个`type`属性:

```js
<script type="text/javascript">
  ...
</script>
```

但这是没有必要的, 因为默认的`type`就是`JavaScript`, 所以不必显式地把`type`指定为`JavaScript`.

#### 如何编写JavaScript

可以用任何文本编辑器来编写`JavaScript`代码. 这里我们推荐以下几种文本编辑器:

##### Visual Studio Code

微软出的Visual Studio Code, 可以看做迷你版Visual Studio, 免费!跨平台!内置`JavaScript`支持, 强烈推荐使用!

##### Sublime Text

Sublime Text是一个好用的文本编辑器, 免费, 但不注册会不定时弹出提示框.

#### 如何运行JavaScript

要让浏览器运行`JavaScript`, 必须先有一个`HTML`页面, 在`HTML`页面中引入`JavaScript`.
然后, 让浏览器加载该`HTML`页面, 就可以执行`JavaScript`代码.

你也许会想, 直接在我的硬盘上创建好`HTML`和`JavaScript`文件, 然后用浏览器打开, 不就可以看到效果了吗?

这种方式运行部分`JavaScript`代码没有问题.
但由于浏览器的安全限制, 以`` file:// ``开头的地址无法执行如联网等`JavaScript`代码.
最终, 你还是需要架设一个`Web`服务器, 然后以`` http:// ``开头的地址来正常执行所有`JavaScript`代码.

俗话说得好, "工欲善其事, 必先利其器",
写 `JavaScript` 的时候, 如果期望显示`ABC`, 结果却显示`XYZ`, 到底代码哪里出了问题?
不要抓狂, 也不要泄气, 作为小白, 要坚信: `JavaScript`本身没有问题, 浏览器执行也没有问题, 有问题的一定是我的代码.

如何找出问题代码?这就需要调试.

怎么在浏览器中调试`JavaScript`代码呢?

首先, 你需要安装Google Chrome浏览器, Chrome浏览器对开发者非常友好, 可以让你方便地调试JavaScript代码.

安装后, 随便打开一个网页,
然后点击菜单"查看(View)"-"开发者(Developer)"-"开发者工具(Developer Tools)",
浏览器窗口就会一分为二, 下方就是开发者工具:

先点击"控制台(Console)", 在这个面板里可以直接输入`JavaScript`代码, 按回车后执行.

要查看一个变量的内容, 在Console中输入`console.log(a)`;,
回车后显示的值就是变量的内容.

关闭Console请点击右上角的"x"按钮. 请熟练掌握Console的使用方法,
在编写`JavaScript`代码时, 经常需要在Console运行测试代码.

如果你对自己还有更高的要求, 可以研究开发者工具的"源码(Sources)", 掌握断点, 单步执行等高级调试技巧.

#### 练习

打开新浪首页, 然后查看页面源代码, 找一找引入的`JavaScript`文件和直接编写在页面中的`JavaScript`代码.
然后在Chrome中打开开发者工具, 在控制台输入`console.log('Hello');`, 回车查看`JavaScript`代码执行结果.

### 基本语法

#### 语法

`JavaScript`的语法和Java语言类似, 每个语句以`;`结束, 语句块用`{...}`.
但是, `JavaScript`并不强制要求在每个语句的结尾加`;`,
浏览器中负责执行`JavaScript`代码的引擎会自动在每个语句的结尾补上`;`.

让`JavaScript`引擎自动加分号在某些情况下会改变程序的语义, 导致运行结果与期望不一致.
在本教程中, 我们不会省略;, 所有语句都会添加`;`

例如, 下面的一行代码就是一个完整的赋值语句:

```js
var x = 1;
```

下面的一行代码是一个字符串, 但仍然可以视为一个完整的语句:

```js
'Hello, world';
```

下面的一行代码包含两个语句, 每个语句用`;`表示语句结束:

```js
var x = 1; var y = 2; // 不建议一行写多个语句!
```

语句块是一组语句的集合, 例如, 下面的代码先做了一个判断, 如果判断成立, 将执行`{...}`中的所有语句:

```js
if (2 > 1) {
    x = 1;
    y = 2;
    z = 3;
}
```

注意花括号`{...}`内的语句具有缩进, 通常是4个空格.
缩进不是JavaScript语法要求必须的, 但缩进有助于我们理解代码的层次, 所以编写代码时要遵守缩进规则.
很多文本编辑器具有"自动缩进"的功能, 可以帮助整理代码.

`{...}`还可以嵌套, 形成层级结构:

```js
if (2 > 1) {
    x = 1;
    y = 2;
    z = 3;
    if (x < y) {
        z = 4;
    }
    if (x > y) {
        z = 5;
    }
}
```

`JavaScript`本身对嵌套的层级没有限制, 但是过多的嵌套无疑会大大增加看懂代码的难度.
遇到这种情况, 需要把部分代码抽出来, 作为函数来调用, 这样可以减少代码的复杂度.

#### 注释

以`//`开头直到行末的字符被视为行注释, 注释是给开发人员看的,
`JavaScript`引擎会自动忽略:

```js
// 这是一行注释
alert('hello'); // 这也是注释
```

另一种块注释是用`/*...*/`把多行字符包裹起来, 把一大"块"视为一个注释:

```js
/* 从这里开始是块注释
仍然是注释
仍然是注释
注释结束 */
```

练习: 分别利用行注释和块注释把下面的语句注释掉, 使它不再执行:

```js
alert('我不想执行');
alert('我也不想执行');
```

### 计算时间

1. 获取当前时间(指定日期)

```js
var myDate = new Date();
var date1 = new Date('yyyy/MM/dd hh:mm:ss');
var t2 = "yyyy-MM-dd hh:mm:ss";
var d2 = t2.replace(/\-/g, "/");
var date2 = new Date(d1);
```

2. 获取日期中的年月日时分秒

```js
myDate.getYear();        // 获取当前年份(2位)
myDate.getFullYear();    // 获取完整的年份(4位,1970-????)
myDate.getMonth();       // 获取当前月份(0-11,0代表1月)
myDate.getDate();        // 获取当前日(1-31)
myDate.getDay();         // 获取当前星期X(0-6,0代表星期天)
myDate.getTime();        // 获取当前时间(从1970.1.1开始的毫秒数)
myDate.getHours();       // 获取当前小时数(0-23)
myDate.getMinutes();     // 获取当前分钟数(0-59)
myDate.getSeconds();     // 获取当前秒数(0-59)
myDate.getMilliseconds();    // 获取当前毫秒数(0-999)
myDate.toLocaleDateString();     // 获取当前日期
var mytime=myDate.toLocaleTimeString();     // 获取当前时间
myDate.toLocaleString( );        // 获取日期与时间
```

### 数据类型和变量

#### 数据类型

计算机顾名思义就是可以做数学计算的机器,因此,计算机程序理所当然地可以处理各种数值.
但是,计算机能处理的远不止数值,还可以处理文本,图形,音频,视频,网页等各种各样的数据,不同的数据,需要定义不同的数据类型.
在`JavaScript`中定义了以下几种数据类型: 

#### Number

`JavaScript` 不区分整数和浮点数,统一用`Number`表示,以下都是合法的`Number`类型: 

```js
123; // 整数123
0.456; // 浮点数0.456
1.2345e3; // 科学计数法表示1.2345x1000,等同于1234.5
-99; // 负数
NaN; // NaN表示Not a Number,当无法计算结果时用NaN表示
Infinity; // Infinity表示无限大,当数值超过了JavaScript的Number所能表示的最大值时,就表示为Infinity
```

计算机由于使用二进制,所以,有时候用十六进制表示整数比较方便,十六进制用`0x`前缀和`0-9`,`a-f`表示,例如: `0xff00`,`0xa5b4c3d2`,等等,它们和十进制表示的数值完全一样.
`Number` 可以直接做四则运算,规则和数学一致: 

```js
1 + 2; // 3
(1 + 2) * 5 / 2; // 7.5
2 / 0; // Infinity
0 / 0; // NaN
10 % 3; // 1
10.5 % 3; // 1.5
```

注意%是求余运算.

#### 字符串

字符串是以单引号`'`或双引号`"`括起来的任意文本,比如`'abc'`, `"xyz"` 等等.
请注意,`''`或`""`本身只是一种表示方式,不是字符串的一部分,因此,字符串`'abc'`只有`a`,`b`,`c`这`3`个字符.

#### 布尔值

`布尔值`和布尔代数的表示完全一致,布尔值只有`true`,`false` 两种值,要么是 `true`,要么是 `false`,可以直接用 `true`,`false` 表示布尔值,也可以通过布尔运算计算出来: 

```js
true; // 这是一个true值
false; // 这是一个false值
2 > 1; // 这是一个true值
2 >= 3; // 这是一个false值
```

`&&` 运算是与运算,只有所有都为 `true`,`&&` 运算结果才是 `true`: 

```js
true && true; // 这个&&语句计算结果为true
true && false; // 这个&&语句计算结果为false
false && true && false; // 这个&&语句计算结果为false
```

`||`运算是或运算,只要其中有一个为 `true`,`||`运算结果就是`true`: 

```js
false || false; // 这个||语句计算结果为false
true || false; // 这个||语句计算结果为true
false || true || false; // 这个||语句计算结果为true
```

`!`运算是非运算,它是一个单目运算符,把`true`变成`false`,`false`变成`true`: 

```js
! true; // 结果为false
! false; // 结果为true
! (2 > 5); // 结果为true
```

布尔值经常用在条件判断中,比如: 

```js
var age = 15;
if (age >= 18) {
    alert('adult');
} else {
    alert('teenager');
}
```

#### 比较运算符

当我们对`Number`做比较时,可以通过比较运算符得到一个布尔值: 

```js
2 > 5; // false
5 >= 2; // true
7 == 7; // true
```

实际上,`JavaScript`允许对任意数据类型做比较: 

```js
false == 0; // true
false === 0; // false
```

要特别注意相等运算符`==`.`JavaScript`在设计时,有两种比较运算符: 

+ 第一种是`==`比较,它会自动转换数据类型再比较,很多时候,会得到非常诡异的结果;
+ 第二种是`===`比较,它不会自动转换数据类型,如果数据类型不一致,返回`false`,如果一致,再比较.

由于`JavaScript`这个设计缺陷,不要使用==比较,始终坚持使用`===`比较.

另一个例外是`NaN`这个特殊的`Number`与所有其他值都不相等,包括它自己: 

```js
NaN === NaN; // false
```

唯一能判断`NaN`的方法是通过`isNaN()`函数: 

```js
isNaN(NaN); // true
```

最后要注意浮点数的相等比较: 

```js
1 / 3 === (1 - 2 / 3); // false
```

这不是`JavaScript`的设计缺陷. 浮点数在运算过程中会产生误差,因为计算机无法精确表示无限循环小数.要比较两个浮点数是否相等,只能计算它们之差的绝对值,看是否小于某个阈值: 

```js
Math.abs(1 / 3 - (1 - 2 / 3)) < 0.0000001; // true
```

#### null和undefined

`null`表示一个"空"的值,它和`0`以及空字符串`''`不同, `0`是一个数值, `''`表示长度为`0`的字符串,而`null`表示`"空"`.
在其他语言中,也有类似`JavaScript`的`null`的表示,例如`Java`也用`null`,`Swift`用 `nil`, `Python` 用 `None` 表示.
但是,在`JavaScript`中,还有一个和 `null` 类似的 `undefined` ,它表示`未定义`.

`JavaScript`的设计者希望用 `null` 表示一个空的值, 而 `undefined` 表示值未定义.
事实证明,这并没有什么卵用,区分两者的意义不大. 大多数情况下,我们都应该用 `null`. `undefined` 仅仅在判断函数参数是否传递的情况下有用.

#### 数组

数组是一组按顺序排列的集合,集合的每个值称为元素.`JavaScript`的数组可以包括任意数据类型.例如: 

```js
[1, 2, 3.14, 'Hello', null, true];
```

上述数组包含`6`个元素.数组用`[]`表示,元素之间用`,`分隔.
另一种创建数组的方法是通过`Array()`函数实现: 

```js
new Array(1, 2, 3); // 创建了数组[1, 2, 3]
```

然而,出于代码的可读性考虑,强烈建议直接使用`[]`.

数组的元素可以通过索引来访问.请注意,索引的起始值为`0`: 

```js
var arr = [1, 2, 3.14, 'Hello', null, true];
arr[0]; // 返回索引为0的元素,即1
arr[5]; // 返回索引为5的元素,即true
arr[6]; // 索引超出了范围,返回undefined
```

#### 对象

`JavaScript`的对象是一组由`键-值`组成的无序集合,例如: 

```js
var person = {
    name: 'Bob',
    age: 20,
    tags: ['js', 'web', 'mobile'],
    city: 'Beijing',
    hasCar: true,
    zipcode: null
};
```

`JavaScript`对象的键都是字符串类型,值可以是任意数据类型.
上述`person`对象一共定义了`6`个键值对,其中每个键又称为对象的属性, 例如,`person` 的 `name` 属性为`'Bob'` , `zipcode` 属性为 `null`.

要获取一个对象的属性,我们用对象`变量.属性名`的方式: 

```js
person.name; // 'Bob'
person.zipcode; // null
```

#### 变量

变量的概念基本上和初中代数的方程变量是一致的,只是在计算机程序中,变量不仅可以是数字,还可以是任意数据类型.
变量在`JavaScript`中就是用一个变量名表示,变量名是大小写英文,数字,`$`和`_`的组合,且不能用数字开头.
变量名也不能是`JavaScript`的关键字,如`if`,`while`等. 申明一个变量用`var`语句,比如: 

```js
var a; // 申明了变量a,此时a的值为undefined
var $b = 1; // 申明了变量$b,同时给$b赋值,此时$b的值为1
var s_007 = '007'; // s_007是一个字符串
var Answer = true; // Answer是一个布尔值true
var t = null; // t的值是null
```

变量名也可以用中文,但是,请不要给自己找麻烦. 在`JavaScript`中,使用等号`=`对变量进行赋值.
可以把任意数据类型赋值给变量,同一个变量可以反复赋值,而且可以是不同类型的变量,但是要注意只能用`var`申明一次,例如: 

```js
var a = 123; // a的值是整数123
a = 'ABC'; // a变为字符串
```

这种变量本身类型不固定的语言称之为动态语言,与之对应的是`静态语言`.
静态语言在定义变量时必须指定变量类型,如果赋值的时候类型不匹配,就会报错.例如`Java`是静态语言,赋值语句如下: 

```js
int a = 123; // a是整数类型变量,类型用int申明
a = "ABC"; // 错误: 不能把字符串赋给整型变量
```

和静态语言相比,动态语言更灵活,就是这个原因.
请不要把赋值语句的等号等同于数学的等号.比如下面的代码: 

```js
var x = 10;
x = x + 2;
```

如果从数学上理解`x = x + 2`那无论如何是不成立的,在程序中,赋值语句先计算右侧的表达式`x + 2`,得到结果`12`,再赋给变量`x`.由于x之前的值是`10`,重新赋值后,`x`的值变成`12`.
要显示变量的内容,可以用`console.log(x)`,打开`Chrome`的控制台就可以看到结果.

```js
// 打印变量x
var x = 100;
console.log(x);
```

使用`console.log()`代替`alert()`的好处是可以避免弹出烦人的对话框.

#### strict模式

`JavaScript`在设计之初,为了方便初学者学习,并不强制要求用`var`申明变量.
这个设计错误带来了严重的后果: 如果一个变量没有通过`var`申明就被使用,那么该变量就自动被申明为全局变量: 

```js
i = 10; // i现在是全局变量
```

在同一个页面的不同的`JavaScript`文件中,如果都不用`var`申明,恰好都使用了变量`i`,将造成变量`i`互相影响,产生难以调试的错误结果.
使用`var`申明的变量则不是全局变量,它的范围被限制在该变量被申明的函数体内(函数的概念将稍后讲解),同名变量在不同的函数体内互不冲突.

为了修补`JavaScript`这一严重设计缺陷,ECMA在后续规范中推出了`strict`模式,
在`strict`模式下运行的`JavaScript`代码,强制通过`var`申明变量,未使用`var`申明变量就使用的,将导致运行错误.

启用`strict`模式的方法是在`JavaScript`代码的第一行写上: 

```js
'use strict`;
```

这是一个字符串,不支持`strict`模式的浏览器会把它当做一个字符串语句执行,支持`strict`模式的浏览器将开启`strict`模式运行`JavaScript`.

来测试一下你的浏览器是否能支持`strict`模式: 

```js
'use strict';
// 如果浏览器支持strict模式,
// 下面的代码将报ReferenceError错误:
abc = 'Hello, world';
console.log(abc);
```

运行代码,如果浏览器报错,请修复后再运行.如果浏览器不报错,说明你的浏览器太古老了,需要尽快升级.
不用`var`申明的变量会被视为`全局变量`,为了避免这一缺陷,所有的`JavaScript`代码都应该使用`strict`模式.我们在后面编写的`JavaScript`代码将全部采用`strict`模式.

### 字符串

`JavaScript`的字符串就是用`''`或`""`括起来的字符表示.
如果`'`本身也是一个字符,那就可以用`""`括起来,比如`"I'm OK"`包含的字符是`I`,`'`,`m`,`空格`,`O`,`K`这`6`个字符.

如果字符串内部既包含`'`又包含`"`怎么办?可以用转义字符`\`来标识,比如: 

```js
'I\'m \"OK\"!';
```

表示的字符串内容是: `I'm "OK"!`

转义字符`\`可以转义很多字符,比如`\n`表示换行,`\t`表示制表符,字符`\`本身也要转义,所以`\\`表示的字符就是`\`.

`ASCII`字符可以以`\x##`形式的十六进制表示,例如: 

```js
'\x41'; // 完全等同于 'A'
```

还可以用`\u####`表示一个`Unicode`字符: 

```js
'\u4e2d\u6587'; // 完全等同于 '中文'
```

#### 多行字符串

由于多行字符串用`\n`写起来比较费事, 所以最新的`ES6`标准新增了一种多行字符串的表示方法,用反引号`` `...` ``表示: 

```js
`这是一个
多行
字符串`;
```

注意: 反引号在键盘的`ESC`下方,数字键`1`的左边: 
练习: 测试你的浏览器是否支持`ES6`标准,如果不支持,请把多行字符串用`\n`重新表示出来: 

```js
// 如果浏览器不支持ES6,将报SyntaxError错误:
console.log(`多行
字符串
测试`);
```

#### 模板字符串

要把多个字符串连接起来,可以用`+`号连接: 

```js
var name = '小明';
var age = 20;
var message = '你好, ' + name + ', 你今年' + age + '岁了!';
alert(message);
```

如果有很多变量需要连接,用`+`号就比较麻烦.`ES6`新增了一种模板字符串,表示方法和上面的多行字符串一样,但是它会自动替换字符串中的变量: 

```js
var name = '小明';
var age = 20;
var message = `你好, ${name}, 你今年${age}岁了!`;
alert(message);
```

练习: 测试你的浏览器是否支持`ES6`模板字符串,如果不支持,请把模板字符串改为`+`连接的普通字符串: 

```js
'use strict';
// 如果浏览器支持模板字符串,将会替换字符串内部的变量:
var name = '小明';
var age = 20;
console.log(`你好, ${name}, 你今年${age}岁了!`);
```

#### 操作字符串

字符串常见的操作如下: 

```js
var s = 'Hello, world!';
s.length; // 13
```

要获取字符串某个指定位置的字符,使用类似`Array`的下标操作,索引号从`0`开始: 

```js
var s = 'Hello, world!';
s[0]; // 'H'
s[6]; // ' '
s[7]; // 'w'
s[12]; // '!'
s[13]; // undefined 超出范围的索引不会报错,但一律返回undefined
```

需要特别注意的是,字符串是不可变的,如果对字符串的某个索引赋值,不会有任何错误,但是,也没有任何效果: 

```js
var s = 'Test';
s[0] = 'X';
alert(s); // s仍然为'Test'
```

`JavaScript`为字符串提供了一些常用方法,注意,调用这些方法本身不会改变原有字符串的内容,而是返回一个新字符串: 

+ toUpperCase

`toUpperCase()`把一个字符串全部变为大写: 

```js
var s = 'Hello';
s.toUpperCase(); // 返回'HELLO'
```

+ `toLowerCase` ; `toLowerCase()`把一个字符串全部变为小写: 

```js
var s = 'Hello';
var lower = s.toLowerCase(); // 返回'hello'并赋值给变量lower
lower; // 'hello'
```

+ `indexOf`; `indexOf()`会搜索指定字符串出现的位置: 

```js
var s = 'hello, world';
s.indexOf('world'); // 返回7
s.indexOf('World'); // 没有找到指定的子串,返回-1
```

+ `substring`; `substring()`返回指定索引区间的子串: 

```js
var s = 'hello, world'
s.substring(0, 5); // 从索引0开始到5(不包括5),返回'hello'
s.substring(7); // 从索引7开始到结束,返回'world'
```

### 数组

`JavaScript`的`Array`可以包含任意数据类型, 并通过`索引`来访问每个元素.

要取得`Array`的长度,直接访问`length`属性: 

```js
var arr = [1, 2, 3.14, 'Hello', null, true];
arr.length; // 6
```

请注意,直接给`Array`的`length`赋一个新的值会导致`Array`大小的变化: 

```js
var arr = [1, 2, 3];
arr.length; // 3
arr.length = 6;
arr; // arr变为[1, 2, 3, undefined, undefined, undefined]
arr.length = 2;
arr; // arr变为[1, 2]
```

`Array`可以通过索引把对应的元素修改为新的值,因此,对`Array`的索引进行赋值会直接修改这个`Array`: 

```js
var arr = ['A', 'B', 'C'];
arr[1] = 99;
arr; // arr现在变为['A', 99, 'C']
```

请注意,如果通过索引赋值时,索引超过了范围,同样会引起`Array`大小的变化: 

```js
var arr = [1, 2, 3];
arr[5] = 'x';
arr; // arr变为[1, 2, 3, undefined, undefined, 'x']
```

大多数其他编程语言不允许直接改变数组的大小,越界访问索引会报错.
然而,`JavaScript`的`Array`却不会有任何错误.在编写代码时,不建议直接修改`Array`的大小,访问索引时要确保索引不会越界.

#### indexOf

与`String`类似, `Array`也可以通过`indexOf()`来搜索一个指定的元素的位置: 

```js
var arr = [10, 20, '30', 'xyz'];
arr.indexOf(10); // 元素10的索引为0
arr.indexOf(20); // 元素20的索引为1
arr.indexOf(30); // 元素30没有找到,返回-1
arr.indexOf('30'); // 元素'30'的索引为2
```

注意了,数字`30`和字符串`'30'`是不同的元素.

#### slice

`slice()`就是对应`String`的`substring()`版本,它截取`Array`的部分元素,然后返回一个新的`Array`: 

```js
var arr = ['A', 'B', 'C', 'D', 'E', 'F', 'G'];
arr.slice(0, 3); // 从索引0开始,到索引3结束,但不包括索引3: ['A', 'B', 'C']
arr.slice(3); // 从索引3开始到结束: ['D', 'E', 'F', 'G']
```

注意到`slice()`的起止参数包括开始索引, 不包括结束索引处的元素.
如果不给`slice()`传递任何参数,它就会从头到尾截取所有元素.利用这一点,我们可以很容易地复制一个`Array`: 

```js
var arr = ['A', 'B', 'C', 'D', 'E', 'F', 'G'];
var aCopy = arr.slice();
aCopy; // ['A', 'B', 'C', 'D', 'E', 'F', 'G']
aCopy === arr; // false
```

#### push和pop

`push()`向`Array`的末尾添加若干元素, `pop()`则把`Array`的最后一个元素删除掉: 

```js
var arr = [1, 2];
arr.push('A', 'B'); // 返回Array新的长度: 4
arr; // [1, 2, 'A', 'B']
arr.pop(); // pop()返回'B'
arr; // [1, 2, 'A']
arr.pop(); arr.pop(); arr.pop(); // 连续pop 3次
arr; // []
arr.pop(); // 空数组继续pop不会报错,而是返回undefined
arr; // []
```

#### unshift和shift

如果要往`Array`的头部添加若干元素,使用`unshift()`方法, `shift()`方法则把`Array`的第一个元素删掉: 

```js
var arr = [1, 2];
arr.unshift('A', 'B'); // 返回Array新的长度: 4
arr; // ['A', 'B', 1, 2]
arr.shift(); // 'A'
arr; // ['B', 1, 2]
arr.shift(); arr.shift(); arr.shift(); // 连续shift 3次
arr; // []
arr.shift(); // 空数组继续shift不会报错,而是返回undefined
arr; // []
```

#### sort

`sort()`可以对当前`Array`进行排序,它会直接修改当前`Array`的元素位置,直接调用时,按照默认顺序排序: 

```js
var arr = ['B', 'C', 'A'];
arr.sort();
arr; // ['A', 'B', 'C']
```

能否按照我们自己指定的顺序排序呢?完全可以,我们将在后面的函数中讲到.

#### reverse

`reverse()`把整个`Array`的元素给调个个,也就是反转: 

```js
var arr = ['one', 'two', 'three'];
arr.reverse(); 
arr; // ['three', 'two', 'one']
```

#### splice

`splice()`方法是修改`Array`的"万能方法". 它可以从指定的索引开始删除若干元素,然后再从该位置添加若干元素: 

```js
var arr = ['Microsoft', 'Apple', 'Yahoo', 'AOL', 'Excite', 'Oracle'];
// 从索引2开始删除3个元素,然后再添加两个元素:
arr.splice(2, 3, 'Google', 'Facebook'); // 返回删除的元素 ['Yahoo', 'AOL', 'Excite']
arr; // ['Microsoft', 'Apple', 'Google', 'Facebook', 'Oracle']
// 只删除,不添加:
arr.splice(2, 2); // ['Google', 'Facebook']
arr; // ['Microsoft', 'Apple', 'Oracle']
// 只添加,不删除:
arr.splice(2, 0, 'Google', 'Facebook'); // 返回[],因为没有删除任何元素
arr; // ['Microsoft', 'Apple', 'Google', 'Facebook', 'Oracle']
```

#### concat

`concat()`方法把当前的`Array`和另一个`Array`连接起来,并返回一个新的`Array`: 

```js
var arr = ['A', 'B', 'C'];
var added = arr.concat([1, 2, 3]);
added; // ['A', 'B', 'C', 1, 2, 3]
arr; // ['A', 'B', 'C']
```

请注意,`concat()`方法并没有修改当前`Array`,而是返回了一个新的`Array`.
实际上,`concat()`方法可以接收任意个元素和`Array`,并且自动把`Array`拆开,然后全部添加到新的`Array`里: 

```js
var arr = ['A', 'B', 'C'];
arr.concat(1, 2, [3, 4]); // ['A', 'B', 'C', 1, 2, 3, 4]
```

#### join

`join()`方法是一个非常实用的方法,它把当前`Array`的每个元素都用指定的字符串连接起来,然后返回连接后的字符串: 

```js
var arr = ['A', 'B', 'C', 1, 2, 3];
arr.join('-'); // 'A-B-C-1-2-3'
```

如果`Array`的元素不是字符串,将`自动转换`为字符串后再连接.

#### 多维数组

如果数组的某个元素又是一个`Array`,则可以形成多维数组,例如: 

```js
var arr = [[1, 2, 3], [400, 500, 600], '-'];
```

上述`Array`包含3个元素,其中头两个元素本身也是`Array`.

练习: 如何通过索引取到`500`这个值: 

```js
'use strict';
var arr = [[1, 2, 3], [400, 500, 600], '-'];
var x = arr[1][1];
console.log(x); // x应该为500
```

#### 小结

`Array`提供了一种顺序存储一组元素的功能,并可以按索引来读写.

练习: 在新生欢迎会上,你已经拿到了新同学的名单,请排序后显示: `欢迎XXX,XXX,XXX和XXX同学!`: 

```js
'use strict';
var arr = ['小明', '小红', '大军', '阿黄'];
console.log(`欢迎${arr[0]},${arr[1]},${arr[2]},和${arr[3]}同学`);
```

### 对象

### 条件判断

#### 条件(三元)运算符

`条件运算符`是`JavaScript`中唯一需要三个`操作数`的`运算符`. 运算的结果根据给定条件在两个值中取其一.语法为: 

```js
条件 ? 值1 : 值2
```

如果`条件`为真,则结果取`值1`.否则为`值2`. 你能够在任何允许使用标准运算符的地方使用条件运算符.
例如,

```js
var status = (age >= 18) ? "adult" : "minor";
```

当 `age` 大于等于18的时候,将 `"adult"` 赋值给 `status`; 否则将 `"minor"` 赋值给 `status`.

### 循环

### 字典和Set

### iterable

## 函数

### 变量作用域与解构赋值

在`JavaScript`中, 用`var`申明的变量实际上是有作用域的.
如果`变量`在`函数体`内部申明, 则该变量的作用域为整个函数体, 在函数体外不可引用该变量:

```js
'use strict';
function foo() {
    var x = 1;
    x = x + 1;
}
x = x + 2; // ReferenceError! 无法在函数体外引用变量x
```

如果两个不同的函数各自申明了同一个变量, 那么该变量只在各自的函数体内起作用. 换句话说, 不同函数内部的同名变量互相独立, 互不影响:

```js
'use strict';
function foo() {
    var x = 1;
    x = x + 1;
}
function bar() {
    var x = 'A';
    x = x + 'B';
}
```

由于`JavaScript` 的函数可以嵌套, 此时, `内部函数`可以访问`外部函数`定义的变量, 反过来则不行:

```js
'use strict';
function foo() {
    var x = 1;
    function bar() {
        var y = x + 1; // bar可以访问foo的变量x!
    }
    var z = y + 1; // ReferenceError! foo不可以访问bar的变量y!
}
```

如果内部函数和外部函数的变量名重名怎么办?来测试一下:

```js
function foo() {
    var x = 1;
    function bar() {
        var x = 'A';
        console.log('x in bar() = ' + x); // 'A'
    }
    console.log('x in foo() = ' + x); // 1
    bar();
}
foo();
```

这说明`JavaScript`的函数在查找变量时从自身函数定义开始, 从 `内` 向 `外` 查找.
如果内部函数定义了与外部函数`重名`的变量, 则内部函数的变量将`屏蔽`外部函数的变量.

#### 变量提前

`JavaScript`的函数定义有个特点, 它会先扫描整个函数体的语句, 把所有申明的变量`提前` 到函数顶部:

```js
'use strict';
function foo() {
    var x = 'Hello, ' + y;
    console.log(x);
    var y = 'Bob';
}
foo();
```

虽然是`strict`模式, 但语句`var x = 'Hello, ' + y;`并不报错, 原因是变量`y`在稍后申明了.
但是`console.log`显示`Hello, undefined`, 说明变量`y`的值为`undefined`.
这正是因为`JavaScript`引擎自动提升了变量`y`的声明, 但不会提升变量`y`的赋值.

对于上述`foo()`函数, `JavaScript`引擎看到的代码相当于:

```js
function foo() {
    var y; // 提升变量y的申明, 此时y为undefined
    var x = 'Hello, ' + y;
    console.log(x);
    y = 'Bob';
}
```

由于`JavaScript`的这一怪异的`特性`, 我们在函数内部定义变量时, 请严格遵守`在函数内部首先申明所有变量`这一规则.
最常见的做法是用一个`var`申明函数内部用到的所有变量:

```js
function foo() {
    var
        x = 1, // x初始化为1
        y = x + 1, // y初始化为2
        z, i; // z和i为undefined
    // 其他语句:
    for (i=0; i<100; i++) {
        ...
    }
}
```

#### 全局作用域

不在任何函数内定义的变量就具有`全局作用域`. 实际上, `JavaScript`默认有一个全局对象`window`, 全局作用域的变量实际上被绑定到`window`的一个属性:

```js
'use strict';
var course = 'Learn JavaScript';
alert(course); // 'Learn JavaScript'
alert(window.course); // 'Learn JavaScript'
```

因此, 直接访问全局变量`course`和访问`window.course`是完全一样的.

你可能猜到了, 由于函数定义有两种方式, 以变量方式`var foo = function () {}`定义的`函数`实际上也是一个全局变量,
因此, `顶层函数`的定义也被视为一个全局变量, 并绑定到`window`对象:

```js
'use strict';
function foo() {
    alert('foo');
}
foo(); // 直接调用foo()
window.foo(); // 通过window.foo()调用
```

进一步大胆地猜测, 我们每次直接调用的`alert()`函数其实也是`window`的一个变量:

```js
'use strict';
window.alert('调用window.alert()');
// 把alert保存到另一个变量:
var old_alert = window.alert;
// 给alert赋一个新函数:
window.alert = function () {}
// 恢复alert:
window.alert = old_alert;
alert('又可以用alert()了!');
```

这说明`JavaScript`实际上只有一个全局作用域.
任何变量(函数也视为变量), 如果没有在当前函数作用域中找到, 就会继续往上查找, 最后如果在全局作用域中也没有找到, 则报`ReferenceError`错误.

#### 名字空间

全局变量会绑定到`window`上, 不同的J`avaScript`文件如果使用了相同的全局变量, 或者定义了相同名字的顶层函数, 都会造成命名冲突, 并且很难被发现.
减少冲突的一个方法是把自己的所有变量和函数全部绑定到一个全局变量中. 例如:

```js
// 唯一的全局变量MYAPP:
var MYAPP = {};
// 其他变量:
MYAPP.name = 'myapp';
MYAPP.version = 1.0;
// 其他函数:
MYAPP.foo = function () {
    return 'foo';
};
```

把自己的代码全部放入唯一的名字空间`MYAPP`中, 会大大减少全局变量冲突的可能.

许多著名的`JavaScript`库都是这么干的: `jQuery`, `YUI`, `underscore` 等等.

#### 局部作用域

由于 `JavaScript` 的变量作用域实际上是函数内部, 我们在`for`循环等语句块中是无法定义具有局部作用域的变量的:

```js
'use strict';
function foo() {
    for (var i=0; i<100; i++) {
        //
    }
    i += 100; // 仍然可以引用变量i
}
```

为了解决块级作用域, `ES6`引入了新的关键字`let`, 用`let`替代`var`可以申明一个块级作用域的变量:

```js
'use strict';
function foo() {
    var sum = 0;
    for (let i=0; i<100; i++) {
        sum += i;
    }
    // SyntaxError:
    i += 1;
}
```

#### 常量

由于`var`和`let`申明的是变量, 如果要申明一个`常量`, 在`ES6`之前是不行的, 我们通常用`全部大写`的变量来表示`这是一个常量, 不要修改它的值`:

```js
var PI = 3.14;
```

ES6标准引入了新的关键字`const`来定义常量, `const`与`let`都具有块级作用域:

```js
'use strict';
const PI = 3.14;
PI = 3; // 某些浏览器不报错, 但是无效果!
PI; // 3.14
```

#### 解构赋值

从ES6开始, `JavaScript` 引入了`解构赋值`, 可以同时对一组变量进行赋值.

什么是`解构赋值`?我们先看看传统的做法, 如何把一个数组的元素分别赋值给几个变量:

```js
var array = ['hello', 'JavaScript', 'ES6'];
var x = array[0];
var y = array[1];
var z = array[2];
```

现在, 在ES6中, 可以使用解构赋值, 直接对多个变量同时赋值:

```js
'use strict';
// 如果浏览器支持解构赋值就不会报错:
var [x, y, z] = ['hello', 'JavaScript', 'ES6'];
```

注意, 对数组元素进行解构赋值时, 多个变量要用`[...]`括起来.

如果数组本身还有嵌套, 也可以通过下面的形式进行解构赋值, 注意`嵌套层次`和位置要保持一致:

```js
let [x, [y, z]] = ['hello', ['JavaScript', 'ES6']];
x; // 'hello'
y; // 'JavaScript'
z; // 'ES6'
```

`解构赋值`还可以忽略某些元素:

```bash
let [, , z] = ['hello', 'JavaScript', 'ES6']; // 忽略前两个元素, 只对z赋值第三个元素
z; // 'ES6'
```

如果需要从一个对象中取出若干属性, 也可以使用`解构赋值`, 便于快速获取对象的指定属性:

```js
'use strict';
var person = {
    name: '小明',
    age: 20,
    gender: 'male',
    passport: 'G-12345678',
    school: 'No.4 middle school'
};
var {name, age, passport} = person;
```

对一个`对象`进行解构赋值时, 同样可以直接对嵌套的`对象属性`进行赋值, 只要保证对应的层次是一致的:

```js
var person = {
    name: '小明',
    age: 20,
    gender: 'male',
    passport: 'G-12345678',
    school: 'No.4 middle school',
    address: {
        city: 'Beijing',
        street: 'No.1 Road',
        zipcode: '100001'
    }
};
var {name, address:{city, zip}} = person;
name; // '小明'
city; // 'Beijing'
zip; // undefined, 因为属性名是zipcode而不是zip
// 注意: address不是变量, 而是为了让city和zip获得嵌套的address对象的属性:
address; // Uncaught ReferenceError: address is not defined
```

使用解构赋值对`对象属性`进行赋值时, 如果对应的属性不存在, 变量将被赋值为`undefined`, 这和引用不存在的属性获得`undefined`是一致的.
如果要使用的`变量名`和`属性名`不一致, 可以用下面的语法获取:

```js
var person = {
    name: '小明',
    age: 20,
    gender: 'male',
    passport: 'G-12345678',
    school: 'No.4 middle school'
};
// 把passport属性赋值给变量id:
let {name, passport:id} = person;
name; // '小明'
id; // 'G-12345678'
// 注意: passport不是变量, 而是为了让变量id获得passport属性:
passport; // Uncaught ReferenceError: passport is not defined
```

`解构赋值`还可以使用`默认值`, 这样就避免了不存在的属性返回`undefined`的问题:

```js
var person = {
    name: '小明',
    age: 20,
    gender: 'male',
    passport: 'G-12345678'
};
// 如果person对象没有single属性, 默认赋值为true:
var {name, single=true} = person;
name; // '小明'
single; // true
```

有些时候, 如果变量已经被声明了, 再次赋值的时候, 正确的写法也会报语法错误:

```js
// 声明变量:
var x, y;
// 解构赋值:
{x, y} = { name: '小明', x: 100, y: 200};
// 语法错误: Uncaught SyntaxError: Unexpected token =
```

这是因为`JavaScript`引擎把`{`开头的语句当作了`块`处理, 于是`=`不再合法. 解决方法是用小括号`()`括起来:

```js
({x, y} = { name: '小明', x: 100, y: 200});
```

#### 使用场景

解构赋值在很多时候可以大大简化代码. 例如, 交换两个变量`x`和`y`的值, 可以这么写, 不再需要临时变量:

```js
var x=1, y=2;
[x, y] = [y, x]
```

快速获取当前页面的域名和路径:

```js
var {hostname:domain, pathname:path} = location;
```

如果函数接收`对象`作为参数, 那么, 可以使用解构直接把`对象`的`属性`绑定到`变量`中. 例如, 下面的函数可以快速创建`Date`对象:

```js
function buildDate({year, month, day, hour=0, minute=0, second=0}) {
    return new Date(year + '-' + month + '-' + day + ' ' + hour + ':' + minute + ':' + second);
}
```

它的方便之处在于传入的对象只需要 `year`, `month` 和 `day` 这三个属性:

```js
buildDate({ year: 2017, month: 1, day: 1 });
// Sun Jan 01 2017 00:00:00 GMT+0800 (CST)
```

也可以传入 `hour`, `minute` 和 `second` 属性:

```js
buildDate({ year: 2017, month: 1, day: 1, hour: 20, minute: 15 });
// Sun Jan 01 2017 20:15:00 GMT+0800 (CST)
```

使用解构赋值可以减少代码量, 但是, 需要在支持ES6解构赋值特性的现代浏览器中才能正常运行. 目前支持解构赋值的浏览器包括`Chrome`, `Firefox`, `Edge` 等.

### 方法

### 高解函数

### map/reduce

### filter

### sort

### Array

### 闭包

#### 函数作为返回值

`高阶函数`除了可以接受`函数`作为参数外, 还可以把`函数`作为结果值返回.

例如通常情况下的求和的函数:

```js
function sum(arr) {
    return arr.reduce(function (x, y) {
        return x + y;
    });
}
sum([1, 2, 3, 4, 5]); // 15
```

但是也可以延迟求和, 返回求和的函数!

```js
function lazy_sum(arr) {
    var sum = function () {
        return arr.reduce(function (x, y) {
            return x + y;
        });
    }
    return sum;
}
```

当我们调用`lazy_sum()`时, 返回的并不是求和结果, 而是求和函数`sum`:

```js
var f = lazy_sum([1, 2, 3, 4, 5]); // function sum()
```

调用函数f时, 才真正计算求和的结果:

```js
f(); // 15
```

在这个例子中, 我们在函数`lazy_sum`中又定义了函数`sum`.
并且, 内部函数`sum`可以引用外部函数`lazy_sum`的参数和局部变量, 当`lazy_sum`返回函数`sum`时,
`相关参数`和`变量`都保存在返回的函数中, 这种称为`闭包(Closure)`的程序结构拥有极大的威力.

请再注意一点, 当我们调用`lazy_sum()`时, 每次调用都会返回一个新的函数, 即使传入相同的参数:

```js
var f1 = lazy_sum([1, 2, 3, 4, 5]);
var f2 = lazy_sum([1, 2, 3, 4, 5]);
f1 === f2; // false
```

`f1()`和`f2()`的调用结果互不影响.

#### 闭包

注意到返回的函数在其定义内部引用了局部变量`arr`.
也就是当函数`lazy_sum`返回了函数`sum`后, `lazy_sum`内部的`局部变量`还被`sum`引用.
所以, 闭包用起来简单, 实现起来可不容易.

另一个需要注意的问题是, 返回的函数并没有立刻执行, 而是直到调用了`f()`才执行. 我们来看一个例子:

```js
function count() {
    var arr = [];
    for (var i=1; i<=3; i++) {
        arr.push(function () {
            return i * i;
        });
    }
    return arr;
}
var results = count();
var f1 = results[0];
var f2 = results[1];
var f3 = results[2];
```

在上面的例子中, 每次循环, 都创建了一个新的函数, 然后, 把创建的`3`个函数都添加到一个`Array`中返回了.

你可能认为调用`f1()`, `f2()`和`f3()`结果应该是`1`, `4`, `9`, 但实际结果是:

```js
f1(); // 16
f2(); // 16
f3(); // 16
```

全部都是`16`!原因就在于返回的函数引用了变量`i`, 但它并非立刻执行. 等到`3`个函数都返回时, 它们所引用的变量`i`已经变成了`4`, 因此最终结果为`16`.
这个`i`是在`for`的`scope`中的, 而不在匿名函数中.

返回`闭包`时牢记的一点就是: 返回函数不要引用任何`循环变量`, 或者后续会发生变化的变量.
如果一定要引用循环变量怎么办?方法是再创建一个函数, 用该函数的参数`绑定`循环变量当前的值, 无论该循环变量后续如何更改, 已绑定到函数参数的值不变:

```js
function count() {
    var arr = [];
    for (var i=1; i<=3; i++) {
        arr.push((function (n) {
            return function () {
                return n * n;
            }
        })(i) // 这里将匿名函数作用到 i 上, i 的值被绑定到n上, 而n是 function (n) scope 下的变量.
        );} return arr; }

var results = count();
var f1 = results[0];
var f2 = results[1];
var f3 = results[2];

f1(); // 1
f2(); // 4
f3(); // 9
```

注意这里用了一个"创建一个匿名函数并立刻执行"的语法:

```js
(function (x) {
    return x * x;
})(3); // 9
```

理论上讲, 创建一个匿名函数并立刻执行可以这么写:

```js
function (x) { return x * x } (3);
```

但是由于`JavaScript`语法解析的问题, 会报`SyntaxError`错误, 因此需要用括号`()`把整个函数定义括起来:

```js
(function (x) { return x * x }) (3);
```

通常, 一个立即执行的匿名函数可以把函数体拆开, 一般这么写:

```js
(function (x) {
    return x * x;
})(3);
```

说了这么多, 难道闭包就是为了返回一个函数然后延迟执行吗?当然不是!闭包有非常强大的功能. 举个栗子:

在面向对象的程序设计语言里, 比如`Java`和`C++`, 要在对象内部封装一个私有变量, 可以用`private`修饰一个成员变量.
在没有`class`机制, 只有函数的语言里, 借助闭包, 同样可以封装一个私有变量. 我们用`JavaScript`创建一个计数器:

```js
'use strict';
function create_counter(initial) {
    var x = initial || 0;
    return {
        inc: function () { // 返回一个字典
            x += 1;
            return x;
        }
    }
}
```

它用起来像这样:

```js
var c1 = create_counter();
c1.inc(); // 1
c1.inc(); // 2
c1.inc(); // 3
var c2 = create_counter(10);
c2.inc(); // 11
c2.inc(); // 12
c2.inc(); // 13
```

在返回的对象中, 实现了一个`闭包`, 该`闭包`携带了局部变量`x`, 并且, 从外部代码根本无法访问到变量`x`. 换句话说, 闭包就是携带`状态`的函数, 并且它的状态可以完全对外隐藏起来.

闭包还可以把多参数的函数变成单参数的函数. 例如, 要计算`x^y`可以用`Math.pow(x, y)`函数, 不过考虑到经常计算`x^2`或`x^3`, 我们可以利用`闭包`创建新的函数`pow2`和`pow3`:

```js
'use strict';
function make_pow(n) {
    return function (x) {
        return Math.pow(x, n);
    }
}
// 创建两个新函数:
var pow2 = make_pow(2);
var pow3 = make_pow(3);
console.log(pow2(5)); // 25
console.log(pow3(7)); // 343
```

### 箭头函数

`ES6`标准新增了一种新的函数: `Arrow Function`(箭头函数).

为什么叫`Arrow Function`?因为它的定义用的就是一个箭头:

```js
x => x * x
```

上面的箭头函数相当于:

```js
function (x) {
    return x * x;
}
```

箭头函数相当于`匿名函数`, 并且简化了函数定义.
箭头函数有两种格式, 一种像上面的, 只包含一个表达式, 连`{ ... }`和`return`都省略掉了. 还有一种可以包含多条语句, 这时候就不能省略`{ ... }`和`return`:

```js
x => {
    if (x > 0) {
        return x * x;
    }
    else {
        return - x * x;
    }
}
```

如果参数不是一个, 就需要用括号`()`括起来:

```js
// 两个参数:
(x, y) => x * x + y * y
// 无参数:
() => 3.14
// 可变参数:
(x, y, ...rest) => {
    var i, sum = x + y;
    for (i=0; i<rest.length; i++) {
        sum += rest[i];
    }
    return sum;
}
```

如果要返回一个`对象`, 就要注意, 如果是`单表达式`, 这么写的话会报错:

```js
// SyntaxError:
x => { foo: x }
```

因为和函数体的`{ ... }`有语法冲突, 所以要改为:

```js
// ok:
x => ({ foo: x })
```

#### this

`箭头函数`看上去是`匿名函数`的一种简写, 但实际上, `箭头函数`和`匿名函数`有个明显的区别: `箭头函数`内部的`this`是`词法作用域`(lexical scope), 由`上下文`确定.

回顾前面的例子, 由于`JavaScript`函数对`this`绑定的错误处理, 下面的例子无法得到预期结果:

```js
var obj = {
    birth: 1990,
    getAge: function () {
        var b = this.birth; // 1990
        var fn = function () {
            return new Date().getFullYear() - this.birth; // this指向window或undefined
        };
        return fn();
    }
};
```

现在, 箭头函数完全修复了`this`的指向, `this`总是指向`词法作用域`, 也就是外层调用者`obj`:

```js
var obj = {
    birth: 1990,
    getAge: function () {
        var b = this.birth; // 1990
        var fn = () => new Date().getFullYear() - this.birth; // this指向obj对象
        return fn();
    }
};
obj.getAge(); // 25
```

如果使用`箭头函数`, 以前的那种`hack`写法:

```js
var that = this;
```

就不再需要了.

由于`this`在箭头函数中已经按照`词法作用域`绑定了, 所以, 用`call()`或者`apply()`调用箭头函数时, 无法对`this`进行绑定, 即传入的`第一个`参数被忽略:

```js
var obj = {
    birth: 1990,
    getAge: function (year) {
        var b = this.birth; // 1990
        var fn = (y) => y - this.birth; // this.birth仍是1990
        return fn.call({birth:2000}, year);
    }
};
obj.getAge(2015); // 25
```

### 词法作用域,动态作用域

+ `lexical`相当于`Mathematica`中的`Module[vars,body]`; 把代码体`body`中的变量`var`看作局部变量, 根据`body`的上下文确定局部变量的值.
+ `dynamical` 相当于`Mathematica`中的`Block[vars,body]`; 先记录`var`的值, 在代码`body`的执行过程中, 
将变量`var`局部化, 执行完成后, 再恢复`var`的值.

```mathematica
m = i^2;
Block[{i = a}, i + m] (* m=i^2 被替换, 全局变量 i 被局部化, 所有绑定都受影响 *)
a + a^2
Module[{i = a}, i + m] (* m=i^2 不会被替换,  module 只做文法替换 *)
a + i^2
```

`i+m` 中只有`显式`出现的`i`才被替换成`local`的值. `lisp`除了支持`lexical scope`, 还支持`dynamic scope`.

### generator生成器

`generator`(生成器)是`ES6`标准引入的新的数据类型. `generator`看上去像一个函数, 但可以返回多次.
`ES6`定义`generator`标准的哥们借鉴了`Python`的`generator`的概念和语法, 如果你对`Python`的`generator`很熟悉, 那么ES6的`generator`就是小菜一碟了.

我们先复习函数的概念. 函数是一段完整的代码, 调用函数就是传入参数, 然后返回结果:

```js
function foo(x) {
    return x + x;
}
var r = foo(1); // 调用foo函数
```

函数在执行过程中, 如果没有遇到`return`语句(函数末尾如果没有`return`, 就是隐含的`return undefined;`), 控制权无法交回被调用的代码.
`generator`跟函数很像, 定义如下:

```js
function* foo(x) {
    yield x + 1;
    yield x + 2;
    return x + 3;
}
```

`generator`和函数不同的是, `generator`由`function*`定义(注意多出的*号), 并且除了`return`语句, 还可以用`yield`返回多次.
大多数同学立刻就晕了, `generator`就是能够返回多次的"函数"?返回多次有啥用?
还是举个栗子吧.

我们以一个著名的斐波那契数列为例, 它由`0`, `1`开头:

  0 1 1 2 3 5 8 13 21 34 ...

要编写一个产生斐波那契数列的函数, 可以这么写:

```js
function fib(max) {
    var
        t,
        a = 0,
        b = 1,
        arr = [0, 1];
    while (arr.length < max) {
        [a, b] = [b, a + b];
        arr.push(b);
    }
    return arr;
}
// 测试:
fib(5); // [0, 1, 1, 2, 3]
fib(10); // [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
```

函数只能返回一次, 所以必须返回`Array`. 但是, 如果换成`generator`, 就可以一次返回一个数, 不断返回多次. 用`generator`改写如下:

```js
function* fib(max) {
    var
        t,
        a = 0,
        b = 1,
        n = 0;
    while (n < max) {
        yield a;
        [a, b] = [b, a + b];
        n ++;
    }
    return;
}
```

直接调用试试:

```js
fib(5); // fib {[[GeneratorStatus]]: "suspended", [[GeneratorReceiver]]: Window}
```

直接调用一个`generator`和调用函数不一样, `fib(5)`仅仅是创建了一个`generator`对象, 还没有去执行它.

调用`generator`对象有两个方法, 一是不断地调用`generator`对象的`next()`方法:

```js
var f = fib(5);
f.next(); // {value: 0, done: false}
f.next(); // {value: 1, done: false}
f.next(); // {value: 1, done: false}
f.next(); // {value: 2, done: false}
f.next(); // {value: 3, done: false}
f.next(); // {value: undefined, done: true}
```

`next()`方法会执行`generator`的代码, 然后, 每次遇到`yield x;`就返回一个对象`{value: x, done: true/false}`, 然后"暂停".
返回的`value`就是`yield`的返回值, `done`表示这个`generator`是否已经执行结束了. 如果`done`为`true`, 则`value`就是`return`的返回值.
当执行到`done`为`true`时, 这个`generator`对象就已经全部执行完毕, 不要再继续调用`next()`了.

第二个方法是直接用`for ... of`循环迭代`generator`对象, 这种方式不需要我们自己判断`done`:

```js
'use strict'
function* fib(max) {
    var
        t,
        a = 0,
        b = 1,
        n = 0;
    while (n < max) {
        yield a;
        [a, b] = [b, a + b];
        n ++;
    }
    return;
}
for (var x of fib(10)) {
    console.log(x); // 依次输出0, 1, 1, 2, 3, ...
}
```

`generator`和普通函数相比, 有什么用?
因为`generator`可以在执行过程中多次返回, 所以它看上去就像一个可以记住执行状态的函数, 利用这一点, 写一个`generator`就可以实现需要用面向对象才能实现的功能. 例
如, 用一个对象来保存状态, 得这么写:

```js
var fib = {
    a: 0,
    b: 1,
    n: 0,
    max: 5,
    next: function () {
        var
            r = this.a,
            t = this.a + this.b;
        this.a = this.b;
        this.b = t;
        if (this.n < this.max) {
            this.n ++;
            return r;
        } else {
            return undefined;
        }
    }
};
```

用对象的属性来保存状态, 相当繁琐.
`generator`还有另一个巨大的好处, 就是把异步回调代码变成"同步"代码. 这个好处要等到后面学了`AJAX`以后才能体会到.

没有`generator`之前的黑暗时代, 用`AJAX`时需要这么写代码:

```js
ajax('http://url-1', data1, function (err, result) {
    if (err) {
        return handle(err);
    }
    ajax('http://url-2', data2, function (err, result) {
        if (err) {
            return handle(err);
        }
        ajax('http://url-3', data3, function (err, result) {
            if (err) {
                return handle(err);
            }
            return success(result);
        });
    });
});
```

回调越多, 代码越难看.

有了`generator`的美好时代, 用`AJAX`时可以这么写:

```js
try {
    r1 = yield ajax('http://url-1', data1);
    r2 = yield ajax('http://url-2', data2);
    r3 = yield ajax('http://url-3', data3);
    success(r3);
}
catch (err) {
    handle(err);
}
```

看上去是同步的代码, 实际执行是异步的.

#### 练习

要生成一个自增的`ID`, 可以编写一个`next_id()`函数:

```js
'use strict';
function* next_id() {
var id =1;
while(true){yield id++;};
}
```

## 标准对象

在`JavaScript`的世界里, 一切都是对象.

但是某些对象还是和其他对象不太一样. 为了区分对象的类型, 我们用`typeof`操作符获取对象的类型, 它总是返回一个字符串:

```js
typeof 123; // 'number'
typeof NaN; // 'number'
typeof 'str'; // 'string'
typeof true; // 'boolean'
typeof undefined; // 'undefined'
typeof Math.abs; // 'function'
typeof null; // 'object'
typeof []; // 'object'
typeof {}; // 'object'
```

可见, `number`, `string`, `boolean`, `function` 和 `undefined` 有别于其他类型. 特别注意`null`的类型是`object`, `Array`的类型也是`object`.
如果我们用`typeof`将无法区分出`null`,` Array`和通常意义上的`object`--`{}`.

### 包装对象

除了这些类型外, `JavaScript`还提供了包装对象, 熟悉`Java`的小伙伴肯定很清楚`int`和`Integer`这种暧昧关系.

`number`, `boolean`和`string`都有包装对象. 没错, 在`JavaScript`中, 字符串也区分`string`类型和它的包装类型. 包装对象用`new`创建:

```js
var n = new Number(123); // 123,生成了新的包装类型
var b = new Boolean(true); // true,生成了新的包装类型
var s = new String('str'); // 'str',生成了新的包装类型
```

虽然包装对象看上去和原来的值一模一样, 显示出来也是一模一样, 但他们的类型已经变为`object`了!所以, 包装对象和原始值用`===`比较会返回`false`:

```js
typeof new Number(123); // 'object'
new Number(123) === 123; // false
typeof new Boolean(true); // 'object'
new Boolean(true) === true; // false
typeof new String('str'); // 'object'
new String('str') === 'str'; // false
```

所以闲的蛋疼也不要使用包装对象!尤其是针对`string`类型!!!

如果我们在使用`Number`, `Boolean`和`String`时, 没有写`new`会发生什么情况?
此时, `Number()`, `Boolean`和`String()`被当做普通函数, 把任何类型的数据转换为`number`, `boolean`和`string`类型(注意不是其`包装类型`):

```js
var n = Number('123'); // 123, 相当于parseInt()或parseFloat()
typeof n; // 'number'
var b = Boolean('true'); // true
typeof b; // 'boolean'
var b2 = Boolean('false'); // true! 'false'字符串转换结果为true!因为它是非空字符串!
var b3 = Boolean(''); // false
var s = String(123.45); // '123.45'
typeof s; // 'string'
```

是不是感觉头大了?这就是`JavaScript`特有的催眠魅力!

总结一下, 有这么几条规则需要遵守:

+ 不要使用`new Number()`, `new Boolean()`, `new String()`创建`包装对象`;
+ 用`parseInt()`或`parseFloat()`来转换任意类型到`number`;
+ 用`String()`来转换任意类型到`string`, 或者直接调用某个对象的`toString()`方法;
+ 通常不必把任意类型转换为`boolean`再判断, 因为可以直接写`if (myVar) {...}`;
+ `typeof`操作符可以判断出`number`, `boolean`, `string`, `function` 和 `undefined`;
+ 判断 `Array` 要使用 `Array.isArray(arr)`;
+ 判断 `null` 请使用 `myVar === null`;
+ 判断某个全局变量是否存在用 `typeof window.myVar === 'undefined'`;
+ 函数内部判断某个变量是否存在用 `typeof myVar === 'undefined'`.

最后有细心的同学指出, 任何对象都有`toString()`方法吗?`null`和`undefined`就没有!确实如此, 这两个特殊值要除外, 虽然`null`还伪装成了`object` 类型.
更细心的同学指出, `number`对象调用`toString()`报`SyntaxError`:

```js
123.toString(); // SyntaxError
```

遇到这种情况, 要特殊处理一下:

```js
123..toString(); // '123', 注意是两个点!
(123).toString(); // '123'
```

不要问为什么, 这就是`JavaScript`代码的乐趣!

### Date

`JavaScript`中, `Date`对象用来表示日期和时间. 要获取系统当前时间, 用:

```js
var now = new Date();
now; // Wed Jun 24 2015 19:49:22 GMT+0800 (CST)
now.getFullYear(); // 2015, 年份
now.getMonth(); // 5, 月份, 注意月份范围是0~11, 5表示六月
now.getDate(); // 24, 表示24号
now.getDay(); // 3, 表示星期三
now.getHours(); // 19, 24小时制
now.getMinutes(); // 49, 分钟
now.getSeconds(); // 22, 秒
now.getMilliseconds(); // 875, 毫秒数
now.getTime(); // 1435146562875, 以number形式表示的时间戳
```

注意, `当前时间`是浏览器从本机操作系统获取的时间, 所以不一定准确, 因为用户可以把当前时间设定为任何值.

如果要创建一个指定日期和时间的`Date`对象, 可以用:

```js
var d = new Date(2015, 5, 19, 20, 15, 30, 123);
d; // Fri Jun 19 2015 20:15:30 GMT+0800 (CST)
```

你可能观察到了一个非常非常坑爹的地方,
就是`JavaScript`的月份范围用整数表示是`0~11`, `0`表示`一月`, `1`表示`二月`……, 所以要表示`6`月, 我们传入的是`5`!
这绝对是`JavaScript`的设计者当时脑抽了一下, 但是现在要修复已经不可能了.

>`JavaScript`的`Date`对象月份值从`0`开始, 牢记`0=1月`, `1=2月`, `2=3月`, ……, `11=12月`.

第二种创建指定日期和时间的方法是解析一个符合`ISO 8601`格式的字符串:

```js
var d = Date.parse('2015-06-24T19:49:22.875+08:00');
d; // 1435146562875
```

但它返回的不是`Date`对象, 而是一个`时间戳`. 不过有`时间戳`就可以很容易地把它转换为一个`Date`:

```bash
var d = new Date(1435146562875);
d; // Wed Jun 24 2015 19:49:22 GMT+0800 (CST)
d.getMonth(); // 5
```

>使用`Date.parse()`时传入的字符串使用实际月份`01~12`, 转换为`Date`对象后`getMonth()`获取的月份值为`0~11`.

#### 时区

`Date`对象表示的时间总是按浏览器所在时区显示的, 不过我们既可以显示本地时间, 也可以显示调整后的`UTC`时间:

```js
var d = new Date(1435146562875);
d.toLocaleString(); // '2015/6/24 下午7:49:22', 本地时间(北京时区+8:00), 显示的字符串与操作系统设定的格式有关
d.toUTCString(); // 'Wed, 24 Jun 2015 11:49:22 GMT', UTC时间, 与本地时间相差8小时
```

那么在`JavaScript`中如何进行时区转换呢?实际上, 只要我们传递的是`number`类型的时间戳, 我们就不用关心`时区转换`. 任何浏览器都可以把`时间戳`正确转换为`本地时间`.

`时间戳`是什么?`时间戳`是一个自增的整数, 它表示从`1970年1月1日零时`整的`GMT`时区开始的那一刻, 到现在的毫秒数.
假设浏览器所在电脑的时间是准确的, 那么世界上无论哪个时区的电脑, 它们此刻产生的`时间戳`数字都是一样的, 所以, `时间戳`可以精确地表示一个时刻, 并且与时区无关.

所以, 我们只需要传递`时间戳`, 或者把`时间戳`从数据库里读出来, 再让`JavaScript`自动转换为当地时间就可以了.
要获取当前`时间戳`, 可以用:

```js
'use strict';
if (Date.now) {
    console.log(Date.now()); // 老版本IE没有now()方法
} else {
    console.log(new Date().getTime());
}
```

#### 练习

小明为了和女友庆祝情人节, 特意制作了网页, 并提前预定了法式餐厅. 小明打算用`JavaScript`给女友一个惊喜留言:

```js
'use strict';
var today = new Date();
if (today.getMonth() === 2 && today.getDate() === 14) {
    alert('亲爱的, 我预定了晚餐, 晚上6点在餐厅见!');
}
```

结果女友并未出现. 小明非常郁闷, 请你帮忙分析他的`JavaScript`代码有何问题.

### RegExp

`字符串`是编程时涉及到的最多的一种数据结构,对字符串进行操作的需求几乎无处不在.
比如判断一个字符串是否是合法的`Email`地址,虽然可以编程提取`@`前后的子串,再分别判断是否是`单词`和`域名`,但这样做不但麻烦,而且代码难以复用.

`正则表达式`是一种用来匹配字符串的强有力的武器.它的设计思想是用一种描述性的语言来给字符串定义一个规则,凡是符合规则的字符串,我们就认为它`匹配`了,否则,该字符串就是不合法的.
所以我们判断一个字符串是否是合法的`Email`的方法是:

+ 创建一个匹配`Email`的正则表达式;
+ 用该正则表达式去匹配用户的输入来判断是否合法.

因为`正则表达式`也是用字符串表示的,所以,我们要首先了解如何用字符来描述字符.
在`正则表达式`中,如果直接给出字符,就是精确匹配.用`\d`可以匹配一个数字,`\w`可以匹配一个字母或数字,所以:

+ `'00\d'`可以匹配`'007'`,但无法匹配`'00A'`;
+ `'\d\d\d'`可以匹配`'010'`;
+ `'\w\w'`可以匹配`'js'`;
+ `.`可以匹配任意字符,所以 `'js.'`可以匹配`'jsp'`, `'jss'`, `'js!'` 等等.

匹配变长的字符: 在正则表达式中,

+ 用`*`表示任意个字符(包括`0`个),
+ 用`+`表示至少一个字符,
+ 用`?`表示`0`个或`1`个字符,
+ 用`{n}`表示`n`个字符,
+ 用`{n,m}`表示`n-m`个字符:

来看一个复杂的例子: `\d{3}\s+\d{3,8}`. 我们来从左到右解读一下:

+ `\d{3}`表示匹配`3`个数字,例如`'010'`;
+ `\s`可以匹配一个`空格`(也包括`Tab`等空白符),所以`\s+`表示至少有一个空格,例如匹配`' '`, `'\t\t'`等;
+ `\d{3,8}`表示`3-8`个数字,例如`'1234567'`.

综合起来,上面的正则表达式可以匹配以任意个空格隔开的带区号的电话号码.

如果要匹配`'010-12345'`这样的号码呢?由于`'-'`是特殊字符,在正则表达式中,要用`'\'`转义,所以,上面的正则是`\d{3}\-\d{3,8}`.
但是,仍然无法匹配`'010 - 12345'`,因为带有`空格`.所以我们需要更复杂的匹配方式.

#### 进阶

要做更精确地匹配,可以用`[]`表示范围,比如:

+ `[0-9a-zA-Z\_]` ; 可以匹配一个数字,字母或者下划线;
+ `[0-9a-zA-Z\_]+` ; 可以匹配至少由一个数字,字母或者下划线组成的字符串,比如`'a100','0_Z'`, `'js2015'` 等等;
+ `[a-zA-Z\_\$][0-9a-zA-Z\_\$]*` ; 可以匹配由字母或下划线,`$`开头,后接任意个由一个数字,字母或者下划线, `$`组成的字符串,也就是`JavaScript`允许的变量名;
+ `[a-zA-Z\_\$][0-9a-zA-Z\_\$]{0, 19}` ; 更精确地限制了变量的长度是`1-20个`字符(前面`1`个字符+后面最多`19`个字符).

+ `A|B`可以匹配`A`或`B`,所以`(J|j)ava(S|s)cript`可以匹配 `'JavaScript'`, `'Javascript'`, `'javaScript'`或者`'javascript'`.
+ `^`表示行的开头, `^\d`表示必须以数字开头.
+ `$`表示行的结束,`\d$`表示必须以数字结束.

你可能注意到了, `js`也可以匹配`'jsp'`, 但是加上`^js$`就变成了整行匹配,就只能匹配`'js'`了.

#### RegExp

有了准备知识,我们就可以在`JavaScript`中使用正则表达式了.
`JavaScript`有两种方式创建一个正则表达式:

第一种方式是直接通过`/正则表达式/`写出来,第二种方式是通过`new RegExp('正则表达式')`创建一个`RegExp`对象.
两种写法是一样的:

```js
var re1 = /ABC\-001/;
var re2 = new RegExp('ABC\\-001');
re1; // /ABC\-001/
re2; // /ABC\-001/
```

注意,如果使用第二种写法,因为字符串的转义问题,字符串的两个`\\`实际上是一个`\`.
先看看如何判断正则表达式是否匹配:

```js
var re = /^\d{3}\-\d{3,8}$/;
re.test('010-12345'); // true
re.test('010-1234x'); // false
re.test('010 12345'); // false
```

`RegExp`对象的`test()`方法用于测试给定的字符串是否符合条件.

#### 切分字符串

用正则表达式切分字符串比用固定的字符更灵活,请看正常的切分代码:

```js
'a b   c'.split(' '); // ['a', 'b', '', '', 'c']
```

嗯,无法识别连续的空格,用`正则表达式`试试:

```js
'a b   c'.split(/\s+/); // ['a', 'b', 'c']
```

无论多少个空格都可以正常分割. 加入`,`试试:

```js
'a,b, c  d'.split(/[\s\,]+/); // ['a', 'b', 'c', 'd']
```

再加入`;`试试:

```js
'a,b;; c  d'.split(/[\s\,\;]+/); // ['a', 'b', 'c', 'd']
```

如果用户输入了一组`标签`,下次记得用`正则表达式`来把不规范的输入转化成正确的数组.

#### 分组

除了简单地判断是否匹配之外,正则表达式还有提取子串的强大功能. 用`()`表示的就是要提取的分组(`Group`).比如:

`^(\d{3})-(\d{3,8})$` 分别定义了两个组,可以直接从匹配的字符串中提取出区号和本地号码:

```js
var re = /^(\d{3})-(\d{3,8})$/;
re.exec('010-12345'); // ['010-12345', '010', '12345']
re.exec('010 12345'); // null
```

如果正则表达式中定义了组,就可以在`RegExp`对象上用`exec()`方法提取出子串来.
`exec()`方法在匹配成功后,会返回一个`Array`, 第一个元素是正则表达式匹配到的整个字符串,后面的字符串表示匹配成功的子串.
`exec()`方法在匹配失败时返回`null`.

提取子串非常有用.来看一个更凶残的例子:

```js
var re = /^(0[0-9]|1[0-9]|2[0-3]|[0-9])\:(0[0-9]|1[0-9]|2[0-9]|3[0-9]|4[0-9]|5[0-9]|[0-9])\:(0[0-9]|1[0-9]|2[0-9]|3[0-9]|4[0-9]|5[0-9]|[0-9])$/;
re.exec('19:05:30'); // ['19:05:30', '19', '05', '30']
```

这个`正则表达式`可以直接识别合法的时间. 但是有些时候,用`正则表达式`也无法做到完全验证,比如`识别日期`:

```js
var re = /^(0[1-9]|1[0-2]|[0-9])-(0[1-9]|1[0-9]|2[0-9]|3[0-1]|[0-9])$/;
```

对于`'2-30'`, `'4-31'` 这样的非法日期,用正则还是识别不了,或者说写出来非常困难,这时就需要程序配合识别了.

#### 贪婪匹配

需要特别指出的是,正则匹配默认是`贪婪匹配`, 也就是匹配尽可能多的字符. 举例如下,匹配出数字后面的`0`:

```js
var re = /^(\d+)(0*)$/;
re.exec('102300'); // ['102300', '102300', '']
```

由于`\d+`采用贪婪匹配,直接把后面的`0`全部匹配了,结果`0*`只能匹配`空字符串`了.
必须让`\d+`采用非贪婪匹配(也就是尽可能少匹配), 才能把后面的`0`匹配出来,加个`?`就可以让`\d+`采用非贪婪匹配:

```js
var re = /^(\d+?)(0*)$/;
re.exec('102300'); // ['102300', '1023', '00']
```

#### 全局搜索

`JavaScript`的正则表达式还有几个特殊的`标志`(flag), 最常用的是`g`,表示`全局匹配`:

```js
var r1 = /test/g;
// 等价于:
var r2 = new RegExp('test', 'g');
```

`全局匹配`可以多次执行`exec()`方法来搜索一个匹配的字符串. 当我们指定`g`标志后,每次运行`exec()`,正则表达式本身会更新`lastIndex`属性,表示上次匹配到的`最后索引`:

```js
var s = 'JavaScript, VBScript, JScript and ECMAScript';
var re=/[a-zA-Z]+Script/g;
// 使用全局匹配:
re.exec(s); // ['JavaScript']
re.lastIndex; // 10
re.exec(s); // ['VBScript']
re.lastIndex; // 20
re.exec(s); // ['JScript']
re.lastIndex; // 29
re.exec(s); // ['ECMAScript']
re.lastIndex; // 44
re.exec(s); // null,直到结束仍没有匹配到
```

全局匹配类似搜索,因此不能使用`/^...$/`, 那样只会最多匹配一次.

正则表达式还可以指定`i`标志, 表示忽略大小写, `m`标志, 表示执行`多行匹配`.

#### 小结

正则表达式非常强大,要在短短的一节里讲完是不可能的.要讲清楚正则的所有内容,可以写一本厚厚的书了.如果你经常遇到正则表达式的问题,你可能需要一本正则表达式的参考书.

#### 练习

请尝试写一个验证`Email`地址的正则表达式.版本一应该可以验证出类似的`Email`:

```js
'use strict';
var re = /^$/;
// 测试:
var
    i,
    success = true,
    should_pass = ['someone@gmail.com', 'bill.gates@microsoft.com', 'tom@voyager.org', 'bob2015@163.com'],
    should_fail = ['test#gmail.com', 'bill@microsoft', 'bill%gates@ms.com', '@voyager.org'];
for (i = 0; i < should_pass.length; i++) {
    if (!re.test(should_pass[i])) {
        console.log('测试失败: ' + should_pass[i]);
        success = false;
        break;
    }
}
for (i = 0; i < should_fail.length; i++) {
    if (re.test(should_fail[i])) {
        console.log('测试失败: ' + should_fail[i]);
        success = false;
        break;
    }
}
if (success) {
    console.log('测试通过!');
}
```

版本二可以验证并提取出带名字的`Email`地址:

```js
'use strict';
var re = /^$/;

// 测试:
var r = re.exec('<Tom Paris> tom@voyager.org');
if (r === null || r.toString() !== ['<Tom Paris> tom@voyager.org', 'Tom Paris', 'tom@voyager.org'].toString()) {
    console.log('测试失败!');
}
else {
    console.log('测试成功!');
}
```

### Json

`JSON`是`JavaScript Object Notation`的缩写,它是一种数据交换格式.

在`JSON`出现之前,大家一直用`XML`来传递数据.因为`XML`是一种纯文本格式,所以它适合在网络上交换数据.
`XML`本身不算复杂, 但是加上`DTD`,`XSD`,`XPath`,`XSLT` 等一大堆复杂的规范以后,任何正常的软件开发人员碰到`XML`都会感觉头大了.
最后大家发现,即使你努力钻研几个月,也未必搞得清楚`XML`的规范.

终于, 在 `2002` 年的一天,道格拉斯·克罗克福特(Douglas Crockford)同学为了拯救深陷水深火热,
同时又被某几个巨型软件企业长期愚弄的软件工程师,发明了`JSON`这种超轻量级的数据交换格式.
道格拉斯同学长期担任雅虎的高级架构师,自然钟情于 `JavaScript`.他设计的`JSON`实际上是`JavaScript`的一个子集.
在`JSON`中,一共就这么几种数据类型:

+ `number`: 和`JavaScript`的`number`完全一致;
+ `boolean`: 就是`JavaScript`的`true`或`false`;
+ `string`: 就是`JavaScript`的`string`;
+ `null`: 就是`JavaScript`的 `null`;
+ `array`: 就是`JavaScript`的`Array`表示方式-- `[]`;
+ `object`: 就是`JavaScript`的`{ ... }`表示方式.

以及上面的任意组合.

并且,`JSON`还定死了字符集必须是`UTF-8`,表示多语言就没有问题了.为了统一解析,`JSON`的字符串规定必须用双引号`""`, `Object` 的键也必须用双引号`""`.
由于`JSON`非常简单,很快就风靡`Web`世界,并且成为ECMA标准. 几乎所有编程语言都有解析`JSON`的库,而在`JavaScript`中,我们可以直接使用`JSON`,因为`JavaScript`内置了`JSON`的解析.

把任何`JavaScript`对象变成`JSON`,就是把这个对象序列化成一个`JSON`格式的字符串,这样才能够通过网络传递给其他计算机.
如果我们收到一个`JSON`格式的字符串,只需要把它反序列化成一个`JavaScript`对象,就可以在`JavaScript`中直接使用这个对象了.
