# javascript

[JavaScript 教程](https://www.w3school.com.cn/js/index.asp)
[JavaScript教程-liaoxuefeng](https://www.liaoxuefeng.com/wiki/1022910821149312)

## intro

`JavaScript` 是属于 HTML 和 Web 的编程语言. 

为何学习 `JavaScript`？

`JavaScript` 是 web 开发者必学的三种语言之一: 

+ HTML 定义网页的内容
+ CSS 规定网页的布局
+ `JavaScript` 对网页行为进行编程

本教程提供关于 `JavaScript`, 以及 `JavaScript` 如何与 HTML 和 CSS 协同工作的知识. 

+ `JavaScript` 和 Java 是完全不同的语言, 不论是概念还是设计. 
+ `JavaScript` 在 1995 年由 Brendan Eich 发明, 并于 1997 年成为一部 ECMA 标准. 
+ ECMA-262 是其官方名称. ECMAScript 6 (发布于 2015 年)是最新的 `JavaScript` 版本. 

`JavaScript`是世界上最流行的脚本语言, 因为你在电脑, 手机, 平板上浏览的所有的网页, 以及无数基于HTML5的手机App, 交互逻辑都是由`JavaScript`驱动的. 

简单地说, `JavaScript`是一种运行在浏览器中的解释型的编程语言. 

那么问题来了, 为什么我们要学`JavaScript`？尤其是当你已经掌握了某些其他编程语言如Java, C++的情况下. 

简单粗暴的回答就是: 因为你没有选择. 在Web世界里, 只有`JavaScript`能跨平台, 跨浏览器驱动网页, 与用户交互. 

### JavaScript简介

#### JavaScript历史

要了解`JavaScript`, 我们首先要回顾一下`JavaScript`的诞生. 

在上个世纪的1995年, 当时的网景公司正凭借其Navigator浏览器成为Web时代开启时最著名的第一代互联网公司. 
由于网景公司希望能在静态`HTML`页面上添加一些动态效果, 于是叫Brendan Eich这哥们在两周之内设计出了`JavaScript`语言. 
你没看错, 这哥们只用了10天时间. 

为什么起名叫`JavaScript`？原因是当时Java语言非常红火, 所以网景公司希望借Java的名气来推广, 但事实上`JavaScript`除了语法上有点像Java, 其他部分基本上没啥关系. 

#### ECMAScript

因为网景开发了`JavaScript`, 一年后微软又模仿`JavaScript`开发了JScript, 为了让`JavaScript`成为全球标准, 几个公司联合ECMA(European Computer Manufacturers Association)组织定制了`JavaScript`语言的标准, 被称为ECMAScript标准. 

所以简单说来就是, ECMAScript是一种语言标准, 而`JavaScript`是网景公司对ECMAScript标准的一种实现. 
那为什么不直接把`JavaScript`定为标准呢？因为`JavaScript`是网景的注册商标. 
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

你也许会想, 直接在我的硬盘上创建好`HTML`和`JavaScript`文件, 然后用浏览器打开, 不就可以看到效果了吗？

这种方式运行部分`JavaScript`代码没有问题. 
但由于浏览器的安全限制, 以`` file:// ``开头的地址无法执行如联网等`JavaScript`代码.
最终, 你还是需要架设一个`Web`服务器, 然后以`` http:// ``开头的地址来正常执行所有`JavaScript`代码. 

俗话说得好, "工欲善其事, 必先利其器", 
写 `JavaScript` 的时候, 如果期望显示`ABC`, 结果却显示`XYZ`, 到底代码哪里出了问题？
不要抓狂, 也不要泄气, 作为小白, 要坚信: `JavaScript`本身没有问题, 浏览器执行也没有问题, 有问题的一定是我的代码. 

如何找出问题代码？这就需要调试. 

怎么在浏览器中调试`JavaScript`代码呢？

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

## 函数

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
如果一定要引用循环变量怎么办？方法是再创建一个函数, 用该函数的参数`绑定`循环变量当前的值, 无论该循环变量后续如何更改, 已绑定到函数参数的值不变: 

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

说了这么多, 难道闭包就是为了返回一个函数然后延迟执行吗？当然不是!闭包有非常强大的功能. 举个栗子: 

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

为什么叫`Arrow Function`？因为它的定义用的就是一个箭头: 

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

`lexical`相当于`Mathematica`中的`Module[vars,body]`; 把代码体`body`中的变量`var`看作局部变量, 根据`body`的上下文确定局部变量的值.
`dynamical` 相当于`Mathematica`中的`Block[vars,body]`; 先记录`var`的值, 在代码`body`的执行过程中, 将变量`var`局部化, 执行完成后, 再恢复`var`的值.

```mathematica
m = i^2;
Block[{i = a}, i + m] (* m 被替换 *)
a + a^2
Module[{i = a}, i + m] (* m 不会被替换*)
a + i^2
```

`i+m` 中只有显式出现的`i`才被替换成`local`的值. `lisp`除了支持`lexical scope`, 还支持`dynamic scope`.

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
大多数同学立刻就晕了, `generator`就是能够返回多次的"函数"？返回多次有啥用？
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

`generator`和普通函数相比, 有什么用？
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

## node.js

有个叫`Ryan Dahl`的歪果仁, 他的工作是用`C/C++`写高性能`Web`服务. 对于高性能, `异步IO`, `事件驱动`是基本原则, 
但是用`C/C++`写就太痛苦了. 于是这位仁兄开始设想用高级语言开发`Web`服务. 
他评估了很多种高级语言, 发现很多语言虽然同时提供了`同步IO`和`异步IO`, 但是开发人员一旦用了`同步IO`, 
他们就再也懒得写`异步IO`了, 所以, 最终, Ryan瞄向了`JavaScript`. 

因为`JavaScript`是单线程执行, 根本不能进行`同步IO`操作, 所以, `JavaScript`的这一"缺陷"导致了它只能使用`异步IO`. 

选定了开发语言, 还要有`运行时引擎`. 这位仁兄曾考虑过自己写一个, 不过明智地放弃了, 因为`V8`就是开源的`JavaScript`引擎. 
让`Google`投资去优化V8, 咱只负责改造一下拿来用, 还不用付钱, 这个买卖很划算. 

于是在2009年, Ryan正式推出了基于`JavaScript`语言和V8引擎的开源Web服务器项目, 命名为`Node.js`. 虽然名字很土, 但是, Node第一次把`JavaScript`带入到后端服务器开发, 加上世界上已经有无数的`JavaScript`开发人员, 所以Node一下子就火了起来. 

在`Node`上运行的`JavaScript`相比其他后端开发语言有何优势？
最大的优势是借助`JavaScript`天生的事件驱动机制加V8高性能引擎, 使编写高性能Web服务轻而易举. 
其次, `JavaScript`语言本身是完善的函数式语言, 
在前端开发时, 开发人员往往写得比较随意, 让人感觉`JavaScript`就是个`玩具语言`. 

但是, 在`Node`环境下, 通过模块化的`JavaScript`代码, 加上函数式编程, 并且无需考虑浏览器兼容性问题, 直接使用最新的`ECMAScript 6`标准, 可以完全满足工程上的需求. 

>我还听说过`io.js`, 这又是什么鬼？

因为`Node.js`是开源项目, 虽然由社区推动, 但幕后一直由`Joyent`公司资助. 由于一群开发者对`Joyent`公司的策略不满, 于2014年从`Node.js`项目fork出了`io.js`项目, 决定单独发展, 但两者实际上是兼容的. 
分家后没多久, `Joyent`公司表示要和解, 于是, `io.js`项目又决定回归`Node.js`. 

具体做法是将来`io.js`将首先添加新的特性, 如果大家测试用得爽, 就把新特性加入`Node.js`. 
`io.js`是"尝鲜版", 而`Node.js`是线上稳定版, 相当于Fedora Linux和RHEL的关系. 

### 安装Node.js和npm

由于`Node.js`平台是在后端运行`JavaScript`代码, 所以, 必须首先在本机安装`Node`环境. 

`ubuntu` 使用`sudo apt install nodejs`安装. `windows`到官网下载安装包.
安装完成后, 输入`node -v`, 如果安装正常, 你应该看到类似`v10.19.0`这样的输出.

#### npm

`npm`是什么东东？`npm` 其实是`Node.js`的包管理工具(`node package manager`). 

为啥我们需要一个包管理工具呢？因为我们在`Node.js`上开发时, 会用到很多别人写的`JavaScript`模块. 
大家都把自己开发的模块打包后放到`npm`官网上, 如果要使用, 直接通过`npm`安装就可以直接用, 不用管代码存在哪, 应该从哪下载. 
更重要的是, `npm`自动处理模块之间的依赖关系. 

同样,  ubuntu 使用 `sudo apt install npm` 安装, 在终端输入`npm -v`, 应该看到类似`6.14.4`的输出.

### 第一个Node程序

在前面的所有章节中, 我们编写的`JavaScript`代码都是在浏览器中运行的, 因此, 我们可以直接在浏览器中敲代码, 然后直接运行. 

从本章开始, 我们编写的`JavaScript`代码将不能在浏览器环境中执行了, 而是在`Node`环境中执行.
因此, `JavaScript`代码将直接在你的计算机上以命令行的方式运行, 
所以, 我们要先选择一个文本编辑器来编写`JavaScript` 代码, 并且把它保存到本地硬盘的某个目录, 才能够执行. 

请注意, **绝对不能用Word和写字板**. Word和写字板保存的不是纯文本文件. 
记事本以UTF-8格式保存文件时, 会自作聪明地在文件开始的地方加上几个特殊字符(`UTF-8 BOM`), 
结果经常会导致程序运行出现莫名其妙的错误. 

可以使用`VSCode`也可以用来编写`JavaScript`代码, 注意用`UTF-8`格式保存. 输入以下代码: 

```js
'use strict';
console.log('Hello, world.');
```

第一行总是写上`'use strict'`;是因为我们总是以严格模式运行`JavaScript`代码, 避免各种潜在陷阱. 
在命令行中输入以下命令来运行这个程序: 

```js
C:\Workspace>node hello.js
Hello, world.
```

也可以保存为别的名字, 但是必须以`.js`结尾. 此外, 文件名只能是英文字母, 数字和下划线的组合. 

在命令行模式运行`.js`文件和在`Node`交互式环境下直接运行`JavaScript`代码有所不同. 
`Node`交互式环境会把每一行`JavaScript`代码的结果自动打印出来, 但是, 直接运行`JavaScript`文件却不会. 
想要输出结果, 必须自己用`console.log()`打印出来. 例如:

```js
console.log(100 + 200 + 300);
```

可以给`node`程序传递参数, 让`Node`直接为所有`js`文件开启严格模式: 

```bash
node --use_strict calc.js
```

后续代码, 如无特殊说明, 我们都会直接给`Node`传递`--use_strict`参数来开启严格模式. 
