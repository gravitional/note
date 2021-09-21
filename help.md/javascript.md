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

如果内部函数和外部函数的变量名重名怎么办？来测试一下: 

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

什么是`解构赋值`？我们先看看传统的做法, 如何把一个数组的元素分别赋值给几个变量: 

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

如果我们在使用`Number`, `Boolean`和`String`时, 没有写`new`会发生什么情况？
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

是不是感觉头大了？这就是`JavaScript`特有的催眠魅力!

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

最后有细心的同学指出, 任何对象都有`toString()`方法吗？`null`和`undefined`就没有!确实如此, 这两个特殊值要除外, 虽然`null`还伪装成了`object` 类型. 
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

aaa

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
