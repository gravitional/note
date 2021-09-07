# javascript

[JavaScript 教程](https://www.w3school.com.cn/js/index.asp)
[JavaScript教程-liaoxuefeng](https://www.liaoxuefeng.com/wiki/1022910821149312)

## intro

JavaScript 是属于 HTML 和 Web 的编程语言. 

为何学习 JavaScript？

JavaScript 是 web 开发者必学的三种语言之一：

+ HTML 定义网页的内容
+ CSS 规定网页的布局
+ JavaScript 对网页行为进行编程

本教程提供关于 JavaScript, 以及 JavaScript 如何与 HTML 和 CSS 协同工作的知识. 

+ JavaScript 和 Java 是完全不同的语言, 不论是概念还是设计. 
+ JavaScript 在 1995 年由 Brendan Eich 发明, 并于 1997 年成为一部 ECMA 标准. 
+ ECMA-262 是其官方名称. ECMAScript 6 (发布于 2015 年)是最新的 JavaScript 版本. 

JavaScript是世界上最流行的脚本语言, 因为你在电脑, 手机, 平板上浏览的所有的网页, 以及无数基于HTML5的手机App, 交互逻辑都是由JavaScript驱动的. 

简单地说, JavaScript是一种运行在浏览器中的解释型的编程语言. 

那么问题来了, 为什么我们要学JavaScript？尤其是当你已经掌握了某些其他编程语言如Java, C++的情况下. 

简单粗暴的回答就是：因为你没有选择. 在Web世界里, 只有JavaScript能跨平台, 跨浏览器驱动网页, 与用户交互. 

### JavaScript简介

#### JavaScript历史

要了解JavaScript, 我们首先要回顾一下JavaScript的诞生. 

在上个世纪的1995年, 当时的网景公司正凭借其Navigator浏览器成为Web时代开启时最著名的第一代互联网公司. 

由于网景公司希望能在静态HTML页面上添加一些动态效果, 于是叫Brendan Eich这哥们在两周之内设计出了JavaScript语言. 你没看错, 这哥们只用了10天时间. 

为什么起名叫JavaScript？原因是当时Java语言非常红火, 所以网景公司希望借Java的名气来推广, 但事实上JavaScript除了语法上有点像Java, 其他部分基本上没啥关系. 

#### ECMAScript

因为网景开发了JavaScript, 一年后微软又模仿JavaScript开发了JScript, 为了让JavaScript成为全球标准, 几个公司联合ECMA(European Computer Manufacturers Association)组织定制了JavaScript语言的标准, 被称为ECMAScript标准. 

所以简单说来就是, ECMAScript是一种语言标准, 而JavaScript是网景公司对ECMAScript标准的一种实现. 

那为什么不直接把JavaScript定为标准呢？因为JavaScript是网景的注册商标. 

不过大多数时候, 我们还是用JavaScript这个词. 如果你遇到ECMAScript这个词, 简单把它替换为JavaScript就行了. 

#### JavaScript版本

JavaScript语言是在10天时间内设计出来的, 虽然语言的设计者水平非常NB, 但谁也架不住“时间紧, 任务重”, 所以, JavaScript有很多设计缺陷, 我们后面会慢慢讲到. 

此外, 由于JavaScript的标准——ECMAScript在不断发展, 最新版ECMAScript 6标准(简称ES6)已经在2015年6月正式发布了, 所以, 讲到JavaScript的版本, 实际上就是说它实现了ECMAScript标准的哪个版本. 

由于浏览器在发布时就确定了JavaScript的版本, 加上很多用户还在使用IE6这种古老的浏览器, 这就导致你在写JavaScript的时候, 要照顾一下老用户, 不能一上来就用最新的ES6标准写, 否则, 老用户的浏览器是无法运行新版本的JavaScript代码的. 

不过, JavaScript的核心语法并没有多大变化. 我们的教程会先讲JavaScript最核心的用法, 然后, 针对ES6讲解新增特性. 

### 快速入门

JavaScript代码可以直接嵌在网页的任何地方, 不过通常我们都把JavaScript代码放到

```javascript
<head>中：

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
然后在HTML中通过`<script src="..."></script>`引入这个文件：

```javascript
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

有些时候你会看到`<script>`标签还设置了一个`type`属性：

```javascript
<script type="text/javascript">
  ...
</script>
```

但这是没有必要的, 因为默认的type就是JavaScript, 所以不必显式地把type指定为JavaScript. 

#### 如何编写JavaScript

可以用任何文本编辑器来编写JavaScript代码. 这里我们推荐以下几种文本编辑器：

##### Visual Studio Code

微软出的Visual Studio Code, 可以看做迷你版Visual Studio, 免费！跨平台！内置JavaScript支持, 强烈推荐使用！

##### Sublime Text

Sublime Text是一个好用的文本编辑器, 免费, 但不注册会不定时弹出提示框. 

#### 如何运行JavaScript

要让浏览器运行JavaScript, 必须先有一个HTML页面, 在HTML页面中引入JavaScript, 然后, 让浏览器加载该HTML页面, 就可以执行JavaScript代码. 

你也许会想, 直接在我的硬盘上创建好HTML和JavaScript文件, 然后用浏览器打开, 不就可以看到效果了吗？

这种方式运行部分JavaScript代码没有问题, 但由于浏览器的安全限制, 以`file://`开头的地址无法执行如联网等JavaScript代码, 最终, 你还是需要架设一个Web服务器, 然后以`http://`开头的地址来正常执行所有JavaScript代码. 

我们提供在页面输入JavaScript代码并直接运行的功能, 让你专注于JavaScript的学习. 

试试直接点击“Run”按钮执行下面的JavaScript代码：

```javascript
// 以双斜杠开头直到行末的是注释, 注释是给人看的, 会被浏览器忽略
/* 在这中间的也是注释, 将被浏览器忽略 */
// 第一个JavaScript代码:
alert('Hello, world'); // 观察执行效果
```

浏览器将弹出一个对话框, 显示“Hello, world”. 
你也可以修改两个单引号中间的内容, 再试着运行. 
调试

俗话说得好, “工欲善其事, 必先利其器”, 
写JavaScript的时候, 如果期望显示`ABC`, 结果却显示`XYZ`, 到底代码哪里出了问题？
不要抓狂, 也不要泄气, 作为小白, 
要坚信：JavaScript本身没有问题, 浏览器执行也没有问题, 有问题的一定是我的代码. 

如何找出问题代码？这就需要调试. 

怎么在浏览器中调试JavaScript代码呢？

首先, 你需要安装Google Chrome浏览器, Chrome浏览器对开发者非常友好, 
可以让你方便地调试JavaScript代码. 从这里下载Chrome浏览器. 
打开网页出问题的童鞋请移步国内镜像. 

安装后, 随便打开一个网页, 
然后点击菜单“查看(View)”-“开发者(Developer)”-“开发者工具(Developer Tools)”, 
浏览器窗口就会一分为二, 下方就是开发者工具：

先点击“控制台(Console)“, 在这个面板里可以直接输入JavaScript代码, 按回车后执行. 

要查看一个变量的内容, 在Console中输入`console.log(a)`;, 
回车后显示的值就是变量的内容. 

关闭Console请点击右上角的“x”按钮. 请熟练掌握Console的使用方法, 
在编写JavaScript代码时, 经常需要在Console运行测试代码. 

如果你对自己还有更高的要求, 可以研究开发者工具的“源码(Sources)”, 掌握断点, 单步执行等高级调试技巧. 

#### 练习

打开新浪首页, 然后查看页面源代码, 
找一找引入的JavaScript文件和直接编写在页面中的JavaScript代码. 
然后在Chrome中打开开发者工具, 在控制台输入console.log('Hello');, 
回车查看JavaScript代码执行结果. 

### 基本语法

#### 语法

JavaScript的语法和Java语言类似, 每个语句以`;`结束, 语句块用`{...}`. 
但是, JavaScript并不强制要求在每个语句的结尾加`;`, 
浏览器中负责执行JavaScript代码的引擎会自动在每个语句的结尾补上`;`. 

让JavaScript引擎自动加分号在某些情况下会改变程序的语义, 导致运行结果与期望不一致. 
在本教程中, 我们不会省略;, 所有语句都会添加`;`

例如, 下面的一行代码就是一个完整的赋值语句：

```javascript
var x = 1;
```

下面的一行代码是一个字符串, 但仍然可以视为一个完整的语句：

```javascript
'Hello, world';
```

下面的一行代码包含两个语句, 每个语句用`;`表示语句结束：

```javascript
var x = 1; var y = 2; // 不建议一行写多个语句!
```

语句块是一组语句的集合, 例如, 下面的代码先做了一个判断, 如果判断成立, 将执行`{...}`中的所有语句：

```javascript
if (2 > 1) {
    x = 1;
    y = 2;
    z = 3;
}
```

注意花括号`{...}`内的语句具有缩进, 通常是4个空格. 
缩进不是JavaScript语法要求必须的, 但缩进有助于我们理解代码的层次, 所以编写代码时要遵守缩进规则. 
很多文本编辑器具有“自动缩进”的功能, 可以帮助整理代码. 

`{...}`还可以嵌套, 形成层级结构：

```javascript
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

JavaScript本身对嵌套的层级没有限制, 但是过多的嵌套无疑会大大增加看懂代码的难度. 
遇到这种情况, 需要把部分代码抽出来, 作为函数来调用, 这样可以减少代码的复杂度. 

#### 注释

以//开头直到行末的字符被视为行注释, 注释是给开发人员看的, 
JavaScript引擎会自动忽略：

```javascript
// 这是一行注释
alert('hello'); // 这也是注释
```

另一种块注释是用`/*...*/`把多行字符包裹起来, 把一大“块”视为一个注释：

```javascript
/* 从这里开始是块注释
仍然是注释
仍然是注释
注释结束 */
```

练习：

分别利用行注释和块注释把下面的语句注释掉, 使它不再执行：

```javascript
alert('我不想执行');
alert('我也不想执行');
```

### 计算时间

1.获取当前时间(指定日期)

var myDate = new Date();
var date1 = new Date('yyyy/MM/dd hh:mm:ss');
var t2 = "yyyy-MM-dd hh:mm:ss";
var d2 = t2.replace(/\-/g, "/");
var date2 = new Date(d1);

2.获取日期中的年月日时分秒

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


