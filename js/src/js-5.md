# node.js

有个叫`Ryan Dahl`的歪果仁, 他的工作是用`C/C++`写高性能`Web`服务. 对于高性能, `异步IO`, `事件驱动`是基本原则,
但是用`C/C++`写就太痛苦了. 于是这位仁兄开始设想用高级语言开发`Web`服务.
他评估了很多种高级语言, 发现很多语言虽然同时提供了`同步IO`和`异步IO`, 但是开发人员一旦用了`同步IO`,
他们就再也懒得写`异步IO`了, 所以, 最终, Ryan瞄向了`JavaScript`.

因为`JavaScript`是单线程执行, 根本不能进行`同步IO`操作, 所以, `JavaScript`的这一"缺陷"导致了它只能使用`异步IO`.

选定了开发语言, 还要有`运行时引擎`. 这位仁兄曾考虑过自己写一个, 不过明智地放弃了, 因为`V8`就是开源的`JavaScript`引擎.
让`Google`投资去优化V8, 咱只负责改造一下拿来用, 还不用付钱, 这个买卖很划算.

于是在2009年, Ryan正式推出了基于`JavaScript`语言和`V8`引擎的开源`Web`服务器项目, 命名为`Node.js`.
虽然名字很土, 但是, `Node` 第一次把`JavaScript`带入到后端服务器开发, 加上世界上已经有无数的`JavaScript`开发人员, 所以`Node`一下子就火了起来.

在`Node`上运行的`JavaScript`相比其他后端开发语言有何优势?
最大的优势是借助`JavaScript`天生的事件驱动机制加`V8`高性能引擎, 使编写高性能`Web`服务轻而易举.
其次, `JavaScript`语言本身是完善的函数式语言,
在前端开发时, 开发人员往往写得比较随意, 让人感觉`JavaScript`就是个`玩具语言`.

但是, 在`Node`环境下, 通过模块化的`JavaScript`代码, 加上函数式编程, 并且无需考虑浏览器兼容性问题, 直接使用最新的`ECMAScript 6`标准, 可以完全满足工程上的需求.

>我还听说过`io.js`, 这又是什么鬼?

因为`Node.js`是开源项目, 虽然由社区推动, 但幕后一直由`Joyent`公司资助. 由于一群开发者对`Joyent`公司的策略不满, 于2014年从`Node.js`项目fork出了`io.js`项目, 决定单独发展, 但两者实际上是兼容的.
分家后没多久, `Joyent`公司表示要和解, 于是, `io.js`项目又决定回归`Node.js`.

具体做法是将来`io.js`将首先添加新的特性, 如果大家测试用得爽, 就把新特性加入`Node.js`.
`io.js`是"尝鲜版", 而`Node.js`是线上稳定版, 相当于 `Fedora Linux` 和 `RHEL` 的关系.

### 安装Node.js和npm

由于`Node.js`平台是在后端运行`JavaScript`代码, 所以, 必须首先在本机安装`Node`环境.

`ubuntu` 使用`sudo apt install nodejs`安装. `windows`到官网下载安装包.
安装完成后, 输入`node -v`, 如果安装正常, 你应该看到类似`v10.19.0`这样的输出.

#### npm

`npm`是什么东东? `npm` 其实是`Node.js`的包管理工具(`node package manager`).

为啥我们需要一个包管理工具呢? 因为我们在`Node.js`上开发时, 会用到很多别人写的`JavaScript`模块.
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

### 模块

在计算机程序的开发过程中,随着程序代码越写越多,在一个文件里代码就会越来越长,越来越不容易维护.
为了编写可维护的代码,我们把很多函数分组,分别放到不同的文件里, 这样,每个文件包含的代码就相对较少,很多编程语言都采用这种组织代码的方式.
在`Node`环境中,一个`.js`文件就称之为一个模块(`module`).

使用模块有什么好处?
最大的好处是大大提高了代码的可维护性.
其次,编写代码不必从零开始.当一个模块编写完毕,就可以被其他地方引用.
我们在编写程序的时候,也经常引用其他模块,包括`Node`内置的模块和来自第三方的模块.

使用模块还可以避免函数名和变量名冲突.
相同名字的函数和变量完全可以分别存在不同的模块中,因此,我们自己在编写模块时,不必考虑名字会与其他模块冲突.
在上一节,我们编写了`hello.js`文件,这个`hello.js`文件就是一个模块,模
块的名字就是文件名(去掉`.js`后缀),所以`hello.js`文件就是名为`hello`的模块.

我们把`hello.js`改造一下,创建一个函数,这样我们就可以在其他地方调用这个函数: 

```js
'use strict';
var s = 'Hello';
function greet(name) {
    console.log(s + ', ' + name + '!');
}
module.exports = greet;
```

函数`greet()`是我们在`hello`模块中定义的,你可能注意到最后一行是一个奇怪的赋值语句,
它的意思是,把函数`greet`作为模块的输出暴露出去,这样其他模块就可以使用`greet`函数了.

问题是其他模块怎么使用`hello`模块的这个`greet`函数呢? 我们再编写一个`main.js`文件,调用`hello`模块的`greet`函数: 

```js
'use strict';
// 引入hello模块:
var greet = require('./hello');
var s = 'Michael';
greet(s); // Hello, Michael!
```

注意到引入`hello`模块用`Node`提供的`require`函数: 

```js
var greet = require('./hello');
```

引入的模块作为变量保存在`greet`变量中,那`greet`变量到底是什么东西?
其实变量`greet`就是在`hello.js`中我们用`module.exports = greet;`输出的`greet`函数.
所以, `main.js`就成功地引用了`hello.js`模块中定义的`greet()`函数,接下来就可以直接使用它了.

在使用`require()`引入模块的时候, 请注意模块的相对路径. 因为`main.js`和 `hello.js` 位于同一个目录,所以我们用了当前目录`.`: 

```js
var greet = require('./hello'); // 不要忘了写相对目录!
```

如果只写模块名: 

```js
var greet = require('hello');
```

则Node会依次在内置模块, 全局模块和当前模块下查找`hello.js`,你很可能会得到一个错误: 

```js
module.js
    throw err;
          ^
Error: Cannot find module 'hello'
    at Function.Module._resolveFilename
    at Function.Module._load
    ...
    at Function.Module._load
    at Function.Module.runMain
```

遇到这个错误,你要检查: 

+ 模块名是否写对了;
+ 模块文件是否存在;
+ 相对路径是否写对了.

#### CommonJS规范

这种模块加载机制被称为`CommonJS`规范.
在这个规范下,每个`.js`文件都是一个模块,它们内部各自使用的变量名和函数名都互不冲突,
例如,`hello.js`和`main.js`都申明了全局变量`var s = 'xxx'`,但互不影响.

`模块`想要对外暴露变量(函数也是变量),可以用`module.exports = variable;`,
`模块`要引用`其他模块`暴露的变量,用`var ref = require('module_name')`;就拿到了引用模块的变量.

#### 结论

要在`模块`中对外输出变量,用: 

```js
module.exports = variable;
```

输出的变量可以是任意`对象`,`函数`, `数组`等等.
要引入其他模块输出的对象,用: 

```js
var foo = require('other_module');
```

引入的对象具体是什么,取决于引入模块输出的对象.

#### 深入了解模块原理

如果你想详细地了解`CommonJS`的模块实现原理,请继续往下阅读.如果不想了解,请直接跳到最后做练习.

当我们编写`JavaScript`代码时,我们可以申明全局变量: 

```js
var s = 'global';
```

在浏览器中,大量使用全局变量可不好. 如果你在`a.js`中使用了全局变量`s`,
那么,在`b.js`中也使用全局变量`s`,将造成冲突,`b.js`中对`s`赋值会改变`a.js`的运行逻辑.
也就是说, `JavaScript`语言本身并没有一种模块机制来保证不同模块可以使用相同的变量名.

那`Node.js`是如何实现这一点的?

其实要实现`模块` 这个功能,并不需要语法层面的支持. `Node.js`也并不会增加任何`JavaScript`语法.
实现`模块` 功能的奥妙就在于`JavaScript`是一种函数式编程语言,它支持`闭包`.
如果我们把一段`JavaScript`代码用函数`包装`起来,这段代码的所有`全局`变量就变成了函数内部的局部变量.

请注意我们编写的`hello.js`代码是这样的: 

```js
var s = 'Hello';
var name = 'world';
console.log(s + ' ' + name + '!');
```

`Node.js`加载了`hello.js`后,它可以把代码包装一下,变成这样执行: 

```js
(function () {
    // 读取的hello.js代码:
    var s = 'Hello';
    var name = 'world';

    console.log(s + ' ' + name + '!');
    // hello.js代码结束
})();
```

这样一来,原来的全局变量`s`现在变成了匿名函数内部的`局部变量`.
如果`Node.js`继续加载其他模块,这些模块中定义的`全局`变量`s`也互不干扰.
所以,`Node`利用`JavaScript`的函数式编程的特性,轻而易举地实现了模块的隔离.

但是,模块的输出`module.exports`怎么实现?

这个也很容易实现,`Node`可以先准备一个对象`module`: 

```js
// 准备module对象:
var module = {
    id: 'hello',
    exports: {}
};
var load = function (module) {
    // 读取的hello.js代码:
    function greet(name) {
        console.log('Hello, ' + name + '!');
    }
    
    module.exports = greet;
    // hello.js代码结束
    return module.exports; // load 最终返回要保留的对象.
};
// 保存module:
var exported = load(module);
save(module, exported);
```

可见,变量`module`是`Node`在加载`js`文件前准备的一个变量,并将其传入加载函数,
我们在`hello.js`中可以直接使用变量`module`原因就在于它实际上是函数的一个参数: 

```js
module.exports = greet;
```

通过把参数`module`传递给`load()`函数, `hello.js` 就顺利地把变量传递给了`Node`执行环境, `Node`会把`module`变量保存到某个地方. 

由于`Node`保存了所有导入的`module`, 当我们用`require()`获取`module`时,
`Node`找到对应的`module`,把这个 `module` 的`exports` 变量返回, 这样,另一个模块就顺利拿到了模块的输出: 

```js
var greet = require('./hello');
```

以上是`Node`实现`JavaScript`模块的简单的原理介绍.

#### module.exports vs exports

很多时候你会看到, 在`Node`环境中, 有两种方法可以在一个模块中输出变量: 

+ 方法一: 对`module.exports`赋值: 

```js
// hello.js
function hello() {
    console.log('Hello, world!');
}
function greet(name) {
    console.log('Hello, ' + name + '!');
}
module.exports = {
    hello: hello,
    greet: greet
};
```

+ 方法二: 直接使用`exports`: 

```js
// hello.js
function hello() {
    console.log('Hello, world!');
}
function greet(name) {
    console.log('Hello, ' + name + '!');
}
function hello() {
    console.log('Hello, world!');
}
exports.hello = hello;
exports.greet = greet;
```

但是你不可以直接对`exports`赋值: 

```js
// 代码可以执行,但是模块并没有输出任何变量:
exports = {
    hello: hello,
    greet: greet
};
```

如果你对上面的写法感到十分困惑,不要着急,我们来分析`Node`的加载机制: 
首先,`Node`会把整个待加载的`hello.js`文件放入一个包装函数`load`中执行. 在执行这个`load()`函数前, `Node`准备好了`module`变量: 

```js
var module = {
    id: 'hello',
    exports: {}
};
```

`load()`函数最终返回`module.exports`. 
`exports` 是 `Node` 为了方便, 给每个模块都配备的一个指向`module.exports`的变量. 其实就是相当于在每个模块都加了一句 : [會仩樹啲豿豿](https://www.liaoxuefeng.com/discuss/1023622307115840/1381112471355425)

    var exports = module.exports

也就是:

```js
var load = function (exts, mo) {
   // hello.js的文件内容
    function greet(n){console.log('greet '+n);}
    function foo(m){console.log('foo '+ m);}
    exts.greet=greet;
    exts.foo=foo;
    /* 如果用下面的形式,并不能改变 mo.exts 的指向
    exts={greet:greet, foo:foo};
    */
    // load函数返回:
    return mo.exts;
};
// 准备模块,导入对象
var mo={id:"hello", exts:{}};
var expted= load(mo.exts,mo); // 当这样调用时,  exts 被绑定到 mo.exts
```

也就是说,默认情况下, `Node` 准备的`exports`变量和`module.exports`变量实际上是同一个变量, 并且初始化为空对象`{}`, 于是我们可以写: 

```js
exports.foo = function () { return 'foo'; };
exports.bar = function () { return 'bar'; };
```

也可以写: 

```js
module.exports.foo = function () { return 'foo'; };
module.exports.bar = function () { return 'bar'; };
```

换句话说, `Node` 默认给你准备了一个空对象`{}`,这样你可以直接往里面加东西.
但是,如果我们要输出的是`函数`或`数组`, 那么只能给`module.exports`赋值: 

```js
module.exports = function () { return 'foo'; };
```

给`exports`赋值是无效的, 因为赋值后`module.exports`仍然是空对象`{}`.

+ 结论:

如果要输出键值对象`{}`,可以利用`exports`这个已存在的空对象`{}`,并继续在上面添加新的`键值`;
如果要输出`函数`或`数组`, 必须直接对`module.exports`对象赋值.

所以我们可以得出结论: 直接对`module.exports`赋值,可以应对任何情况: 

```js
module.exports = {
    foo: function () { return 'foo'; }
};
```

或者: 

```bash
module.exports = function () { return 'foo'; };
```

最终,我们强烈建议使用`module.exports = xxx`的方式来输出模块变量, 这样你只需要记忆一种方法.

#### 练习

编写`hello.js`,输出一个或多个函数;
编写`main.js`,引入`hello`模块,调用其函数.

### 基本模块

因为`Node.js`是运行在服务区端的`JavaScript`环境,服务器程序和浏览器程序相比,最大的特点是没有浏览器的安全限制了,
而且,服务器程序必须能接收网络请求,读写文件,处理二进制内容. 所以,`Node.js`内置的常用模块就是为了实现基本的服务器功能.
这些模块在浏览器环境中是无法被执行的,因为它们的底层代码是用`C/C++`在`Node.js`运行环境中实现的.

#### global

在前面的`JavaScript`课程中,我们已经知道,`JavaScript`有且仅有一个全局对象, 在浏览器中,叫`window`对象.
而在`Node.js`环境中,也有唯一的全局对象,但不叫`window`,而叫 `global`,这个对象的属性和方法也和浏览器环境的 `window` 不同.
进入`Node.js`交互环境,可以直接输入: 

```js
> global.console
Console {
  log: [Function: bound ],
  info: [Function: bound ],
...
```

#### process

`process`也是`Node.js`提供的一个对象,它代表当前`Node.js`进程. 通过`process`对象可以拿到许多有用信息: 

```js
> process === global.process;
true
> process.version;
'v5.2.0'
> process.platform;
'darwin'
> process.arch;
'x64'
> process.cwd(); //返回当前工作目录
'/Users/michael'
> process.chdir('/private/tmp'); // 切换当前工作目录
undefined
> process.cwd();
'/private/tmp'
```

`JavaScript` 程序是由事件驱动执行的单线程模型,`Node.js`也不例外.
`Node.js`不断执行响应事件的`JavaScript`函数,直到没有任何响应事件的函数可以执行时,`Node.js`就退出了.

如果我们想要在下一次事件响应中执行代码,可以调用`process.nextTick()`: 

```js
// test.js
// process.nextTick()将在下一轮事件循环中调用:
process.nextTick(function () {
    console.log('nextTick callback!');
});
console.log('nextTick was set!');
```

用`Node`执行上面的代码`node test.js`,你会看到,打印输出是: 

```js
nextTick was set!
nextTick callback!
```

这说明传入`process.nextTick()`的函数不是立刻执行,而是要等到下一次事件循环.

`Node.js`进程本身的事件就由`process`对象来处理. 如果我们响应`exit`事件,就可以在程序即将退出时执行某个回调函数: 

```js
// 程序即将退出时的回调函数:
process.on('exit', function (code) {
    console.log('about to exit with code: ' + code);
});
```

#### 判断JavaScript执行环境

有很多`JavaScript`代码既能在浏览器中执行,也能在`Node`环境执行,
但有些时候,程序本身需要判断自己到底是在什么环境下执行的,常用的方式就是根据`浏览器`和`Node`环境提供的全局变量名称来判断: 

```js
if (typeof(window) === 'undefined') {
    console.log('node.js');
} else {
    console.log('browser');
}
```

后面,我们将介绍`Node.js`的常用`内置模块`.

[参考源码](https://github.com/michaelliao/learn-javascript/blob/master/samples/node/global/gl.js)

### fs

`Node.js`内置的`fs`模块就是文`件系统`模块,负责读写文件.
和所有其它`JavaScript`模块不同的是, `fs`模块同时提供了`异步`和`同步`的方法.

回顾一下什么是`异步方法`.因为`JavaScript`的单线程模型,执行`IO`操作时,`JavaScript`代码无需等待,
而是传入`回调函数`后,继续执行后续`JavaScript`代码. 比如`jQuery`提供的`getJSON()`操作: 

```js
$.getJSON('http://example.com/ajax', function (data) {
    console.log('IO结果返回后执行...');
});
console.log('不等待IO结果直接执行后续代码...');
```

而`同步`的IO操作则需要等待函数返回: 

```js
// 根据网络耗时,函数将执行几十毫秒~几秒不等:
var data = getJSONSync('http://example.com/ajax');
```

同步操作的好处是代码简单,缺点是程序将等待IO操作,在等待时间内,无法响应其它任何事件.而异步读取不用等待IO操作,但代码较麻烦.

#### 异步读文件

按照`JavaScript`的标准, `异步`读取文本文件的代码如下: 

```js
'use strict';
var fs = require('fs');
fs.readFile('sample.txt', 'utf-8', function (err, data) {
    if (err) {
        console.log(err);
    } else {
        console.log(data);
    }
});
```

请注意, `sample.txt` 文件必须在当前目录下,且文件编码为`utf-8`.

异步读取时,传入的`回调函数`接收两个参数,
当正常读取时,`err`参数为`null`,`data`参数为读取到的`String`. 
当读取发生错误时,`err`参数代表`错误对象`, `data`为`undefined`.
这也是`Node.js`标准的回调函数: 第一个参数代表`错误信息`,第二个参数代表`结果`. 后面我们还会经常编写这种`回调函数`.

由于`err是`否为`null`就是判断是否出错的标志,所以通常的判断逻辑总是: 

```js
if (err) {
    // 出错了
} else {
    // 正常
}
```

如果我们要读取的文件不是`文本文件`,而是`二进制文件`,怎么办?
下面的例子演示了如何读取一个图片文件: 

```js
'use strict';
var fs = require('fs');
fs.readFile('sample.png', function (err, data) {
    if (err) {
        console.log(err);
    } else {
        console.log(data);
        console.log(data.length + ' bytes');
    }
});
```

当读取二进制文件时,不传入`文件编码`时,回调函数的`data`参数将返回一个`Buffer`对象.
在`Node.js`中,`Buffer`对象就是包含`零个`或`任意个`字节(byte)的`数组`(注意和`Array`不同).
`Buffer`对象可以和`String`作转换. 例如,把`Buffer`对象转换成`String`: 

```js
// Buffer -> String
var text = data.toString('utf-8');
console.log(text);
```

或者把`String`转换成`Buffer`: 

```js
// String -> Buffer
var buf = Buffer.from(text, 'utf-8');
console.log(buf);
```

#### 同步读文件

除了标准的异步读取模式外, `fs`也提供相应的`同步读取`函数.
`同步读取`的函数和异步函数相比,多了一个`Sync`后缀,并且不接收`回调函数`,函数直接返回结果.

用`fs`模块同步读取`文本文件`的代码如下: 

```js
'use strict';
var fs = require('fs');
var data = fs.readFileSync('sample.txt', 'utf-8');
console.log(data);
```

可见, 原`异步调用`的回调函数的`data`被函数直接返回, 函数名需要改为`readFileSync`, 其它参数不变.
如果同步读取文件发生错误,则需要用`try...catch`捕获该错误: 

```js
try {
    var data = fs.readFileSync('sample.txt', 'utf-8');
    console.log(data);
} catch (err) {
    // 出错了
}
```

#### 写文件

将数据写入文件是通过`fs.writeFile()`实现的: 

```js
'use strict';
var fs = require('fs');
var data = 'Hello, Node.js';
fs.writeFile('output.txt', data, function (err) {
    if (err) {
        console.log(err);
    } else {
        console.log('ok.');
    }
});
```

`writeFile()`的参数依次为`文件名`,`数据`和`回调函数`.
如果传入的数据是`String`, 默认按`UTF-8`编码写入文本文件,如果传入的参数是`Buffer`, 则写入的是二进制文件.
回调函数由于只关心成功与否,因此只需要一个`err`参数.

和`readFile()`类似, `writeFile()`也有一个同步方法,叫`writeFileSync()`: 

```js
'use strict';
var fs = require('fs');
var data = 'Hello, Node.js';
fs.writeFileSync('output.txt', data);
```

#### stat

如果我们要获取文件大小,创建时间等信息,可以使用`fs.stat()`, 它返回`Stat`对象,能告诉我们`文件`或`目录`的详细信息: 

```js
'use strict';
var fs = require('fs');
fs.stat('sample.txt', function (err, stat) {
    if (err) {
        console.log(err);
    } else {
        // 是否是文件:
        console.log('isFile: ' + stat.isFile());
        // 是否是目录:
        console.log('isDirectory: ' + stat.isDirectory());
        if (stat.isFile()) {
            // 文件大小:
            console.log('size: ' + stat.size);
            // 创建时间, Date对象:
            console.log('birth time: ' + stat.birthtime);
            // 修改时间, Date对象:
            console.log('modified time: ' + stat.mtime);
        }
    }
});
```

运行结果如下: 

```js
isFile: true
isDirectory: false
size: 181
birth time: Fri Dec 11 2015 09:43:41 GMT+0800 (CST)
modified time: Fri Dec 11 2015 12:09:00 GMT+0800 (CST)
```

`stat()`也有对应的同步函数`statSync()`,请试着改写上述异步代码为同步代码.

#### 异步还是同步

在`fs`模块中,提供同步方法是为了方便使用.那我们到底是应该用异步方法还是同步方法呢?

由于`Node`环境执行的`JavaScript`代码是`服务器`端代码, 所以绝大部分需要在`服务器`运行期反复执行业务逻辑的代码,必须使用异步代码,
否则,同步代码在执行时期,服务器将停止响应,因为`JavaScript`只有一个执行线程.

服务器启动时如果需要读取配置文件, 或者结束时需要写入到状态文件时, 可以使用同步代码,因为这些代码只在启动和结束时执行一次,不影响服务器正常运行时的异步执行.
