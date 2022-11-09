# 理解JavaScript的函数调用和 `this`

[Understanding JavaScript Function Invocation and "this"](https://yehudakatz.com/2011/08/11/understanding-javascript-function-invocation-and-this/)

多年来, 我看到很多关于JavaScript函数调用的困惑.
特别是, 很多人抱怨说, 函数调用中this的语义很混乱.

在我看来, 通过理解核心的函数调用原语,
然后把所有其他调用函数的方式看成是这个原语的 语法糖, 就可以消除很多这种困惑了.
事实上, 这正是ECMAScript规范对它的看法.
在某些方面, 这篇文章是对规范的简化, 但其基本思想是相同的.

## 核心原语

首先, 让我们来看看核心的函数调用原语, 即一个函数的调用方法(1).
调用方法是相对直接的.

+ 从 `参数1` 到 `末参数` 组建参数列表(`argList`).
+ 第一个参数是 `thisValue`
+ 调用 `函数`, 并将 `this` 设置为 `thisValue`, 将 `argList` 作为函数的 `参数列表`

比如说:

```js
function hello(thing) {
  console.log(this + " says hello " + thing);
}

hello.call("Yehuda", "world") //=> Yehuda says hello world
```

正如你所看到的, 我们在调用 `hello` 方法时, 将 `this` 设置为 `Yehuda`, 并带上一个参数 `world`.
这就是JavaScript函数调用的 核心原语(core primitive).
你可以认为所有其他的函数调用都是对这个 原语的 desugaring.
(`desugar` 是指采用一种方便的语法, 用更基本的核心原语来描述它).

(1) 在 ES5 规范中, 调用方法是用另一个更低级的基元来描述的,
但它是该基元之上的一个非常薄的包装,
所以我在这里做了一些简化. 更多信息请见本篇文章的结尾.

## 简单的函数调用

很明显, 如果一直用 `call` 的方式来调用函数会很烦人.
`JavaScript` 允许我们使用 `括号` 语法直接调用函数-- `hello("world")`.
当我们这样做的时候, 调用的内容就会被 desugars:

```js
function hello(thing) {
  console.log("Hello " + thing);
}

// this:
hello("world")

// desugars to:
hello.call(window, "world");
```

这种行为在ECMAScript 5中只在使用 `严格模式`(2) 时发生了变化.

```js
// this:
hello("world")

// desugars to:
hello.call(undefined, "world");
```

简而言之: 像 `fn(...args)` 这样的函数调用与
`fn.call(window [ES5-strict: undefined], ...args)` 相同.

请注意, 对于内联声明的函数也是如此:
`(function() {})()` 与 `(function() {}).call(window [ES5-strict: undefined)` 相同.

>(2) 实际上, 我撒了点谎.
>ECMAScript 5规范说, `undefined`(几乎)总是被传递的,
>但被调用的函数在 `非严格模式` 下应将其 `thisValue` 改为全局对象.
>这使得严格模式的调用者可以避免破坏现有的非严格模式的库.

## 成员函数

下一个很常见的 invoke 方法的方式是,作为一个对象的成员 `person.hello()`.
在这种情况下, 调用会 desugar:

```js
var person = {
  name: "Brendan Eich",
  hello: function(thing) {
    console.log(this + " says hello " + thing);
  }
}

// this:
person.hello("world")

// desugars to this:
person.hello.call(person, "world");
```

请注意, 在这种形式下, `hello` 方法如何附加到 `对象` 并不重要.
请记住, 我们之前将 `hello` 定义为一个独立的函数.
让我们看看如果我们动态地将其附加到对象上会发生什么:

```js
function hello(thing) {
  console.log(this + " says hello " + thing);
}

person = { name: "Brendan Eich" }
person.hello = hello; // 动态绑定

person.hello("world") // still desugars to person.hello.call(person, "world")

hello("world") // "[object DOMWindow]world"
```

请注意, 这个函数并没有一个持久的(persistent ) `this` 的概念.
它总是 `在调用时` 根据它被调用者 invoke 的方式来设置.

## 使用 Function.prototype.bind

因为有时, 被引用的函数具有持久性的 `this` 值是很方便的,
所以人们历来使用一个简单的 `闭包技巧` 来将函数转换成具有不变的this的函数:

```js
var person = {
  name: "Brendan Eich",
  hello: function(thing) {
    console.log(this.name + " says hello " + thing);
  }
}

var boundHello = function(thing) { return person.hello.call(person, thing); }

boundHello("world");
```

尽管我们的 `boundHello` 调用仍然desugars为 `boundHello.call(window, "world")`,
但我们强行转到原始调用方法, 将 `this`值 改回我们想要的样子.

我们可以通过一些调整使这一技巧具有通用性:

```js
var bind = function(func, thisValue) {
  return function() {
    return func.apply(thisValue, arguments);
  }
}

var boundHello = bind(person.hello, person);
boundHello("world") // "Brendan Eich says hello world"
```

为了理解这一点, 你只需要再得到两个信息.
首先, `arguments` 是类似于 `数组` 的对象, 它代表传入函数的 `所有参数`.
第二, `apply` 方法的工作原理与 `call` 原语完全相同, 只是它接收一个类似 `数组` 的对象,
而不是一个一个地列出参数.

我们的 `bind` 方法只是返回 `新函数`.
当它被调用时, 我们的新函数只是调用被传入的原始函数,
将 `this` 设置为 `thisValue`. 它还传递了参数.

因为这是一个有点常见的用法,
ES5在所有的Function对象上引入了一个新的方法 `bind`, 实现了这种行为.

```js
var boundHello = person.hello.bind(person);
boundHello("world") // "Brendan Eich says hello world"
```

当你需要将 raw函数 作为回调传递时, 这是最有用的.

```js
var person = {
  name: "Alex Russell",
  hello: function() { console.log(this.name + " says hello world"); }
}

$("#some-div").click(person.hello.bind(person));

// when the div is clicked, "Alex Russell says hello world" is printed
```

当然, 这有点笨拙, TC39(负责ECMAScript下一版本的委员会)
继续致力于开发一个更优雅的, 仍然向后兼容的解决方案.

## 关于jQuery

由于 `jQuery` 大量使用 `匿名回调函数`,
它在内部使用 `call` 方法将回调的 `this` 值设置为更有用的值.
例如, 在 event handlers 中, jQuery不使用 `window` 作为 `this`值
(因为你没有特别的干预),
它在 callback 中 invoke `call`, 并将建立 event handlers 的元素作为其第一个参数.

这是非常有用的, 因为 匿名回调中 `this` 的默认值不是特别有用,
但它可能会给JavaScript的初学者留下这样的印象:
一般来说, `this` 是一个奇怪的, 经常变异的概念, 很难推理.

如果你理解了将 sugary 函数调用, 转换为 desugared
`func.call(thisValue, ...args)` 的基本规则,
你就应该能够在JavaScript this值这个并不险恶的水域中游刃有余.

Type    this

+ `func(...args)`;  `window`
+ `func(...args)` ES5严格模式;  `undefined`
+ `path.to.obj.func(...args)`;  `path.to.obj`

## PS: I Cheated

在一些地方, 我根据规范的确切措辞简化了现实.
最重要的作弊方式可能是我把 func.call 称为 `原语`.
实际上, 规范中有一个基元(内部称为 `[[Call]]`),
`func.call` 和 `[obj.]func()` 都调用它.

然而, 看看 `func.call` 的定义吧.

+ 如果 `IsCallable(func)` 为假, 那么抛出 `TypeError异常`.
+ 让argList是一个空的List.
+ 如果这个方法被调用时有多个参数, 那么按照从左到右的顺序, 从arg1开始, 将每个参数追加为argList的最后一个元素.
+ 返回调用 `func` 的 `[[Call]]` 内部方法的结果, 提供thisArg作为this值, argList作为参数的列表.

正如你所看到的, 这个定义本质上是一个非常简单的JavaScript语言
与原始的 `[[Call]]`操作的绑定.

如果你看一下调用函数的定义, 前七步设置了 `thisValue` 和 `argList`,
最后一步是. "返回调用func的 `[[Call]]`内部方法的结果,
提供thisValue作为this值, 提供argList列表作为参数值. "

这基本上是相同的措辞, 一旦 `argList` 和 `thisValue` 被确定下来.
在把 `call` 称为原语这点, 我有点作弊,
但其含义与我在本文开始时拿出规范, 并逐章逐节引用的含义基本相同.

还有一些额外的情况(最明显的是涉及 `with`), 我在这里没有涉及.
