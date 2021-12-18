# 函数式编程

[什么是 Mona - 17dian的回答](https://www.zhihu.com/question/19635359/answer/420267395)
[函数式编程别烦恼](https://juejin.cn/post/6844903621507678216)

+ 纯函数;
    + 定义: 函数如果 `输入参数` 确定, `输出结果` 是唯一确定的, 那么他就是纯函数.
    + 特点: 无状态, 无副作用, 无关时序, 幂等(无论调用多少次, 结果相同).

## 柯里化(curry)

定义: 只传递给函数一部分参数来调用它, 让它返回一个函数去处理剩下的参数.

```javascript
function add(x, y) {
     return x + y;
}
add(1, 2)

******* 柯里化之后 *************

function addX(y) {
   return function (x) {
    return x + y;
   };
}
var newAdd =  addX(2)
 newAdd (1)
```

+ 高阶函数
    定义: `函数`当参数, 把 `传入的函数` 做一个封装, 然后返回这个 `封装函数`,达到更高程度的抽象.

+ 等价函数
    定义 : 调用函数本身的地方都可以其 `等价函数`;
    `等价函数` 被用来实现拦截和监控: javascript

    ```javascript
    function __watch__(fn){
        //偷偷干点啥
         return function(...args){
            //偷偷干点啥
            let ret = fn.apply(this,args); // 调用原先的函数
            //偷偷干点啥
            return ret
             }
    }
    ```

## 节流函数(throtle)

前端开发中, 会遇到一些频繁的事件触发, 为了解决这个问题, 一般有两种解决方案:

+ throttle 节流,
+ debounce 防抖

```javascript
function throttle(fn,wait){
     var timer;
     return function(...args){
        if(!timer){
            timer = setTimeout( ()=>timer=null , wait);
            console.log(timer)
            return fn.apply(this,args)
        }
     }
}

const fn  = function(){ console.log("btn clicked")}
const btn = document.getElementById('btn'); //html 元素, btn 按钮
btn.onclick = throttle(fn , 5000); // 点击 btn 按钮, 执行的操作
```

分析代码:

+ 首先我们定义了一个 `timer`
+ 当 `timer` 不存在的时候, 执行 `if` 判断里函数
+ `setTimeout` 给 `timer` 赋一个 `id` 值, `fn` 也执行
+ 如果继续点击, `timer` 存在, `if` 判断里函数不执行
+ 当时间到时, `setTimeout` 的 `回调函数` 清空 `timer`, 此时再去执行 `if` 判断里的函数

所以, 我们通过对 `等价函数` 监控和拦截, 很好的实现了 `节流函数`(throtle). 而对函数 `fn` 执行的结果丝毫没有影响.
这里给大家留一个作业, 既然我们实现了 `节流函数`, 那么你能不能根据同样的原理写出防抖函数呢?

## 命令式与声明式

在平时, 如果我们不借助 `方法函数` 去实现节流函数, 我们可能会直接这么去实现节流函数.

```javascript
var timer;
  btn.onclick = function(){
   if(!timer){
      timer = setTimeout(()=>timer=null , 5000);
      console.log("btn clicked")
   }
}
```

那么与之前的高阶函数有什么区别呢?

很显然, 在下面的这例子中, 我们每次在需要做节流的时候, 我们每次都需要这样重新写一次代码. `告诉` 程序如何执行.
而上面的高阶函数的例子, 我们定义好了一个功能函数之后, 我们只需要告诉程序, `你要做` 什么就可以啦.

+ 命令式 : 上面的例子就是命令式
+ 声明式 : `高阶函数` 的例子就是声明式

那下面大家看看, 如果遍历一个数组,打印出每个数组中的元素, 如何用两种方法实现呢?

```javascript
//命令式
var array = [1,2,3];
for (i=0; i<array.length;i++){
  console.log(array[i])
}
//声明式
array.forEach((i) => console.log(i))
```

看到 `forEach` 是不是很熟悉, 原来我们早就在大量使用函数式编程啦.

这里我们可以先停下来从头回顾一下, 函数式编程.

+ 函数式编程, 更关注的是动作, 比如我们定义的节流函数, 就是把节流的这个动作抽象出来.
+ 所以这样的函数必须要输入输出确定, 且对外界没有影响, 我们把这样的函数叫纯函数
+ 对于不纯的函数提纯的过程中, 用到了 `柯里化` 方法.
+ 我们柯里化过程中, 我们传进去的参数恰恰是 `函数`, 返回的也是 `函数`, 这就叫高阶函数.
+ 高阶函数往往能抽象写出像节流这样的功能函数.
+ 声明式就是在使用这些功能函数

## 组合,compose

```javascript
function double(x) {
  return x * 2
}
function add5(x) {
  return x + 5
}
double(add5(1))
```

上面的代码我们实现的是完成了两个动作, 不过我们觉得这样写 `double(add5(x))`, 不是很舒服.
换一个角度思考, 我们是不是可以把函数合并在一起.
我们定义了 `compose` 函数

```javascript
var compose = function(f, g) {

    return function(x) {
        return f(g(x));
    };
};
```

有了 `compose` 这个函数, 显然我们可以把 `double` 和 `add5` 合并到一起

```javascript
var numDeal =  compose(double,add5)
numDeal(1)
```

+ 首先我们知道 `compose` 合并的 `double`,`add5` 是从右往左执行的
+ 所以1先执行 `加5`, 再完成了`乘2`

+ 那么这时候就有几个问题, 这只使用与一个参数, 如果是多个参数怎么办? 有的同学已经想到了用柯里化
+ 还有这只是两个函数, 如果是多个函数怎么办. 知道 `reduce` 用法的同学, 可能已经有了思路.
+ `compose` 是从从右往左执行, 我想左往右行不行? 当然, 他还有个专门的名字叫 `管道函数`(pipe)

问题: 现在我们想完成一些功能都需要去合并函数, 而且合并的函数还会有一定顺序, 我们能不能像 `JQuery` 的链式调用那样去处理数据呢.

## 函子,Functor

讲到函子, 我们首先回到我们的问题上来. 之前我们执行函数通常是下面这样.

```javascript
function double(x) {
  return x * 2
}
function add5(x) {
  return x + 5
}

double(add5(1))
//或者
var a = add5(5)
double(a)
```

那现在我们想以数据为核心, 一个动作一个动作去执行.

```javascript
(5).add5().double()
```

显然, 如果能这样执行函数的话, 就舒服多啦.
那么我们知道, 这样的去调用要满足:

+ `(5)` 必须是 `引用类型`, 因为需要挂载 `方法`.
+ `引用类型` 上要有可以调用的`方法`, 所以我们试着去给他创建一个 `引用类型`

```javascript
class Num{
       constructor (value) {
          this.value = value ;
       }
       add5(){
           return this.value + 5
       }
       double(){
           return this.value * 2
       }
    }

var num = new Num(5);
num.add5()
```

这个时候, 我们发现一个问题, 就是经过调用后, 返回的就是 `值` 了, 我们没法进行下一步处理.
所以我们需要返回 `对象`:

```javascript
class Num{
       constructor (value) {
          this.value = value ;
       }
       add5 () {
           return  new Num( this.value + 5)
       }
       double () {
           return  new Num( this.value * 2)
       }
    }
var num = new Num(2);
num.add5 ().double ()
```

+ 我们通过 `new Num`, 创建了和 `num` 相同类型的实例
+ 把处理的 `值`, 作为 `参数` 传了进去, 从而改变了 `this.value` 的值
+ 我们把这个 `对象` 返了回去, 可以继续调用 `方法` 去处理函数

我们发现, `new Num( this.value + 5)` 中对 `this.value` 的处理, 完全可以通过传入 `函数` 去处理,
并且在真实情况中, 我们也不可能为每个 `实例` 都创建这样有不同方法的 `构造函数`, 它们需要一个统一的 `方法`:

```javascript
class Num{
       constructor (value) {
          this.value = value ;
       }
       map (fn) {
         return new Num(fn(this.value))
       }
    }
var num = new Num(2);
num.map(add5).map(double)
```

我们创建了一个 `map`方法(平铺映射), 把处理的函数 `fn` 传了进去. 这样就完美实现我们设想的功能啦.
最后我们整理一下这个函数.

```javascript
class Functor{
       constructor (value) {
          this.value = value ;
       }
       map (fn) { // 对盒中对象 map 操作
         return Functor.of(fn(this.value))
       }
    }
Functor.of = function (val) { // 用来生成新的容器
     return new Functor(val);
}

Functor.of(5).map(add5).map(double)
```

+ 我们把原来的构造函数 `Num` 的名字改成了 `Functor`.
+ 我们给 `new Functor(val);` 封住了一个方法 `Functor.of`

现在 `Functor.of(5).map(add5).map(double)` 去调用函数. 有没有觉得很爽.

哈哈, 更爽的是, 你已经在不知不觉间把函子的概念学完啦. 上面这个例子中的 `Functor` 就是函子.
现在我们来总结一下它的特点:

+ `Functor` 是个容器, 它包含了 `值`, 就是 `this.value`.(想一想你最开始的 `new Num(5)`)
+ `Functor` 具有 `map` 方法. 该方法将 `容器` 里面的 `每个值`, 映射到另一个 `容器`. (想一想里面的操作 `new Num(fn(this.value)`)
+ `函数式编程` 里面的运算, 都是通过 `函子` 完成, 即 `运算` 不直接针对`值`, 而是针对这个 `值` 的 `容器`--`函子`.(想一想你是不是没直接去操作值)
+ `函子` 本身具有 `对外接口`(map方法), 各种 `函数` 就是 `运算符`, 通过 `接口` 接入 `容器`, 引发 `容器` 里面的 `值`的 `变形`. (说的就是你传进去那个函数把 `this.value` 给处理啦)
+ `函数式编程` 一般约定, 函子有个 `of方法`, 用来生成 `新的容器`. (就是最后咱们整理了一下函数嘛)

嗯, 这下明白什么是函子了吧. 在初学函数编程时, 一定不要太过于纠结概念.
看到好多教程上在讲 `函子` 时全然不提 `JavaScript` 语法. 用生硬的数学概念去解释.
我个人觉得书读百遍, 其义自见.
对于编程范式的概念理解也是一样的, 你先知道它是什么, 怎么用, 多写多练, 自然就理解其中的含义啦.  总抱着一堆概念看, 是很难看懂的.

问题: 我们实现了一个最通用的函子, 现在别问问题, 我们趁热打铁, 再学一个函子

## Maybe 函子

我们知道, 在做 `字符串` 处理的时候, 如果字符串是 `null`, 那么对它进行 `toUpperCase();` 就会报错.

```javascript
Functor.of(null).map(function (s) {
  return s.toUpperCase();
});
```

那么我们在 `Functor函子` 上去进行调用, 同样也会报错.

那么我们有没有什么办法, 在函子里把 `空值` 过滤掉呢?

```javascript
class Maybe{
       constructor (value) {
          this.value = value ;
       }
       map (fn) {
          return this.value ? Maybe.of(fn(this.value)) : Maybe.of(null); // 三元运算符, 跳转分支处理
       }
    }
Maybe.of = function (val) {
     return new Maybe(val);
}

var a = Maybe.of(null).map(function (s) { //尝试调用
  return s.toUpperCase();
});
```

我们看到只需要在 `map` 中设置 `空值过滤`, 就可以完成这样的 `Maybe` 函子.
所以各种不同类型的 `函子`, 会完成不同的功能.
学到这, 我们发现, 每个 `函子` 并没有直接去操作需要处理的数据, 也没有参与到处理数据的 `函数` 中来.
而是在这中间做了一些 `拦截` 和 `过滤`.

这和我们的高阶函数是不是有点像呢. 所以你现在对函数式编程是不是有了更深的了解啦.

现在我们就用函数式编程做个小练习:  我们有字符串 `"li"`, 我们希望处理成大写的字符串, 然后加载到 `id` 为 `text` 的 `div` 上:

```javascript
var str = 'li';
   Maybe.of(str).map(toUpperCase).map(html('text'))
```

如果在有编写好的 `Maybe` 函子, 和两个功能函数的时候, 我们只需要一行代码就可以搞定啦.
那么下面看看, 我们的依赖函数吧.

```javascript
let $$ = id => Maybe.of(document.getElementById(id)); // xx => xx 是匿名函数

class Maybe{
   constructor(value){
        this.__value = value;
   }
   map(fn){
    return this.__value ? Maybe.of(fn(this.__value)) : Maybe.of(null);
   }
   static of(value){
      return new Maybe(value);
   }
}

let toUpperCase = str => str.toUpperCase();

let html = id =>  // 参数为 id
        html => { // 修改 html 的匿名函数, 参数为 html
            $$(id).map(
                dom => {dom.innerHTML = html;});};
```

我们来分析一下代码:

+ 因为 `Maybe.of(document.getElementById(id)` 我们会经常用到, 所以用 `$$` 封装了一下,
+ 然后是很熟悉的 `Maybe函子`, 这里 `of` 用的 `Class` 的 `静态方法`
+ `toUpperCase` 是个普通 `纯函数`(es6如果不是很好的同学, 可以用 babel 编译成es5)
+ `html` 是个 `高阶函数`, 我们先传入目标 `dom` 的 `id`, 然后返回函数, 将 `字符串` 挂载到目标 `dom` 上

```javascript
var html = function(id) {
   return function (html) {
      $$(id).map(function (dom) {
         dom.innerHTML = html;
      });
   };
};
```

大家再来想一个问题, `Maybe.of(str).map(toUpperCase).map(html('text'))` 最后的值是什么呢?

我们发现, 最后处理的函数没有 `返回值`, 所以最后结果应该是 `Maybe {__value: undefined};`
这里面给大家留一个问题, 我们把字符串打印在 `div` 上之后, 想继续操作 `字符串` 该怎么办呢?

问题: 在理解了函子这个概念之后, 我们来学习本文最后一节内容. 有没有很开心

## Monad函子

`Monad` 也是个 `函子`, 其实原理很简单, 只不过它的功能比较重要.
那我们来看看它与其它的函子有什么不同吧.

我们先来看这样一个例子, 手敲在控制台打印一下:

```javascript
var a = Maybe.of( Maybe.of( Maybe.of('str') ) )
console.log(a);
console.log(a.map(fn));
console.log(a.map(fn).map(fn));

function fn(e){ return e.value }
```

我们有时候会遇到一种情况, 需要处理的数据是 `Maybe {value: Maybe}`, 显然我们需要一层一层的解开.
这样很麻烦, 那么我们有没有什么办法得到 `里面的值` 呢:

```javascript
class Maybe{
       constructor (value) {
          this.value = value ;
       }
       map (fn) {
          return this.value ? Maybe.of(fn(this.value)) : Maybe.of(null);
       }
       join ( ) {
          return this.value;
       }
    }
Maybe.of = function (val) {
     return new Maybe(val);
}
```

我们想取到里面的值, 就用 `join` 方法把它返回来就好了啊. 所以我给它加了个 `join方法`

```javascript
var  a = Maybe.of( Maybe.of('str') )
console.log(a.join().map(toUpperCase))
```

所以现在我们可以通过, `join` 的方法一层一层得到里面的数据,并把它处理成大写
现在你肯定会好奇为什么会产生 `Maybe.of( Maybe.of('str'))` 结构呢?

还记得 `html` 那个函数吗? 我们之前留了一个问题, 字符串打印在 `div` 上之后, 想继续操作 `字符串` 该怎么办呢?
很显然我们需要让这个函数有 `返回值`:

```javascript
let $$ = id => Maybe.of(document.getElementById(id)); // xx => xx 是匿名函数

let html = id =>
    html2 => { // 外层函数, 传入 html2 内容, $$(id) 返回 Maybe 类型
        return  $$(id).map( // 内层函数, map 也返回 Maybe 类型
            dom => { dom.innerHTML = html2;
                return html2});
                };
```

分析一下代码:

+ 如果只在里面加 `return html`, 外面函数并没有 `返回值`
+ 如果只在外面加 `return`, 则取不到 `html`
+ 所以只能里面外面都加,
+ 这就出现了 `Maybe.of( Maybe.of('LI') )`

那么这时候我们想, 既然我们在执行的时候就知道, 它会有影响, 那我能不能在执行的时候, 就把这个 `影响` 给消除呢.

```javascript
class Maybe{
       constructor (value) {
          this.value = value ;
       }
       map (fn) {
          return this.value ? Maybe.of(fn(this.value)) : Maybe.of(null);
       }
       join ( ){
          return this.value;
       }
       chain(fn) {
          return this.map(fn).join(); //去掉一层函子的嵌套
       }
    }
```

我们写了个 `chain` 函数. 首先它调用了 `map`方法, 执行结束后, 再去掉一层嵌套的`函子`.
所以在执行的时候, 我们就可以这样去写.

```javascript
Maybe.of(str).map(toUpperCase).chain(html('text'))
```

这样返回的函数就是只有`一层嵌套`的 `函子` 啦.

学到这里, 我们已经把全部的函数式编程所涉及到概念都学习完啦.
现在要是面试官拿这样一道题问题, 答案是什么? 是不是有点太简单啦.

```javascript
var Container = function(x) { this.__value = x;  }
Container.of = x => new Container(x);

Container.prototype.map = function(f){
      console.log(f)
     return Container.of(f(this.__value))
}

Container.of(3).map(x=>x+1).map(x => 'Result is ' + x);
console.log(Container.of(3).map(x=>x+1).map(x => 'Result is ' + x))
```

但你发现, 我们并没有具体纠结每一个概念上, 而是更多的体现在可实现的代码上, 而这些代码你也并不陌生.

哈哈, 那你可能会问, 我是不是学了假的函数式编程, 并没有.

因为我觉得 `函数式编程` 也是编程, 最终都是要回归到日常项目的实践中.
而应对不同难度的项目, 所运用的知识当然也是不一样的, 就好比造船, 小船有小船的造法, 邮轮有邮轮的造法, 航母有航母的造法.

你没有 `必要` 把全部的造船知识点, 逐一学完才开始动手.  日常况且在工作中, 你可能也并有真正的机会去造航母(比如写框架).
与其把大量的时间都花在理解那些概念上, 不如先动手造一艘小船踏实.
所以本文中大量淡化了不需要去立即学习的概念.

## mma 函数式编程

guide/FunctionalProgramming
guide/FunctionCompositionAndOperatorForms

一些内置函数也直接支持 "curried" 形式, 在 Curry 形式下, 它们相当于符号算子(symbolic operators).

## CurryApplied

```mathematica
CurryApplied[f,n]; 代表 `n` 个参数的函数 `f` 的算符形式,
因此 CurryApplied[f,n][x1] ... [xn] 等同于 f[x1, ..., xn].

CurryApplied[n]
表示 `CurryApplied` 的算符形式, 可以应用到函数上, 表示有 `n` 个参数的算符形式.

CurryApplied[f,{i1, ..., in}];
表示 `n` 个参数的函数 `f` 的算符形式,
而 CurryApplied[f,{i1, ..., in}][x1] ... [xn] 等同于 f[x_i1, ..., x_in].
也即会对参数进行重排.

CurryApplied[f, k->{i1, ..., in}];
代表需要 `k` 个参数的算符形式.
```

### 细节

+ `CurryApplied[f,arity][x1, ...][y1, ...] ...[z1, ...]` 等同于 `CurryApplied[f,arity][x1, ...,y1, ..., z1, ...]`, 因此, 方括号`[]`的结构不相关, 只需关注参数的数量.

+ `CurryApplied[f,n]`等同于 `CurryApplied[f,{1,2, ...,n}]`.

+ `CurryApplied[f, {i1, ..., in}]` 等于 `CurryApplied[f,Max[{i1, ..., in}]->{i1, ..., in}]`.

+ `CurryApplied[f,{i1, ..., in, opts}][x1] ... [xk]` 等同于 `f[x_i1, ..., x_in],opts]`, 对于选项序列 `opts`.

+ 对于`CurryApplied[f,{i1, ..., in}]` 形式, `f` 的第 `p` 个参数, 是第 `i_p` 个 curried 参数.

        CurryApplied[f,{i1, ..., in}]  [i2] [i1] [i4] [i3] [i5]

    | f 的参数 | 1 | 2 | 3 | 4 | 5 |
    | --- | --- | --- | --- | --- | --- |
    | curried 参数 | i1 | i2 | i3 | i4 | i5 |

+ `CurryApplied[arity][f]` 等同于 `CurryApplied[f,arity]`.

arity: 变元数目

### 例子

`柯里化` 两参数函数:

```mathematica
CurryApplied[f, 2][x][y]
Out[1]= f[x, y]
```

使用 `CurryApplied` 的算符形式, 完成同样的功能:

```mathematica
CurryApplied[2][f][x][y]
Out[2]= f[x, y]
```

`柯里化` 三参数的函数, 保持它们的顺序:

```mathematica
CurryApplied[f, 3][x][y][z]
Out[1]= f[x, y, z]
```

这是 `Integrate` 的柯里化形式, 科里了两个积分变量:

```mathematica
CurryApplied[Integrate, {3, 1, 2}][x][y]
Out[1]= CurryApplied[Integrate, {3, 1, 2}][x, y]
```

将其应用于变量x和y的函数:

```mathematica
%[x Sin[y]]
Out[2]= -(1/2) x^2 Cos[y]
```

这等价于:

```mathematica
Integrate[x Sin[y], x, y]
Out[3]= -(1/2) x^2 Cos[y]
```

### 范围

柯里 两参数函数的首参数:

```mathematica
truncatedJD = CurryApplied[2][JulianDate]["Truncated"];
```

将该函数应用于任何日期对象.

```mathematica
truncatedJD[Now]
Out[2]= 18820.02587830785

truncatedJD[DateObject[{2000, 1, 1, 12, 0, 0}, TimeZone -> 0]]
Out[3]= 11544.5
```

科里函数的第二个参数:

```mathematica
Dx = CurryApplied[{2, 1}][D][x]
Out[1]= CurryApplied[D, {2, 1}][x]
```

应用算符:

```mathematica
Dx[f[x]]
Out[2]= Derivative[1][f][x]
```

柯里三参数函数, 保持参数次序:

```mathematica
CurryApplied[Nest, 3][f][x][4]
Out[1]= f[f[f[f[x]]]]
```

柯里 3参数函数, 在参数被传递给函数之前, 应用置换:

```mathematica
CurryApplied[Nest, {3, 1, 2}][x][4][f]
Out[1]= f[f[f[f[x]]]]
```

这些科里算符收到  `4` 个参数, 但其中只有 `2个参数` 被传递给函数 `f`:

```mathematica
CurryApplied[f, {2, 4}][a][b][c][d]
Out[1]= f[b, d]

CurryApplied[f, 4 -> {2, 3}][a][b][c][d]
Out[2]= f[b, c]
```

curried函数的参数, 可以和任何括号结构一起使用:

```mathematica
op = CurryApplied[f, 3]
Out[1]= CurryApplied[f, 3]

op[a][b][c]
Out[2]= f[a, b, c]

op[a, b, c]
Out[3]= f[a, b, c]

op[a, b][c]
Out[4]= f[a, b, c]

op[][a][][][b, c]
Out[5]= f[a, b, c]
```

+ 科里 `Level`, 并带有默认选项值:

```mathematica
Sin[x[0] + 3] // CurryApplied[Level, {2, 1}][2]
Out[1]= {3, x[0], 3 + x[0]}
```

传递选项给 `Level`:

```mathematica
Sin[x[0] + 3] // CurryApplied[Level, {2, 1, Heads -> True}][2]
Out[2]= {Sin, Plus, 3, x[0], 3 + x[0]}

Sin[x[0] + 3] // CurryApplied[Level[##, Heads -> True] &, {2, 1}][2]
Out[3]= {Sin, Plus, 3, x[0], 3 + x[0]}
```

### 应用

柯里化三个函数的 `Composition`:

```mathematica
CurryApplied[Composition, 3]

Out[1]= CurryApplied[Composition, 3]
```

按顺序填入 `3` 个函数:

```mathematica
%[f]
Out[2]= CurryApplied[Composition, 3][f]

%[g]
Out[3]= CurryApplied[Composition, 3][f, g]

%[h]
Out[4]= f@*g@*h
```

将 `composition` 应用到表达式:

```mathematica
Construct[%, x]
Out[5]= f[g[h[x]]]
```

指定有多少个 `参数`, 是要 `composed` 的 `函数`:

```mathematica
CurryApplied[Composition, 3][f, g, h, x, y]
Out[1]= f[g[h[x, y]]]
```

建立一个 `下标变量` 的数组:

```mathematica
Array[CurryApplied[Subscript, 4][x], {2, 3, 2}]
```

#### 组合子

guide/CombinatoryLogic

组合逻辑(Combinatory logic)是一个形式化系统, 等价于 `Lambda` 表达式, 可以在不使用 `形式化变量` 的情况下, 表示 `函数`.
每个 `术语` 都是 `函数`, 只有一个 `二元` 操作, 即 `应用`(application).

+ 基本组合子:
    + `K` 组合子性质为: `K.x.y -> x`
    + `S` 组合子性质为: `S.x.y.z -> x.z.(y.z)`

+ `B` 组合子性质为: `B.x.y.z -> x.(y.z)`.
+ `C` 组合子性质为: `C.x.y.z -> x.y.z`.

使用 `CurryApplied` 建立 `K` 和 `S` 组合子(combinators):

```mathematica
k = CurryApplied[Identity, 2 -> {1}];
s = CurryApplied[Function[#1[#3][#2[#3]]], 3];
```

组合 `SKK` 和 `SKS` 等价于 `恒元`:

KK->K
SKK.z-> K.z.(K.z) -> z
SKS-> K.z(S.z)->z

```mathematica
s[k][k][x]
Out[2]= x

s[k][s][x]
Out[3]= x
```

用 `S` 和 `K` 建立 `B` 和 `C` 组合子:

```mathematica
b = s[k[s]][k];
c = s[s[k[s[k[s][k]]][s]][k[k]]];

b[f][g][x]
Out[5]= f[g[x]]

c[f][x][y]
Out[6]= f[x][y]
```

### 性质和关系

`CurryApplied[f,arity]` 表示相同的算符, 和 `OperatorApplied[f,arity]`:

```mathematica
CurryApplied[f, 3][a][b][c]
Out[1]= f[a, b, c]

OperatorApplied[f, 3][a][b][c]
Out[2]= f[a, b, c]
```

`CurryApplied[n][f]` 等价于 `CurryApplied[f,n]`:

```mathematica
b = s[k[s]][k];
c = s[s[k[s[k[s][k]]][s]][k[k]]];

b[f][g][x]
Out[5]= f[g[x]]

c[f][x][y]
Out[6]= f[x][y]
```

`OperatorApplied[f]` 等价于 `OperatorApplied[f,{2,1}]`:

```mathematica
OperatorApplied[f][x][y]
Out[3]= f[y, x]

OperatorApplied[f, {2, 1}][x][y]
Out[4]= f[y, x]
```

对于 `零参数` 函数, `CurryApplied[f,0]` 返回 `f[]`:

```mathematica
CurryApplied[f, 0]
Out[1]= f[]
```

如果提供了 `额外参数`, 仍然会插入一对 `空括号`(brackets):

```mathematica
CurryApplied[f, 0][a, b]

Out[2]= f[][a, b]
```

科里 `CurryApplied` 它自己:

```mathematica
CurryApplied[2][CurryApplied][f][3][a][b][c]
Out[1]= f[a, b, c]
```

和 `Construct` 比较:

```mathematica
Construct[f, a, b, c]
Out[2]= f[a, b, c]
```

对于正整数 `n`, `CurryApplied[Construct,n][f]` 等价于 `CurryApplied[f,n-1]`:

```mathematica
CurryApplied[Construct, 3][f][a][b][c]

Out[1]= f[a, b][c]
```

此关系对 `n=1` 也成立:

```mathematica
CurryApplied[Construct, 1][f]
Out[3]= f[]

CurryApplied[f, 0]
Out[4]= f[]
```

复合 两个 `CurryApplied` 算符, 以及置换和 permutation and its inverse:

```mathematica
perm = {2, 4, 1, 3}
invperm = InversePermutation[perm]
```

以下结果等同于, 使用 `CurryApplied` 而不对参数进行重新排序:

```mathematica
CurryApplied[CurryApplied[f, perm], invperm][a][b][c][d]
Out[2]= f[a, b, c, d]

CurryApplied[f, 4][a][b][c][d]
Out[3]= f[a, b, c, d]
```

取两个相同长度的 `排列组合` 列表:

```mathematica
perm1 = {2, 4, 1, 3};
perm2 = {3, 2, 4, 1};
```

复合相应的 `CurryApplied` 算符:

```mathematica
CurryApplied[CurryApplied[f, perm1], perm2][a][b][c][d]
Out[2]= f[b, a, c, d]
```

或者, 使用 `CurryApplied` 与它们的互换乘积, 按相同的次序:

```mathematica
CurryApplied[f, PermutationProduct[perm1, perm2]][a, b, c, d]
Out[3]= f[b, a, c, d]
```
