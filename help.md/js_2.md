# javascript_2

## 面向对象编程

`JavaScript`的所有数据都可以看成对象, 那是不是我们已经在使用`面向对象`编程了呢? 
当然不是. 如果我们只使用 `Number`, `Array`, `string` 以及基本的 `{...}` 定义的对象, 还无法发挥出面向对象编程的威力.

`JavaScript`的面向对象编程和大多数其他语言如 `Java`, `C#` 的面向对象编程都不太一样.
如果你熟悉 `Java` 或 `C#` , 很好, 你一定明白面向对象的两个基本概念:

+ `类`: `类`是`对象`的类型模板. 例如, 定义 `Student` 类来表示学生, `类`本身是一种`类型`, `Student` 表示学生类型, 但不表示任何具体的某个学生;
+ `实例`: `实例`是根据`类`创建的`对象`. 例如, 根据 `Student` `类可以创建出xiaoming`, `xiaohong`, `xiaojun` 等多个实例.
每个实例表示一个具体的学生, 他们全都属于 `Student` 类型.

所以, `类`和`实例`是大多数面向对象编程语言的基本概念.

不过, 在`JavaScript`中, 这个概念需要改一改. `JavaScript`不区分`类`和`实例`的概念, 而是通过`原型`(`prototype`)来实现面向对象编程.

`原型`是指当我们想要创建 `xiaoming` 这个具体的学生时, 我们并没有一个 `Student` 类型可用. 那怎么办? 
如果恰好有这么一个现成的对象:

```js
var robot = {
    name: 'Robot',
    height: 1.6,
    run: function () {
        console.log(this.name + ' is running...');
    }
};
```

我们看这个 `robot` 对象有名字, 有身高, 还会跑, 有点像小明, 干脆就根据它来 `创建`小明得了!

于是我们把它改名为 `Student`, 然后创建出 `xiaoming`:

```js
var Student = {
    name: 'Robot',
    height: 1.2,
    run: function () {
        console.log(this.name + ' is running...');
    }
};
//创建一个小明对象
var xiaoming = {
    name: '小明'
};
xiaoming.__proto__ = Student; //给出小明的原型
```

注意最后一行代码把`xiaoming`的原型指向了对象`Student`, 看上去`xiaoming`仿佛是从`Student`继承下来的:

```js
xiaoming.name; // '小明'
xiaoming.run(); // 小明 is running...
```

`xiaoming`有自己的`name`属性, 但并没有定义`run()`方法.
不过, 由于小明是从`Student`继承而来, 只要`Student`有`run()`方法, `xiaoming`也可以调用:

![xiaoming-prototype](https://static.liaoxuefeng.com/files/attachments/1024674367146144/l)

`JavaScript`的原型链和`Java`的`Class`区别就在: 它没有"Class"的概念, 所有对象都是`实例`,
所谓`继承`关系不过是把一个`对象`的 `原型`, 通过 `__proto__ ` 属性指向另一个`对象`而已.

如果你把`xiaoming`的`原型`指向其他对象:

```js
var Bird = {
    fly: function () {
        console.log(this.name + ' is flying...');
    }
};
xiaoming.__proto__ = Bird;
```

现在`xiaoming`已经无法`run()`了, 他已经变成了一只鸟:

```js
xiaoming.fly(); // 小明 is flying...
```

在 `JavaScrip` 代码运行时期, 你可以把`xiaoming`从`Student`变成`Bird`, 或者变成任何对象.

请注意, 上述代码仅用于演示目的.
在编写`JavaScript`代码时, 不要直接用`obj.__proto__`去改变一个对象的原型, 并且, 低版本的IE也无法使用`__proto__`.
`Object.create()`方法可以传入一个`prototype`对象, 并基于该`原型`创建`新对象`, 但是新对象什么`属性`都没有.
因此, 我们可以编写一个函数来创建`xiaoming`:

```js
// 原型对象:
var Student = {
    name: 'Robot',
    height: 1.2,
    run: function () {
        console.log(this.name + ' is running...');
    }
};
function createStudent(name) {
    var s = Object.create(Student); // 基于Student原型创建一个新对象:
    s.name = name; // 初始化新对象:
    return s;
}
var xiaoming = createStudent('小明'); //创建小明对象
xiaoming.run(); // 小明 is running...
xiaoming.__proto__ === Student; // true
```

这是创建`原型继承`的一种方法, `JavaScript`还有其他方法来创建对象, 我们在后面会一一讲到.

### 创建对象

`JavaScript` 对每个创建的对象都会设置`原型`, 指向它的原型对象.

当我们用`obj.xxx`访问`对象`的`属性`时, `JavaScript`引擎先在当前对象上查找该`属性`,
如果没有找到, 就到其`原型对象`上找, 如果还没有找到, 就一直上溯到`Object.prototype`对象, 最后如果还没有找到, 就只能返回`undefined`.

例如, 创建一个`Array`对象:

```js
var arr = [1, 2, 3];
```

其原型链是:

```js
arr ----> Array.prototype ----> Object.prototype ----> null
```

`Array.prototype`定义了`indexOf()`, `shift()` 等方法, 因此你可以在所有的`Array`对象上直接调用这些方法.

当我们创建一个函数时:

```js
function foo() {
    return 0;
}
```

函数也是一个对象, 它的原型链是:

```js
foo ----> Function.prototype ----> Object.prototype ----> null
```

由于 `Function.prototype` 定义了  `apply()` 等方法, 因此, 所有函数都可以调用 `apply()` 方法.

很容易想到, 如果原型链很长, 那么访问对象的`属性`就会因为花更多的时间查找而变得更慢, 因此要注意不要把原型链搞得太长.

#### 构造函数

除了直接用 `{ ... }` 创建一个对象外, `JavaScript` 还可以用`构造函数`的方法来创建对象. 它的用法是, 先定义`构造函数`:

```js
function Student(name) {
    this.name = name;
    this.hello = function () {
        alert('Hello, ' + this.name + '!');
    }
}
```

你会问, 咦, 这不是个普通函数吗? 
这确实是个普通函数, 但是在`JavaScript`中, 可以用关键字`new`来调用这个函数, 并返回对象:

```js
var xiaoming = new Student('小明');
xiaoming.name; // '小明'
xiaoming.hello(); // Hello, 小明!
```

注意, 如果不写`new`, 这就是一个普通函数, 它返回`undefined`. 
但是, 如果写了`new`, 它就变成了一个构造函数, 它绑定的`this`指向新创建的对象, 并默认返回`this`, 也就是说, 不需要在最后写`return this;`.

新创建的`xiaoming`的`原型链`是:

```js
xiaoming ----> Student.prototype ----> Object.prototype ----> null
```

也就是说, `xiaoming`的原型指向函数`Student`的原型. 如果你又创建了`xiaohong`, `xiaojun`, 那么这些对象的原型与`xiaoming`是一样的:

```js
xiaoming ↘
xiaohong -→ Student.prototype ----> Object.prototype ----> null
xiaojun  ↗
```

用`new Student()`创建的对象还从原型上获得了`constructor`属性, 它指向函数`Student`本身:

```js
xiaoming.constructor === Student.prototype.constructor; // true
Student.prototype.constructor === Student; // true
Object.getPrototypeOf(xiaoming) === Student.prototype; // true
xiaoming instanceof Student; // true
```

看晕了吧? 用一张图来表示这些乱七八糟的关系就是:

![protos](https://static.liaoxuefeng.com/files/attachments/1024698721053600/l)

红色箭头是`原型链`. 注意, `Student.prototype`框框指向的对象就是 `xiaoming`, `xiaohong` 的`原型对象`, 
这个`原型对象`自己还有个属性`constructor`, 指向`Student`函数本身.

另外, `函数 Student`恰好有个属性`prototype`指向`xiaoming`, `xiaohong` 的原型对象, `但是xiaoming`, `xiaohong` 这些`对象`可没有`prototype`这个属性, 不过可以用`__proto__`这个非标准用法来查看.

现在我们就认为 `xiaoming`, `xiaohong` 这些对象`继承`自 `Student`.

不过还有一个小问题, 注意观察:

```js
xiaoming.name; // '小明'
xiaohong.name; // '小红'
xiaoming.hello; // function: Student.hello()
xiaohong.hello; // function: Student.hello()
xiaoming.hello === xiaohong.hello; // false
```

`xiaoming` 和 `xiaohong` 各自的`name`不同, 这是对的, 否则我们无法区分谁是谁了.
`xiaoming` 和 `xiaohong` 各自的`hello`是一个函数, 但它们是两个不同的函数, 虽然函数名称和代码都是相同的!

如果我们通过`new Student()`创建了很多对象, 这些对象的`hello`函数实际上只需要共享同一个函数就可以了, 这样可以节省很多内存.
要让创建的对象共享一个`hello`函数, 根据对象的属性查找原则, 我们只要把`hello`函数移动到`xiaoming`, `xiaohong` 这些对象共同的`原型`上就可以了, 也就是 `Student.prototype`:

![protos2](https://static.liaoxuefeng.com/files/attachments/1024700039819712/l)

修改代码如下:

```js
function Student(name) {
    this.name = name;
}

Student.prototype.hello = function () {
    alert('Hello, ' + this.name + '!');
};
```

用 `new` 创建基于原型的`JavaScript`的对象就是这么简单!

#### 忘记写new怎么办

如果一个函数被定义为用于创建对象的`构造函数`, 但是调用时忘记了写`new`怎么办? 

在`strict`模式下, `this.name = name`将报错, 因为 `this` 绑定为 `undefined`, 
在非 `strict` 模式下, `this.name = name` 不报错, 因为`this`绑定为 `window`, 于是无意间创建了全局变量 `name`, 并且返回 `undefined`, 这个结果更糟糕.

所以, 调用`构造函数`千万不要忘记写`new.` 为了区分普通函数和`构造函数`, 按照约定, `构造函数`首字母应当大写, 而普通函数首字母应当小写, 
这样, 一些语法检查工具如`jslint`将可以帮你检测到漏写的`new.`

最后, 我们还可以编写一个`createStudent()`函数, 在内部封装所有的`new`操作. 一个常用的编程模式像这样:

```js
function Student(props) {
    this.name = props.name || '匿名'; // 默认值为'匿名'
    this.grade = props.grade || 1; // 默认值为1
}
Student.prototype.hello = function () {
    alert('Hello, ' + this.name + '!');
};
function createStudent(props) {
    return new Student(props || {})
}
```

这个`createStudent()`函数有几个巨大的优点: 一是不需要`new`来调用, 二是参数非常灵活, 可以不传, 也可以这么传:

```js
var xiaoming = createStudent({
    name: '小明'
});
xiaoming.grade; // 1
```

如果创建的对象有很多属性, 我们只需要传递需要的某些属性, 剩下的属性可以用`默认值`. 
由于参数是一个`Object`, 我们无需记忆参数的顺序. 如果恰好从`JSON`拿到了一个对象, 就可以直接创建出`xiaoming`.

#### 练习

请利用构造函数定义`Cat`, 并让所有的`Cat`对象有`name`属性, 并共享方法`say()`, 返回字符串`'Hello, xxx!'`: 

```js
'use strict';
function Cat(name) {
    //
}
// 测试:
var kitty = new Cat('Kitty');
var doraemon = new Cat('哆啦A梦');
if (kitty && kitty.name === 'Kitty'
    && kitty.say
    && typeof kitty.say === 'function'
    && kitty.say() === 'Hello, Kitty!'
    && kitty.say === doraemon.say
) {
    console.log('测试通过!');
} else {
    console.log('测试失败!');
}
```

### 原型&原型链

[JS原型&原型链](https://segmentfault.com/a/1190000021232132)

原型链的图示:

![JS原型&原型链](https://image-static.segmentfault.com/186/543/1865431362-c3d090d5d00755cf_fix732)

图中`Parent`是构造函数, `p1`是通过 `Parent` 构造的`对象`.
如果你看到这张图一脸懵, 不要怕, 往下看, 下面会一步一步教你认识`原型`&`原型链`

#### 前置知识

+ 想要弄清楚原型和原型链, 这几个属性必须要搞清楚; `__proto__`, `prototype`,  `constructor`.
+ 其次要知道`js`中`对象`和`函数`的关系, `函数`其实是`对象`的一种.
+ `函数` 和 `构造函数`的关系; 任何`函数`都可以作为`构造函数`, 但是它需要被`new`关键字调用. 如:

  ```js
  //定义函数, 现在它只是一个普通函数
  var Parent = function(){ ... }
  // 通过new关键字调用Parent, Parent 变成对象 p1 的构造函数
  var p1 = new Parent();
  ```

关于三个属性, `__proto__`, `prototype`,  `constructor`, 我们记住两点

1. `__proto__`,  `constructor` 属性是`对象`所独有的;
1. `prototype` 属性是`函数`独有的;
2. `js`中`函数`也是`对象`的一种, 所以`函数`同样也有属性`__proto__`,  `constructor`;

下面开始进入正题, 将上面的一张图拆分成`3`张图, 分别讲解对应的`3`个属性.

#### prototype属性

为了方便举例, 我们在这模拟一个场景. `父类`比作`师父`, `子类`比作`徒弟`.
师父收徒弟, 徒弟还可以收徒孙. 徒弟可以得到师父传授的武功, 然后徒弟再传给徒孙.

师父想要传授给徒弟们的武功就放到`prototype`这个 "琅嬛福地" 中. 徒弟徒孙们就去这里学习武功.

`prototype`属性可以看成是特殊的存储空间, 存储了供`徒弟`, `徒孙`们使用的`方法`和`属性`.

![prototype属性.png](https://image-static.segmentfault.com/323/548/323548351-1642fae65598fc65_fix732)

它是`函数`独有的`属性`.
从图中可以看到它从`Parent()`函数指向`Parent.prototype`对象, 说明后者是前者的`原型对象`(英文名称就是这个意思).
``Parent.prototype``也是当前函数创建的`实例`的`原型对象`.

`prototype`设计之初就是为了实现`继承,` 让由特定函数创建的所有实例共享`属性`和`方法`, 或者说让某`构造函数`创造出的所有`对象`可以找到`公共`的方法和属性.

有了`prototype`我们不需要为每一个实例创建重复的`属性`和`方法`.
而是将它们放到`构造函数`的`prototype`这个属性上, `prototype`本身也是一个对象.
那些不需要共享的才放在`构造函数`中.

继续引用上面的代码, 当我们想为`Parent`创建的所有实例添加共享的`属性`时,

```js
Parent.prototype.name = "我是原型属性, 所有实例都可以读取到我";
```

这就是`原型属性`, 当然你也可以添加`原型方法`.
那问题来了, `p1`怎么知道他的`原型对象`上有这个方法呢, 往下看.

#### proto属性

`__proto__` 属性相当于通往 `prototype` 这个"琅嬛福地" 唯一的路(指针).
让`徒弟`, `徒孙` 们找到自己的 `师父`, `师父的师父` 提供给自己的`方法`和`属性`.

![proto属性.png](https://image-static.segmentfault.com/267/800/2678004328-ae93e0e517266b2a_fix732)

`__proto__`属性是`对象`(包括`函数`)独有的.
从图中可以看到`__proto__`属性从某个对象指向它的`原型对象`, 显然它的含义就是告诉我们对象的`原型对象`是谁.

`prototype`部分我们说到,
`Parent.prototype` 对象拥有的`属性`和`方法`叫做`原型属性`和`原型方法`, `Parent`函数构造的`实例`都可以访问或调用.
`实例`能够找到`原型属性`和`原型方法`, 靠的就是它的 `__proto__`属性.

每个`对象`都有`__proto__`属性, 该`属性`指出所属对象的`prototype`.

```js
p1.__proto__ === Parent.prototype; // true
```

`__proto__`通常称为`隐式原型`, `prototype` 通常称为`显式原型`.
我们可以说, 对象的`隐式原型`指向了, 该对象的`构造函数`的`显式原型`.

那么我们在`显式原型`上定义的`属性`和`方法`, 通过`隐式原型`传递给了`构造函数`的`实例`.
这样一来`实例`就能很容易的访问到构造函数`prototype`上的`方法`和`属性`了.

我们之前说过`__proto__`属性是`对象`(包括函数)独有的.
那么`Parent.prototype`也是对象, 它有`隐式原型`么, 又指向谁? 

```js
Parent.prototype.__proto__ === Object.prototype; //true
```

可以看到, `构造函数`的`prototype`对象的`隐式原型`属性指向了`Object`的`原型对象`.
那么`Parent`的原型对象就继承自`Object`的原型对象.
由此我们可以验证一个结论, 万物继承自`Object.prototype`.
这也就是为什么我们可以构造一个`对象`, 并且可以调用该对象上没有的`属性`和`方法了`. 例如:

```js
//我们并没有在Parent中定义任何方法属性, 但是我们可以调用
p1.toString(); // hasOwnProperty 等等的一些方法
```

这些没有在构造函数中定义的方法是哪来的呢? 现在引出`原型链`的概念.

+ 当我们调用`p1.toString()`的时候, 先在`p1`对象本身寻找.
+ 若没有找到, 则通过`p1.__proto__`找到了原型对象`Parent.prototype`,
+ 若还没有找到, 再通过`Parent.prototype.__proto__`找到了上一层原型对象`Object.prototype`. 比如在这一层找到了`toString`方法. 返回该方法供`p1`使用.

当然如果找到`Object.prototype`上也没找到, 就在`Object.prototype.__proto__`中寻找.
但是`Object.prototype.__proto__ === null`所以就返回`undefined`.
这就是为什么当访问对象中一个不存在的属性时, 返回`undefined`了.

#### constructor属性

`constructor`属性是让`徒弟`, `徒孙` 们知道是谁创造了自己.
这里可不是`师父`..., 而是自己的`父母`, `父母`创造了自己, `父母`又是由上一辈人创造的, …… 追溯到头就是`Function()`(女娲).

![constructor属性.png](https://image-static.segmentfault.com/485/683/48568374-4a2ca8b9a839496e_fix732)

`constructor` 是对象才有的属性.
从图中看到它是从`对象`指向`函数`, 也就是自己的`构造函数`.
每个对象都有`构造函数`, 好比我们上面的代码 `p1` 就是一个对象, 那`p1`的构造函数是谁呢? 我们打印一下.

```js
console.log(p1.constructor); // Parent(){}
```

通过输出结果看到, 很显然是`Parent`函数.
我们有说过函数也是对象, 那`Parent`函数是不是也有`构造函数`呢? 显然是有的. 再次打印下.

```js
console.log(Parent.constructor); // Function() { [native code] }
```

通过输出看到`Parent`函数的构造函数是`Function()`.
这并不奇怪, 我们每次定义函数其实就是调用`new Function()`, 下面两种用法的效果相同:

```js
var fn1 = new Function('msg','alert(msg)');
function fn1(msg){ // 参数
    alert(msg); // 定义
}
```

那么我们再回来看下, 再次打印`Function.constructor`

```js
console.log(Function.constructor); //  Function() { [native code] }
```

可以看到`Function`的`构造函数`就是它本身, 我们也就可以说`Function`是所有函数的`根构造函数`.

到这里我们已经对`constructor`属性有了初步的认识, 它引用`对象`的构造函数.

### 原型继承

[构造函数的继承](http://www.ruanyifeng.com/blog/2010/05/object-oriented_javascript_inheritance.html)

在传统的基于`Class`的语言如 `Java`, `C++` 中, 继承的本质是扩展一个已有的`Class`, 并生成新的 `Subclass`.

由于这类语言严格区分`类`和`实例`, `继承`实际上是`类型`的扩展.
但是, 由于 `JavaScript`采用原型继承, 我们无法直接扩展一个`Class`, 因为根本不存在`Class`这种类型(Type).

但是办法还是有的. 我们先回顾`Student`构造函数:

```js
function Student(props) {
    this.name = props.name || 'Unnamed';
}

Student.prototype.hello = function () {
    alert('Hello, ' + this.name + '!');
}
```

以及`Student`的原型链:

![js-proto](https://static.liaoxuefeng.com/files/attachments/1034288810160288/l)

现在, 我们要基于 `Student` 扩展出`PrimaryStudent`, 可以先定义出`PrimaryStudent`:

```js
function PrimaryStudent(props) {
    Student.call(this, props);// 调用Student构造函数, 绑定this变量:
    this.grade = props.grade || 1; //添加其他的属性
}
```

但是, 调用了 `Student` 构造函数不等于继承了 `Student`, `PrimaryStudent` 创建的对象的`prototype`是:

    new PrimaryStudent() ----> PrimaryStudent.prototype ----> Object.prototype ----> null

必须想办法把`原型链`修改为:

    new PrimaryStudent() ----> PrimaryStudent.prototype ----> Student.prototype ----> Object.prototype ----> null

这样, `原型链`对了, `继承关系`就对了.
新的基于 `PrimaryStudent` 创建的对象不但能调用 `PrimaryStudent.prototype` 中定义的方法,
也可以调用 `Student.prototype` 定义的方法.

如果你想用最简单粗暴的方法这么干:

```js
PrimaryStudent.prototype = Student.prototype;
```

是不行的!如果这样的话,  `PrimaryStudent` 和 `Student` 共享一个原型对象, 那还定义 `PrimaryStudent` 干啥? 

我们可以借助`中间对象`来实现正确的`原型链`, 这个中间对象的`原型`要指向`Student.prototype`.
为了实现这一点, 参考`道爷`(就是发明`JSON`的那个道格拉斯)的代码, `中间对象`可以用`空函数F`来实现:

```js
// PrimaryStudent构造函数:
function PrimaryStudent(props) {
    Student.call(this, props);
    this.grade = props.grade || 1;
}
// 空函数F:
function F() {
}
// 把F的原型指向Student.prototype:
F.prototype = Student.prototype;
// 把PrimaryStudent的原型指向一个新的F对象, F对象的原型正好指向Student.prototype:
PrimaryStudent.prototype = new F();
// 把PrimaryStudent原型的构造函数修复为PrimaryStudent:
PrimaryStudent.prototype.constructor = PrimaryStudent;
// 继续在PrimaryStudent原型, 也就是new F()对象上定义方法:
PrimaryStudent.prototype.getGrade = function () {
    return this.grade;
};

// 创建xiaoming:
var xiaoming = new PrimaryStudent({
    name: '小明',
    grade: 2
});
xiaoming.name; // '小明'
xiaoming.grade; // 2
// 验证原型:
xiaoming.__proto__ === PrimaryStudent.prototype; // true
xiaoming.__proto__.__proto__ === Student.prototype; // true
// 验证继承关系:
xiaoming instanceof PrimaryStudent; // true
xiaoming instanceof Student; // true
```

用一张图来表示新的`原型链`:

![js-proto-extend](https://static.liaoxuefeng.com/files/attachments/1034288859918112/l)

注意, `函数F`仅用于桥接, 我们仅创建了一个`new F()`实例, 而且没有改变原有的 `Student` 定义的`原型链`.

这里解释下为什么要用`空函数F`做桥接. 如果你用 `Student` 的实例作继承, 例如:

    PrimaryStudent.prototype = new Student("Ming");

`PrimaryStudent`的`prototype`上会额外继承到`Student`中定义的`属性`和`方法`,
就是`Student`的`构造函数`定义在`this`下的`属性`和`方法`,
这样的原型继承不纯粹, 重复占用空间存储相同的定义, 而空函数`F`基本不会占用额外空间.

我们可以把`继承`这个动作用函数 `inherits()` 封装起来, 这样可以隐藏`F`的定义, 并简化代码:

```js
function inherits(Child, Parent) { // 子类, 父类
    var F = function () {}; // 桥接函数
    F.prototype = Parent.prototype; // 桥接父类
    Child.prototype = new F(); //桥接子类
    Child.prototype.constructor = Child; //修复构造函数指向
}
```

这个`inherits()`函数可以复用:

```js
function Student(props) {
    this.name = props.name || 'Unnamed';
}// 定义父类
Student.prototype.hello = function () {
    alert('Hello, ' + this.name + '!');
}// 父类公共方法
function PrimaryStudent(props) {
    Student.call(this, props);
    this.grade = props.grade || 1;
}// 定义子类
// 重塑子类的原型继承链:
inherits(PrimaryStudent, Student);
// 绑定其他方法到PrimaryStudent原型:
PrimaryStudent.prototype.getGrade = function () {
    return this.grade;
};
```

#### 小结

`JavaScript`的`原型继承`实现方式就是:

+ 定义新的`构造函数`, 并在内部用`call()`调用希望`继承`的构造函数, 并绑定`this`;
+ 借助`中间函数F`实现原型`链继承`, 最好通过封装的`inherits`函数完成;
+ 继续在新的`构造函数`的`prototype`上定义新方法.

### class继承

在上面的章节中我们看到了`JavaScript`的对象模型是基于`原型`实现的.
特点是简单, 缺点是理解起来比传统的类－实例模型要困难, 最大的缺点是继承的实现需要编写大量代码, 并且需要正确实现原型链. 

有没有更简单的写法? 有! 

新的关键字`class`从`ES6`开始正式被引入到`JavaScript`中. `class`的目的就是让定义类更简单. 

我们先回顾用函数实现`Student`的方法: 

```js
function Student(name) {
    this.name = name;
}

Student.prototype.hello = function () {
    alert('Hello, ' + this.name + '!');
}
```

如果用新的`class`关键字来编写`Student`, 可以这样写: 

```js
class Student {
    constructor(name) {
        this.name = name;
    }
    hello() {
        alert('Hello, ' + this.name + '!');
    }
}
```

比较一下就可以发现, `class`的定义包含了构造函数`constructor`和定义在原型对象上的函数`hello()`.
(注意没有`function`关键字), 这样就避免了`Student.prototype.hello = function () {...}`这样分散的代码. 

最后, 创建一个`Student`对象代码和前面章节完全一样: 

```js
var xiaoming = new Student('小明');
xiaoming.hello();
```

#### class继承

用`class`定义对象的另一个巨大的好处是继承更方便了. 想一想我们从`Student`派生一个`PrimaryStudent`需要编写的代码量. 
现在, 原型`继承`的中间对象, 原型对象的`构造函数`等等都不需要考虑了, 直接通过`extends`来实现: 

```js
class PrimaryStudent extends Student {
    constructor(name, grade) {
        super(name); // 记得用super调用父类的构造方法!
        this.grade = grade;
    }
    myGrade() {
        alert('I am at grade ' + this.grade);
    }
}
```

注意`PrimaryStudent`的定义也是`class`关键字实现的, 而`extends`则表示原型链对象来自`Student`. 
子类的构造函数可能会与父类不太相同, 例如, `PrimaryStudent`需要`name`和`grade`两个参数, 
并且需要通过`super(name)`来调用父类的构造函数, 否则父类的`name`属性无法正常初始化. 

`PrimaryStudent`已经自动获得了父类`Student`的`hello`方法, 我们又在子类中定义了新的`myGrade`方法. 

`ES6`引入的`class`和原有的`JavaScript`原型继承有什么区别呢? 
实际上它们没有任何区别, `class`的作用就是让`JavaScript`引擎去实现原来需要我们自己编写的原型链代码. 
简而言之, 用`class`的好处就是极大地简化了原型链代码. 

你一定会问, `class` 这么好用, 能不能现在就用上? 
现在用还早了点, 因为不是所有的主流浏览器都支持`ES6`的`class`. 
如果一定要现在就用上, 就需要一个工具把`class`代码转换为传统的`prototype`代码, 可以试试`Babel`这个工具. 

#### 练习

请利用`class`重新定义`Cat`, 并让它从已有的`Animal`继承, 然后新增一个方法`say()`, 返回字符串`'Hello, xxx!'`: 

```js
'use strict';
class Animal {
    constructor(name) {
        this.name = name;
    }
}
class Cat ???
// 测试:
var kitty = new Cat('Kitty');
var doraemon = new Cat('哆啦A梦');
if ((new Cat('x') instanceof Animal)
    && kitty 
    && kitty.name === 'Kitty'
    && kitty.say
    && typeof kitty.say === 'function'
    && kitty.say() === 'Hello, Kitty!'
    && kitty.say === doraemon.say)
{
    console.log('测试通过!');
} else {
    console.log('测试失败!');
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
