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

## 浏览器

不同的浏览器对 `JavaScript` 支持的差异主要是,有些`API`的接口不一样,比如 `AJAX`,`File` 接口.对于 `ES6` 标准,不同的浏览器对各个特性支持也不一样.
在编写 `JavaScript` 的时候,就要充分考虑到浏览器的差异,尽量让同一份 `JavaScript` 代码能运行在不同的浏览器中.

### 浏览器对象

`JavaScript`可以获取浏览器提供的很多对象,并进行操作.

#### window

`window`对象不但充当`全局作用域`,而且表示`浏览器窗口`.

`window`对象有`innerWidth`和`innerHeight`属性,可以获取浏览器窗口的`内部宽度`和`高度`. 
`内部宽高`是指除去`菜单栏`,`工具栏`,`边框`等占位元素后,用于显示网页的净宽高.
兼容性: `IE<=8`不支持.

```js
// 可以调整浏览器窗口大小试试:
console.log('window inner size: ' + window.innerWidth + ' x ' + window.innerHeight);
```

对应的,还有一个`outerWidth`和`outerHeight`属性,可以获取浏览器窗口的整个宽高.

#### navigator

`navigator` 对象表示浏览器的信息,最常用的属性包括: 

+ `navigator.appName` ;  浏览器名称;
+ `navigator.appVersion` ;  浏览器版本;
+ `navigator.language` ;  浏览器设置的语言;
+ `navigator.platform` ;  操作系统类型;
+ `navigator.userAgent` ;  浏览器设定的User-Agent字符串.

例如: 

```js
'use strict';
console.log('appName = ' + navigator.appName);
console.log('appVersion = ' + navigator.appVersion);
console.log('language = ' + navigator.language);
console.log('platform = ' + navigator.platform);
console.log('userAgent = ' + navigator.userAgent);
```

请注意, `navigator` 的信息可以很容易地被用户修改,所以`JavaScript`读取的值不一定是正确的.很多初学者为了针对不同浏览器编写不同的代码,喜欢用`if`判断浏览器版本,例如: 

```js
var width;
if (getIEVersion(navigator.userAgent) < 9) {
    width = document.body.clientWidth;
} else {
    width = window.innerWidth;
}
```

但这样既可能判断不准确,也很难维护代码. 正确的方法是充分利用`JavaScript`对不存在属性返回`undefined`的特性,直接用短路运算符`||`计算: 

```js
var width = window.innerWidth || document.body.clientWidth;
```

#### screen

`screen` 对象表示屏幕的信息,常用的属性有: 

+ `screen.width` ; 屏幕宽度,以`像素`为单位;
+ `screen.height` ; 屏幕高度,以`像素`为单位;
+ `screen.colorDepth` ; 返回颜色位数, 如`8`,`16`,`24`   .

```js
'use strict';
console.log('Screen size = ' + screen.width + ' x ' + screen.height);
```

#### location

`location` 对象表示当前页面的`URL`信息.例如,一个完整的`URL`: 

    http://www.example.com:8080/path/index.html?a=1&b=2#TOP

可以用 `location.href` 获取.要获得`URL`各个部分的值,可以这么写: 

```js
location.protocol; // 'http'
location.host; // 'www.example.com'
location.port; // '8080'
location.pathname; // '/path/index.html'
location.search; // '?a=1&b=2'
location.hash; // 'TOP'
```

要加载一个新页面,可以调用`location.assign()`. 如果要重新加载当前页面,调用`location.reload()`方法非常方便.

```js
'use strict';
if (confirm('重新加载当前页' + location.href + '?')) {
    location.reload();
} else {
    location.assign('/'); // 设置一个新的URL地址
}
```

#### document

+ `document`对象表示当前页面. 由于`HTML`在浏览器中以`DOM`形式表示为树形结构,`document`对象就是整个`DOM`树的根节点.
+ `document`的`title`属性是从`HTML`文档中的`<title>xxx</title>`读取的,但是可以动态改变: 

```js
'use strict';
document.title = '努力学习JavaScript!';
```

请观察浏览器窗口标题的变化.

要查找`DOM`树的某个节点,需要`从document`对象开始查找.最常用的查找是根据`ID`和`Tag Name`.
我们先准备`HTML`数据: 

```js
<dl id="drink-menu" style="border:solid 1px #ccc;padding:6px;">
    <dt>摩卡</dt>
    <dd>热摩卡咖啡</dd>
    <dt>酸奶</dt>
    <dd>北京老酸奶</dd>
    <dt>果汁</dt>
    <dd>鲜榨苹果汁</dd>
</dl>
```

用`document`对象提供的`getElementById()`和`getElementsByTagName()`可以按`ID`获得一个`DOM`节点和按`Tag`名称获得一组`DOM`节点: 

```js
'use strict';
var menu = document.getElementById('drink-menu');
var drinks = document.getElementsByTagName('dt');
var i, s;
s = '提供的饮料有:';
for (i=0; i<drinks.length; i++) {
    s = s + drinks[i].innerHTML + ',';
}
console.log(s);
```

`document`对象还有一个`cookie`属性,可以获取当前页面的`Cookie`.

`Cookie`是由服务器发送的`key-value`标示符.
因为`HTTP`协议是无状态的,但是服务器要区分到底是哪个用户发过来的请求,就可以用`Cookie`来区分.
当用户成功登录后,服务器发送一个`Cookie`给浏览器,例如`user=ABC123XYZ`(加密的字符串).... 
此后,浏览器访问该网站时,会在请求头附上这个`Cookie`,服务器根据`Cookie`即可区分出用户.

`Cookie`还可以存储网站的一些设置, 例如页面显示的语言等等.

`JavaScript`可以通过`document.cookie`读取到当前页面的`Cookie`: 

```js
document.cookie; // 'v=123; remember=true; prefer=zh'
```

由于`JavaScript`能读取到页面的`Cookie`,而用户的登录信息通常也存在`Cookie`中,这就造成了巨大的安全隐患,这是因为在`HTML`页面中引入第三方的J`avaScript`代码是允许的: 

```js
<!-- 当前页面在wwwexample.com -->
<html>
    <head>
        <script src="http://www.foo.com/jquery.js"></script>
    </head>
    ...
</html>
```

如果引入的第三方的`JavaScript`中存在恶意代码,则`www.foo.com`网站将直接获取到`www.example.com`网站的用户登录信息.

为了解决这个问题,服务器在设置`Cookie`时可以使用`httpOnly`,设定了`httpOnly`的`Cookie`将不能被`JavaScript`读取.
这个行为由浏览器实现,主流浏览器均支持`httpOnly`选项,IE从IE6 SP1开始支持.
为了确保安全,服务器端在设置`Cookie`时,应该始终坚持使用`httpOnly`.

#### history

`history`对象保存了浏览器的历史记录, `JavaScript`可以调用`history`对象的`back()`或`forward ()`,相当于用户点击了浏览器的"后退"或"前进"按钮.

这个对象属于历史遗留对象,对于现代`Web`页面来说,由于大量使用`AJAX`和页面交互,简单粗暴地调用`history.back()`可能会让用户感到非常愤怒.
新手开始设计`Web`页面时喜欢在登录页登录成功时调用`history.back()`,试图回到登录前的页面.这是一种错误的方法.

任何情况,你都不应该使用`history`这个对象了.

### 操作DOM

[文档对象模型 (DOM)](https://developer.mozilla.org/zh-CN/docs/Web/API/Document_Object_Model)

文档对象模型 (`DOM`) 将 `web` 页面与脚本或编程语言连接起来.
通常是指  `JavaScript`,但将 HTML,SVG 或 XML 文档建模为对象并不是 `JavaScript` 语言的一部分.
`DOM` 模型用`逻辑树`来表示一个文档,`树`的每个分支的终点都是一个节点(`node`),每个节点都包含着对象(`objects`).
`DOM`的方法(methods)让你可以用特定方式操作这个树,用这些方法你可以改变文档的结构,样式或者内容.
节点可以关联上`事件处理器`,一旦某一事件被触发了,那些事件处理器就会被执行.

由于`HTML`文档被浏览器解析后就是一棵`DOM`树,要改变`HTML`的结构,就需要通过`JavaScript`来操作`DOM`.
始终记住`DOM`是一个树形结构.操作一个`DOM`节点实际上就是这么几个操作: 

+ 更新 ; 更新该`DOM`节点的内容,相当于更新了该`DOM`节点表示的`HTML`的内容;
+ 遍历 ; 遍历该`DOM`节点下的子节点,以便进行进一步操作;
+ 添加 ; 在该`DOM`节点下新增一个子节点,相当于动态增加了一个`HTML`节点;
+ 删除 ; 将该节点从`HTML`中删除,相当于删掉了该`DOM`节点的内容以及它包含的所有子节点.

在操作一个`DOM`节点前,我们需要通过各种方式先拿到这个`DOM`节点.
最常用的方法是`document.getElementById()`和`document.getElementsByTagName()`,以及CSS选择器`document.getElementsByClassName()`.

由于`ID`在 `HTML` 文档中是唯一的, 所以`document.getElementById()`可以直接定位唯一的一个`DOM`节点.
`document.getElementsByTagName()`和`document.getElementsByClassName()`总是返回一组`DOM`节点.要精确地选择`DOM`,
可以先定位父节点,再从父节点开始选择,以缩小范围.

例如: 

```js
// 返回ID为'test'的节点: 
var test = document.getElementById('test');
// 先定位ID为'test-table'的节点,再返回其内部所有tr节点: 
var trs = document.getElementById('test-table').getElementsByTagName('tr');
// 先定位ID为'test-div'的节点,再返回其内部所有class包含red的节点: 
var reds = document.getElementById('test-div').getElementsByClassName('red');
// 获取节点test下的所有直属子节点:
var cs = test.children;
// 获取节点test下第一个,最后一个子节点: 
var first = test.firstElementChild;
var last = test.lastElementChild;
```

第二种方法是使用`querySelector()`和`querySelectorAll()`,需要了解`selector`语法,然后使用条件来获取节点,更加方便: 

```js
// 通过querySelector获取ID为q1的节点: 
var q1 = document.querySelector('#q1');
// 通过querySelectorAll获取q1节点内的符合条件的所有节点: 
var ps = q1.querySelectorAll('div.highlighted > p');
```

注意: 低版本的`IE<8`不支持`querySelector`和`querySelectorAll`. `IE8`仅有限支持.

严格地讲,我们这里的`DOM`节点是指`Element`, 但是`DOM`节点实际上是`Node`. 
在`HTML`中, `Node`包括`Element`,`Comment`,`CDATA_SECTION`等很多种,以及根节点`Document`类型.
但是,绝大多数时候我们只关心 `Element`,也就是实际控制页面结构的 `Node`,其他类型的 `Node` 忽略即可.
根节点`Document`已经自动绑定为全局变量`document`.

#### 练习

如下的`HTML`结构: 

```js
<!-- HTML结构 -->
<div id="test-div">
<div class="c-red">
    <p id="test-p">JavaScript</p>
    <p>Java</p>
  </div>
  <div class="c-red c-green">
    <p>Python</p>
    <p>Ruby</p>
    <p>Swift</p>
  </div>
  <div class="c-green">
    <p>Scheme</p>
    <p>Haskell</p>
  </div>
</div>
```

请选择出指定条件的节点: 

```js
'use strict';
// 选择<p>JavaScript</p>:
var js = document.getElementById("test-p");
// 选择<p>Python</p>,<p>Ruby</p>,<p>Swift</p>:
var arr = document.querySelectorAll(".c-red.c-green p");
// 选择<p>Haskell</p>:
var haskell = document.querySelector("#test-div div:last-child p:last-child");
// 测试:
if (!js || js.innerText !== 'JavaScript') {
    alert('选择JavaScript失败!');
} else if (!arr || arr.length !== 3 || !arr[0] || !arr[1] || !arr[2] || arr[0].innerText !== 'Python' || arr[1].innerText !== 'Ruby' || arr[2].innerText !== 'Swift') {
    console.log('选择Python,Ruby,Swift失败!');
} else if (!haskell || haskell.innerText !== 'Haskell') {
    console.log('选择Haskell失败!');
} else {
    console.log('测试通过!');
}
```

### 更新DOM

拿到一个`DOM`节点后,我们可以对它进行更新. 可以直接修改节点的文本,方法有两种: 

一种是修改 `innerHTML` 属性,这个方式非常强大,不但可以修改一个`DOM`节点的文本内容, 还可以直接通过`HTML`片段修改`DOM`节点内部的子树: 

```js
// 获取<p id="p-id">...</p>
var p = document.getElementById('p-id');
// 设置文本为abc:
p.innerHTML = 'ABC'; // <p id="p-id">ABC</p>
// 设置HTML:
p.innerHTML = 'ABC <span style="color:red">RED</span> XYZ';
// <p>...</p>的内部结构已修改
```

用`innerHTML`时要注意,是否需要写入`HTML`.`如果写入的字符串是通过网络拿到的, 要注意对字符编码来避免`XSS`攻击.

第二种是修改`innerText`或`textContent`属性,这样可以自动对字符串进行`HTML`编码,保证无法设置任何`HTML`标签: 

```js
// 获取<p id="p-id">...</p>
var p = document.getElementById('p-id');
// 设置文本:
p.innerText = '<script>alert("Hi")</script>';
// HTML被自动编码,无法设置一个<script>节点:
// <p id="p-id">&lt;script&gt;alert("Hi")&lt;/script&gt;</p>
```

两者的区别在于读取属性时, `innerText`不返回隐藏元素的文本,而`textContent`返回所有文本.另外注意`IE<9`不支持`textContent`.

修改`CSS`也是经常需要的操作. `DOM`节点的`style`属性对应所有的`CSS`,可以直接获取或设置.
因为`CSS`允许`font-size`这样的名称,但它并非`JavaScript`有效的属性名,所以需要在`JavaScript`中改写为驼峰式命名`fontSize`: 

```js
// 获取<p id="p-id">...</p>
var p = document.getElementById('p-id');
// 设置CSS:
p.style.color = '#ff0000';
p.style.fontSize = '20px';
p.style.paddingTop = '2em';
```

#### 练习

有如下的HTML结构: 

```js
<!-- HTML结构 -->
<div id="test-div">
  <p id="test-js">javascript</p>
  <p>Java</p>
</div>
```

请尝试获取指定节点并修改: 

```js
'use strict';
// 获取<p>javascript</p>节点:
var js = document.getElementById('test-js');
// 修改文本为JavaScript:
// TODO:
js.innerHTML = 'JavaScript';
// 修改CSS为: color: #ff0000, font-weight: bold
// TODO:
js.style.color = '#ff0000';
js.style.fontWeight = 'bold';
// 测试:
if (js && js.parentNode && js.parentNode.id === 'test-div' && js.id === 'test-js') {
    if (js.innerText === 'JavaScript') {
        if (js.style && js.style.fontWeight === 'bold' && (js.style.color === 'red' || js.style.color === '#ff0000' || js.style.color === '#f00' || js.style.color === 'rgb(255, 0, 0)')) {
            console.log('测试通过!');
        } else {
            console.log('CSS样式测试失败!');
        }
    } else {
        console.log('文本测试失败!');
    }
} else {
    console.log('节点测试失败!');
}
```

### 插入DOM

当我们获得了某个`DOM`节点,想在这个`DOM`节点内插入新的`DOM`,应该如何做？

如果这个`DOM`节点是空的,例如,`<div></div>`,那么,直接使用 `innerHTML = '<span>child</span>'` 就可以修改`DOM`节点的内容, 相当于"插入"了新的`DOM`节点.
如果这个`DOM`节点不是空的,那就不能这么做,因为`innerHTML`会直接替换掉原来的所有子节点.

有两个办法可以插入新的节点.一个是使用`appendChild`,把`子节点`添加到父节点的最后一个子节点.例如: 

```js
<!-- HTML结构 -->
<p id="js">JavaScript</p>
<div id="list">
    <p id="java">Java</p>
    <p id="python">Python</p>
    <p id="scheme">Scheme</p>
</div>
```

把`<p id="js">JavaScript</p>添加到<div id="list">`的最后一项: 

```js
var
    js = document.getElementById('js'),
    list = document.getElementById('list');
list.appendChild(js);
```

现在, `HTML` 结构变成了这样: 

```js
<!-- HTML结构 -->
<div id="list">
    <p id="java">Java</p>
    <p id="python">Python</p>
    <p id="scheme">Scheme</p>
    <p id="js">JavaScript</p>
</div>
```

因为我们插入的`js`节点已经存在于当前的文档树, 因此这个节点首先会从原先的位置删除,再插入到新的位置.
更多的时候我们会从零创建一个新的节点,然后插入到指定位置: 

```js
var
    list = document.getElementById('list'),
    haskell = document.createElement('p');
haskell.id = 'haskell';
haskell.innerText = 'Haskell';
list.appendChild(haskell);
```

这样我们就动态添加了一个新的节点: 

```js
<!-- HTML结构 -->
<div id="list">
    <p id="java">Java</p>
    <p id="python">Python</p>
    <p id="scheme">Scheme</p>
    <p id="haskell">Haskell</p>
</div>
```

动态创建一个节点然后添加到`DOM`树中,可以实现很多功能.
举个例子,下面的代码动态创建了一个`<style>`节点,然后把它添加到`<head>`节点的末尾,这样就动态地给文档添加了新的`CSS`定义: 

```js
var d = document.createElement('style');
d.setAttribute('type', 'text/css');
d.innerHTML = 'p { color: red }';
document.getElementsByTagName('head')[0].appendChild(d);
```

可以在`Chrome`的控制台执行上述代码,观察页面样式的变化.

#### insertBefore

如果我们要把子节点插入到指定的位置怎么办？可以使用`parentElement.insertBefore(newElement, referenceElement);`, 子节点会插入到`referenceElement`之前.
还是以上面的`HTML`为例,假定我们要把`Haskell`插入到`Python`之前: 

```js
<!-- HTML结构 -->
<div id="list">
    <p id="java">Java</p>
    <p id="python">Python</p>
    <p id="scheme">Scheme</p>
</div>
```

可以这么写: 

```js
var
    list = document.getElementById('list'),
    ref = document.getElementById('python'),
    haskell = document.createElement('p');
haskell.id = 'haskell';
haskell.innerText = 'Haskell';
list.insertBefore(haskell, ref);
```

新的`HTML`结构如下: 

```js
<!-- HTML结构 -->
<div id="list">
    <p id="java">Java</p>
    <p id="haskell">Haskell</p>
    <p id="python">Python</p>
    <p id="scheme">Scheme</p>
</div>
```

可见,使用`insertBefore`的重点是, 拿到一个"参考子节点"的引用.很多时候,需要循环一个父节点的所有子节点,可以通过迭代children属性实现: 

```js
var
    i, c,
    list = document.getElementById('list');
for (i = 0; i < list.children.length; i++) {
    c = list.children[i]; // 拿到第i个子节点
}
```

#### 练习

对于一个已有的`HTML`结构: 

```js
<!-- HTML结构 -->
<ol id="test-list">
    <li class="lang">Scheme</li>
    <li class="lang">JavaScript</li>
    <li class="lang">Python</li>
    <li class="lang">Ruby</li>
    <li class="lang">Haskell</li>
</ol>
```

按字符串顺序重新排序`DOM`节点: 

```js
'use strict';
// sort list:
list = document.getElementById('test-list');
sort = Array.from(list.children).sort((a, b) => a.innerText > b.innerText ? 1:-1);
for (let e of sort) list.appendChild(e); // 因为插入的节点已经存在于当前的文档树, 插入相当于 `移动' 操作.
// 测试:
;(function () {
    var
        arr, i,
        t = document.getElementById('test-list');
    if (t && t.children && t.children.length === 5) {
        arr = [];
        for (i=0; i<t.children.length; i++) {
            arr.push(t.children[i].innerText);
        }
        if (arr.toString() === ['Haskell', 'JavaScript', 'Python', 'Ruby', 'Scheme'].toString()) {
            console.log('测试通过!');
        }
        else {
            console.log('测试失败: ' + arr.toString());
        }
    }
    else {
        console.log('测试失败!');
    }
})();
```

#### 删除DOM

删除一个`DOM`节点就比插入要容易得多.
要删除一个节点,首先要获得该节点本身以及它的父节点,然后,调用父节点的`removeChild`把自己删掉: 

```js
// 拿到待删除节点:
var self = document.getElementById('to-be-removed');
// 拿到父节点:
var parent = self.parentElement;
// 删除:
var removed = parent.removeChild(self);
removed === self; // true
```

注意到删除后的节点虽然不在文档树中了,但其实它还在内存中,可以随时再次被添加到别的位置.
当你遍历一个父节点的子节点并进行删除操作时,要注意, `children` 属性是一个只读属性,并且它在子节点变化时会实时更新.

例如,对于如下`HTML`结构: 

```js
<div id="parent">
    <p>First</p>
    <p>Second</p>
</div>
```

当我们用如下代码删除子节点时: 

```js
var parent = document.getElementById('parent');
parent.removeChild(parent.children[0]);
parent.removeChild(parent.children[1]); // <-- 浏览器报错
```

浏览器报错: `parent.children[1]`不是一个有效的节点.
原因就在于,当`<p>First</p>`节点被删除后, `parent.children` 的节点数量已经从`2`变为了`1`, 索引`[1]`已经不存在了.

因此,删除多个节点时,要注意`children`属性时刻都在变化.

#### 练习

```js
<!-- HTML结构 -->
<ul id="test-list">
    <li>JavaScript</li>
    <li>Swift</li>
    <li>HTML</li>
    <li>ANSI C</li>
    <li>CSS</li>
    <li>DirectX</li>
</ul>
```

把与`Web`开发技术不相关的节点删掉: 

```js
'use strict';
// TODO
var arr = ['JavaScript','HTML','CSS'], removed = []; // 存储删除的节点
var parent = document.getElementById('test-list');
Array.from(parent.children).forEach(ele => arr.includes(ele.innerText) ?{}: removed.push(parent.removeChild(ele)))
console.log(removed)
// 测试:
;(function () {
    var
        arr, i,
        t = document.getElementById('test-list');
    if (t && t.children && t.children.length === 3) {
        arr = [];
        for (i = 0; i < t.children.length; i ++) {
            arr.push(t.children[i].innerText);
        }
        if (arr.toString() === ['JavaScript', 'HTML', 'CSS'].toString()) {
            console.log('测试通过!');
        }
        else {
            console.log('测试失败: ' + arr.toString());
        }
    }
    else {
        console.log('测试失败!');
    }
})();
```

### 操作表单

