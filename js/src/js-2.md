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
这里可不是`师父`..., 而是自己的`父母`, `父母`创造了自己, `父母`又是由上一辈人创造的, ... ...  追溯到头就是`Function()`(女娲).

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

当我们获得了某个`DOM`节点,想在这个`DOM`节点内插入新的`DOM`,应该如何做?

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

可以在`Firefox`的控制台执行上述代码,观察页面样式的变化.

#### insertBefore

如果我们要把子节点插入到指定的位置怎么办?可以使用`parentElement.insertBefore(newElement, referenceElement);`, 子节点会插入到`referenceElement`之前.
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

用`JavaScript`操作表单和操作DOM是类似的,因为表单本身也是DOM树.
不过表单的`输入框`,`下拉框`等可以接收用户输入,所以用`JavaScript`来操作表单,可以获得用户输入的内容,或者对一个`输入框`设置新的内容.

`HTML`表单的输入控件主要有以下几种:

+ `文本框` ; 对应的`<input type="text">`, 用于输入文本;
+ `口令框` ; 对应的`<input type="password">`, 用于输入口令;
+ `单选框` ; 对应的`<input type="radio">`,  radio button, 用于选择一项;
+ `复选框` ; 对应的`<input type="checkbox">`,  用于选择多项;
+ `下拉框` ; 对应的`<select>`, 用于选择一项;
+ `隐藏文本` ; 对应的`<input type="hidden">`, 用户不可见, 但表单提交时会把隐藏文本发送到服务器.

#### 获取值

如果我们获得了一个`<input>`节点的引用,就可以直接调用`value`获得对应的用户输入值:

```js
// <input type="text" id="email">
var input = document.getElementById('email');
input.value; // '用户输入的值'
```

这种方式可以应用于 `text`,`password`,`hidden` 以及 `select`.
但是,对于`单选框`和`复选框`,`value`属性返回的永远是`HTML`预设的值, 而我们需要获得的实际是用户是否`勾上了`选项, 所以应该用`checked`判断:

```js
// <label><input type="radio" name="weekday" id="monday" value="1"> Monday</label>
// <label><input type="radio" name="weekday" id="tuesday" value="2"> Tuesday</label>
var mon = document.getElementById('monday');
var tue = document.getElementById('tuesday');
mon.value; // '1'
tue.value; // '2'
mon.checked; // true或者false
tue.checked; // true或者false
```

#### 设置值

设置值和获取值类似, 对于 `text`,`password`,`hidden` 以及 `select`,直接设置`value`就可以:

```js
// <input type="text" id="email">
var input = document.getElementById('email');
input.value = 'test@example.com'; // 文本框的内容已更新
```

对于单选框和复选框, 设置 `checked` 为 `true` 或 `false` 即可.

#### HTML5控件

`HTML5`新增了大量标准控件,常用的包括 `date`,`datetime`,`datetime-local`,`color` 等,它们都使用`<input>`标签:

```js
<input type="date" value="2015-07-01">
<input type="datetime-local" value="2015-07-01T02:03:04">
<input type="color" value="#ff0000">
```

不支持`HTML5`的浏览器无法识别新的控件,会把它们当做`type="text"`来显示.
支持`HTML5`的浏览器将获得格式化的字符串.例如, `type="date"`类型的`input`的`value`将保证是一个有效的`YYYY-MM-DD`格式的日期,或者`空字符串`.

#### 提交表单

最后,`JavaScript`可以以两种方式来处理表单的提交(`AJAX` 方式在后面章节介绍).
方式一是通过`<form>`元素的`submit()`方法提交一个表单,例如, 响应一个`<button>`的`click`事件,在`JavaScript`代码中提交表单:

```js
<!-- HTML -->
<form id="test-form">
    <input type="text" name="test">
    <button type="button" onclick="doSubmitForm()">Submit</button>
</form>

<script>
function doSubmitForm() {
    var form = document.getElementById('test-form');
    // 可以在此修改form的input...
    // 提交form:
    form.submit();
}
</script>
```

这种方式的缺点是扰乱了浏览器对`form`的正常提交. 浏览器默认点击`<button type="submit">`时提交表单,或者用户在最后一个输入框按回车键.

因此,第二种方式是响应`<form>`本身的`onsubmit`事件,在提交`form`时作修改:

```js
<!-- HTML -->
<form id="test-form" onsubmit="return checkForm()">
    <input type="text" name="test">
    <button type="submit">Submit</button>
</form>

<script>
function checkForm() {
    var form = document.getElementById('test-form');
    // 可以在此修改form的input...
    // 继续下一步:
    return true;
}
</script>
```

注意要`return true`来告诉浏览器继续提交, 如果`return false`, 浏览器将不会继续提交`form`,这种情况通常对应用户输入有误,提示用户错误信息后终止提交`form`.
在检查和修改`<input>`时,要充分利用`<input type="hidden">`来传递数据.

例如,很多登录表单希望用户输入用户名和`口令`,但是,安全考虑,提交表单时不传输明文`口令`,而是`口令`的`MD5`. 普通`JavaScript`开发人员会直接修改`<input>`:

```js
<!-- HTML -->
<form id="login-form" method="post" onsubmit="return checkForm()">
    <input type="text" id="username" name="username">
    <input type="password" id="password" name="password">
    <button type="submit">Submit</button>
</form>

<script>
function checkForm() {
    var pwd = document.getElementById('password');
    // 把用户输入的明文变为MD5:
    pwd.value = toMD5(pwd.value);
    // 继续下一步:
    return true;
}
</script>
```

这个做法看上去没啥问题,但用户输入了口令提交时,口令框的显示会突然从几个`*`变成`32`个`*`(因为`MD5`有`32`个字符).
要想不改变用户的输入,可以利用`<input type="hidden">`实现:

```js
<!-- HTML -->
<form id="login-form" method="post" onsubmit="return checkForm()">
    <input type="text" id="username" name="username">
    <input type="password" id="input-password">
    <input type="hidden" id="md5-password" name="password">
    <button type="submit">Submit</button>
</form>

<script>
function checkForm() {
    var input_pwd = document.getElementById('input-password');
    var md5_pwd = document.getElementById('md5-password');
    // 把用户输入的明文变为MD5:
    md5_pwd.value = toMD5(input_pwd.value);
    // 继续下一步:
    return true;
}
</script>
```

注意到`id`为`md5-password`的`<input>`标记了`name="password"`, 而用户输入的`id`为`input-password`的`<input>`没有`name`属性.
没有`name`属性的`<input>`的数据不会被提交.

#### 练习

利用`JavaScript`检查用户注册信息是否正确,在以下情况不满足时报错并阻止提交表单:

+ 用户名必须是`3-10`位英文字母或数字;
+ 口令必须是`6-20`位;
+ 两次输入口令必须一致.

```js
var reU = /^[0-9a-zA-Z_]{3,10}$/,
    reP = /\S{6,20}$/,
    uname = document.getElementById('username').value,
    pwd1 = document.getElementById('password').value,
    pwd2 = document.getElementById('password-2').value;
// 继续下一步:
if ( ! reU.test(uname)) {
    window.alert('用户名必须是3-10位英文字母或数字 '); return false;
} else if (!reP.test(pwd1)) {
    window.alert('口令必须是6-20位非空白字符'); return false;
} else if ( ! (pwd1 === pwd2)) {
    window.alert('两次输入口令必须一致'); return false;
}
else { return true; }
}
// 测试:
;(function () {
    window.testFormHandler = checkRegisterForm;
    var form = document.getElementById('test-register');
    if (form.dispatchEvent) {
        var event = new Event('submit', {
            bubbles: true,
            cancelable: true
          });
        form.dispatchEvent(event);
    } else {
        form.fireEvent('onsubmit');
    }
})();
```

### 操作文件

在`HTML`表单中,可以上传文件的唯一控件就是`<input type="file">`.

注意: 当一个表单包含<input type="file">时,表单的`enctype`必须指定为`multipart/form-data`,
`method`必须指定为`post`,浏览器才能正确编码并以`multipart/form-data`格式发送表单的数据.

出于安全考虑,浏览器只允许用户点击`<input type="file">`来选择本地文件,
用`JavaScript`对`<input type="file">`的`value`赋值是没有任何效果的.当用户选择了上传某个文件后,`JavaScript`也无法获得该文件的真实路径:

通常, 上传的文件都由后台服务器处理, `JavaScript` 可以在提交表单时对文件扩展名做检查,以便防止用户上传无效格式的文件:

```js
var f = document.getElementById('test-file-upload');
var filename = f.value; // 'C:\fakepath\test.png'
if (!filename || !(filename.endsWith('.jpg') || filename.endsWith('.png') || filename.endsWith('.gif'))) {
    alert('Can only upload image file.');
    return false;
}
```

#### File API

由于`JavaScript`对用户上传的文件操作非常有限,尤其是无法读取文件内容,使得很多需要操作文件的网页不得不用`Flash`这样的第三方插件来实现.

随着`HTML5`的普及,新增的`File API`允许`JavaScript`读取文件内容,获得更多的文件信息.
`HTML5`的`File API`提供了`File`和`FileReader`两个主要对象,可以获得文件信息并读取文件.

下面的例子演示了如何读取用户选取的图片文件,并在一个`<div>`中预览图像, 图片预览:

```js
var
    fileInput = document.getElementById('test-image-file'), // 输入的文件
    info = document.getElementById('test-file-info'), // 文件信息
    preview = document.getElementById('test-image-preview'); //文件预览
// 监听change事件:
fileInput.addEventListener('change', function () {
    preview.style.backgroundImage = '';    // 清除背景图片:
    if (!fileInput.value) {    // 检查文件是否选择:
        info.innerHTML = '没有选择文件';
        return;
    }
    var file = fileInput.files[0];    // 获取File引用:
    // 获取File信息:
    info.innerHTML = '文件: ' + file.name + '<br>' +
                     '大小: ' + file.size + '<br>' +
                     '修改: ' + file.lastModifiedDate;
    if (file.type !== 'image/jpeg' && file.type !== 'image/png' && file.type !== 'image/gif') {
        alert('不是有效的图片文件!');
        return;
    }
    // 读取文件:
    var reader = new FileReader();
    reader.onload = function(e) { // on load,在读取完成的时候
        var
            data = e.target.result; // 'data:image/jpeg;base64,/9j/4AAQSk...(base64编码)...'
        preview.style.backgroundImage = 'url(' + data + ')';
    };
    reader.readAsDataURL(file); // 以DataURL的形式读取文件:
});
```

上面的代码演示了如何通过`HTML5`的`File API`读取文件内容.
以`DataURL`的形式读取到的文件是一个字符串,类似于`data:image/jpeg;base64,/9j/4AAQSk...(base64编码)...`,常用于设置图像.
如果需要服务器端处理,把字符串`base64,`后面的字符发送给服务器并用`Base64`解码就可以得到原始文件的二进制内容.

#### 回调

上面的代码还演示了`JavaScript`的一个重要的特性就是`单线程`执行模式.
在`JavaScript`中,浏览器的`JavaScript`执行引擎在执行`JavaScript`代码时,总是以`单线程`模式执行.
也就是说,任何时候,`JavaScript`代码都不可能同时有多于`1`个线程在执行.

你可能会问,`单线程`模式执行的`JavaScript`,如何处理多任务? 在`JavaScript`中,执行多任务实际上都是异步调用, 比如上面的代码:

```js
reader.readAsDataURL(file);
```

就会发起一个`异步操作`来读取文件内容. 因为是异步操作,所以我们在`JavaScript`代码中就不知道什么时候操作结束,因此需要先设置一个`回调函数`:

```js
reader.onload = function(e) {
        // 当文件读取完成后,自动调用此函数:
};
```

当文件读取完成后,`JavaScript`引擎将自动调用我们设置的`回调函数`. 执行`回调函数`时,文件已经读取完毕,所以我们可以在`回调函数`内部安全地获得文件内容.

回调函数: 一般是`用户`调用`系统`的函数; `回调函数`与通常相反,用户提供`函数`, 被`系统`调用, 所以称为`callback`.

### AJAX

`AJAX` 不是 `JavaScript` 的规范,它只是一个哥们"发明"的缩写: `Asynchronous JavaScript and XML`,意思就是用`JavaScript`执行异步网络请求.

如果仔细观察一个`Form`的提交,你就会发现,一旦用户点击`Submit`按钮,表单开始提交,浏览器就会刷新页面,然后在新页面里告诉你操作是成功了还是失败了.
如果不幸由于网络太慢或者其他原因,就会得到一`个404页面`.
这就是`Web`的运作原理: 一次`HTTP`请求对应一个页面.

如果要让用户留在`当前页面`中,同时发出新的`HTTP`请求,就必须用`JavaScript`发送这个新请求,接收到数据后,再用`JavaScript`更新页面,
这样一来,用户就感觉自己仍然停留在当前页面,但是数据却可以不断地更新.
最早大规模使用`AJAX`的就是 `Gmail`,`Gmail`的页面在首次加载后,剩下的所有数据都依赖于`AJAX`来更新.

用`JavaScript`写一个完整的`AJAX`代码并不复杂,但是需要注意: `AJAX`请求是异步执行的,也就是说,要通过`回调函数`获得响应.
在现代浏览器上写`AJAX`主要依靠`XMLHttpRequest`对象:

```js
'use strict';
function success(text) { // 响应成功
    var textarea = document.getElementById('test-response-text');  // 返回的文本
    textarea.value = text;
}
function fail(code) { // 响应失败
    var textarea = document.getElementById('test-response-text');
    textarea.value = 'Error code: ' + code;
}
var request = new XMLHttpRequest(); // 新建XMLHttpRequest对象
request.onreadystatechange = function () { // 状态发生变化时,函数被回调
    if (request.readyState === 4) { // 成功完成
        if (request.status === 200) { // 判断响应结果:
            return success(request.responseText); // 成功,通过responseText拿到响应的文本:
        } else { // 失败,根据响应码判断失败原因:
            return fail(request.status);
        }
    } else {
        // HTTP请求还在继续...
    }
}
// 发送请求:
request.open('GET', '/api/categories');
request.send();
alert('请求已发送,请等待响应...');
```

对于低版本的`IE`,需要换一个`ActiveXObject`对象:

```js
'use strict';
function success(text) {
    var textarea = document.getElementById('test-ie-response-text');
    textarea.value = text;
}
function fail(code) {
    var textarea = document.getElementById('test-ie-response-text');
    textarea.value = 'Error code: ' + code;
}
var request = new ActiveXObject('Microsoft.XMLHTTP');  // 新建Microsoft.XMLHTTP对象
request.onreadystatechange = function () { // 状态发生变化时,函数被回调
    if (request.readyState === 4) { // 成功完成
        // 判断响应结果:
        if (request.status === 200) {
            // 成功,通过responseText拿到响应的文本:
            return success(request.responseText);
        } else {
            // 失败,根据响应码判断失败原因:
            return fail(request.status);
        }
    } else {
        // HTTP请求还在继续...
    }
}
// 发送请求:
request.open('GET', '/api/categories');
request.send();
alert('请求已发送,请等待响应...');
```

如果你想把标准写法和`IE`写法混在一起,可以这么写:

```js
var request;
if (window.XMLHttpRequest) {
    request = new XMLHttpRequest();
} else {
    request = new ActiveXObject('Microsoft.XMLHTTP');
}
```

通过检测 `window` 对象是否有 `XMLHttpRequest` 属性来确定浏览器是否支持标准的 `XMLHttpRequest`.
注意,不要根据浏览器的`navigator.userAgent`来检测浏览器是否支持某个`JavaScript`特性,一是因为这个字符串本身可以伪造, 二是通过`IE`版本判断`JavaScript`特性将非常复杂.

当创建了`XMLHttpRequest`对象后,要先设置`onreadystatechange`的回调函数.
在回调函数中, 通常我们只需通过`readyState === 4`判断请求是否完成,如果已完成,再根据`status === 200`判断是否是一个成功的响应.

`XMLHttpRequest`对象的`open()`方法有`3`个参数,第一个参数指定是`GET`还是`POST`,第二个参数指定`URL`地址,第三个参数指定是否使用`异步`,默认是 `true`,所以不用写.
注意,千万不要把第三个参数指定为`false`, 否则浏览器将停止响应,直到`AJAX`请求完成. 如果这个请求耗时`10`秒,那么`10`秒内你会发现浏览器处于`假死`状态.

最后调用`send()`方法才真正发送请求. `GET` 请求不需要参数,`POST` 请求需要把`body`部分以字符串或者`FormData`对象传进去.

#### 安全限制

上面代码的`URL`使用的是`相对路径`. 如果你把它改为`'http://www.sina.com.cn/'`, 再运行,肯定报错. 在`Chrome`的控制台里,还可以看到错误信息.
这是因为浏览器的同源策略导致的. 默认情况下,`JavaScript` 在发送`AJAX`请求时, `URL` 的域名必须和当前页面完全一致. 完全一致的意思是,

+ 域名要相同(`www.example.com和example.com`不同),
+ 协议要相同(`http`和`https`不同),
+ 端口号要相同(默认是`:80`端口,它和`:8080`就不同).

有的浏览器口子松一点,允许端口不同, 大多数浏览器都会严格遵守这个限制.
那是不是用`JavaScript`无法请求外域(就是其他网站)的`URL`了呢?方法还是有的,大概有这么几种:

+ 一是通过`Flash`插件发送`HTTP`请求,这种方式可以绕过浏览器的安全限制,但必须安装`Flash`,并且跟`Flash`交互.不过`Flash`用起来麻烦,而且现在用得也越来越少了.
+ 二是通过在同源域名下架设一个`代理服务器`来转发, `JavaScript` 负责把请求发送到代理服务器:

    '/proxy?url=http://www.sina.com.cn'

代理服务器再把结果返回,这样就遵守了浏览器的同源策略. 这种方式麻烦之处在于需要服务器端额外做开发.

+ 第三种方式称为`JSONP`,它有个限制,只能用`GET`请求,并且要求返回`JavaScript`.这种方式跨域实际上是利用了浏览器允许跨域引用`JavaScript`资源:

```js
<html>
<head>
    <script src="http://example.com/abc.js"></script>
    ...
</head>
<body>
...
</body>
</html>
```

`JSONP`通常以函数调用的形式返回,例如,返回`JavaScript`内容如下:

```js
foo('data');
```

这样一来,我们如果在页面中先准备好`foo()`函数,然后给页面动态加一个`<script>`节点,相当于动态读取外域的`JavaScript`资源,最后就等着接收回调了.
以`163`的股票查询`URL`为例,对于`URL`: [api.money.126.net](http://api.money.126.net/data/feed/0000001,1399001?callback=refreshPrice),你将得到如下返回:

```js
refreshPrice({"0000001":{"code": "0000001", ... });
```

因此我们需要首先在页面中准备好回调函数:

```js
function refreshPrice(data) {
    var p = document.getElementById('test-jsonp');
    p.innerHTML = '当前价格: ' +
        data['0000001'].name +': ' +
        data['0000001'].price + ';' +
        data['1399001'].name + ': ' +
        data['1399001'].price;
}
```

最后用`getPrice()`函数触发:

```js
function getPrice() {
    var
        js = document.createElement('script'),
        head = document.getElementsByTagName('head')[0];
    js.src = 'http://api.money.126.net/data/feed/0000001,1399001?callback=refreshPrice';
    head.appendChild(js);
}
```

就完成了跨域加载数据.

#### CORS

如果浏览器支持`HTML5`,那么就可以一劳永逸地使用新的`跨域策略`: `CORS`了.

`CORS`全称`Cross-Origin Resource Sharing`, 是`HTML5`规范定义的如何`跨域访问资源`.

了解`CORS`前,我们先搞明白概念:

`Origin`表示本域,也就是浏览器当前页面的域. 当`JavaScript`向外域(如`sina.com`)发起请求后,
浏览器收到响应后,首先检查`Access-Control-Allow-Origin`是否包含本域,如果是,则此次跨域请求成功,如果不是,则请求失败,`JavaScript`将无法获取到响应的任何数据.

用一个图来表示就是:

![js-cors](https://static.liaoxuefeng.com/files/attachments/1027024093709472/l)

假设本域是`my.com`,外域是`sina.com`,只要响应头`Access-Control-Allow-Origin`为`http://my.com`,或者是`*`,本次请求就可以成功.
可见,跨域能否成功,取决于对方服务器是否愿意给你设置一个正确的`Access-Control-Allow-Origin`,决定权始终在对方手中.

上面这种跨域请求,称之为`简单请求`.
简单请求包括`GET`,`HEAD`和`POST`(`POST`的`Content-Type`类型, 仅限`application/x-www-form-urlencoded`,`multipart/form-data`和`text/plain`),
并且不能出现任何自定义头(例如,`X-Custom: 12345`),通常能满足`90%`的需求.

无论你是否需要用`JavaScript`通过`CORS`跨域请求资源,你都要了解`CORS`的原理.
最新的浏览器全面支持`HTML5`. 在引用外域资源时,除了`JavaScript`和`CSS`外,都要验证`CORS`.例如,当你引用了某个第三方`CDN`上的字体文件时:

```js
/* CSS */
@font-face {
  font-family: 'FontAwesome';
  src: url('http://cdn.com/fonts/fontawesome.ttf') format('truetype');
}
```

如果该`CDN`服务商未正确设置`Access-Control-Allow-Origin`,那么浏览器无法加载字体资源.

对于`PUT`,`DELETE` 以及其他类型如`application/json`的`POST`请求,
在发送`AJAX`请求之前,浏览器会先发送一个`OPTIONS`请求(称为`preflighted`请求)到这个`URL`上,询问目标服务器是否接受:

```js
OPTIONS /path/to/resource HTTP/1.1
Host: bar.com
Origin: http://my.com
Access-Control-Request-Method: POST
```

服务器必须响应并明确指出允许的`Method`:

```js
HTTP/1.1 200 OK
Access-Control-Allow-Origin: http://my.com
Access-Control-Allow-Methods: POST, GET, PUT, OPTIONS
Access-Control-Max-Age: 86400
```

浏览器确认服务器响应的`Access-Control-Allow-Methods`头确实包含将要发送的`AJAX`请求的`Method`,才会继续发送`AJAX`, 否则,抛出一个错误.

由于以`POST`,`PUT`方式传送`JSON`格式的数据在`REST`中很常见, 所以要跨域正确处理`POST`和`PUT`请求,服务器端必须正确响应`OPTIONS`请求.

需要深入了解`CORS`的童鞋请移步[W3C文档](https://fetch.spec.whatwg.org/#http-cors-protocol).

### Promise

[Promise 对象](https://es6.ruanyifeng.com/#docs/promise)

在`JavaScript`的世界中,所有代码都是单线程执行的.
由于这个"缺陷",导致`JavaScript`的所有网络操作,浏览器事件,都必须是`异步执行`. `异步执行`可以用`回调函数`实现:

```js
Promise( f(x,y) ).then(A).catch(B)
```

其中, `f`的两个参数`x,y`也是函数, `x,y`作为头部, 用来封装产生的数据.
如果`Promise`执行`f(x,y)`的结果是`x(data)`, 就保证执行`A(data)`,  `A`存储在`then`中.
如果结果是`y(data)`, 就保证执行`B(data)`,  `B`存储在`catch`中.

```js
function callback() {
    console.log('Done');
}
console.log('before setTimeout()');
setTimeout(callback, 1000); // 1秒钟后调用callback函数
console.log('after setTimeout()');
```

观察上述代码执行,在`Firefox`的控制台输出可以看到:

```js
before setTimeout()
after setTimeout()
(等待1秒后)
Done
```

可见,异步操作会在将来的某个时间点触发一个函数调用.  `AJAX` 就是典型的异步操作.以上一节的代码为例:

```js
request.onreadystatechange = function () {
    if (request.readyState === 4) {
        if (request.status === 200) {
            return success(request.responseText);
        } else {
            return fail(request.status);
        }
    }
}
```

把回调函数`success(request.responseText)` 和 `fail(request.status)`写到一个`AJAX`操作里很正常, 但是不好看,而且不利于代码复用.
有没有更好的写法?比如写成这样:

```js
var ajax = ajaxGet('http://...');
ajax.ifSuccess(success).ifFail(fail);
```

这种链式写法的好处在于,先统一执行`AJAX`逻辑,不关心如何处理结果,
然后,根据结果是成功还是失败,在将来的某个时候调用`success`函数或`fail`函数.
古人云: "君子一诺千金",这种"承诺将来会执行"的对象在`JavaScript`中称为`Promise`对象.

`Promise`有各种开源实现,在`ES6`中被统一规范,由浏览器直接支持. 先测试一下你的浏览器是否支持`Promise`:

```js
'use strict';
new Promise(function () {});
// 直接运行测试:
console.log('支持Promise!');
```

我们先看一个最简单的`Promise`例子: 生成一个`0-2`之间的随机数,如果小于`1`,则等待一段时间后返回成功,否则返回失败:

```js
function test(resolve, reject) {
    var timeOut = Math.random() * 2;
    log('set timeout to: ' + timeOut + ' seconds.');
    setTimeout(function () { // 等待一段时间之后再执行
        if (timeOut < 1) {
            log('call resolve()...');
            resolve('200 OK'); // 执行成功,就调用 resolve
        }
        else {
            log('call reject()...');
            reject('timeout in ' + timeOut + ' seconds.'); // 执行失败, 就调用 reject
        }
    }, timeOut * 1000);
}
```

这个`test()`函数有两个参数,这两个参数都是`函数`, 如果执行成功,我们将调用`resolve('200 OK')`,
如果执行`失败`,我们将调用`reject('timeout in ' + timeOut + ' seconds.')`.
可以看出,`test()`函数只关心自身的逻辑,并不关心具体的`resolve`和`reject`将如何处理结果.

有了执行函数,我们就可以用一个`Promise`对象来执行它,并在将来某个时刻获得成功或失败的结果:

```js
var p1 = new Promise(test);
var p2 = p1.then(function (result) { // then, 成功就执行这个
    console.log('成功: ' + result);
});
var p3 = p2.catch(function (reason) { // 不成功就执行这个
    console.log('失败: ' + reason);
});
```

变量`p1`是一个`Promise`对象,它负责执行`test`函数. 由于`test`函数在内部是异步执行的,当`test`函数执行成功时,我们告诉`Promise`对象:

```js
// 如果成功,执行这个函数:
p1.then(function (result) {
    console.log('成功: ' + result);
});
```

当`test`函数执行失败时,我们告诉`Promise`对象:

```js
p2.catch(function (reason) {
    console.log('失败: ' + reason);
});
```

`Promise`对象可以串联起来,所以上述代码可以简化为:

```js
new Promise(test).then(function (result) { // test 执行后, 如果返回 resolve(data), 就调用 then(f) 中的f,  返回 f(data)
    console.log('成功: ' + result);
}).catch(function (reason) { // 如果返回 reject(data), 就调用 catch(g) 中的g,  返回 g(data)
    console.log('失败: ' + reason);
});
```

实际测试一下,看看`Promise`是如何异步执行的:

```js
'use strict';
// 清除log:
var logging = document.getElementById('test-promise-log');
while (logging.children.length > 1) {
    logging.removeChild(logging.children[logging.children.length - 1]);
}
// 输出log到页面:
function log(s) {
    var p = document.createElement('p');
    p.innerHTML = s;
    logging.appendChild(p);
}
new Promise(function (resolve, reject) {
    log('start new Promise...');
    var timeOut = Math.random() * 2;
    log('set timeout to: ' + timeOut + ' seconds.');
    setTimeout(function () {
        if (timeOut < 1) {
            log('call resolve()...');
            resolve('200 OK');
        }
        else {
            log('call reject()...');
            reject('timeout in ' + timeOut + ' seconds.');
        }
    }, timeOut * 1000);
}).then(function (r) { // 给出 resolve的具体形式
    log('Done: ' + r);
}).catch(function (reason) { // 给出 reject 的具体形式
    log('Failed: ' + reason);
});
```

可见`Promise`最大的好处是在异步执行的流程中,把执行代码和处理结果的代码清晰地分离了:

![promise](https://static.liaoxuefeng.com/files/attachments/1027242914217888/l)

`Promise` 还可以做更多的事情,比如,有若干个`异步任务`,需要先做`任务1`,如果成功后再做`任务2`,任何任务失败则不再继续并执行错误处理函数.
要串行执行这样的异步任务,不用`Promise`需要写一层一层的嵌套代码.有了`Promise`,我们只需要简单地写:

```js
job1.then(job2).then(job3).catch(handleError);
```

其中, `job1`, `job2` 和 `job3` 都是 `Promise`对象.

下面的例子演示了如何`串行`执行一系列需要`异步计算`获得结果的任务:

```js
'use strict';
var logging = document.getElementById('test-promise2-log');
while (logging.children.length > 1) {
    logging.removeChild(logging.children[logging.children.length - 1]); //删除末尾的节点
}
function log(s) { // 将结果添加到 log
    var p = document.createElement('p');
    p.innerHTML = s;
    logging.appendChild(p);
}
// 0.5秒后返回 input*input 的计算结果:
function multiply(input) { // 测试中第一行的 multiply
    return new Promise(function (resolve, reject) { // 在测试中, resolve 函数对应第二行的 add
        log('calculating ' + input + ' x ' + input + '...');
        setTimeout(resolve, 500, input * input); // 执行成功,将返回 add(input*input)
    });
}
function add(input) { // 0.5秒后返回 input+input 的计算结果:
    return new Promise(function (resolve, reject) { // resolve 函数对应第三行的 divide
        log('calculating ' + input + ' + ' + input + '...');
        setTimeout(resolve, 500, input + input);
    });
}
function divide(input) {
    return new Promise(function (resolve, reject) { // resolve 函数对应第四行的 add
        log('calculating ' + input + ' x ' + input + '...');
        setTimeout(resolve, 500, input *0.5 );
    });
}
var p = new Promise(function (resolve, reject) { // 承诺执行一个函数,
    log('start new Promise...');
    resolve(123); // resolve 函数对应第一行的 multiply
});
// 开始执行异步操作
p.then(multiply)
 .then(add)
 .then(divide)
 .then(add)
 .then(function (result) {
    log('Got value: ' + result);
});
```

`setTimeout`可以看成一个模拟网络等异步执行的函数.

现在,我们把上一节的`AJAX`异步执行函数转换为`Promise`对象,看看用`Promise`如何简化异步处理:

```js
'use strict';
// ajax函数将返回Promise对象:
function ajax(method, url, data) {
    var request = new XMLHttpRequest();
    return new Promise(function (resolve, reject) { //返回 Promise 对象
        request.onreadystatechange = function () {
            if (request.readyState === 4) {
                if (request.status === 200) {
                    resolve(request.responseText); // 成功则调用 resolve
                } else {
                    reject(request.status); // 调用 reject
                }
            }
        };
        request.open(method, url);
        request.send(data);
    });
}
```

除了串行执行若干异步任务外, `Promise` 还可以并行执行异步任务.

试想一个页面聊天系统,我们需要从两个不同的`URL`分别获得用户的`个人信息`和`好友列表`,这两个任务是可以`并行执行`的, 用`Promise.all()`实现如下:

```js
var p1 = new Promise(function (resolve, reject) {
    setTimeout(resolve, 500, 'P1');
});
var p2 = new Promise(function (resolve, reject) {
    setTimeout(resolve, 600, 'P2');
});
// 同时执行p1和p2,并在它们都完成后执行then:
Promise.all([p1, p2]).then(function (results) {
    console.log(results); // 获得一个Array: ['P1', 'P2']
});
```

有些时候,多个`异步任务`是为了容错. 比如,同时向两个`URL`读取用户的个人信息, 只需要获得先返回的结果即可.这种情况下,用`Promise.race()`实现:

```js
var p1 = new Promise(function (resolve, reject) {
    setTimeout(resolve, 500, 'P1');
});
var p2 = new Promise(function (resolve, reject) {
    setTimeout(resolve, 600, 'P2');
});
Promise.race([p1, p2]).then(function (result) {
    console.log(result); // 'P1'
});
```

由于`p1`执行较快, `Promise`的`then()`将获得结果`'P1'`. `p2`仍在继续执行,但执行结果将被丢弃.
如果我们组合使用`Promise`,就可以把很多异步任务以`并行`和`串行`的方式组合起来执行.

### Canvas

`Canvas`是`HTML5`新增的组件,它就像一块幕布,可以用`JavaScript`在上面绘制各种图表,动画等.

没有`Canvas`的年代,绘图只能借助`Flash`插件实现,页面不得不用`JavaScript`和`Flash`进行交互.
有了`Canvas`,我们就再也不需要`Flash`了,直接使用`JavaScript`完成绘制.
一个`Canvas`定义了一个指定尺寸的`矩形框`,在这个范围内我们可以随意绘制:

```js
<canvas id="test-canvas" width="300" height="200"></canvas>
```

由于浏览器对`HTML5`标准支持不一致,所以,通常在`<canvas>`内部添加一些说明性`HTML`代码,
如果浏览器支持`Canvas`,它将忽略`<canvas>`内部的`HTML`,如果浏览器不支持`Canvas`,它将显示`<canvas>`内部的`HTML`:

```js
<canvas id="test-stock" width="300" height="200">
    <p>Current Price: 25.51</p>
</canvas>
```

在使用`Canvas`前,用`canvas.getContext`来测试浏览器是否支持`Canvas`:

```js
<!-- HTML代码 -->
<canvas id="test-canvas" width="200" heigth="100">
    <p>你的浏览器不支持Canvas</p>
</canvas>

'use strict';
var canvas = document.getElementById('test-canvas');
if (canvas.getContext) {
    console.log('你的浏览器支持Canvas!');
} else {
    console.log('你的浏览器不支持Canvas!');
}
```

`getContext('2d')`方法让我们拿到一个`CanvasRenderingContext2D`对象,所有的绘图操作都需要通过这个对象完成.

```js
var ctx = canvas.getContext('2d');
```

如果需要绘制`3D`怎么办?`HTML5`还有一个`WebGL`规范,允许在`Canvas`中绘制`3D`图形:

```js
gl = canvas.getContext("webgl");
```

本节我们只专注于绘制`2D`图形.

#### 绘制形状

我们可以在`Canvas`上绘制各种形状.在绘制前,我们需要先了解一下`Canvas`的坐标系统:

![canvas-xy](https://static.liaoxuefeng.com/files/attachments/1028111602807456/l)

`Canvas`的坐标以左上角为原点,水平向右为`X`轴,垂直向下为`Y`轴,以像素为单位, 所以每个点都是非负整数.
`CanvasRenderingContext2D`对象有若干方法来绘制图形:

```js
'use strict';
var
    canvas = document.getElementById('test-shape-canvas'),
    ctx = canvas.getContext('2d');
ctx.clearRect(0, 0, 200, 200); // 擦除(0,0)位置大小为200x200的矩形,擦除的意思是把该区域变为透明
ctx.fillStyle = '#dddddd'; // 设置颜色
ctx.fillRect(10, 10, 130, 130); // 把(10,10)位置大小为130x130的矩形涂色
// 利用Path绘制复杂路径:
var path=new Path2D();
path.arc(75, 75, 60, 0, Math.PI*2, true);
path.moveTo(110,75);
path.arc(75, 75, 35, 0, Math.PI, false);
path.moveTo(65, 65);
path.arc(60, 65, 5, 0, Math.PI*2, true);
path.moveTo(95, 65);
path.arc(90, 65, 5, 0, Math.PI*2, true);
ctx.strokeStyle = '#0000ff';
ctx.stroke(path);
```

绘制文本就是在指定的位置输出文本,可以设置文本的字体,样式,阴影等,与`CSS`完全一致:

```js
'use strict';
var
    canvas = document.getElementById('test-text-canvas'),
    ctx = canvas.getContext('2d');
ctx.clearRect(0, 0, canvas.width, canvas.height);
ctx.shadowOffsetX = 2;
ctx.shadowOffsetY = 2;
ctx.shadowBlur = 2;
ctx.shadowColor = '#666666';
ctx.font = '24px Arial';
ctx.fillStyle = '#333333';
ctx.fillText('带阴影的文字', 20, 40);
```

`Canvas`除了能绘制基本的形状和文本,还可以实现动画,缩放,各种滤镜和像素转换等高级操作. 如果要实现非常复杂的操作,考虑以下优化方案:

+ 通过创建一个不可见的`Canvas`来绘图,然后将最终绘制结果复制到页面的可见`Canvas`中;
+ 尽量使用整数坐标而不是浮点数;
+ 可以创建多个重叠的`Canvas`绘制不同的层,而不是在一个`Canvas`中绘制非常复杂的图;
+ 背景图片如果不变可以直接用`<img>`标签并放到最底层.

#### 练习

请根据从`163`获取的`JSON`数据绘制最近`30`个交易日的`K`线图,数据已处理为包含一组对象的数组:

[_时空秋千](https://www.liaoxuefeng.com/discuss/1023622307115840/1429842354503713)

```js
'use strict';
// 导入数据
window.loadStockData = function (r) {
    var
        NUMS = 30,
        data = r.data;
    if (data.length > NUMS) {
        data = data.slice(data.length - NUMS);
    }
    data = data.map(function (x) {
        return {
            date: x[0],
            open: x[1],
            close: x[2],
            high: x[3],
            low: x[4],
            vol: x[5],
            change: x[6]
        };
    });
    window.drawStock(data);
}
// 绘图函数
window.drawStock = function (data) {
    var
        canvas = document.getElementById('stock-canvas'),
        width = canvas.width,
        height = canvas.height,
        ctx = canvas.getContext('2d'),
        colWidth = canvas.width / 30 / 1.5, // 矩形宽
        start,  // 中心线起始位置
        spacing,    // 中心线间隔
        max = data.map(x => x.high).sort()[data.length - 1],    // 最高价
        min = data.map(x => x.low).sort()[0],   // 最低价
        unitLen = canvas.height / (max - min),  // 单位价格区间长度对应的坐标长度
        bottom = canvas.height; // 底部
    // 限制宽度和高度
    if (colWidth > 10) {
        colWidth = 10;
    }
    if (unitLen > 1) {
        bottom = 1 / unitLen * bottom;  // 使图像靠上
        unitLen = 1;
    }
    // aaa
    start = colWidth / 2;
    spacing = colWidth * 1.5;
    console.log(JSON.stringify(data[0]));
    // {"date":"20150602","open":4844.7,"close":4910.53,"high":4911.57,"low":4797.55,"vol":62374809900,"change":1.69}
    ctx.clearRect(0, 0, width, height);
    // ctx.fillText('Test Canvas', 10, 10);
    for (let i = 0; i < data.length; i++) {
        // 绘制中心线
        var path = new Path2D();
        let coord = start + spacing * i;    // 中心线坐标
        path.moveTo(coord, bottom - (data[i].low - min) * unitLen);
        path.lineTo(coord, bottom - (data[i].high - min) * unitLen);
        ctx.strokeStyle = 'black';
        ctx.stroke(path);
        // 绘制矩形
        let higher, lower;
        if (data[i].open < data[i].close) {
            ctx.fillStyle = 'red';
            higher = data[i].close;
            lower = data[i].open;
        } else {
            ctx.fillStyle = 'green';
            higher = data[i].open;
            lower = data[i].close;
        }
        ctx.fillRect(coord - colWidth / 2, bottom - (higher - min) * unitLen, colWidth, (higher - lower) * unitLen);
    }
};
// 加载最近30个交易日的K线图数据:
var js = document.createElement('script');
js.src = 'http://img1.money.126.net/data/hs/kline/day/history/2015/0000001.json?callback=loadStockData&t=' + Date.now();
document.getElementsByTagName('head')[0].appendChild(js);
```
