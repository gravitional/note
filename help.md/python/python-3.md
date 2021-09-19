# python-3.md

ref: [这是小白的Python新手教程][]

[这是小白的Python新手教程]: https://www.liaoxuefeng.com/wiki/1016959663602400

## 面向对象编程

面向对象编程——Object Oriented Programming, 简称OOP, 是一种程序设计思想. OOP把对象作为程序的基本单元, 一个对象包含了数据和操作数据的函数. 

面向过程的程序设计把计算机程序视为一系列的命令集合, 即一组函数的顺序执行. 为了简化程序设计, 面向过程把函数继续切分为子函数, 即把大块函数通过切割成小块函数来降低系统的复杂度. 

而面向对象的程序设计把计算机程序视为一组对象的集合, 而每个对象都可以接收其他对象发过来的消息, 并处理这些消息, 计算机程序的执行就是一系列消息在各个对象之间传递. 

在Python中, 所有数据类型都可以视为对象, 当然也可以自定义对象. 自定义的对象数据类型就是面向对象中的类(Class)的概念. 

我们以一个例子来说明面向过程和面向对象在程序流程上的不同之处. 

假设我们要处理学生的成绩表, 为了表示一个学生的成绩, 面向过程的程序可以用一个dict表示：

```python
std1 = { 'name': 'Michael', 'score': 98 }
std2 = { 'name': 'Bob', 'score': 81 }
```

而处理学生成绩可以通过函数实现, 比如打印学生的成绩：

```python
def print_score(std):
    print('%s: %s' % (std['name'], std['score']))
```

如果采用面向对象的程序设计思想, 我们首选思考的不是程序的执行流程, 而是Student这种数据类型应该被视为一个对象, 这个对象拥有name和score这两个属性(Property). 如果要打印一个学生的成绩, 首先必须创建出这个学生对应的对象, 然后, 给对象发一个print_score消息, 让对象自己把自己的数据打印出来. 

```python
class Student(object):

    def __init__(self, name, score):
        self.name = name
        self.score = score

    def print_score(self):
        print('%s: %s' % (self.name, self.score))
```

给对象发消息实际上就是调用对象对应的关联函数, 我们称之为对象的方法(Method). 面向对象的程序写出来就像这样：

```python
bart = Student('Bart Simpson', 59)
lisa = Student('Lisa Simpson', 87)
bart.print_score()
lisa.print_score()
```

面向对象的设计思想是从自然界中来的, 因为在自然界中, 类(Class)和实例(Instance)的概念是很自然的. Class是一种抽象概念, 比如我们定义的Class——Student, 是指学生这个概念, 而实例(Instance)则是一个个具体的Student, 比如, Bart Simpson和Lisa Simpson是两个具体的Student. 

所以, 面向对象的设计思想是抽象出Class, 根据Class创建Instance. 

面向对象的抽象程度又比函数要高, 因为一个Class既包含数据, 又包含操作数据的方法. 

小结

数据封装, 继承和多态是面向对象的三大特点, 我们后面会详细讲解. 

### 类和实例

面向对象最重要的概念就是类(`Class`)和实例(`Instance`), 必须牢记类是抽象的模板, 比如`Student`类, 而实例是根据类创建出来的一个个具体的“对象”, 每个对象都拥有相同的方法, 但各自的数据可能不同. 

仍以`Student`类为例, 在Python中, 定义类是通过`class`关键字：

```python
class Student(object):
    pass
```

class后面紧接着是类名, 即`Student`, 类名通常是大写开头的单词, 紧接着是(`object`), 表示该类是从哪个类继承下来的, 继承的概念我们后面再讲, 通常, 如果没有合适的继承类, 就使用`object`类, 这是所有类最终都会继承的类. 

定义好了`Student`类, 就可以根据`Student`类创建出`Student`的实例, 创建实例是通过`类名`+`()`实现的：

```python
>>> bart = Student()
>>> bart
<__main__.Student object at 0x10a67a590>
>>> Student
<class '__main__.Student'>
```

可以看到, 变量`bart`指向的就是一个`Student`的实例, 后面的`0x10a67a590`是内存地址, 每个`object`的地址都不一样, 而`Student`本身则是一个类. 

可以自由地给一个实例变量绑定属性, 比如, 给实例`bart`绑定一个`name`属性：

```python
>>> bart.name = 'Bart Simpson'
>>> bart.name
'Bart Simpson'
```

由于类可以起到模板的作用, 因此, 可以在创建实例的时候, 把一些我们认为必须绑定的属性强制填写进去. 通过定义一个特殊的`__init__`方法, 在创建实例的时候, 就把`name`, `score`等属性绑上去：

```python
class Student(object):

    def __init__(self, name, score):
        self.name = name
        self.score = score
```

注意：特殊方法“__init__”前后分别有两个下划线！！！

注意到`__init__`方法的第一个参数永远是`self`, 表示创建的**实例**本身, 因此, 在`__init__`方法内部, 就可以把各种属性绑定到`self`, 因为`self`就指向创建的实例本身. 

有了`__init__`方法, 在创建实例的时候, 就不能传入空的参数了, 必须传入与`__init__`方法匹配的参数, 但`self`不需要传, Python解释器自己会把实例变量传进去：

```python
>>> bart = Student('Bart Simpson', 59)
>>> bart.name
'Bart Simpson'
>>> bart.score
59
```

和普通的函数相比, 在类中定义的函数只有一点不同, 就是第一个参数永远是实例变量`self`, 并且, 调用时, 不用传递该参数. 除此之外, 类的方法和普通函数没有什么区别, 所以, 你仍然可以用默认参数, 可变参数, 关键字参数和命名关键字参数. 

`self` 可以是别的字母组合, 关键是前后一致

#### 数据封装

面向对象编程的一个重要特点就是数据封装. 在上面的Student类中, 每个实例就拥有各自的`name`和`score`这些数据. 我们可以通过函数来访问这些数据, 比如打印一个学生的成绩：

```python
>>> def print_score(std):
...     print('%s: %s' % (std.name, std.score))
...
>>> print_score(bart)
Bart Simpson: 59
```

但是, 既然Student实例本身就拥有这些数据, 要访问这些数据, 就没有必要从外面的函数去访问, 可以直接在Student类的内部定义访问数据的函数, 这样, 就把“数据”给封装起来了. 这些封装数据的函数是和`Student`类本身是关联起来的, 我们称之为类的方法：

```python
class Student(object):

    def __init__(self, name, score):
        self.name = name
        self.score = score

    def print_score(self):
        print('%s: %s' % (self.name, self.score))
```

要定义一个方法, 除了第一个参数是`self`外, 其他和普通函数一样. 要调用一个方法, 只需要在实例变量上直接调用, 除了`self`不用传递, 其他参数正常传入：

```python
>>> bart.print_score()
Bart Simpson: 59
```

这样一来, 我们从外部看Student类, 就只需要知道, 创建实例需要给出`name`和`score`, 而如何打印, 都是在Student类的内部定义的, 这些数据和逻辑被“封装”起来了, 调用很容易, 但却不用知道内部实现的细节. 

封装的另一个好处是可以给`Student`类增加新的方法, 比如`get_grade`：

```python
class Student(object):
    ...

    def get_grade(self):
        if self.score >= 90:
            return 'A'
        elif self.score >= 60:
            return 'B'
        else:
            return 'C'
```

同样的, `get_grade`方法可以直接在实例变量上调用, 不需要知道内部实现细节：

```python
# -*- coding: utf-8 -*-

class Student(object):
    def __init__(self, name, score):
        self.name = name
        self.score = score

    def get_grade(self):
        if self.score >= 90:
            return 'A'
        elif self.score >= 60:
            return 'B'
        else:
            return 'C'

lisa = Student('Lisa', 99)
bart = Student('Bart', 59)
print(lisa.name, lisa.get_grade())
print(bart.name, bart.get_grade())
```

#### 小结-类和实例

类是创建实例的模板, 而实例则是一个一个具体的对象, 各个实例拥有的数据都互相独立, 互不影响；

方法就是与实例绑定的函数, 和普通函数不同, 方法可以直接访问实例的数据；
通过在实例上调用方法, 我们就直接操作了对象内部的数据, 但无需知道方法内部的实现细节. 

和静态语言不同, Python允许对实例变量绑定任何数据, 也就是说, 对于两个实例变量, 虽然它们都是同一个类的不同实例, 但拥有的变量名称都可能不同：

```python
>>> bart = Student('Bart Simpson', 59)
>>> lisa = Student('Lisa Simpson', 87)
>>> bart.age = 8
>>> bart.age
8
>>> lisa.age
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
AttributeError: 'Student' object has no attribute 'age'
```

### 访问限制

在Class内部, 可以有属性和方法, 而外部代码可以通过直接调用实例变量的方法来操作数据, 这样, 就隐藏了内部的复杂逻辑. 

但是, 从前面Student类的定义来看, 外部代码还是可以自由地修改一个实例的`name`, `score`属性：

```python
>>> bart = Student('Bart Simpson', 59)
>>> bart.score
59
>>> bart.score = 99
>>> bart.score
99
```

如果要让内部属性不被外部访问, 可以把属性的名称前加上**两个下划线**`__`, 在Python中, 实例的变量名如果以`__`开头, 就变成了一个私有变量(`private`), 只有内部可以访问, 外部不能访问, 所以, 我们把`Student`类改一改：

```python
class Student(object):

    def __init__(self, name, score):
        self.__name = name
        self.__score = score

    def print_score(self):
        print('%s: %s' % (self.__name, self.__score))
```

改完后, 对于外部代码来说, 没什么变动, 但是已经无法从外部访问实例变量`.__name`和实例变量`.__score`了：

```python
>>> bart = Student('Bart Simpson', 59)
>>> bart.__name
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
AttributeError: 'Student' object has no attribute '__name'
```

这样就确保了外部代码不能随意修改对象内部的状态, 这样通过访问限制的保护, 代码更加健壮. 

但是如果外部代码要获取`name`和`score`怎么办？可以给Student类增加`get_name`和`get_score`这样的方法：

```python
class Student(object):
    ...

    def get_name(self):
        return self.__name

    def get_score(self):
        return self.__score
```

如果又要允许外部代码修改`score`怎么办？可以再给Student类增加`set_score`方法：

```python
class Student(object):
    ...

    def set_score(self, score):
        self.__score = score
```

你也许会问, 原先那种直接通过`bart.score = 99`也可以修改啊, 为什么要定义一个方法大费周折？因为在方法中, 可以对参数做检查, 避免传入无效的参数：

```python
class Student(object):
    ...

    def set_score(self, score):
        if 0 <= score <= 100:
            self.__score = score
        else:
            raise ValueError('bad score')
```

需要注意的是, 在Python中, 变量名类似`__xxx__`的, 也就是以双下划线开头, 并且以双下划线结尾的, 是特殊变量, 特殊变量是可以直接访问的, 不是`private`变量, 所以, 不能用`__name__`, `__score__`这样的变量名. 

有些时候, 你会看到以一个下划线开头的实例变量名, 比如`_name`, 这样的实例变量外部是可以访问的, 但是, 按照约定俗成的规定, 当你看到这样的变量时, 意思就是, “虽然我可以被访问, 但是, 请把我视为私有变量, 不要随意访问”. 

双下划线开头的实例变量是不是一定不能从外部访问呢？其实也不是. 
不能直接访问`__name`是因为Python解释器对外把`__name`变量改成了`_Student__name`, 所以, 仍然可以通过`_Student__name`来访问`__name`变量：

```python
>>> bart._Student__name
'Bart Simpson'
```

但是强烈建议你不要这么干, 因为不同版本的Python解释器可能会把`__name`改成不同的变量名. 

总的来说就是, Python本身没有任何机制阻止你干坏事, 一切全靠自觉. 

最后注意下面的这种错误写法：

```python
>>> bart = Student('Bart Simpson', 59)
>>> bart.get_name()
'Bart Simpson'
>>> bart.__name = 'New Name' # 设置__name变量！
>>> bart.__name
'New Name'
```

表面上看, 外部代码“成功”地设置了`__name`变量, 但实际上这个`__name`变量和class内部的`__name`变量不是一个变量！内部的`__name`变量已经被Python解释器自动改成了`_Student__name`, 而外部代码给`bart`新增了一个`__name`变量. 不信试试：

```python
>>> bart.get_name() # get_name()内部返回self.__name
'Bart Simpson'
```

#### 练习-访问限制

请把下面的`Student`对象的`gender`字段对外隐藏起来, 用`get_gender()`和`set_gender()`代替, 并检查参数有效性：

```python
# -*- coding: utf-8 -*-

class Student(object):
    def __init__(self, name, gender):
        self.__name = name
        self.__gender = gender

    def get_gender(self):
        return self.__gender

    def set_gender(self,gender):
        if  (gender == 'male' or gender =='female'):
            self.__gender = gender
        else:
            raise ValueError('bad gender')

# 测试:
bart = Student('Bart', 'male')
if bart.get_gender() != 'male':
    print('测试失败!')
else:
    bart.set_gender('female')
    if bart.get_gender() != 'female':
        print('测试失败!')
    else:
        print('测试成功!')
```

### 继承和多态

在OOP程序设计中, 当我们定义一个class的时候, 可以从某个现有的class继承, 新的class称为子类(Subclass), 而被继承的class称为基类, 父类或超类(Base class, Super class). 

比如, 我们已经编写了一个名为Animal的class, 有一个`run()`方法可以直接打印：

```python
class Animal(object):
    def run(self):
        print('Animal is running...')
```

当我们需要编写`Dog`和`Cat`类时, 就可以直接从Animal类继承：

```python
class Dog(Animal):
    pass

class Cat(Animal):
    pass
```

对于Dog来说, Animal就是它的父类, 对于Animal来说, Dog就是它的子类. 
Cat和Dog类似. 

继承有什么好处？最大的好处是子类获得了父类的全部功能. 由于Animial实现了`run()`方法, 因此, Dog和Cat作为它的子类, 什么事也没干, 就自动拥有了`run()`方法：

```python
dog = Dog()
dog.run()
cat = Cat()
cat.run()
```

运行结果如下：

```python
Animal is running...
Animal is running...
```

当然, 也可以对子类增加一些方法, 比如`Dog`类：

```python
class Dog(Animal):

    def run(self):
        print('Dog is running...')

    def eat(self):
        print('Eating meat...')
```

继承的第二个好处需要我们对代码做一点改进. 
你看到了, 无论是Dog还是Cat, 它们`run()`的时候, 显示的都是`Animal is running...`, 符合逻辑的做法是分别显示`Dog is running..`.和`Cat is running...`, 因此, 对Dog和Cat类改进如下：

```python
class Dog(Animal):

    def run(self):
        print('Dog is running...')

class Cat(Animal):

    def run(self):
        print('Cat is running...')
```

再次运行, 结果如下：

```python
Dog is running...
Cat is running...
```

当子类和父类都存在相同的`run()`方法时, 我们说, 子类的`run()`覆盖了父类的`run()`, 在代码运行的时候, 总是会调用子类的`run()`. 这样, 我们就获得了继承的另一个好处：多态. 

要理解什么是多态, 我们首先要对数据类型再作一点说明. 当我们定义一个class的时候, 我们实际上就定义了一种数据类型. 我们定义的数据类型和Python自带的数据类型, 比如`str`, `list`, `dict`没什么两样：

```python
a = list() # a是list类型
b = Animal() # b是Animal类型
c = Dog() # c是Dog类型
```

判断一个变量是否是某个类型可以用`isinstance()`判断：

```python
>>> isinstance(a, list)
True
>>> isinstance(b, Animal)
True
>>> isinstance(c, Dog)
True
```

看来`a`, `b`, `c`确实对应着list, Animal, Dog这3种类型. 

但是等等, 试试：

```python
>>> isinstance(c, Animal)
True
```

看来`c`不仅仅是Dog, `c`还是Animal！

不过仔细想想, 这是有道理的, 因为Dog是从`Animal`继承下来的, 当我们创建了一个`Dog`的实例`c`时, 我们认为`c`的数据类型是`Dog`没错, 但`c`同时也是`Animal`也没错, `Dog`本来就是`Animal`的一种！

所以, 在继承关系中, 如果一个实例的数据类型是某个子类, 那它的数据类型也可以被看做是父类. 但是, 反过来就不行：

```python
>>> b = Animal()
>>> isinstance(b, Dog)
False
```

`Dog`可以看成`Animal`, 但`Animal`不可以看成`Dog`. 

要理解多态的好处, 我们还需要再编写一个函数, 这个函数接受一个`Animal`类型的变量：

```python
def run_twice(animal):
    animal.run()
    animal.run()
```

当我们传入`Animal`的实例时, `run_twice()`就打印出：

```python
>>> run_twice(Animal())
Animal is running...
Animal is running...
```

当我们传入`Dog`的实例时, `run_twice()`就打印出：

```python
>>> run_twice(Dog())
Dog is running...
Dog is running...
```

当我们传入`Cat`的实例时, `run_twice()`就打印出：

```python
>>> run_twice(Cat())
Cat is running...
Cat is running...
```

看上去没啥意思, 但是仔细想想, 现在, 如果我们再定义一个`Tortoise`类型, 也从`Animal`派生：

```python
class Tortoise(Animal):
    def run(self):
        print('Tortoise is running slowly...')
```

当我们调用`run_twice()`时, 传入`Tortoise`的实例：

```python
>>> run_twice(Tortoise())
Tortoise is running slowly...
Tortoise is running slowly...
```

你会发现, 新增一个Animal的子类, 不必对`run_twice()`做任何修改, 实际上, 任何依赖Animal作为参数的函数或者方法都可以不加修改地正常运行, 原因就在于多态. 

多态的好处就是, 当我们需要传入Dog, Cat, Tortoise……时, 我们只需要接收Animal类型就可以了, 因为Dog, Cat, Tortoise……都是Animal类型, 然后, 按照Animal类型进行操作即可. 由于Animal类型有run()方法, 因此, 传入的任意类型, 只要是Animal类或者子类, 就会自动调用实际类型的`run()`方法, 这就是多态的意思：

对于一个变量, 我们只需要知道它是Animal类型, 无需确切地知道它的子类型, 就可以放心地调用`run()`方法, 而具体调用的`run()`方法是作用在Animal, Dog, Cat还是Tortoise对象上, 由运行时该对象的确切类型决定, 这就是多态真正的威力：调用方只管调用, 不管细节, 而当我们新增一种Animal的子类时, 只要确保`run()`方法编写正确, 不用管原来的代码是如何调用的. 这就是著名的“开闭”原则：

+ 对扩展开放：允许新增Animal子类；
+ 对修改封闭：不需要修改依赖Animal类型的`run_twice()`等函数. 

继承还可以一级一级地继承下来, 就好比从爷爷到爸爸, 再到儿子这样的关系. 而任何类, 最终都可以追溯到根类object, 这些继承关系看上去就像一颗倒着的树. 

#### 静态语言 vs 动态语言

对于静态语言(例如`Java`)来说, 如果需要传入`Animal`类型, 则传入的对象必须是`Animal`类型或者它的子类, 否则, 将无法调用`run()`方法. 

对于Python这样的动态语言来说, 则不一定需要传入Animal类型. 我们只需要保证传入的对象有一个`run()`方法就可以了：

```python
class Timer(object):
    def run(self):
        print('Start...')
```

这就是动态语言的“鸭子类型”, 它并不要求严格的继承体系, 一个对象只要“看起来像鸭子, 走起路来像鸭子”, 那它就可以被看做是鸭子. 

Python的“`file-like object`“就是一种鸭子类型. 对真正的文件对象, 它有一个`read()`方法, 返回其内容. 但是, 许多对象, 只要有`read()`方法, 都被视为“file-like object“. 许多函数接收的参数就是“file-like object“, 你不一定要传入真正的文件对象, 完全可以传入任何实现了`read()`方法的对象. 

#### 小结-继承和多态

继承可以把父类的所有功能都直接拿过来, 这样就不必重零做起, 子类只需要新增自己特有的方法, 也可以把父类不适合的方法覆盖重写. 

动态语言的鸭子类型特点决定了继承不像静态语言那样是必须的. 

### 获取对象信息

当我们拿到一个对象的引用时, 如何知道这个对象是什么类型, 有哪些方法呢？

#### 使用type()

首先, 我们来判断对象类型, 使用`type()`函数：

基本类型都可以用`type()`判断：

```python
>>> type(123)
<class 'int'>
>>> type('str')
<class 'str'>
>>> type(None)
<type(None) 'NoneType'>
```

如果一个变量指向函数或者类, 也可以用`type()`判断：

```python
>>> type(abs)
<class 'builtin_function_or_method'>
>>> type(a)
<class '__main__.Animal'>
```

但是`type()`函数返回的是什么类型呢？它返回对应的Class类型. 如果我们要在if语句中判断, 就需要比较两个变量的type类型是否相同：

```python
>>> type(123)==type(456)
True
>>> type(123)==int
True
>>> type('abc')==type('123')
True
>>> type('abc')==str
True
>>> type('abc')==type(123)
False
```

判断基本数据类型可以直接写int, str等, 但如果要判断一个对象是否是函数怎么办？可以使用types模块中定义的常量：

```python
>>> import types
>>> def fn():
...     pass
...
>>> type(fn)==types.FunctionType
True
>>> type(abs)==types.BuiltinFunctionType
True
>>> type(lambda x: x)==types.LambdaType
True
>>> type((x for x in range(10)))==types.GeneratorType
True
```

#### 使用isinstance()

对于class的继承关系来说, 使用`type()`就很不方便. 我们要判断class的类型, 可以使用`isinstance()`函数. 

我们回顾上次的例子, 如果继承关系是：

`object -> Animal -> Dog -> Husky`

那么, `isinstance()`就可以告诉我们, 一个对象是否是某种类型. 先创建3种类型的对象：

```python
>>> a = Animal()
>>> d = Dog()
>>> h = Husky()
```

然后, 判断：

```python
>>> isinstance(h, Husky)
True
```

没有问题, 因为`h`变量指向的就是`Husky`对象. 

再判断：

```python
>>> isinstance(h, Dog)
True
```

`h`虽然自身是`Husky`类型, 但由于`Husky`是从`Dog`继承下来的, 所以, `h`也还是`Dog`类型. 换句话说, `isinstance()`判断的是一个对象是否是该类型本身, 或者位于该类型的父继承链上. 

因此, 我们可以确信, h还是Animal类型：

```python
>>> isinstance(h, Animal)
True
```

同理, 实际类型是Dog的d也是Animal类型：

```python
>>> isinstance(d, Dog) and isinstance(d, Animal)
True
```

但是, d不是`Husky`类型：

```python
>>> isinstance(d, Husky)
False
```

能用`type()`判断的基本类型也可以用`isinstance()`判断：

```python
>>> isinstance('a', str)
True
>>> isinstance(123, int)
True
>>> isinstance(b'a', bytes)
True
```

并且还可以判断一个变量是否是某些类型中的一种, 比如下面的代码就可以判断是否是list或者tuple：

```python
>>> isinstance([1, 2, 3], (list, tuple))
True
>>> isinstance((1, 2, 3), (list, tuple))
True
```

总是优先使用isinstance()判断类型, 可以将指定类型及其子类“一网打尽”. 

#### 使用dir()

如果要获得一个对象的所有属性和方法, 可以使用`dir()`函数, 它返回一个包含字符串的list, 比如, 获得一个str对象的所有属性和方法：

```python
>>> dir('ABC')
['__add__', '__class__',..., '__subclasshook__', 'capitalize', 'casefold',..., 'zfill']
```

类似`__xxx__`的属性和方法在Python中都是有特殊用途的, 比如`__len__`方法返回长度. 在Python中, 如果你调用`len()`函数试图获取一个对象的长度, 实际上, 在`len()`函数内部, 它自动去调用该对象的`__len__()`方法, 所以, 下面的代码是等价的：

```python
>>> len('ABC')
3
>>> 'ABC'.__len__()
3
```

我们自己写的类, 如果也想用`len(myObj)`的话, 就自己写一个`__len__()`方法：

```python
>>> class MyDog(object):
...     def __len__(self):
...         return 100
...
>>> dog = MyDog()
>>> len(dog)
100
```

剩下的都是普通属性或方法, 比如`lower()`返回小写的字符串：

```python
>>> 'ABC'.lower()
'abc'
```

仅仅把属性和方法列出来是不够的, 配合`getattr()`, `setattr()`以及`hasattr()`, 我们可以直接操作一个对象的状态：

```python
>>> class MyObject(object):
...     def __init__(self):
...         self.x = 9
...     def power(self):
...         return self.x * self.x
...
>>> obj = MyObject()
```

紧接着, 可以测试该对象的属性：

```python
>>> hasattr(obj, 'x') # 有属性'x'吗？
True
>>> obj.x
9
>>> hasattr(obj, 'y') # 有属性'y'吗？
False
>>> setattr(obj, 'y', 19) # 设置一个属性'y'
>>> hasattr(obj, 'y') # 有属性'y'吗？
True
>>> getattr(obj, 'y') # 获取属性'y'
19
>>> obj.y # 获取属性'y'
19
```

如果试图获取不存在的属性, 会抛出`AttributeError`的错误：

```python
>>> getattr(obj, 'z') # 获取属性'z'
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
AttributeError: 'MyObject' object has no attribute 'z'
```

可以传入一个`default`参数, 如果属性不存在, 就返回默认值：

```python
>>> getattr(obj, 'z', 404) # 获取属性'z', 如果不存在, 返回默认值404
404
```

也可以获得对象的方法：

```python
>>> hasattr(obj, 'power') # 有属性'power'吗？
True
>>> getattr(obj, 'power') # 获取属性'power'
<bound method MyObject.power of <__main__.MyObject object at 0x10077a6a0>>
>>> fn = getattr(obj, 'power') # 获取属性'power'并赋值到变量fn
>>> fn # fn指向obj.power
<bound method MyObject.power of <__main__.MyObject object at 0x10077a6a0>>
>>> fn() # 调用fn()与调用obj.power()是一样的
81
```

#### 小结-获取对象信息

通过内置的一系列函数, 我们可以对任意一个Python对象进行剖析, 拿到其内部的数据. 要注意的是, 只有在不知道对象信息的时候, 我们才会去获取对象信息. 如果可以直接写：

```python
sum = obj.x + obj.y
```

就不要写：

```python
sum = getattr(obj, 'x') + getattr(obj, 'y')
```

一个正确的用法的例子如下：

```python
def readImage(fp):
    if hasattr(fp, 'read'):
        return readData(fp)
    return None
```

假设我们希望从文件流`fp`中读取图像, 我们首先要判断该`fp`对象是否存在`read`方法, 如果存在, 则该对象是一个流, 如果不存在, 则无法读取. `hasattr()`就派上了用场. 

请注意, 在Python这类动态语言中, 根据鸭子类型, 有`read()`方法, 不代表该`fp`对象就是一个文件流, 它也可能是网络流, 也可能是内存中的一个字节流, 但只要`read()`方法返回的是有效的图像数据, 就不影响读取图像的功能. 

### 实例属性和类属性

由于Python是动态语言, 根据类创建的实例可以任意绑定属性. 

给实例绑定属性的方法是通过实例变量, 或者通过`self`变量：

```python
class Student(object):
    def __init__(self, name):
        self.name = name

s = Student('Bob')
s.score = 90
```

但是, 如果Student类本身需要绑定一个属性呢？可以直接在class中定义属性, 这种属性是类属性, 归Student类所有：

```python
class Student(object):
    name = 'Student'
```

当我们定义了一个类属性后, 这个属性虽然归类所有, 但类的所有实例都可以访问到. 来测试一下：

```python
>>> class Student(object):
...     name = 'Student'
...
>>> s = Student() # 创建实例s
>>> print(s.name) # 打印name属性, 因为实例并没有name属性, 所以会继续查找class的name属性
Student
>>> print(Student.name) # 打印类的name属性
Student
>>> s.name = 'Michael' # 给实例绑定name属性
>>> print(s.name) # 由于实例属性优先级比类属性高, 因此, 它会屏蔽掉类的name属性
Michael
>>> print(Student.name) # 但是类属性并未消失, 用Student.name仍然可以访问
Student
>>> del s.name # 如果删除实例的name属性
>>> print(s.name) # 再次调用s.name, 由于实例的name属性没有找到, 类的name属性就显示出来了
Student
```

从上面的例子可以看出, 在编写程序的时候, 千万不要对实例属性和类属性使用相同的名字, 因为相同名称的实例属性将屏蔽掉类属性, 但是当你删除实例属性后, 再使用相同的名称, 访问到的将是类属性. 

#### 练习-实例属性和类属性

(有了`__init__`方法, 在创建实例的时候, 就不能传入空的参数了, 必须传入与`__init__`方法匹配的参数, 但`self`不需要传, Python解释器自己会把实例变量传进去)

为了统计学生人数, 可以给Student类增加一个类属性, 每创建一个实例, 该属性自动增加：

```python
# -*- coding: utf-8 -*-

class Student(object):
    count=0

    def __init__(self, name):
        self.name = name
        Student.count +=1


# 测试:
print(Student.count)

if Student.count != 0:
    print('测试失败!')
else:
    bart = Student('Bart')
    print(Student.count)
    if Student.count != 1:
        print('测试失败!')
    else:
        lisa = Student('Bart')
        print(Student.count)
        if Student.count != 2:
            print('测试失败!')
        else:
            print('Students:', Student.count)
            print('测试通过!')
```

#### 小结-实例属性和类属性

+ 实例属性属于各个实例所有, 互不干扰；
+ 类属性属于类所有, 所有实例共享一个属性；
+ 不要对实例属性和类属性使用相同的名字, 否则将产生难以发现的错误. 

## 面向对象高级编程

数据封装, 继承和多态只是面向对象程序设计中最基础的3个概念. 
在Python中, 面向对象还有很多高级特性, 允许我们写出非常强大的功能. 

我们会讨论多重继承, 定制类, 元类等概念. 

### 使用__slots__

正常情况下, 当我们定义了一个class, 创建了一个class的实例后, 我们可以给该实例绑定任何属性和方法, 这就是动态语言的灵活性. 先定义class：

```python
class Student(object):
    pass
```

然后, 尝试给实例绑定一个属性：

```python
>>> s = Student()
>>> s.name = 'Michael' # 动态给实例绑定一个属性
>>> print(s.name)
Michael
```

还可以尝试给实例绑定一个方法：

```python
>>> def set_age(self, age): # 定义一个函数作为实例方法
...     self.age = age
...
>>> from types import MethodType
>>> s.set_age = MethodType(set_age, s) # 给实例绑定一个方法
>>> s.set_age(25) # 调用实例方法
>>> s.age # 测试结果
25
```

但是, 给一个实例绑定的方法, 对另一个实例是不起作用的：

```python
>>> s2 = Student() # 创建新的实例
>>> s2.set_age(25) # 尝试调用方法
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
AttributeError: 'Student' object has no attribute 'set_age'
```

为了给所有实例都绑定方法, 可以给class绑定方法：

```python
>>> def set_score(self, score):
...         self.score = score
...
>>> Student.set_score = set_score
```

给class绑定方法后, 所有实例均可调用：

```python
>>> s.set_score(100)
>>> s.score
100
>>> s2.set_score(99)
>>> s2.score
99
```

通常情况下, 上面的`set_score`方法可以直接定义在class中, 但动态绑定允许我们在程序运行的过程中动态给class加上功能, 这在静态语言中很难实现. 

#### 使用__slots__-2

但是, 如果我们想要限制实例的属性怎么办？
比如, 只允许对Student实例添加`name`和`age`属性. 

为了达到限制的目的, Python允许在定义`class`的时候, 定义一个特殊的`__slots__`变量, 来限制该`class`实例能添加的属性：

```python
class Student(object):
    __slots__ = ('name', 'age') # 用tuple定义允许绑定的属性名称
```

然后, 我们试试：

```python
>>> s = Student() # 创建新的实例
>>> s.name = 'Michael' # 绑定属性'name'
>>> s.age = 25 # 绑定属性'age'
>>> s.score = 99 # 绑定属性'score'
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
AttributeError: 'Student' object has no attribute 'score'
```

由于`'score'`没有被放到`__slots__`中, 所以不能绑定`score`属性, 
试图绑定`score`将得到`AttributeError`的错误. 

使用`__slots__`要注意, `__slots__`定义的属性仅对当前类实例起作用, 对继承的子类是不起作用的：

```python
>>> class GraduateStudent(Student):
...     pass
...
>>> g = GraduateStudent()
>>> g.score = 9999
```

除非在子类中也定义`__slots__`, 这样, 
子类实例允许定义的属性就是自身的`__slots__`加上父类的`__slots__`. 

### 使用@property

在绑定属性时, 如果我们直接把属性暴露出去, 
虽然写起来很简单, 但是, 没办法检查参数, 导致可以把成绩随便改：

```python
s = Student()
s.score = 9999
```

这显然不合逻辑. 为了限制`score`的范围, 可以通过一个`set_score()`方法来设置成绩, 再通过一个`get_score()`来获取成绩, 这样, 在`set_score()`方法里, 就可以检查参数：

```python
class Student(object):

    def get_score(self):
         return self._score

    def set_score(self, value):
        if not isinstance(value, int):
            raise ValueError('score must be an integer!')
        if value < 0 or value > 100:
            raise ValueError('score must between 0 ~ 100!')
        self._score = value
```

现在, 对任意的Student实例进行操作, 就不能随心所欲地设置score了：

```python
>>> s = Student()
>>> s.set_score(60) # ok!
>>> s.get_score()
60
>>> s.set_score(9999)
Traceback (most recent call last):
  ...
ValueError: score must between 0 ~ 100!
```

但是, 上面的调用方法又略显复杂, 没有直接用属性这么直接简单. 

有没有既能检查参数, 又可以用类似属性这样简单的方式来访问类的变量呢？对于追求完美的Python程序员来说, 这是必须要做到的！

还记得装饰器(decorator)可以给函数动态加上功能吗？对于类的方法, 装饰器一样起作用. Python内置的@property装饰器就是负责把一个方法变成属性调用的：

```python
class Student(object):

    @property
    def score(self):
        return self._score

    @score.setter
    def score(self, value):
        if not isinstance(value, int):
            raise ValueError('score must be an integer!')
        if value < 0 or value > 100:
            raise ValueError('score must between 0 ~ 100!')
        self._score = value
```

`@property`的实现比较复杂, 我们先考察如何使用. 把一个`getter`方法变成属性, 只需要加上`@property`就可以了, 此时, `@property`本身又创建了另一个装饰器`@score.setter`, 负责把一个`setter`方法变成属性赋值, 于是, 我们就拥有一个可控的属性操作：

```python
>>> s = Student()
>>> s.score = 60 # OK, 实际转化为s.set_score(60)
>>> s.score # OK, 实际转化为s.get_score()
60
>>> s.score = 9999
Traceback (most recent call last):
  ...
ValueError: score must between 0 ~ 100!
```

注意到这个神奇的`@property`, 我们在对实例属性操作的时候, 就知道该属性很可能不是直接暴露的, 而是通过`getter`和`setter`方法来实现的. 

还可以定义只读属性, 只定义`getter`方法, 不定义`setter`方法就是一个只读属性：

```python
class Student(object):

    @property
    def birth(self):
        return self._birth

    @birth.setter
    def birth(self, value):
        self._birth = value

    @property
    def age(self):
        return 2015 - self._birth
```

上面的`birth`是可读写属性, 而`age`就是一个只读属性, 因为`age`可以根据`birth`和当前时间计算出来. 

#### 小结-使用@property

`@property`广泛应用在类的定义中, 可以让调用者写出简短的代码, 同时保证对参数进行必要的检查, 这样, 程序运行时就减少了出错的可能性. 

#### 练习-使用@property

请利用`@property`给一个`Screen`对象加上`width`和`height`属性, 以及一个只读属性`resolution`：

```python
# -*- coding: utf-8 -*-

class Screen(object):
    @property
    def width(self):
        return self._width

    @width.setter
    def width(self, value):
        self._width = value

    @property
    def height(self):
        return self._height

    @height.setter
    def height(self, value):
        self._height = value

    @property
    def resolution(self):
        self._resolution=786432
        return self._resolution


# 测试:
s = Screen()
s.width = 1024
s.height = 768
print('resolution =', s.resolution)
if s.resolution == 786432:
    print('测试通过!')
else:
    print('测试失败!')
```

### 多重继承

继承是面向对象编程的一个重要的方式, 因为通过继承, 子类就可以扩展父类的功能. 

回忆一下Animal类层次的设计, 假设我们要实现以下4种动物：

+ Dog - 狗狗；
+ Bat - 蝙蝠；
+ Parrot - 鹦鹉；
+ Ostrich - 鸵鸟. 

如果按照哺乳动物和鸟类归类, 我们可以设计出这样的类的层次：

```python
Animal->{Mammal, Bird}->{Dog,Bat,Parrot,Ostrich}
```

但是如果按照“能跑”和“能飞”来归类, 我们就应该设计出这样的类的层次：

```python
Animal->{Runnable,Flyable}->{Dog,Ostrich,Parrot,Bat}
```

如果要把上面的两种分类都包含进来, 我们就得设计更多的层次：

+ 哺乳类：能跑的哺乳类, 能飞的哺乳类；
+ 鸟类：能跑的鸟类, 能飞的鸟类. 

这么一来, 类的层次就复杂了：

```python
Animal ->{Mammal,Bird}->
{MRun,MFly,BRun,BFly}->{Dog,Bat,Ostrich,Parrot}
```

如果要再增加“宠物类”和“非宠物类”, 
这么搞下去, 类的数量会呈指数增长, 很明显这样设计是不行的. 

正确的做法是采用多重继承. 首先, 主要的类层次仍按照哺乳类和鸟类设计：

```python
class Animal(object):
    pass

# 大类:
class Mammal(Animal):
    pass

class Bird(Animal):
    pass

# 各种动物:
class Dog(Mammal):
    pass

class Bat(Mammal):
    pass

class Parrot(Bird):
    pass

class Ostrich(Bird):
    pass
```

现在, 我们要给动物再加上`Runnable`和`Flyable`的功能, 
只需要先定义好`Runnable`和`Flyable`的类：

```python
class Runnable(object):
    def run(self):
        print('Running...')

class Flyable(object):
    def fly(self):
        print('Flying...')
```

对于需要Runnable功能的动物, 就多继承一个R`unnable`, 例如`Dog`：

```python
class Dog(Mammal, Runnable):
    pass
```

对于需要Flyable功能的动物, 就多继承一个Flyable, 例如Bat：

```python
class Bat(Mammal, Flyable):
    pass
```

通过多重继承, 一个子类就可以同时获得多个父类的所有功能. 

#### MixIn

在设计类的继承关系时, 通常, 主线都是单一继承下来的, 例如, `Ostrich`继承自`Bird`. 但是, 如果需要“混入”额外的功能, 通过多重继承就可以实现, 比如, 让`Ostrich`除了继承自`Bird`外, 再同时继承`Runnable`. 这种设计通常称之为`MixIn`. 

为了更好地看出继承关系, 我们把`Runnable`和`Flyable`改为`RunnableMixIn`和`FlyableMixIn`. 类似的, 你还可以定义出肉食动物CarnivorousMixIn和植食动物HerbivoresMixIn, 让某个动物同时拥有好几个MixIn：

```python
class Dog(Mammal, RunnableMixIn, CarnivorousMixIn):
    pass
```

MixIn的目的就是给一个类增加多个功能, 
这样, 在设计类的时候, 
**我们优先考虑通过多重继承来组合多个MixIn的功能**, 
而不是设计多层次的复杂的继承关系. 

Python自带的很多库也使用了MixIn. 举个例子, Python自带了`TCPServer`和`UDPServer`这两类网络服务, 而要同时服务多个用户就必须使用多进程或多线程模型, 这两种模型由`ForkingMixIn`和`ThreadingMixIn`提供. 通过组合, 我们就可以创造出合适的服务来. 

比如, 编写一个多进程模式的TCP服务, 定义如下：

```python
class MyTCPServer(TCPServer, ForkingMixIn):
    pass
```

编写一个多线程模式的UDP服务, 定义如下：

```python
class MyUDPServer(UDPServer, ThreadingMixIn):
    pass
```

如果你打算搞一个更先进的协程模型, 可以编写一个`CoroutineMixIn`：

```python
class MyTCPServer(TCPServer, CoroutineMixIn):
    pass
```

这样一来, 我们不需要复杂而庞大的继承链, 只要选择组合不同的类的功能, 就可以快速构造出所需的子类. 

#### 小结-多重继承

+ 由于Python允许使用多重继承, 因此, `MixIn`就是一种常见的设计. 
+ 只允许单一继承的语言(如Java)不能使用MixIn的设计. 

### 定制类

[rest api介绍][]
[什么是API, SDK和API之间有什么关系呢？][]

[什么是API, SDK和API之间有什么关系呢？]:https://www.jianshu.com/p/dd2eff92e8fc

[rest api介绍]: https://www.jianshu.com/p/75389ea9a90b

看到类似`__slots__`这种形如`__xxx__`的变量或者函数名就要注意, 这些在Python中是有特殊用途的. 

`__slots__`我们已经知道怎么用了, `__len__()`方法我们也知道是为了能让class作用于`len()`函数. 

除此之外, Python的class中还有许多这样有特殊用途的函数, 可以帮助我们定制类. 

#### __str__

我们先定义一个Student类, 打印一个实例：

```python
>>> class Student(object):
...     def __init__(self, name):
...         self.name = name
...
>>> print(Student('Michael'))
<__main__.Student object at 0x109afb190>
```

打印出一堆`<__main__.Student object at 0x109afb190>`, 不好看. 

怎么才能打印得好看呢？只需要定义好`__str__()`方法, 返回一个好看的字符串就可以了：

```python
>>> class Student(object):
...     def __init__(self, name):
...         self.name = name
...     def __str__(self):
...         return 'Student object (name: %s)' % self.name
...
>>> print(Student('Michael'))
Student object (name: Michael)
```

这样打印出来的实例, 不但好看, 而且容易看出实例内部重要的数据. 

但是细心的朋友会发现直接敲变量不用print, 打印出来的实例还是不好看：

```python
>>> s = Student('Michael')
>>> s
<__main__.Student object at 0x109afb310>
```

这是因为直接显示变量调用的不是`__str__()`, 而是`__repr__()`, 两者的区别是`__str__()`返回用户看到的字符串, 而`__repr__()`返回程序开发者看到的字符串, 也就是说, `__repr__()`是为调试服务的. 

解决办法是再定义一个`__repr__()`. 但是通常`__str__()`和`__repr__()`代码都是一样的, 所以, 有个偷懒的写法：

```python
class Student(object):
    def __init__(self, name):
        self.name = name
    def __str__(self):
        return 'Student object (name=%s)' % self.name
    __repr__ = __str__

```

#### __iter__

如果一个类想被用于`for ... in`循环, 类似list或tuple那样, 就必须实现一个`__iter__()`方法, 该方法返回一个迭代对象, 然后, Python的for循环就会不断调用该迭代对象的`__next__()`方法拿到循环的下一个值, 直到遇到`StopIteration`错误时退出循环. 

我们以斐波那契数列为例, 写一个`Fib`类, 可以作用于`for`循环：

```python
class Fib(object):
    def __init__(self):
        self.a, self.b = 0, 1 # 初始化两个计数器a, b

    def __iter__(self):
        return self # 实例本身就是迭代对象, 故返回自己

    def __next__(self):
        self.a, self.b = self.b, self.a + self.b # 计算下一个值
        if self.a > 100000: # 退出循环的条件
            raise StopIteration()
        return self.a # 返回下一个值
```

现在, 试试把`Fib`实例用于`for`循环：

```python
>>> for n in Fib():
...     print(n)
...
1
1
2
3
5
...
46368
75025
```

#### __getitem__

Fib实例虽然能作用于for循环, 看起来和list有点像, 但是, 把它当成list来使用还是不行, 比如, 取第`5`个元素：

```python
>>> Fib()[5]
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: 'Fib' object does not support indexing
```

要表现得像list那样按照下标取出元素, 需要实现`__getitem__()`方法：

```python
class Fib(object):
    def __getitem__(self, n):
        a, b = 1, 1
        for x in range(n):
            a, b = b, a + b
        return a
```

现在, 就可以按下标访问数列的任意一项了：

```python
>>> f = Fib()
>>> f[0]
1
>>> f[1]
1
>>> f[2]
2
>>> f[3]
3
>>> f[10]
89
>>> f[100]
573147844013817084101
```

但是list有个神奇的切片方法：

```python
>>> list(range(100))[5:10]
[5, 6, 7, 8, 9]
```

对于Fib却报错. 原因是`__getitem__()`传入的参数可能是一个`int`, 也可能是一个切片对象`slice`, 所以要做判断：

```python
class Fib(object):
    def __getitem__(self, n):
        if isinstance(n, int): # n是索引
            a, b = 1, 1
            for x in range(n):
                a, b = b, a + b
            return a
        if isinstance(n, slice): # n是切片
            start = n.start
            stop = n.stop
            if start is None:
                start = 0
            a, b = 1, 1
            L = []
            for x in range(stop):
                if x >= start:
                    L.append(a)
                a, b = b, a + b
            return L
```

现在试试Fib的切片：

```python
>>> f = Fib()
>>> f[0:5]
[1, 1, 2, 3, 5]
>>> f[:10]
[1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
```

但是没有对`step`参数作处理：

```python
>>> f[:10:2]
[1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89]
```

也没有对负数作处理, 所以, 要正确实现一个`__getitem__()`还是有很多工作要做的. 

此外, 如果把对象看成`dict`, `__getitem__()`的参数也可能是一个可以作`key`的`object`, 例如`str`. 

与之对应的是`__setitem__()`方法, 把对象视作list或dict来对集合赋值. 最后, 还有一个`__delitem__()`方法, 用于删除某个元素. 

总之, 通过上面的方法, 我们自己定义的类表现得和Python自带的list, tuple, dict没什么区别, 这完全归功于动态语言的“鸭子类型”, 不需要强制继承某个接口. 

#### __getattr__

正常情况下, 当我们调用类的方法或属性时, 如果不存在, 就会报错. 
比如定义Student类：

```python
class Student(object):

    def __init__(self):
        self.name = 'Michael'
```

调用name属性, 没问题, 但是, 调用不存在的score属性, 就有问题了：

```python
>>> s = Student()
>>> print(s.name)
Michael
>>> print(s.score)
Traceback (most recent call last):
  ...
AttributeError: 'Student' object has no attribute 'score'
```

错误信息很清楚地告诉我们, 没有找到`score`这个`attribute`. 

要避免这个错误, 除了可以加上一个`score`属性外, Python还有另一个机制, 那就是写一个`__getattr__()`方法, 动态返回一个属性. 修改如下：

```python
class Student(object):

    def __init__(self):
        self.name = 'Michael'

    def __getattr__(self, attr):
        if attr=='score':
            return 99
```

当调用不存在的属性时, 比如`score`, Python解释器会试图调用`__getattr__(self, 'score')`来尝试获得属性, 这样, 我们就有机会返回`score`的值：

```python
>>> s = Student()
>>> s.name
'Michael'
>>> s.score
99
```

返回函数也是完全可以的：

```python
class Student(object):

    def __getattr__(self, attr):
        if attr=='age':
            return lambda: 25
```

只是调用方式要变为：

```python
>>> s.age()
25
```

注意, 只有在没有找到属性的情况下, 才调用`__getattr__`, 已有的属性, 比如`name`, 不会在`__getattr__`中查找. 

此外, 注意到任意调用如`s.abc`都会返回`None`, 这是因为我们定义的`__getattr__`默认返回就是`None`. 要让class只响应特定的几个属性, 我们就要按照约定, 抛出`AttributeError`的错误：

```python
class Student(object):

    def __getattr__(self, attr):
        if attr=='age':
            return lambda: 25
        raise AttributeError('\'Student\' object has no attribute \'%s\'' % attr)
```

这实际上可以把一个类的所有属性和方法调用全部动态化处理了, 不需要任何特殊手段. 

这种完全动态调用的特性有什么实际作用呢？作用就是, 可以针对完全动态的情况作调用. 

举个例子：

现在很多网站都搞`REST API`, 比如新浪微博, 豆瓣啥的, 调用API的URL类似：

+ `http://api.server/user/friends`
+ `http://api.server/user/timeline/list`

如果要写SDK, 给每个URL对应的API都写一个方法, 那得累死, 而且, API一旦改动, SDK也要改. 

利用完全动态的`__getattr__`, 我们可以写出一个链式调用：

```python
class Chain(object):

    def __init__(self, path=''):
        self._path = path

    def __getattr__(self, path):
        return Chain('%s/%s' % (self._path, path))

    def __str__(self):
        return self._path

    __repr__ = __str__
```

这是一个链式调用, 依次传入参数, 产生新的实例

试试：

```python
>>> Chain().status.user.timeline.list
'/status/user/timeline/list'
```

这样, 无论API怎么变, SDK都可以根据URL实现完全动态的调用, 而且, 不随API的增加而改变！

还有些REST API会把参数放到URL中, 比如GitHub的API：

```python
GET /users/:user/repos
```

调用时, 需要把`:user`替换为实际用户名. 如果我们能写出这样的链式调用：

```python
Chain().users('michael').repos
```

就可以非常方便地调用API了. 有兴趣的童鞋可以试试写出来. 

```python
class Chain(object):

    def __init__(self, path=''):
        self._path = path

    def users(self, user):
        return Chain('%s/users/:%s' % (self._path, user))

    def __getattr__(self, path):
        return Chain('%s/%s' % (self._path, path))

    def __str__(self):
        return self._path

    __repr__ = __str__

print(Chain().users('michael').repos)
```

#### __call__

一个对象实例可以有自己的属性和方法, 当我们调用实例方法时, 我们用`instance.method()`来调用. 能不能直接在实例本身上调用呢？在Python中, 答案是肯定的. 

任何类, 只需要定义一个`__call__()`方法, 就可以直接对实例进行调用. 请看示例：

```python
class Student(object):
    def __init__(self, name):
        self.name = name

    def __call__(self):
        print('My name is %s.' % self.name)
```

调用方式如下：

```python
>>> s = Student('Michael')
>>> s() # self参数不要传入
My name is Michael.
```

`__call__()`还可以定义参数. 对实例进行直接调用就好比对一个函数进行调用一样, 所以你完全可以把对象看成函数, 把函数看成对象, 因为这两者之间本来就没啥根本的区别. 

如果你把对象看成函数, 那么函数本身其实也可以在运行期动态创建出来, 因为类的实例都是运行期创建出来的, 这么一来, 我们就模糊了对象和函数的界限. 

那么, 怎么判断一个变量是对象还是函数呢？其实, 更多的时候, 我们需要判断一个对象是否能被调用, 能被调用的对象就是一个`Callable`对象, 比如函数和我们上面定义的带有`__call__()`的类实例：

```python
>>> callable(Student())
True
>>> callable(max)
True
>>> callable([1, 2, 3])
False
>>> callable(None)
False
>>> callable('str')
False
```

通过`callable()`函数, 我们就可以判断一个对象是否是“可调用”对象. 

#### 小结-

Python的class允许定义许多定制方法, 可以让我们非常方便地生成特定的类. 

本节介绍的是最常用的几个定制方法, 还有很多可定制的方法, 请参考Python的官方文档. 

[Python的官方文档][]

[Python的官方文档]: docs.python.org/3/reference/datamodel.html#special-method-names

### 使用枚举类

当我们需要定义常量时, 一个办法是用大写变量通过整数来定义, 例如月份：

```python
JAN = 1
FEB = 2
MAR = 3
...
NOV = 11
DEC = 12
```

好处是简单, 缺点是类型是int, 并且仍然是变量. 

更好的方法是为这样的枚举类型定义一个class类型, 
然后, 每个常量都是class的一个唯一实例. 
Python提供了`Enum`类来实现这个功能：

```python
from enum import Enum

Month = Enum('Month', ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
```

这样我们就获得了Month类型的枚举类, 
可以直接使用`Month.Jan`来引用一个常量, 或者枚举它的所有成员：

```python
for name, member in Month.__members__.items():
    print(name, '=>', member, ',', member.value)
```

`value`属性则是自动赋给成员的int常量, 默认从`1`开始计数. 

如果需要更精确地控制枚举类型, 可以从`Enum`派生出自定义类：

```python
from enum import Enum, unique

@unique
class Weekday(Enum):
    Sun = 0 # Sun的value被设定为0
    Mon = 1
    Tue = 2
    Wed = 3
    Thu = 4
    Fri = 5
    Sat = 6
```

`@unique`装饰器可以帮助我们检查保证没有重复值. 

访问这些枚举类型可以有若干种方法：

```python
>>> day1 = Weekday.Mon
>>> print(day1)
Weekday.Mon
>>> print(Weekday.Tue)
Weekday.Tue
>>> print(Weekday['Tue'])
Weekday.Tue
>>> print(Weekday.Tue.value)
2
>>> print(day1 == Weekday.Mon)
True
>>> print(day1 == Weekday.Tue)
False
>>> print(Weekday(1))
Weekday.Mon
>>> print(day1 == Weekday(1))
True
>>> Weekday(7)
Traceback (most recent call last):
  ...
ValueError: 7 is not a valid Weekday
>>> for name, member in Weekday.__members__.items():
...     print(name, '=>', member)
...
Sun => Weekday.Sun
Mon => Weekday.Mon
Tue => Weekday.Tue
Wed => Weekday.Wed
Thu => Weekday.Thu
Fri => Weekday.Fri
Sat => Weekday.Sat
```

可见, 既可以用成员名称引用枚举常量, 又可以直接根据`value`的值获得枚举常量

练习

把`Student`的`gender`属性改造为枚举类型, 可以避免使用字符串：

```python
# -*- coding: utf-8 -*-
from enum import Enum, unique

class Gender(Enum):
    Male = 0
    Female = 1

class Student(object):
    def __init__(self, name, gender):
        self.name = name
        self.gender = gender

# 测试:
bart = Student('Bart', Gender.Male)
if bart.gender == Gender.Male:
    print('测试通过!')
else:
    print('测试失败!')
```

[Python 的枚举类型][]

[Python 的枚举类型]: https://segmentfault.com/a/1190000017327003

### 使用元类

#### type()

动态语言和静态语言最大的不同, 就是函数和类的定义, 不是编译时定义的, 而是运行时动态创建的. 

比方说我们要定义一个Hello的class, 就写一个hello.py模块：

```python
class Hello(object):
    def hello(self, name='world'):
        print('Hello, %s.' % name)
```

当Python解释器载入hello模块时, 就会依次执行该模块的所有语句, 执行结果就是动态创建出一个Hello的class对象, 测试如下：

```python
>>> from hello import Hello
>>> h = Hello()
>>> h.hello()
Hello, world.
>>> print(type(Hello))
<class 'type'>
>>> print(type(h))
<class 'hello.Hello'>
```

`type()`函数可以查看一个类型或变量的类型, Hello是一个class, 它的类型就是type, 而`h`是一个实例, 它的类型就是class Hello. 

我们说class的定义是运行时动态创建的, 而创建`class`的方法就是使用`type()`函数. 

type()函数既可以返回一个对象的类型, 又可以创建出新的类型, 比如, 我们可以通过`type()`函数创建出Hello类, 而无需通过class Hello(object)...的定义：

```python
>>> def fn(self, name='world'): # 先定义函数
...     print('Hello, %s.' % name)
...
>>> Hello = type('Hello', (object,), dict(hello=fn)) # 创建Hello class
>>> h = Hello()
>>> h.hello()
Hello, world.
>>> print(type(Hello))
<class 'type'>
>>> print(type(h))
<class '__main__.Hello'>
```

要创建一个class对象, `type()`函数依次传入3个参数：

+ class的名称；
+ 继承的父类集合, 注意Python支持多重继承, 如果只有一个父类, 别忘了tuple+ 单元素写法；
+ class的方法名称与函数绑定, 这里我们把函数fn绑定到方法名hello上. 

通过`type()`函数创建的类和直接写class是完全一样的, 因为Python解释器遇到class定义时, 仅仅是扫描一下class定义的语法, 然后调用`type()`函数创建出class. 

正常情况下, 我们都用`class Xxx...`来定义类, 但是, `type()`函数也允许我们动态创建出类来, 也就是说, 动态语言本身支持运行期动态创建类, 这和静态语言有非常大的不同, 要在静态语言运行期创建类, 必须构造源代码字符串再调用编译器, 或者借助一些工具生成字节码实现, 本质上都是动态编译, 会非常复杂. 
metaclass

除了使用`type()`动态创建类以外, 要控制类的创建行为, 还可以使用`metaclass`. 

`metaclass`, 直译为元类, 简单的解释就是：

当我们定义了类以后, 就可以根据这个类创建出实例, 所以：先定义类, 然后创建实例. 

但是如果我们想创建出类呢？那就必须根据`metaclass`创建出类, 所以：先定义`metaclass`, 然后创建类. 

连接起来就是：先定义`metaclass`, 就可以创建类, 最后创建实例. 

所以, `metaclass`允许你创建类或者修改类. 换句话说, 你可以把类看成是`metaclass`创建出来的“实例”. 

`metaclass`是Python面向对象里最难理解, 也是最难使用的魔术代码. 正常情况下, 你不会碰到需要使用`metaclass`的情况, 所以, 以下内容看不懂也没关系, 因为基本上你不会用到. 

我们先看一个简单的例子, 这个`metaclass`可以给我们自定义的MyList增加一个`add`方法：

定义List`Metaclass`, 按照默认习惯, `metaclass`的类名总是以`Metaclass`结尾, 以便清楚地表示这是一个`metaclass`：

```python
# metaclass是类的模板, 所以必须从 type 类型派生：
class ListMetaclass(type):
    def __new__(cls, name, bases, attrs):
        attrs['add'] = lambda self, value: self.append(value)
        return type.__new__(cls, name, bases, attrs)
```

有了ListMetaclass, 我们在定义类的时候还要指示使用ListMetaclass来定制类, 传入关键字参数metaclass：

```python
class MyList(list, metaclass=ListMetaclass):
    pass
```

当我们传入关键字参数metaclass时, 魔术就生效了, 它指示Python解释器在创建MyList时, 要通过`ListMetaclass.__new__()`来创建, 在此, 我们可以修改类的定义, 比如, 加上新的方法, 然后, 返回修改后的定义. 

`__new__()`方法接收到的参数依次是：

+ 当前准备创建的类的对象；
+ 类的名字；
+ 类继承的父类集合；
+ 类的方法集合. 

测试一下MyList是否可以调用`add()`方法：

```python
>>> L = MyList()
>>> L.add(1)
>> L
[1]
```

而普通的list没有`add()`方法：

```python
>>> L2 = list()
>>> L2.add(1)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
AttributeError: 'list' object has no attribute 'add'
```

动态修改有什么意义？直接在MyList定义中写上`add()`方法不是更简单吗？正常情况下, 确实应该直接写, 通过metaclass修改纯属变态. 

但是, 总会遇到需要通过metaclass修改类定义的. ORM就是一个典型的例子. 

ORM全称“Object Relational Mapping”, 即对象-关系映射, 就是把关系数据库的一行映射为一个对象, 也就是一个类对应一个表, 这样, 写代码更简单, 不用直接操作SQL语句. 

要编写一个ORM框架, 所有的类都只能动态定义, 因为只有使用者才能根据表的结构定义出对应的类来. 

#### 小结-使用元类

metaclass是Python中非常具有魔术性的对象, 它可以改变类创建时的行为. 这种强大的功能使用起来务必小心. 
