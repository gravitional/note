# perlobj - Perl 对象参考

+ 名称; `perlobj` - Perl对象参考

+ 描述;

本文件提供了 Perl 的面向对象特性的参考.
如果你正在寻找关于 Perl 中面向对象的介绍编程, 请看 `perlootut`.

为了理解 `Perl对象`, 你首先需要理解 Perl 中的 `引用`. 详见 `perlreftut`.

本文档从头到尾描述了 Perl 的所有面向对象(OO)的特性.
如果你只是想写一些你自己的面向对象的代码,
你可能更适合使用 `perlootut` 中描述的 CPAN 中的对象系统.

如果你想编写自己的对象系统, 或者你需要维护从头开始实现对象的代码,
那么这份文件将帮助你了解 Perl 是如何面向对象的.

有几个基本原则定义了面向对象的 Perl.

1. 对象是一个简单的数据结构, 它知道自己属于哪个类.
2. `类` 是一个简单的`pacakge`(包). `类` 提供了期望对 `对象` 进行操作的 `方法`.
3. `方法` 是一个简单的 `subroutine`(`子程序`), 它期望以 `对象` 的 `引用` (或 `包`的名称, 用于`类方法`)作为`首参数`.

让我们深入了解一下这些原则中的每一条.

## 对象只是一个数据结构

不像其他许多支持 `面向对象` 的语言, Perl 没有为 `构造对象` 提供任何特殊的语法.
对象是只是 Perl 的 `数据结构`(`hashes`, `arrays`, `scalars`, `filehandles` 等)
它被明确地与特定的 `类` 联系起来.

这种明确的 `联系` 是由内置的 `bless` 函数创建的.
它通常在 `类` 的 `constructor` 子程序中使用.

这是个简单的 构造函数:

```perl
package File;

sub new {
    my $class = shift;
    return bless {}, $class;
    }
```

`new` 这个名字并不特别. 我们可以给 `构造函数` 起别的名称.

```perl
package File;

sub load {
    my $class = shift;
    return bless {}, $class;
}
```

现代 `OO模块` 的惯例是, 总是使用 `new` 作为 `构造函数` 的名称, 但并不要求必须这样做.
在Perl中, 任何将数据结构 `祝福到` 类中的子程序都是有效的 `构造函数`.

在前面的例子中, `{}` 代码创建了对 `空的匿名哈希` 的引用.
然后, `bless` 函数接收该 `引用`, 并将该 `reference` 与 `$class` 中的类联系起来.
在最简单的情况下, `$class` 变量最终将包含字符串 `File`.

我们还可以使用 `变量` 来存储对 `数据结构` 的引用, 然后用它作为 `对象` 被祝福.

```perl
sub new {
    my $class = shift;

    my $self = {};
    bless $self, $class;

    return $self;
}
```

一旦我们祝福了 `$self` 所指的 `哈希`, 我们就可以开始对其调用 `方法`.
如果你想把对象的 `初始化` 放在自己单独的 `方法` 中, 这很有用.

```perl
sub new {
    my $class = shift;

    my $self = {};
    bless $self, $class;

    $self->_initialize();

    return $self;
    }
```

由于该对象也是 `哈希`, 你可以把它当作 `哈希`, 用它来存储与该对象相关的数据.
通常情况下, 类内的代码可以将 `哈希` 作为 `可访问` 的数据结构, 而类外的代码应该始终将该对象视为 `不透明的` . 这就是所谓的封装.
`封装` 意味着对象的用户不需要知道它是如何实现的. 用户只需在对象上调用记录的 `方法`.

然而, 请注意, (与其他大多数 `OO` 语言不同) `Perl` 并没有以任何方式确保或强制执行封装.
如果你想让对象真正 `不透明`, 你需要自己安排.
 这可以通过各种方式来实现, 包括使用 `Inside-Out 对象` 或 CPAN 中的模块.

## 对象是被Blessed; 变量不是

当我们 `bless` 某样东西时, 我们不是在祝福包含 `该事物引用` 的 `变量`,
也不是在祝福该 `变量` 所存储的 `引用`;
我们是在祝福该变量所指向的 `事物`(有时被称为 `referent` ).
这一点最好用这段代码来证明.

```perl
use Scalar::Util 'blessed';
my $foo = {};
my $bar = $foo;
bless $foo, 'Class';
print blessed( $bar ) // 'not blessed';    # prints "Class"
$bar = "some other value";
print blessed( $bar ) // 'not blessed';    # prints "not blessed"
```

当我们在 `变量` 上调用 `bless` 时, 我们实际上是在祝福这个 `变量` 所指向的 `底层数据结构`.
我们不是在祝福 `引用` 本身, 也不是在祝福包含该 `引用` 的 `变量`.
这就是为什么第二次调用 `blessed( $bar )` 会返回 `错误`.
在这一点上, `$bar` 不再存储 `对象` 的 `引用`.

你有时会看到 `旧书` 或 `文档` 中提到 `blessing a reference` 或将 `对象` 描述为 `blessed reference"`, 但这是不正确的.
被祝福为 `对象` 的不是 `引用` , 而是 `引用` 所指向的东西(即 `referent`).

## 类只是包

`Perl` 没有为 `类` 的定义提供任何特殊的语法. `包` 只是包含 `变量` 和 `子程序` 的 `命名空间` .
唯一的区别是, 在 `类` 中, 子程序可能希望以 `对象` 的 `引用`, 或 `类` 的名称作为 `首参数`.
这纯粹是约定俗成的问题, 所以 `类` 可以同时包含 `方法`,  以及 **不对** 对象或类进行操作的`子程序`.

每个 `包` 都包含叫做 `@ISA` 的特殊数组.
`@ISA` 数组包含了该类的 `父类` 的列表, 如果有的话.
这个 `数组` 在Perl进行 `方法解析` 时被检查, 这一点我们将在后面介绍.

当然, 从 `包` 中调用 `方法` 意味着它必须被 `加载`(loaded), 所以你经常想同时 `加载模块` 并将其添加到 `@ISA` 中.
你可以使用 `parent pragma` 在单个步骤中做到这点.
(在较早的代码中, 你可能会遇到 `base pragm`a, 现在已经不鼓励使用这个 `pragma` 了, 除非你要和同样不鼓励的 `fields pragma` 一起工作).

不管 `父类` 是如何设置的, 包的 `@ISA` 变量将包含这些 `父类` 的列表.
它只是 `标量` 的列表, 每个标量都是一个 `字符串`, 对应于包的名称.

所有的 `类` 都隐含地继承于 `UNIVERSAL` 类.
`UNIVERSAL类` 是由Perl核心实现的, 它提供了几个默认的方法,
如 `isa()`, `can()` 和 `VERSION()`.
`UNIVERSAL` 类永远**不会** 出现在 `包` 的 `@ISA` 变量中.

`Perl` 只提供了 method inheritance 作为内置功能.
`属性的继承`(Attribute inheritance) 是由类来实现的. 详见 `编写访问器` 一节.

## `方法`就是`子程序`

`Perl` 并没有为定义 `方法` 提供任何特殊的语法.
`方法` 只是普通的 `子程序`, 用 `sub` 来声明.
`方法` 的特殊之处在于, 它希望收到 `对象` 或 `类` 的名字作为它的 `首参数`.

`Perl` **确实** 为方法的 `调用` 提供了特殊的语法, 即 `->` 操作符.
我们将在后面更详细地讨论这个问题.

你写的大多数 `方法`, 都希望对 `对象` 进行操作.

```perl
sub save {
    my $self = shift;
    open my $fh, '>', $self->path() or die $!;
    print {$fh} $self->data()       or die $!;
    close $fh                       or die $!;
}
```

### 方法调用

在 `对象` 上调用 `方法` 被写成 `$object->method`.

`方法调用`(或 `箭头`)操作符的左边是 `对象`(或类名), 右边是 `方法名`.

```perl
my $pod = File->new( 'perlobj.pod', $data );
$pod->save();
```

在 `解引用` 时也使用 `->` 语法. 它看起来像同一 `操作符`, 但这是两个不同的操作.

当你调用 `方法` 时, `箭头` 左边的东西会作为 `首参数` 传给该方法.
这意味着当我们调用 `Critter->new()` 时, `new()` 方法会收到字符串 `Critter` 作为其首参数.
当我们调用 `$fred->speak()` 时, `$fred` 变量被作为 `首参数` 传递给 `speak()`.

就像任何 `Perl子程序` 一样, 所有在 `@_` 中传递的 `参数` 都是 `原始参数` 的 `别名` . 这包括 `对象本身`.
如果你直接向 `$_[0]` 赋值, 你将改变某 `变量` 的内容, 而它持有该 `对象` 的 `引用` .
我们建议你不要这样做, 除非你清楚地知道你在做什么.

`Perl` 通过观察 `箭头` 的左边知道这个 `方法` 是在哪个 `包` 里.
如果左边是 `包` 的名字, 它就会在那个 `包` 里寻找这个方法.
如果左边是 `对象`, 那么 `Perl` 就会在这个 `对象` 被 blessed into 的 `包` 中寻找这个方法.

如果左边既不是 `包名` 也不是 `对象`, 那么这个 `方法` 的调用将导致 `错误`,
但更多的细微差别请参见 "Method Call Variations" 部分.

### 继承性,Inheritance

我们已经谈到了特殊的 `@ISA` 数组和 `父类pragma`.

当一个 `类` 继承自另一个 `类` 时, `父类` 中定义的任何方法, 对 `子类` 都是可用的.
如果你试图在 `对象` 上调用在它自己的 `类` 中没有定义的 `方法`,
Perl 也会在它可能拥有的任何 `父类` 中寻找这个方法.

```perl
package File::MP3;
use parent 'File';    # sets @File::MP3::ISA = ('File');

my $mp3 = File::MP3->new( 'Andvari.mp3', $data );
$mp3->save();
```

由于我们没有在 `File::MP3` 类中定义一个 `save()` 方法,
`Perl` 将从 `File::MP3` 类的 `父类` 中寻找 `save()` 方法.
如果 `Perl` 不能在继承层次(inheritance hierarchy)中的任何地方找到 `save()` 方法, 它就会 die.

在这个例子中, 它在 `File` 类中找到了 `save()` 方法.
注意, 在这种情况下, 传递给 `save()` 的对象仍然是 `File::MP3` 对象, 尽管该方法是在 `File` 类中找到的.

我们可以在 `子类` 中覆盖 `父类` 的方法.
当我们这样做时, 我们仍然可以用 `SUPER` 伪类(pseudo-class)调用父类的方法.

```perl
sub save {
    my $self = shift;
    say 'Prepare to rock';
    $self->SUPER::save();
}
```

`SUPER` 修饰符**只能**用于方法调用.
你不能把它用于普通的 `子程序调用` 或 `类方法`.

```perl
SUPER::save($thing); # FAIL: 寻找包SUPER中的save() sub

SUPER->save($thing); # FAIL: 寻找SUPER类中的save()方法

$thing->SUPER::save(); # 好: 在父类中寻找save()方法
```

### SUPER是如何解析的

`SUPER` 伪类在 `调用` 所处的 `包` 中解析. 它 **不是** 根据对象的 `类` 来解析的.
这一点很重要, 因为它可以让 `深层继承结构` 中不同层次的 `方法`, 正确调用各自的 `父方法`.

```perl
package A;
    sub new {
        return bless {}, shift;
    }
    sub speak {
        my $self = shift;
        say 'A';
    }

package B;
    use parent -norequire, 'A';
    sub speak {
        my $self = shift;
        $self->SUPER::speak();
        say 'B';
    }

package C;
    use parent -norequire, 'B';
    sub speak {
        my $self = shift;
        $self->SUPER::speak();
        say 'C';
    }

my $c = C->new();
$c->speak();
```

在本例中, 输出如下:

```perl
A
B
C
```

这展示了 `SUPER` 的解析过程.
即使对象被祝福到 `C` 类中, `B` 类中的 `speak()` 方法仍然可以调用 `SUPER::speak()`,
并期望它能正确地在 `B` 的父类(即方法调用所在的类)中查找, 而不是在 `C` 的父类(即 `对象` 所属的类)中查找.

在一些罕见的情况下, 这种基于 `包` 的解决方式可能会成为问题.
如果你把 `子程序` 从一个包复制到另一个包, `SUPER` 解析将基于 `原来的包` 进行.

## 多重继承,Multiple Inheritance

`多重继承` 往往表明了设计上的问题, 但如果你要求的话, Perl 总是给你足够的绳子来吊死你[狗头.jpg].

要声明多个 `父类`, 你只需要将多个 `类` 的名字传递给 `use parent` 即可.

```perl
package MultiChild;
use parent 'Parent1', 'Parent2';
```

## 方法解析顺序

`方法解析顺序` 只在 `多继承` 的情况下重要. 在 `单继承` 的情况下, Perl 只需在 `继承链` 上查找 `方法`.

祖父母
  |
父母
  |
孩子

如果我们在 `孩子` 对象上调用 `方法`, 而这个方法并没有在 `孩子` 类中定义,
那么Perl将在 `父母` 类中寻找这个方法, 如果有必要的话, 再在 `祖父母` 类中寻找.

如果Perl不能在这些类中找到该方法, 它就会出现错误信息而死亡.

当类有多个父类时, 方法的查找顺序会变得更加复杂.

默认情况下, Perl 对 `方法` 进行 `深度优先` 的从 `左到右` 的搜索.
这意味着它从 `@ISA数组` 中的 `第一个父类` 开始, 然后搜索其所有的父类, 祖类等等.
如果找不到这个方法, 它就会去找 `原始类` 的 `@ISA数组` 中的下一个父类, 然后从那里开始搜索.

                共享曾祖
          /                   \
    祖父               祖母
          \                     /
           父亲        母亲
                 \      /
                    孩子

所以根据 `上图`, Perl会搜索 `子`, `父`, `祖父`, `共享曾祖`, `母`, 最后是 `祖母`.
这可能带来问题, 因为在检查 `共享曾祖` 的所有派生类 **之前**, 我们即已尝试在`共享曾祖` 中查找了. (即在搜索 `母` 和 `祖母` 之前).

我们可以用 `mro pragma` 来要求不同的 `方法` 解析顺序.

```perl
package Child;

use mro 'c3';
use parent 'Father', 'Mother';
```

这个 `pragma` 让你切换到 `C3` 解析顺序.
简单地说, "C3" 顺序确保 `共享的父类` 永远不会在 `子类` 之前被搜索到, 所以 `Perl` 现在会搜索:
`孩子`, `父亲`, `祖父", "母", "祖母", 最后是 "共享曾祖".

但是请注意, 这不是 `广度优先`(breadth-first) 的搜索:
在考虑任何 `母亲` 的祖先之前, 所有 `父亲` 的祖先(除了共同的祖先)都会被搜索到.

`C3` 顺序还可以让你用 `next` 伪类调用 `兄弟类` 中的方法.
关于这个功能的更多细节, 请参见 `mro文档`.

### 方法解析缓存

当 `Perl` 搜索 方法时, 它会 `缓存` 该查找结果, 这样以后调用该方法时就不需要再去搜索它了.
改变 类的 `父类` 或向 `类` 添加 `子程序` 将使该类的 `缓存` 失效.

`mro pragma` 提供了一些直接操作方法缓存的函数.

### 编写构造函数

正如我们前面提到的, Perl没有提供特殊的 `构造函数` 语法.
这意味着 `类` 必须实现它自己的构造函数. `构造函数` 是个简单的 `类`方法, 它返回 `新对象`的 `引用`.

`构造函数` 还可以接受定义对象的 `额外参数`. 让我们为我们先前使用的 `文件` 类写个真正的构造函数.

```perl
package File;

sub new {
    my $class = shift;
    my ( $path, $data ) = @_;

    my $self = bless {
        path => $path,
        data => $data,
    }, $class;

    return $self;
}
```

正如你所看到的, 我们已经将 `路径` 和 `文件数据` 存储在 `对象` 本身.
记住, 在引擎盖(hood)下, 这个对象仍然只是 `哈希`.
稍后, 我们将编写 `访问器` 来操作这些数据.

对于我们的 `File::MP3` 类, 我们可以检查, 以确保路径以 `.mp3` 结尾.

```perl
package File::MP3;
sub new {
    my $class = shift;
    my ( $path, $data ) = @_;
    die "You cannot create a File::MP3 without an mp3 extension\n"
        unless $path =~ /\.mp3\z/;
    return $class->SUPER::new(@_);
}

```

这个 `构造函数` 让它的 `父类` 做实际的对象构造.

## 属性,Attributes

An attribute is a piece of data belonging to a particular object. Unlike most object-oriented languages, Perl provides no special syntax or support for declaring and manipulating attributes.

Attributes are often stored in the object itself. For example, if the object is an anonymous hash, we can store the attribute values in the hash using the attribute name as the key.

While it's possible to refer directly to these hash keys outside of the class, it's considered a best practice to wrap all access to the attribute with accessor methods.

This has several advantages. Accessors make it easier to change the implementation of an object later while still preserving the original API.

An accessor lets you add additional code around attribute access. For example, you could apply a default to an attribute that wasn't set in the constructor, or you could validate that a new value for the attribute is acceptable.

Finally, using accessors makes inheritance much simpler. Subclasses can use the accessors rather than having to know how a parent class is implemented internally.

Writing Accessors

As with constructors, Perl provides no special accessor declaration syntax, so classes must provide explicitly written accessor methods. There are two common types of accessors, read-only and read-write.

A simple read-only accessor simply gets the value of a single attribute:

sub path {
    my $self = shift;
    return $self->{path};
}

A read-write accessor will allow the caller to set the value as well as get it:

sub path {
    my $self = shift;
    if (@_) {
        $self->{path} = shift;
    }
    return $self->{path};
}

An Aside About Smarter and Safer Code

Our constructor and accessors are not very smart. They don't check that a $path is defined, nor do they check that a $path is a valid filesystem path.

Doing these checks by hand can quickly become tedious. Writing a bunch of accessors by hand is also incredibly tedious. There are a lot of modules on CPAN that can help you write safer and more concise code, including the modules we recommend in perlootut.

Method Call Variations

Perl supports several other ways to call methods besides the "$object->method()" usage we've seen so far.

Method Names with a Fully Qualified Name

Perl allows you to call methods using their fully qualified name (the package and method name):

my $mp3 = File::MP3->new( 'Regin.mp3', $data );
$mp3->File::save();

When you call a fully qualified method name like "File::save", the method resolution search for the "save" method starts in the "File" class, skipping any "save" method the "File::MP3" class may have defined. It still searches the "File" class's parents if necessary.

While this feature is most commonly used to explicitly call methods inherited from an ancestor class, there is no technical restriction that enforces this:

my $obj = Tree->new();
$obj->Dog::bark();

This calls the "bark" method from class "Dog" on an object of class "Tree", even if the two classes are completely unrelated. Use this with great care.

The "SUPER" pseudo-class that was described earlier is *not* the same as calling a method with a fully-qualified name. See the earlier "Inheritance" section for details.

Method Names as Strings

Perl lets you use a scalar variable containing a string as a method name:

my $file = File->new( $path, $data );

my $method = 'save';
$file->$method();

This works exactly like calling "$file->save()". This can be very useful for writing dynamic code. For example, it allows you to pass a method name to be called as a parameter to another method.

Class Names as Strings

Perl also lets you use a scalar containing a string as a class name:

my $class = 'File';
my $file = $class->new( $path, $data );

Again, this allows for very dynamic code.

Subroutine References as Methods

You can also use a subroutine reference as a method:

my $sub = sub {
    my $self = shift;
    $self->save();
};

$file->$sub();

This is exactly equivalent to writing "$sub->($file)". You may see this idiom in the wild combined with a call to "can":

if ( my $meth = $object->can('foo') ) {
    $object->$meth();
}

Dereferencing Method Call

Perl also lets you use a dereferenced scalar reference in a method call. That's a mouthful, so let's look at some code:

$file->${ \'save' };
$file->${ returns_scalar_ref() };
$file->${ \( returns_scalar() ) };
$file->${ returns_ref_to_sub_ref() };

This works if the dereference produces a string *or* a subroutine reference.

Method Calls on Filehandles

Under the hood, Perl filehandles are instances of the "IO::Handle" or "IO::File" class. Once you have an open filehandle, you can call methods on it. Additionally, you can call methods on the "STDIN", "STDOUT", and "STDERR" filehandles.

open my $fh, '>', 'path/to/file';
$fh->autoflush();
$fh->print('content');
STDOUT->autoflush();

Invoking Class Methods

Because Perl allows you to use barewords for package names and subroutine names, it sometimes interprets a bareword's meaning incorrectly. 
For example, the construct "Class->new()" can be interpreted as either "'Class'->new()" or "Class()->new()". 
In English, that second interpretation reads as "call a subroutine named Class(), then call new() as a method on the return value of Class()". 
If there is a subroutine named "Class()" in the current namespace, Perl will always interpret "Class->new()" as the second alternative: a call to "new()" on the object returned by a call to "Class()"

You can force Perl to use the first interpretation (i.e. as a method call on the class named "Class") in two ways. First, you can append a "::" to the class name:

Class::->new()

Perl will always interpret this as a method call.

Alternatively, you can quote the class name:

'Class'->new()

Of course, if the class name is in a scalar Perl will do the right thing as well:

my $class = 'Class';
$class->new();

Indirect Object Syntax

Outside of the file handle case, use of this syntax is discouraged as it can confuse the Perl interpreter. See below for more details.

Perl supports another method invocation syntax called "indirect object" notation. This syntax is called "indirect" because the method comes before the object it is being invoked on.

This syntax can be used with any class or object method:

my $file = new File $path, $data;
save $file;

We recommend that you avoid this syntax, for several reasons.

First, it can be confusing to read. In the above example, it's not clear if "save" is a method provided by the "File" class or simply a subroutine that expects a file object as its first argument.

When used with class methods, the problem is even worse. Because Perl allows subroutine names to be written as barewords, Perl has to guess whether the bareword after the method is a class name or subroutine name. In other words, Perl can resolve the syntax as either "File->new( $path, $data )" or "new( File( $path, $data ) )".

To parse this code, Perl uses a heuristic based on what package names it has seen, what subroutines exist in the current package, what barewords it has previously seen, and other input. Needless to say, heuristics can produce very surprising results!

Older documentation (and some CPAN modules) encouraged this syntax, particularly for constructors, so you may still find it in the wild. However, we encourage you to avoid using it in new code.

You can force Perl to interpret the bareword as a class name by appending "::" to it, like we saw earlier:

      my $file = new File:: $path, $data;

"bless", "blessed", and "ref"

As we saw earlier, an object is simply a data structure that has been blessed into a class via the "bless" function. The "bless" function can take either one or two arguments:

my $object = bless {}, $class;
my $object = bless {};

In the first form, the anonymous hash is being blessed into the class in $class. In the second form, the anonymous hash is blessed into the current package.

The second form is strongly discouraged, because it breaks the ability of a subclass to reuse the parent's constructor, but you may still run across it in existing code.

If you want to know whether a particular scalar refers to an object, you can use the "blessed" function exported by Scalar::Util, which is shipped with the Perl core.

use Scalar::Util 'blessed';
if ( defined blessed($thing) ) { ... }

If $thing refers to an object, then this function returns the name of the package the object has been blessed into. If $thing doesn't contain a reference to a blessed object, the "blessed" function returns "undef".

Note that "blessed($thing)" will also return false if $thing has been blessed into a class named "0". This is a possible, but quite pathological. Don't create a class named "0" unless you know what you're doing.

Similarly, Perl's built-in "ref" function treats a reference to a blessed object specially. If you call "ref($thing)" and $thing holds a reference to an object, it will return the name of the class that the object has been blessed into.

If you simply want to check that a variable contains an object reference, we recommend that you use "defined blessed($object)", since "ref" returns true values for all references, not just objects.

The UNIVERSAL Class

    All classes automatically inherit from the UNIVERSAL class, which is built-in to the Perl core. This class provides a number of methods, all of which can be called on either a class or an object. You can also choose to override some of these methods in your class. If you do so, we recommend that you follow the built-in semantics described below.

    isa($class)

        The "isa" method returns *true* if the object is a member of the class in $class, or a member of a subclass of $class.

        If you override this method, it should never throw an exception.

DOES($role)

The "DOES" method returns *true* if its object claims to perform the role $role. By default, this is equivalent to "isa". This method is provided for use by object system extensions that implement roles, like "Moose" and "Role::Tiny".

You can also override "DOES" directly in your own classes. If you override this method, it should never throw an exception.

can($method)

The "can" method checks to see if the class or object it was called on has a method named $method. This checks for the method in the class and all of its parents. If the method exists, then a reference to the subroutine is returned. If it does not then "undef" is returned.

If your class responds to method calls via "AUTOLOAD", you may want to overload "can" to return a subroutine reference for methods which your "AUTOLOAD" method handles.

If you override this method, it should never throw an exception.

VERSION($need)

The "VERSION" method returns the version number of the class (package).

If the $need argument is given then it will check that the current version (as defined by the $VERSION variable in the package) is greater than or equal to $need; it will die if this is not the case. This method is called automatically by the "VERSION" form of "use".

    use Package 1.2 qw(some imported subs);
    # implies:
    Package->VERSION(1.2);

We recommend that you use this method to access another package's version, rather than looking directly at $Package::VERSION. The package you are looking at could have overridden the "VERSION" method.

We also recommend using this method to check whether a module has a sufficient version. The internal implementation uses the version module to make sure that different types of version numbers are compared correctly.

AUTOLOAD

If you call a method that doesn't exist in a class, Perl will throw an error. However, if that class or any of its parent classes defines an "AUTOLOAD" method, that "AUTOLOAD" method is called instead.

"AUTOLOAD" is called as a regular method, and the caller will not know the difference. Whatever value your "AUTOLOAD" method returns is returned to the caller.

The fully qualified method name that was called is available in the $AUTOLOAD package global for your class. Since this is a global, if you want to refer to do it without a package name prefix under "strict 'vars'", you need to declare it.

```perl
# XXX - this is a terrible way to implement accessors, but it makes
# for a simple example.
our $AUTOLOAD;
sub AUTOLOAD {
    my $self = shift;
    # Remove qualifier from original method name...
    my $called =  $AUTOLOAD =~ s/.*:://r;
    # Is there an attribute of that name?
    die "No such attribute: $called"
        unless exists $self->{$called};
    # If so, return it...
    return $self->{$called};
}

sub DESTROY { } # see below
```

Without the "our $AUTOLOAD" declaration, this code will not compile under the strict pragma.

As the comment says, this is not a good way to implement accessors. It's slow and too clever by far. However, you may see this as a way to provide accessors in older Perl code. See perlootut for recommendations on OO coding in Perl.

If your class does have an "AUTOLOAD" method, we strongly recommend that you override "can" in your class as well. Your overridden "can" method should return a subroutine reference for any method that your "AUTOLOAD" responds to.

Destructors

When the last reference to an object goes away, the object is destroyed. If you only have one reference to an object stored in a lexical scalar, the object is destroyed when that scalar goes out of scope. If you store the object in a package global, that object may not go out of scope until the program exits.

If you want to do something when the object is destroyed, you can define a "DESTROY" method in your class. This method will always be called by Perl at the appropriate time, unless the method is empty.

This is called just like any other method, with the object as the first argument. It does not receive any additional arguments. However, the $_[0] variable will be read-only in the destructor, so you cannot assign a value to it.

If your "DESTROY" method throws an exception, this will not cause any control transfer beyond exiting the method. The exception will be reported to "STDERR" as a warning, marked "(in cleanup)", and Perl will continue with whatever it was doing before.

Because "DESTROY" methods can be called at any time, you should localize any global status variables that might be set by anything you do in your "DESTROY" method. If you are in doubt about a particular status variable, it doesn't hurt to localize it. There are five global status variables, and the safest way is to localize all five of them:

sub DESTROY {
    local($., $@, $!, $^E, $?);
    my $self = shift;
    ...;
}

If you define an "AUTOLOAD" in your class, then Perl will call your "AUTOLOAD" to handle the "DESTROY" method. You can prevent this by defining an empty "DESTROY", like we did in the autoloading example. You can also check the value of $AUTOLOAD and return without doing anything when called to handle "DESTROY".

Global Destruction

The order in which objects are destroyed during the global destruction before the program exits is unpredictable. This means that any objects contained by your object may already have been destroyed. You should check that a contained object is defined before calling a method on it:

sub DESTROY {
    my $self = shift;
    $self->{handle}->close() if $self->{handle};
}

You can use the "${^GLOBAL_PHASE}" variable to detect if you are currently in the global destruction phase:

sub DESTROY {
    my $self = shift;
    return if ${^GLOBAL_PHASE} eq 'DESTRUCT';
    $self->{handle}->close();
}

Note that this variable was added in Perl 5.14.0. If you want to detect the global destruction phase on older versions of Perl, you can use the "Devel::GlobalDestruction" module on CPAN.

If your "DESTROY" method issues a warning during global destruction, the Perl interpreter will append the string " during global destruction" to the warning.

During global destruction, Perl will always garbage collect objects before unblessed references. See "PERL_DESTRUCT_LEVEL" in perlhacktips for more information about global destruction.

Non-Hash Objects

All the examples so far have shown objects based on a blessed hash. However, it's possible to bless any type of data structure or referent, including scalars, globs, and subroutines. You may see this sort of thing when looking at code in the wild.

Here's an example of a module as a blessed scalar:

package Time;
use strict;
use warnings;
sub new {
    my $class = shift;
    my $time = time;
    return bless \$time, $class;
}
sub epoch {
    my $self = shift;
    return ${ $self };
}
my $time = Time->new();
print $time->epoch();

Inside-Out objects

In the past, the Perl community experimented with a technique called "inside-out objects". An inside-out object stores its data outside of the object's reference, indexed on a unique property of the object, such as its memory address, rather than in the object itself. This has the advantage of enforcing the encapsulation of object attributes, since their data is not stored in the object itself.

This technique was popular for a while (and was recommended in Damian Conway's *Perl Best Practices*), but never achieved universal adoption. The Object::InsideOut module on CPAN provides a comprehensive implementation of this technique, and you may see it or other inside-out modules in the wild.

Here is a simple example of the technique, using the Hash::Util::FieldHash core module. This module was added to the core to support inside-out object implementations.

package Time;
use strict;
use warnings;
use Hash::Util::FieldHash 'fieldhash';
fieldhash my %time_for;
sub new {
    my $class = shift;
    my $self = bless \( my $object ), $class;
    $time_for{$self} = time;
    return $self;
}
sub epoch {
    my $self = shift;
    return $time_for{$self};
}
my $time = Time->new;
print $time->epoch;

Pseudo-hashes

The pseudo-hash feature was an experimental feature introduced in earlier versions of Perl and removed in 5.10.0. A pseudo-hash is an array reference which can be accessed using named keys like a hash. You may run in to some code in the wild which uses it. See the fields pragma for more information.

SEE ALSO

A kinder, gentler tutorial on object-oriented programming in Perl can be found in perlootut. You should also check out perlmodlib for some style guides on constructing both modules and classes.
