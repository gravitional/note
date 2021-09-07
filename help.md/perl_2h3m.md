# perl

[Learn Perl in about 2 hours 30 minutes ](https://qntm.org/perl_en)

Practical Extraction and Report Language

`Perl`是一种动态的,动态类型的,高级脚本(解释)语言,类似于PHP和Python.
`Perl`的语法在很大程度上归功于古老的Shell脚本,并且因过度使用混乱的符号而闻名,大多数都无法Google 到.
`Perl`的shell脚本继承使其非常适合编写 glue 代码：将其他脚本和程序链接在一起的脚本.
`Perl`非常适合处理文本数据并生成更多文本数据.
`Perl`广泛,流行,高度可移植并且得到了良好的支持.
`Perl`的设计理念是 '条条大道通罗马'(TMTOWTDI)(与Python相比,'应该有一种,或者只有一种更好的方法').

Perl令人恐惧,但它也具有一些很棒的补偿功能.在这方面,它就像过去创造的所有其他编程语言一样.

本文档旨在提供信息,而不是福音.它针对像我这样的人：

+ 不喜欢[http://perl.org/](http://perl.org/)上的Perl官方文档,因为它技术性很强,并且对不常见的边缘情况讲的太多
+ 通过`公理和示例`快速学习新的编程语言
+ 希望 Larry Wall 能讲到重点
+ 已经知道编程大概是什么
+ 除了完成工作所必需的之外,不太关心Perl

本文档旨在尽可能短,但不要更短.

初步说明

关于本文档中几乎每条陈述,都要附上：`严格来说,这不是正确的；情况实际上要复杂得多`.
在整个文档中,我使用示例打印语句输出数据,但未明确附加换行符.这样做是为了防止我发疯,并更加关注每种情况下要打印的实际字符串,这总是更加重要.
在许多示例中,如果代码在现实中运行,则会导致大量的单词挤在一行中.尝试忽略这一点.

## hello world

Perl 脚本的后缀名为`.pl`,`helloworld.pl`的全文是：

```perl
use strict;
use warnings;

print "Hello world";
```

Perl脚本由解释器`perl`或`perl.exe`解释：

```perl
perl helloworld.pl [arg0 [arg1 [arg2 ...]]]
```

一些即时笔记. Perl的语法是高度自由的.
避免它们的方法是在每个Perl脚本或模块的最顶部加上 `use strict; use warnings;` .
`use foo;`这种形式称为`pragmas`. 它们是给perl.exe的信号,在程序开始运行之前执行初始语法验证时生效. 当解释器在运行时遇到它们时,这些行不起作用.
分号`;`是语句终止符. 符号`＃`开始注释. 注释一直持续到该行的末尾. Perl没有块注释语法.

## 变量

Perl变量分为三种类型： `scalar`, `arrays` and `hashes`.
每种类型都有自己的标志：分别为`$`,`@`和`％`.
变量使用`my`声明,并一直保留到`scope`中,直到块封闭或文件结束.

### scalar variable

标量变量可以包含：

+ `undef` (对应于Python中的`None`,PHP中的`null`)
+ 一个数字(`Perl`不区分整数和浮点数)
+ 一个字符串
+ 对任何其他变量的引用.

```perl
my $undef = undef;
print $undef; # prints the empty string "" and raises a warning

# implicit undef:
my $undef2;
print $undef2; # prints "" and raises exactly the same warning

my $num = 4040.5;
print $num; # "4040.5"

my $string = "world";
print $string; # "world"
```

使用`.`做字符串连接(与`PHP`相同)：

```perl
print "Hello ".$string; # "Hello world"
```

### booleans

`Perl`没有布尔数据类型.如果且仅当它是以下之一时,`if`语句中的标量求值为布尔值 `false`：

+ `undef`
+ `number 0`
+ `string ""`
+ `string "0"`

Perl文档反复声明函数在某些情况下返回`true`或` false`值.
实际上,当一个函数声称返回` true`时,通常返回`1`,而当一个函数声称返回`false`时,通常返回空字符串`""`.

### Weak typing

无法确定标量包含`数字`还是`字符串`.更确切地说,永远不需要这样做.
标量的行为像数字还是字符串取决于使用它的运算符.当用作字符串时,标量的行为类似于字符串.当用作数字时,标量的行为将类似于数字(如果不可能,则发出警告)：

```perl
my $str1 = "4G";
my $str2 = "4H";

print $str1 .  $str2; # "4G4H"
print $str1 +  $str2; # "8" with two warnings
print $str1 eq $str2; # "" (empty string, i.e. false)
print $str1 == $str2; # "1" with two warnings

# The classic error
print "yes" == "no"; # "1" with two warnings; both values evaluate to 0 when used as numbers
```

须知是使用正确的运算符.将标量作为数字比较,与作为字符比较,有各自的运算符：

```perl
# Numerical operators:  <,  >, <=, >=, ==, !=, <=>, +, *
# String operators:    lt, gt, le, ge, eq, ne, cmp, ., x
```

### array 变量

数组变量是由从`0`开始的整数索引的标量的列表.在Python中,这称为`list`,在PHP中,其称为`array`.使用带括号的标量列表声明数组：

```perl
my @array = (
    "print",
    "these",
    "strings",
    "out",
    "for",
    "me", # trailing comma is okay
);
```

您必须使用`$`符号来访问数组中的值,因为要检索的值不是数组,而是标量：

```perl
print $array[0]; # "print"
print $array[1]; # "these"
print $array[2]; # "strings"
print $array[3]; # "out"
print $array[4]; # "for"
print $array[5]; # "me"
print $array[6]; # returns undef, prints "" and raises a warning
```

您可以使用负数索引从尾部开始打印：

```perl
print $array[-1]; # "me"
print $array[-2]; # "for"
print $array[-3]; # "out"
print $array[-4]; # "strings"
print $array[-5]; # "these"
print $array[-6]; # "print"
print $array[-7]; # returns undef, prints "" and raises a warning
```

标量`$var`和包含标量`$var [0]`的数组`@var`之间没有冲突.但是,可能会使读者感到困惑,因此请避免这种情况.

获取数组的长度：

```perl
print "This array has ".(scalar @array)."elements"; # "This array has 6 elements"
print "The last populated index is ".$#array;       # "The last populated index is 5"
```

调用`Perl`脚本的参数存储在内置数组变量`@ARGV`中.

变量可以插入到字符串中：

```perl
print "Hello $string"; # "Hello world"
print "@array";        # "print these strings out for me"
```

注意,有时候你会需要将某人的电子邮件地址放入字符串` jeff@gmail.com`中.
这将导致`Perl`查找名为`@gmail`的数组变量以插值到字符串中,但找不到它,从而导致运行时错误.
可以通过两种方式来防止插值：通过反斜杠`\`对符号进行转义,或使用单引号而不是双引号.

```perl
print "Hello \$string"; # "Hello $string"
print 'Hello $string';  # "Hello $string"
print "\@array";        # "@array"
print '@array';         # "@array"
```

### Hash变量

哈希变量是由字符串索引的标量的列表.在Python中,这被称为`dictionary`,而在PHP中,其被称为`array`.

```perl
my %scientists = (
    "Newton"   => "Isaac",
    "Einstein" => "Albert",
    "Darwin"   => "Charles",
);
```

请注意,此声明与数组声明有多相似.实际上,双箭头符号`=>`被称为`fat comma`,因为它只是逗号分隔符的同义词.
使用具有偶数个元素的列表声明`hash`,其中偶数个元素(`0,2,...`)全部被当成字符串.

同样,你必须使用`$`符号从哈希中访问值,因为要检索出的值不是哈希,而是标量：

```perl
print $scientists{"Newton"};   # "Isaac"
print $scientists{"Einstein"}; # "Albert"
print $scientists{"Darwin"};   # "Charles"
print $scientists{"Dyson"};    # returns undef, prints "" and raises a warning
```

请注意此处使用的括号.同样,标量`$var`和包含标量条目`$var {foo`}`的哈希`％var`之间没有冲突.

你可以将哈希直接转换为具有两倍条目的数组,元素在键和值之间进行交替(逆过程也很容易)：

`my @scientists = %scientists;`

但是与数组不同,哈希键没有基础顺序.它们将返回更有效率的顺序.因此,请注意在结果数组中顺序的重新排列,它们之保持成对：

```perl
print "@scientists"; # something like "Einstein Albert Darwin Charles Newton Isaac"
```

回顾一下,您必须使用方括号从数组中检索值,但必须使用花括号从哈希中检索值.
方括号实际上是数字运算符,而花括号实际上是字符串运算符.提供的索引是数字或字符串这一事实绝对没有意义：

```perl
my $data = "orange";
my @data = ("purple");
my %data = ( "0" => "blue");

print $data;      # "orange"
print $data[0];   # "purple"
print $data["0"]; # "purple"
print $data{0};   # "blue"
print $data{"0"}; # "blue"
```

### lists

Perl中的`列表`不同于`数组`或`哈希`.你刚刚看到了几个`lists`：

```perl
(
    "print",
    "these",
    "strings",
    "out",
    "for",
    "me",
)
```

```perl
(
    "Newton"   => "Isaac",
    "Einstein" => "Albert",
    "Darwin"   => "Charles",
)
```

列表不是变量.列表是一个临时(ephemeral)`vale`,可以分配给数组或哈希变量.这就是为什么声明数组和哈希变量的语法相同的原因.
在许多情况下,术语`list`和`array`可以互换使用,但是在很多情况下,列表和数组的行为不同,令人感到混乱.

`请记住,`=>`只是伪装的`,`然后看这个例子：

```perl
("one", 1, "three", 3, "five", 5)
("one" => 1, "three" => 3, "five" => 5)
```

`=>`提示了列表之一是数组声明,而另一个是哈希声明.
但是就它们自己而言,它们都不是任何东西的声明.它们只是`lists`, identical lists：

```perl
()
```

这里甚至没有提示.
该列表可用于声明一个空数组或一个空哈希,并且`perl`解释器显然无法区分.
一旦了解了Perl这个奇怪的方面,你就理解了为什么：列表值不能嵌套.尝试一下：

```perl
my @array = (
    "apples",
    "bananas",
    (
        "inner",
        "list",
        "several",
        "entries",
    ),
    "cherries",
);
```

`Perl`无法知道`("inner", "list", "several", "entries")`应该是内部数组还是内部哈希.因此,Perl假定两者都不是,并将列表展平为单个长列表：

```perl
print $array[0]; # "apples"
print $array[1]; # "bananas"
print $array[2]; # "inner"
print $array[3]; # "list"
print $array[4]; # "several"
print $array[5]; # "entries"
print $array[6]; # "cherries"
```

是否使用`fat comma`都是同样的情况：

```perl
my %hash = (
    "beer" => "good",
    "bananas" => (
        "green"  => "wait",
        "yellow" => "eat",
    ),
);

# The above raises a warning because the hash was declared using a 7-element list

print $hash{"beer"};    # "good"
print $hash{"bananas"}; # "green"
print $hash{"wait"};    # "yellow";
print $hash{"eat"};     # undef, so prints "" and raises a warning
```

当然,这确实使连接多个数组起变得容易：

```perl
my @bones   = ("humerus", ("jaw", "skull"), "tibia");
my @fingers = ("thumb", "index", "middle", "ring", "little");
my @parts   = (@bones, @fingers, ("foot", "toes"), "eyeball", "knuckle");
print @parts;
```

不久之后会涉及更多.

## Context

`Perl` 最独特的功能是其代码是上下文敏感的. `Perl`中的每个表达式都是在标量上下文或列表上下文中求值的,具体取决于期望产生标量还是列表.
如果不了解表达式的上下文,就无法确定表达式的计算结果.

```perl
array-> 长度
list -> 末尾元素
reverser
scalar
```

标量分配`my $scalar =`在标量上下文中评估其表达式.在这里,表达式为`"Mendeleev"`：

```perl
my $scalar = "Mendeleev";
```

数组或哈希分配(例如,` my @array =`或`my %hash = `)在列表上下文中求值.
在此,表达式为`("Alpha", "Beta", "Gamma", "Pie")`或`("Alpha" => "Beta", "Gamma" => "Pie"),`,两者等效：

```perl
my @array = ("Alpha", "Beta", "Gamma", "Pie");
my %hash = ("Alpha" => "Beta", "Gamma" => "Pie");
```

在列表上下文中求值的标量表达式将自动转换为单元素列表：

```perl
my @array = "Mendeleev"; # same as 'my @array = ("Mendeleev");'

```

在标量上下文中求值的`array`表达式返回`array`的长度：

```perl
my @array = ("Alpha", "Beta", "Gamma", "Pie");
my $scalar = @array;
print $scalar; # "4"
```

在标量上下文中求值的`list`表达式(`list`与数组不同,还记得吗？)不返回`list`的长度,而是返回`list`中的末位标量：

```perl
my $scalar = ("Alpha", "Beta", "Gamma", "Pie");
print $scalar; # "Pie"
```

`print` 内置函数在列表上下文中评估其所有参数.
实际上, `print` 接受无限数量的参数列表,并且一个接一个地打印,这意味着可以将其直接用于打印数组：

```perl
my @array = ("Alpha", "Beta", "Goo");
my $scalar = "-X-";
print @array;              # "AlphaBetaGoo";
print $scalar, @array, 98; # "-X-AlphaBetaGoo98";
```

警告.许多`Perl`表达式和内置函数根据它们的上下文表现出完全不同的行为.
最突出的例子是 function ` reverse`.在列表上下文中,`reverse`将其参数视为列表,然后反转该列表.
在标量上下文中,`reverse`将整个列表连接在一起,然后将其作为单个单词反向.

```perl
print reverse "hello world"; # "hello world"

my $string = reverse "hello world";
print $string; # "dlrow olleh"
```

您可以使用`scalar`内置函数强制任何表达式在标量上下文中计算：

```perl
print scalar reverse "hello world"; # "dlrow olleh"
```

还记得我们之前如何使用`scalar`来获取数组的长度吗？

## 引用和嵌套数据结构

类似`list`不能包含`list`作为元素,`array`和`hash`不能包含其他数组和`hash`作为元素.它们只能包含标量.观看当我们尝试时会发生什么：

```perl
my @outer = ("Sun", "Mercury", "Venus", undef, "Mars");
my @inner = ("Earth", "Moon");

$outer[3] = @inner;

print $outer[3]; # "2"
```

`$outer [3]`是一个`scalar`,因此它需要一个`scalar`值.当您尝试为其分配一个数组值(`@inner`)时,`@inner`在`scalar`上下文中求值.这与分配`saclar @inner`相同,后者是array `@inner`的长度,即`2`.

但是,`scalar`变量可以包含对任何变量的引用,包括`array`变量或哈希变量.这就是在`Perl`中创建更复杂的数据结构的方式.

引用使用反斜杠`\`创建.

```perl
my $colour    = "Indigo";
my $scalarRef = \$colour;
```

任何时候使用变量名时,都可以用引用代替,引用变量外加上一层`braces`.

```perl
print $colour;         # "Indigo"
print $scalarRef;      # e.g. "SCALAR(0x182c180)"
print ${ $scalarRef }; # "Indigo"
```

只要结果没有歧义,您也可以省略花括号：

```perl
print $$scalarRef; # "Indigo"
```

如果您的引用是对数组或哈希变量的引用,则可以使用花括号或使用更流行的箭头运算符`->`从其中获取数据.

```perl
my @colours = ("Red", "Orange", "Yellow", "Green", "Blue");
my $arrayRef = \@colours;

print $colours[0];       # direct array access
print ${ $arrayRef }[0]; # use the reference to get to the array
print $arrayRef->[0];    # exactly the same thing

my %atomicWeights = ("Hydrogen" => 1.008, "Helium" => 4.003, "Manganese" => 54.94);
my $hashRef = \%atomicWeights;

print $atomicWeights{"Helium"}; # direct hash access
print ${ $hashRef }{"Helium"};  # use a reference to get to the hash
print $hashRef->{"Helium"};     # exactly the same thing - this is very common
```

### 声明数据结构

这里有四个示例,但实际上最后一个是最有用的.

```perl
my %owner1 = (
    "name" => "Santa Claus",
    "DOB"  => "1882-12-25",
);

my $owner1Ref = \%owner1;

my %owner2 = (
    "name" => "Mickey Mouse",
    "DOB"  => "1928-11-18",
);

my $owner2Ref = \%owner2;

my @owners = ( $owner1Ref, $owner2Ref );

my $ownersRef = \@owners;

my %account = (
    "number" => "12345678",
    "opened" => "2000-01-01",
    "owners" => $ownersRef,
);
```

这显然是不必要的麻烦,因为您可以将其缩短为：

```perl
my %owner1 = (
    "name" => "Santa Claus",
    "DOB"  => "1882-12-25",
);

my %owner2 = (
    "name" => "Mickey Mouse",
    "DOB"  => "1928-11-18",
);

my @owners = ( \%owner1, \%owner2 );

my %account = (
    "number" => "12345678",
    "opened" => "2000-01-01",
    "owners" => \@owners,
);
```

也可以使用不同的符号声明匿名数组和哈希.使用方括号声明匿名`array`,使用圆括号声明匿名`array`.
在每种情况下返回的值都是对所讨论的匿名数据结构的引用.请仔细观察,其结果是与上面的`%account`完全相同：

```perl
# Braces denote an anonymous hash
my $owner1Ref = {
    "name" => "Santa Claus",
    "DOB"  => "1882-12-25",
};

my $owner2Ref = {
    "name" => "Mickey Mouse",
    "DOB"  => "1928-11-18",
};

# Square brackets denote an anonymous array
my $ownersRef = [ $owner1Ref, $owner2Ref ];

my %account = (
    "number" => "12345678",
    "opened" => "2000-01-01",
    "owners" => $ownersRef,
);
```

或者,简而言之(这是 in-line 声明复杂数据结构时,实际中应该使用的形式)：

```perl
my %account = (
    "number" => "31415926",
    "opened" => "3000-01-01",
    "owners" => [
        {
            "name" => "Philip Fry",
            "DOB"  => "1974-08-06",
        },
        {
            "name" => "Hubert Farnsworth",
            "DOB"  => "2841-04-09",
        },
    ],
);
```

### 从数据结构中获取信息

现在,让我们假设您还有`%account`,但是其他所有内容(如果有其他内容)都超出了范围.
您可以通过在每种情况下相反的步骤来打印信息.同样,这里有四个示例,其中最后一个是最有用的：

```perl
my $ownersRef = $account{"owners"};
my @owners    = @{ $ownersRef };
my $owner1Ref = $owners[0];
my %owner1    = %{ $owner1Ref };
my $owner2Ref = $owners[1];
my %owner2    = %{ $owner2Ref };
print "Account #", $account{"number"}, "\n";
print "Opened on ", $account{"opened"}, "\n";
print "Joint owners:\n";
print "\t", $owner1{"name"}, " (born ", $owner1{"DOB"}, ")\n";
print "\t", $owner2{"name"}, " (born ", $owner2{"DOB"}, ")\n";
```

或者,简而言之：

```perl
my @owners = @{ $account{"owners"} };
my %owner1 = %{ $owners[0] };
my %owner2 = %{ $owners[1] };
print "Account #", $account{"number"}, "\n";
print "Opened on ", $account{"opened"}, "\n";
print "Joint owners:\n";
print "\t", $owner1{"name"}, " (born ", $owner1{"DOB"}, ")\n";
print "\t", $owner2{"name"}, " (born ", $owner2{"DOB"}, ")\n";
```

或使用引用和`->`运算符：

```perl
my $ownersRef = $account{"owners"};
my $owner1Ref = $ownersRef->[0];
my $owner2Ref = $ownersRef->[1];
print "Account #", $account{"number"}, "\n";
print "Opened on ", $account{"opened"}, "\n";
print "Joint owners:\n";
print "\t", $owner1Ref->{"name"}, " (born ", $owner1Ref->{"DOB"}, ")\n";
print "\t", $owner2Ref->{"name"}, " (born ", $owner2Ref->{"DOB"}, ")\n";
```

或者跳过所有中间取值

```perl
print "Account #", $account{"number"}, "\n";
print "Opened on ", $account{"opened"}, "\n";
print "Joint owners:\n";
print "\t", $account{"owners"}->[0]->{"name"}, " (born ", $account{"owners"}->[0]->{"DOB"}, ")\n";
print "\t", $account{"owners"}->[1]->{"name"}, " (born ", $account{"owners"}->[1]->{"DOB"}, ")\n";
```

### 如何使用数组引用让自己快乐

该数组包含五个元素：

```perl
my @array1 = (1, 2, 3, 4, 5);
print @array1; # "12345"
```

但是,此数组具有`单个`元素(它是对匿名的五元素数组的引用)：

```perl
my @array2 = [1, 2, 3, 4, 5];
print @array2; # e.g. "ARRAY(0x182c180)"
```

此`scalar`是对一个匿名的五元素数组的引用：

```perl
my $array3Ref = [1, 2, 3, 4, 5];
print $array3Ref;      # e.g. "ARRAY(0x22710c0)"
print @{ $array3Ref }; # "12345"
print @$array3Ref;     # "12345"
```

### Conditionals

### if ... elsif ... else ...

除了`elsif`的拼写,这里没有其他惊喜：

```perl
my $word = "antidisestablishmentarianism";
my $strlen = length $word;

if($strlen >= 15) {
    print "'", $word, "' is a very long word";
} elsif(10 <= $strlen && $strlen < 15) {
    print "'", $word, "' is a medium-length word";
} else {
    print "'", $word, "' is a short word";
}
```

Perl提供了一个较短的`statement if condition`语法,强烈建议在短语句中使用：

```perl
print "'", $word, "' is actually enormous" if $strlen >= 20;
```

### unless ... else ...

我的$温度= 20;

my $temperature = 20;

```perl
unless($temperature > 30) {
    print $temperature, " degrees Celsius is not very hot";
} else {
    print $temperature, " degrees Celsius is actually pretty hot";
}
```

最好避免使用`unless`,因为它们会造成混乱.可以通过否定条件(或通过保留条件并交换这些块)来将`"unless [... else]"`微不足道地重构为``if [... else]``块.
幸运的是,没有`elsunless`关键字.

相比之下,强烈建议您这样做,因为它很容易阅读：

```perl
print "Oh no it's too cold" unless $temperature > 15;
```

### Ternary 三元运算符

三元运算符`?:`允许将简单的`if`语句嵌入到一条语句中.一个标准用法是单数/复数形式：

```perl
my $gain = 48;
print "You gained ", $gain, " ", ($gain == 1 ? "experience point" : "experience points"), "!";
```

旁白:在两种情况下,最好将单数和复数完整地拼写.不要做下面这种奇怪的事情,因为任何人搜索代码库来替换单词` tooth`或`teeth`时,都找不到此行：

```perl
my $lost = 1;
print "You lost ", $lost, " t", ($lost == 1 ? "oo" : "ee"), "th!";
```

三元运算符可以嵌套：

```perl
my $eggs = 5;
print "You have ", $eggs == 0 ? "no eggs" :
                   $eggs == 1 ? "an egg"  :
                   "some eggs";
```

`if`语句在标量上下文中计算其条件.
例如,当且仅当`@array`具有`1`个或多个元素时,`if(@array)`返回`true`.
这些元素是什么无关紧要-对于我们关心的所有元素,它们可能包含`undef`或其他`false`.

### 循环

有不止一种方法可以做到这一点.
Perl有一个常规的`while`循环：

```perl
my $i = 0;
while($i < scalar @array) {
    print $i, ": ", $array[$i];
    $i++;
}
```

Perl还提供了`until`关键字：

```perl
my $i = 0;
until($i >= scalar @array) {
    print $i, ": ", $array[$i];
    $i++;
}
```

这些`do`循环与上面的`几乎`等效(如果`@array`为空,则会发出警告)：

```perl
my $i = 0;
do {
    print $i, ": ", $array[$i];
    $i++;
} while ($i < scalar @array);
```

and

```perl
my $i = 0;
do {
    print $i, ": ", $array[$i];
    $i++;
} until ($i >= scalar @array);
```

也可以使用基本的`C`风格的循环.注意我们如何在`for`语句中放入`my`,仅在循环范围内声明`$i`：

```perl
for(my $i = 0; $i < scalar @array; $i++) {
    print $i, ": ", $array[$i];
}
# $i has ceased to exist here, which is much tidier.
```

这种`for`循环被认为是老式的,应尽可能避免.list 上的原生迭代要好得多.
注意：与PHP不同,`for`和`foreach`关键字是同义词.只需选用最容易理解的即可：

```perl
foreach my $string ( @array ) {
    print $string;
}
```

如果确实需要索引,则`range operator ..`将创建一个匿名整数列表：

```perl
foreach my $i ( 0 .. $#array ) {
    print $i, ": ", $array[$i];
}
```

您不能遍历哈希.但是,您可以遍历其键.使用`keys`内置函数检索包含哈希的所有键的`array`.然后对`array`使用`foreach`方法：

```perl
foreach my $key (keys %scientists) {
    print $key, ": ", $scientists{$key};
}
```

由于哈希没有基础顺序,因此可以按任何顺序返回键.使用内置的`sort`函数可以按字母顺序对键数组进行排序：

```perl
foreach my $key (sort keys %scientists) {
    print $key, ": ", $scientists{$key};
}
```

如果您不提供显式的迭代器,则Perl使用默认的迭代器`$ _`. `$_`是第一个也是最友好的内置变量：

```perl
foreach ( @array ) {
    print $_;
}
```

如果使用默认的迭代器,而您只希望在循环中放入一条语句,则可以使用超短循环语法：

```perl
print $_ foreach @array;
```

### Loop control

`next`和`last`可以用来控制循环的进度.在大多数编程语言中,它们分别称为`continue`和`break`.
我们还可以选择为任何循环提供`label`.按照惯例,标签全部以大写书写.
在标记了循环之后,`next and last`可以定位到该标记.本示例查找低于`100`的素数：

```perl
CANDIDATE: for my $candidate ( 2 .. 100 ) {
    for my $divisor ( 2 .. sqrt $candidate ) {
        next CANDIDATE if $candidate % $divisor == 0;
    }
    print $candidate." is prime\n";
}
```

## Array functions

in-place array modification

我们将使用`@stack`演示这些：

```perl
my @stack = ("Fred", "Eileen", "Denise", "Charlie");
print @stack; # "FredEileenDeniseCharlie"
```

`pop`提取并返回数组的最后一个元素.这可以认为是堆栈的顶部：

```perl
print pop @stack; # "Charlie"
print @stack;     # "FredEileenDenise"
```

`push`将多余的元素追加到数组的末尾：

```perl
push @stack, "Bob", "Alice";
print @stack; # "FredEileenDeniseBobAlice"
```

`shift`提取并返回数组的第一个元素：

```perl
print shift @stack; # "Fred"
print @stack;       # "EileenDeniseBobAlice"
```

`unshift`在数组的开头插入新元素：

```perl
unshift @stack, "Hank", "Grace";
print @stack; # "HankGraceEileenDeniseBobAlice"
```

`pop`,`push`,`shift` 和 `unshift` 都是`splice`的特例.` splice`删除并返回一个数组切片,将其替换为另一个数组切片：

```perl
print splice(@stack, 1, 4, "<<<", ">>>"); # "GraceEileenDeniseBob"
print @stack;                             # "Hank<<<>>>Alice"
```

### 从旧数组创建新数组

Perl提供了以下功能,这些功能可作用于数组以创建其他数组.

`join`函数将许多字符串合并为一个：

```perl
my @elements = ("Antimony", "Arsenic", "Aluminum", "Selenium");
print @elements;             # "AntimonyArsenicAluminumSelenium"
print "@elements";           # "Antimony Arsenic Aluminum Selenium"
print join(", ", @elements); # "Antimony, Arsenic, Aluminum, Selenium"
```

在`list`上下文中,`reverse`函数以相反的顺序返回列表.在标量上下文中,``reverse``将整个列表连接在一起,然后将其作为单个单词反向.

```perl
print reverse("Hello", "World");        # "WorldHello"
print reverse("HelloWorld");            # "HelloWorld"
print scalar reverse("HelloWorld");     # "dlroWolleH"
print scalar reverse("Hello", "World"); # "dlroWolleH"
```

`map`函数将一个数组作为输入,并对该数组中的每个标量`$_`应用一个运算.然后,它根据结果构造一个新的数组.大括号内的单个表达式提供了要执行的操作：

```perl
my @capitals = ("Baton Rouge", "Indianapolis", "Columbus", "Montgomery", "Helena", "Denver", "Boise");

print join ", ", map { uc $_ } @capitals;
# "BATON ROUGE, INDIANAPOLIS, COLUMBUS, MONTGOMERY, HELENA, DENVER, BOISE"
```

`grep`函数将一个数组作为输入,并返回一个经过过滤的数组作为输出, 语法类似于`map`.
它将计算第二个参数中的每个标量`$_`.如果返回布尔值`true`,则将标量放入输出数组,否则不放入.

```perl
print join ", ", grep { length $_ == 6 } @capitals;
# "Helena, Denver"
```

显然,结果数组的长度就是成功匹配的次数,这意味着您可以使用`grep`快速检查数组是否包含元素：

```perl
print scalar grep { $_ eq "Columbus" } @capitals; # "1"
```

`grep` 和 `map` 可以结合起来形成 `list comprehensions`,这是许多其他编程语言所不具备的非常强大的功能.

默认情况下,`sort`函数返回输入数组,并按词汇(字母顺序)顺序排序：

```perl
my @elevations = (19, 1, 2, 100, 3, 98, 100, 1056);
print join ", ", sort @elevations;
# "1, 100, 100, 1056, 19, 2, 3, 98"
```

但是,类似于`grep`和`map`,您可以提供一些自己的代码.
排序总是通过在两个元素之间使用一系列比较完成的,你的块接收`$a`和`$b`作为输入,如果`$a`小于`$b`,则返回`-1`；如果它们相等,则返回`0`；如果`$a`大于`$b`,则返回`1`.

`cmp`运算符就可以实现此操作：

```perl
print join ", ", sort { $a cmp $b } @elevations;
# "1, 100, 100, 1056, 19, 2, 3, 98"
```

"spaceship operator" `<=>`对数字的作用相同：

```perl
print join ", ", sort { $a <=> $b } @elevations;
# "1, 2, 3, 19, 98, 100, 100, 1056"
```

`$a`和`$b`始终是标量,但是它们可以引用很难比较的相当复杂的对象.如果需要更多空间进行比较,则可以创建一个单独的子例程并提供其名称,而不是：

```perl
sub comparator {
    # lots of code...
    # return -1, 0 or 1
}

print join ", ", sort comparator @elevations;
```

对于`grep`或`map`操作,您无法执行此操作.

注意,永远不会显式地提供`$a`和`$b`给`subroutine`和`block`.像`$_`一样,`$a`和`$b`实际上是全局变量,每次比较时,给它们填充一对值.

## Built-in 函数

到目前为止,您至少已经看到了十二种内置函数： `print`, `sort`, `map`, `grep`, `keys`, `scalar` 等.内置功能是Perl的最大优势之一.他们

+ 很多
+ 非常有用
+ 文档很多
+ 语法差异很大,因此请查看文档
+ 有时接受正则表达式作为参数
+ 有时接受整个代码块作为参数
+ 有时参数之间不需要逗号
+ 有时会使用任意数量的逗号分隔的参数,有时不会
+ 如果提供的参数太少,有时会填写自己的参数
+ 一般情况下,除非模棱两可的情况,否则通常不需要在参数周围加上方括号

关于内置功能的最佳建议是`know that they exist`.浏览文档以备将来参考.如果您正在执行的任务看起来像是已经完成了很多次的低级通用任务,那么很有可能已经完成了.

## 用户定义的子例程

子例程使用`sub`关键字声明.
与内置函数相反,用户定义的子例程始终接受相同的输入：标量列表.
该列表当然可以只有一个元素,也可以是空的.单个标量被当成单个元素的列表.具有`N`个元素的`hash`被视为具有`2N`个元素的列表.

尽管`brackets`是可选的,但子例程应始终使用`bracket`来调用,即使没有参数的时候.这清楚地表明正在调用子例程.

进入子例程后,可通过`built-in array variable @_`使用自变量.例：

```perl
sub hyphenate {

  # Extract the first argument from the array, ignore everything else
  my $word = shift @_;

  # An overly clever list comprehension
  $word = join "-", map { substr $word, $_, 1 } (0 .. (length $word) - 1);
  return $word;
}

print hyphenate("exterminate"); # "e-x-t-e-r-m-i-n-a-t-e"
```

### Perl通过引用进行调用

与几乎所有其他主流编程语言不同,`Perl`通过`reference`进行调用.
这意味着子例程主体内部可用的变量或值不是`originals`的副本.它们`are the originals`.

```perl
my $x = 7;

sub reassign {
  $_[0] = 42;
}

reassign($x);
print $x; # "42"
```

如果您尝试类似

```perl
reassign(8);
```

那么就会发生错误并停止执行,因为`reassign()`的第一行等于

```perl
8 = 42;
```

这河里吗？

要学习的是,在子例程的主体中,在使用参数之前,应该先对它们进行解包(`unpack`).

### 解包参数

解压缩`@_`的方式不止一种,但有些方法更好.

下面的示例子例程`left_pad`用指定的填充字符将字符串填充到所需的长度.(`x`函数将一个字符串拓展到n倍)
(注意：为简便起见,这些子例程都缺少一些基本的错误检查,即确保填充字符长度为`1`,并检查宽度是否大于等于现有字符串的长度,以及检查所有必需的参数是否齐全)

`left_pad`通常按以下方式调用：

```perl
print left_pad("hello", 10, "+"); # "+++++hello"
```

+ 逐项解包`@_`是有效的,但并不是很漂亮：

```perl
sub left_pad {
    my $oldString = $_[0];
    my $width     = $_[1];
    my $padChar   = $_[2];
    my $newString = ($padChar x ($width - length $oldString)) . $oldString;
    return $newString;
}
```

+ 对`4`个参数以下的情况,建议使用`shift`(移位)来解包`@_`：

```perl
sub left_pad {
    my $oldString = shift @_;
    my $width     = shift @_;
    my $padChar   = shift @_;
    my $newString = ($padChar x ($width - length $oldString)) . $oldString;
    return $newString;
}
```

如果没有为`shift`函数提供数组,则它将隐式对`@_`进行操作.这种方法很常见：

```perl
sub left_pad {
    my $oldString = shift;
    my $width     = shift;
    my $padChar   = shift;
    my $newString = ($padChar x ($width - length $oldString)) . $oldString;
    return $newString;
}
```

超过`4`个参数时,很难跟踪萝卜插在哪个坑.

1. 你可以使用多标量赋值,一次性解包`@_`.同样,最好在`4`个参数以下：

```perl
sub left_pad {
    my ($oldString, $width, $padChar) = @_;
    my $newString = ($padChar x ($width - length $oldString)) . $oldString;
    return $newString;
}
```

对于具有大量参数的子例程,或其中某些参数是可选的或不能与其他参数组合使用的子例程,
最佳做法是要求用户在调用子例程时提供参数的`hash`,然后将`@_`解包为参数的`hash`.
使用这种方法,我们的子例程调用看起来会有所不同：

```perl
print left_pad("oldString" => "pod", "width" => 10, "padChar" => "+");
```

子例程本身看起来像这样：

```perl
sub left_pad {
    my %args = @_;
    my $newString = ($args{"padChar"} x ($args{"width"} - length $args{"oldString"})) . $args{"oldString"};
    return $newString;
}
```

### 返回值

像其他`Perl`表达式一样,子例程调用可以展示上下文行为.
您可以使用`wantarray`函数(应该叫做`wantlist`,算了别管了)来检测子例程所处的上下文,并返回适合该上下文的结果：

```perl
sub contextualSubroutine {
    # Caller wants a list. Return a list
    return ("Everest", "K2", "Etna", "\n") if wantarray;

    # Caller wants a scalar. Return a scalar
    return 3 ."\n";
}

my @array = contextualSubroutine();
print @array; # "EverestK2Etna"

my $scalar = contextualSubroutine();
print $scalar; # "3"
```

## System calls

每次在Windows或Linux系统上完成进程时,它会以一个16位`status word`结束(并且我假设在大多数其他系统上也成立).
最高的8位构成一个介于`0`和`255`之间(含`0`和`255`)的`return code`,其中`0`通常表示未验证的运行成功,
而其他值则表示不同程度的失败.其他`8`的出场频率较低-它们"reflect mode of failure, like signal death and core dump information"(核心转储信息).

你可以在退出Perl脚本时,使用`exit`选择返回码(`0`到`255`)中.

Perl提供了`More Than One Way To`在调用中生成子进程,然后暂停当前脚本的执行,直到该子进程完成,再恢复对当前脚本的解释.
无论使用哪种方法, 子进程执行之后,都将立即发现内置标量变量`$?`包含了子进程的`status word`(`16`位).
您可以只取这`16`位中的最高`8`位来获取`return code`：`$? >> 8`.

`system`函数可用于,使用给定参数调用其他程序.`system`返回的值与`$? `相同：

```perl
my $rc = system "perl", "anotherscript.pl", "foo", "bar", "baz";
$rc >>= 8;
print $rc; # "37"
```

或者,您可以使用反引号 ` `` `在命令行上运行实际命令并捕获该命令的标准输出.
在标量上下文中,整个输出作为单个字符串返回.
在列表上下文中,整个输出以字符串数组的形式返回,每个字符串代表一行输出.

```perl
my $text = `perl anotherscript.pl foo bar baz`;
print $text; # "foobarbaz"
```

`anotherscript.pl `代码示例：

```perl
use strict;
use warnings;

print @ARGV;
exit 37;
```

## 文件和文件句柄

除了数字/字符串/引用或undef, 标量变量也可以包含`file handle`(文件句柄).
文件句柄本质上是对特定文件中特定位置的引用.

使用`open`将标量变量转换为文件句柄. `open`必须提供一个`mode`.模式`<`表示我们希望打开文件并读取它：

```perl
my $f = "text.txt";
my $result = open my $fh, "<", $f;

if(!$result) {
    die "Couldn't open '".$f."' for reading because: ".$!;
}
```

如果成功,则`open`返回一个真值.
否则,它返回`false`并将错误消息填充到内置变量`$!`中.如上所示,您应始终检查打开操作是否成功完成.这种检查非常繁琐,一个常见的习惯用法是：

```perl
open(my $fh, "<", $f) || die "Couldn't open <".$f."> for reading because: ".$!;
```

请注意,在打开调用的参数周围需要括号`()`.

要从文件句柄读取一行文本,请使用`readline`内置函数. 
`readline`返回一整行文本,并在其末尾保留换行符(文件的最后一行可能除外),如果到达文件末尾,则为`undef`.

```perl
while(1) {
    my $line = readline $fh;
    last unless defined $line;
    # process the line...
}
```

要截断可能的`trailing`(尾随)换行符,请使用`chomp`(啃)：

```perl
chomp $line;
```

Note that chomp acts on `$line` in place. `$line = chomp $line`可能不是您想要的.

您也可以使用 `eof` 来检测是否已到达文件末尾：

```perl
while(!eof $fh) {
    my $line = readline $fh;
    # process $line...
}
```

但是请注意,仅使用`while(my $line = readline $fh)`,因为如果`$line`最终为`0`,则循环将提前终止.
如果您想编写类似的内容,Perl提供了`<>`运算符,该运算符以一种较为安全的方式包装了`readline`.这是很常见且非常安全的：

```perl
while(my $line = <$fh>) {
    # process $line...
}
```

乃至：

```perl
while(<$fh>) {
    # process $_...
}
```

写入文件涉及首先以其他模式打开文件.模式`>`表示我们希望打开文件进行写入.
`>`将破坏目标文件的内容, 如果目标文件已经存在并且具有内容.要仅附加到现有文件,请使用模式`>>`.然后,只需将 `filehandle` 作为 `print` 函数的第 `0` 个参数即可.

```perl
open(my $fh2, ">", $f) || die "Couldn't open '".$f."' for writing because: ".$!;
print $fh2 "The eagles have left the nest";
```

请注意,`$fh2`和下一个参数之间没有逗号.

实际上,文件句柄超出范围时会自动关闭,否则应该使用：

```perl
close $fh2;
close $fh;
```

存在三个文件句柄作为`global`常量：`STDIN`,`STDOUT`和`STDERR`.这些在脚本启动时自动打开.读取一行用户输入：

```perl
my $line = <STDIN>;
```

要仅等待用户按下Enter键：

```perl
<STDIN>;
```

没有文件句柄的调用`<>`将从`STDIN`,或调用`Perl`脚本时从参数命名的任何文件中读取数据.

正如您可能已经注意到的,如果未给出文件句柄,则默认情况下`print`打印输出到`STDOUT`.

### 文件测试

函数`-e`是一个内置函数,用于测试文件是否存在.

```perl
print "what" unless -e "/usr/bin/perl";
```

函数`-d`是一个内置函数,用于测试文件是否为目录.

函数`-f`是一个内置函数,用于测试命名文件是否为纯文件.

这些只是`-X`形式的一大类函数中的三个,其中`X`是一些小写或大写字母.这些功能称为`file tests`.注意前面的减号.
在Google查询中,减号表示排除包含该搜索词的结果.这使得Google`file tests` 比较困难.只需搜索`perl file tests`”即可.

## 正则表达式

正则表达式在`Perl`之外的许多语言和工具中都有. 
`Perl`的核心正则表达式语法与其他地方基本相同,但是`Perl`的完整正则表达式功能极其复杂且难以理解.
我能给您的最好建议是,尽可能避免这种复杂性.

匹配操作使用`=~ m//`进行.在标量上下文中,`=~ m//`成功则返回`true`,失败则返回`false`.

```perl
my $string = "Hello world";
if($string =~ m/(\w+)\s+(\w+)/) {
    print "success";
}
```

括号执行`sub-matches`.执行成功的匹配操作后,子匹配将填充到内置变量`$1`, `$2`, `$3`, ...中：

```perl
print $1; # "Hello"
print $2; # "world"
```

在`list`上下文中,`=~ m//`返回`$1`, `$2`...的列表.

```perl
my $string = "colourless green ideas sleep furiously";
my @matches = $string =~ m/(\w+)\s+((\w+)\s+(\w+))\s+(\w+)\s+(\w+)/;
print join ", ", map { "'".$_."'" } @matches;
# prints "'colourless', 'green ideas', 'green', 'ideas', 'sleep', 'furiously'"
```

替换操作使用`=~ s///`进行.

```perl
my $string = "Good morning world";
$string =~ s/world/Vietnam/;
print $string; # "Good morning Vietnam"
```

注意`$string`的内容如何改变.您必须在`=~ s///`操作的左侧传递一个标量变量.如果传递 literal 字符串,则会出现错误.

`/g` flag 表示"全局匹配".

在标量上下文中,`=~ m//g`调用将在都在前一个匹配之后寻找后一个匹配,成功则返回`true`,失败则返回`false`.
之后,您可以按照通常的方式访问`$1`,依此类推.例如：

```perl
my $string = "a tonne of feathers or a tonne of bricks";
while($string =~ m/(\w+)/g) {
  print "'".$1."'\n";
}
```

在`list`上下文中,`=~ m//g`调用一次返回所有匹配项.

```perl
my @matches = $string =~ m/(\w+)/g;
print join ", ", map { "'".$_."'" } @matches;
```

`=~ s///g`调用执行全局搜索/替换并返回匹配的数目.在这里,我们将所有元音都替换为字母`"r"`.

```perl
# 不带 /g 使用.
$string =~ s/[aeiou]/r/;
print $string,"\n"; # "r tonne of feathers or a tonne of bricks"

# 再次使用
$string =~ s/[aeiou]/r/;
print $string,"\n"; # "r trnne of feathers or a tonne of bricks"

# 使用 /g 处理所有剩余匹配
$string =~ s/[aeiou]/r/g;
print $string,"\n"; # "r trnnr rf frrthrrs rr r trnnr rf brrcks"
```

`/i`标志使匹配和替换不区分大小写.

`/x`标志允许您的正则表达式包含空格(例如换行符)和注释.

```perl
"Hello world" =~ m/
  (\w+) # 单个或者更多字母
  [ ]   #单个 literal 空白, 写在一个字符类[]中
  world # literal "world"
/x;
# 返回true
```

## 模块和包装

在`Perl`中,模块和包是不同的东西.

### 模组

模块是一个`.pm`文件,您可以将其包含在另一个`Perl`文件(脚本或模块)中.
模块是一个文本文件,语法与`.pl` Perl脚本完全相同.
示例模块可能位于`C:\foo\bar\baz\Demo\StringUtils.pm or /foo/bar/baz/Demo/StringUtils.pm`,其内容如下：

```bash
use strict;
use warnings;

sub zombify {
    my $word = shift @_;
    $word =~ s/[aeiou]/r/g;
    return $word;
}

return 1;
```

由于模块在加载时从上到下执行,因此您需要在最后返回一个`true`值以表明模块已成功加载.

为了使`Perl`解释器可以找到它们,在调用`perl`之前,应在环境变量`PERL5LIB`中列出包含Perl模块的目录.
列出包含模块的根目录,而不是模块目录或模块本身：

```bash
set PERL5LIB="C:\foo\bar\baz;%PERL5LIB%"
```

要么

```bash
export PERL5LIB="/foo/bar/baz:$PERL5LIB"
```

一旦创建了`Perl`模块,并且`perl`知道在哪里寻找它,就可以在`Perl`脚本中使用`require`内置函数来搜索并执行它.
例如,调用`require Demo::StringUtils`会使`Perl`解释器依次搜索`PERL5LIB`中列出的每个目录,以查找名为`Demo/StringUtils.pm`的文件.
执行完模块后,在那里定义的子例程可用于主脚本.我们的示例脚本叫做`main.pl`,其内容如下：

```perl
use strict;
use warnings;

require Demo::StringUtils;

print zombify("i want brains"); # "r wrnt brrrns"
```

注意双冒号`::`被用作目录分隔符.

现在出现一个问题：如果`main.pl`包含许多`require`调用,并且如此加载的模块中包含更多的`require`调用,那么可能难以追踪`zombify()`子例程的原始声明.解决此问题的方法是使用`package`.

### 包

`package`是可以在其中声明`subroutine`的命令空间. 您声明的任何子例程都隐式声明在当前包内. 
在执行开始时,您位于`main`包中,但是您可以使用`package`内置函数来切换程序包：

```perl
use strict;
use warnings;

sub subroutine {
    print "universe";
}

package Food::Potatoes;

# no collision:
sub subroutine {
    print "kingedward";
}
```

请注意,使用双冒号`::`作为名称空间分隔符.

每次调用子例程时,其实是在调用当前程序包内部的子例程.或者,您可以显式提供程序包名.如果继续执行上面的脚本,看看会发生什么：

```perl
subroutine();                 # "kingedward"
main::subroutine();           # "universe"
Food::Potatoes::subroutine(); # "kingedward"
```

因此,逻辑上,前述问题的解决方案是将`C:\foo\bar\baz\Demo\StringUtils.pm`或`/foo/bar/baz/Demo/StringUtils.pm`修改为：

```perl
use strict;
use warnings;

package Demo::StringUtils;

sub zombify {
    my $word = shift @_;
    $word =~ s/[aeiou]/r/g;
    return $word;
}

return 1;
```

并将`main.pl`修改为：

```perl
use strict;
use warnings;

require Demo::StringUtils;

print Demo::StringUtils::zombify("i want brains"); # "r wrnt brrrns"
```

现在,请仔细阅读此内容.

`Packages`和`modules`是`Perl`编程语言的两个完全独立且截然不同的功能.

它们都使用相同的`::`分隔符,这是 a huge red herring.
可以在脚本或模块的过程中多次切换`Packages`,也可以在多个文件的多个位置使用相同的`Packages`声明.
调用`require Foo::Bar`不会在其中的某个位置查找并加载带有`package Foo::Bar `声明的文件,也不一定会加载在`Foo::Bar`命名空间中的子例程.
调用`require Foo::Bar `只会加载一个名为`Foo/Bar.pm`的文件,该文件根本不需要任何包声明,实际上可以声明`package Baz::Qux`或者其他任何东西.

同样,子例程`Baz::Qux::processThis()`不一定必须在名为`Baz/Qux.pm`的文件中声明.可以在任何地方声明它.

分离这两个概念是`Perl`最愚蠢的功能之一,将它们视为分离的概念总是会导致混乱,令人发疯的代码.幸运的是,大多数`Perl`程序员都遵循以下两个定律：

1. `Perl`脚本(`.pl`文件)必须始终包含零个软件包声明.
2. 一个`Perl`模块(`.pm`文件)必须始终只包含一个与它的名称和位置相对应的软件包声明.例如, 模块`Demo/StringUtils.pm`必须以包`Demo :: StringUtils`开头.

因此,在实践中您会发现,可靠的第三方生产的大多数`Packages`和`modules`都可以被交换使用.
但是,请不要将其视为理所当然,这很重要,因为有一天您会遇到疯子写的代码.

## 面向对象的Perl

`Perl`不是面向对象编程的出色语言.`Perl`的`OO`功能是事后移植的.

+ 对象只是一个`reference`(即 `scalar variable`),它知道其引用对象属于哪个类.
要告诉其引用对象是一个类,请使用`bless`.要找出\引用对象所属的类(如果有),请使用`ref`.
+ 方法只是一个子例程,它将对象(对于类方法而言是包名)作为第一个参数.使用`$obj->method()`调用对象方法.使用`Package::Name->method()`调用类方法.
+ 类只是一个包含方法的包.

一个简单的例子使这一点更加清楚.包含`Animal`类的示例模块`Animal.pm`的内容如下：

```perl
use strict;
use warnings;

package Animal;

sub eat {
    # First argument is always the object to act upon.
    my $self = shift @_;

    foreach my $food ( @_ ) {
        if($self->can_eat($food)) {
            print "Eating ", $food;
        } else {
            print "Can't eat ", $food;
        }
    }
}

# For the sake of argument, assume an Animal can eat anything.
sub can_eat {
    return 1;
}

return 1;
```

我们可以像这样使用这个类：

```perl
require Animal;

my $animal = {
    "legs"   => 4,
    "colour" => "brown",
};                       # $animal is an ordinary hash reference
print ref $animal;       # "HASH"
bless $animal, "Animal"; # now it is an object of class "Animal"
print ref $animal;       # "Animal"
```

注意：从字面上看,引用可以被blessed到任何类.你需要确保 (1)引用对象实际上可以用作此类的实例,(2)涉及的类已存在并已被加载.

您仍然可以按常规方式使用原始 hash：

print "Animal has ", $animal->{"legs"}, " leg(s)";

但是您现在也可以使用相同的`-> `运算符在对象上调用方法,如下所示：

$animal->eat("insects", "curry", "eucalyptus");

最后的调用等效于`Animal::eat($animal, "insects", "curry", "eucalyptus")`.

### 构造器

构造函数是一个返回新对象的类方法,如果需要就声明一个,您可以使用任何喜欢的名称.对于类方法,传递的第一个参数不是对象,而是类名.在这种情况下,`"Animal"`：

```perl
use strict;
use warnings;

package Animal;

sub new {
    my $class = shift @_;
    return bless { "legs" => 4, "colour" => "brown" }, $class;
}

# ...etc.
```

然后像这样使用它：

```perl
my $animal = Animal->new();
```

### 继承

要创建从父类继承的类,请使用`use parent`.假设我们创建`Animal`的子类`Koala`,写到`Koala.pm`中：

```perl
use strict;
use warnings;

package Koala;

# Inherit from Animal
use parent ("Animal");

# Override one method
sub can_eat {
    my $self = shift @_; # Not used. You could just put "shift @_;" here
    my $food = shift @_;
    return $food eq "eucalyptus";
}

return 1;
```

和一些示例代码：

```perl
use strict;
use warnings;

require Koala;

my $koala = Koala->new();

$koala->eat("insects", "curry", "eucalyptus"); # eat only the eucalyptus , 只吃桉树
```

最后一个方法调用尝试调用`Koala::eat($koala, "insects", "curry", "eucalyptus")`,但是`Koala`包中未定义子例程`eat()`.但是,由于`Koala`具有父类`Animal`,因此`Perl`解释器尝试调用`Animal::eat($koala, "insects", "curry", "eucalyptus")`.注意`Koala.pm`如何自动加载`Animal`类.

由于`use parent`接受父类名称的列表,因此`Perl`支持多重继承,同时也带来了所有的好处和恐惧.

## 开始块

`perl`完成对`BEGIN`的解析之后,将会立即执行该块,甚至在解析文件的其余部分之前.在执行(`execution`)时它将被忽略：

```perl
use strict;
use warnings;

print "This gets printed second";

BEGIN {
    print "This gets printed first";
}

print "This gets printed third";
```

`BEGIN`块始终首先执行.如果创建多个`BEGIN`块(不要哇),则它们在编译器遇到它们时从上到下依次执行.
即使将`BEGIN`块放置在脚本的中间(不要哦)或放在结尾(不要啊),它总是最先执行.不要弄乱代码的自然顺序.将`BEGIN`块放在开头！

`BEGIN`块在解析完后立即执行.完成此操作后,解析将在`BEGIN`块的末尾继续.仅在解析了整个脚本或模块之后,才执行`BEGIN`块之外的任何代码.

```perl
use strict;
use warnings;

print "This 'print' statement gets parsed successfully but never executed";

BEGIN {
    print "This gets printed first";
}

print "This, also, is parsed successfully but never executed";

...because e4h8v3oitv8h4o8gch3o84c3 there is a huge parsing error down here.
```

因为它们是在编译时执行的,所以即使条件取值为`false`,或者判断还没有进行,并且实际上可能永远不会进行,放置在条件块内的`BEGIN`块仍将首先执行 .

```perl
if(0) {
    BEGIN {
        print "This will definitely get printed";
    }
    print "Even though this won't";
}
```

不要将`BEGIN`块放在`conditionals`中！如果要在编译时有条件地执行某些操作,则需要将条件放入`BEGIN`块中：

```perl
BEGIN {
    if($condition) {
        # etc.
    }
}
```

### use

好的.既然您已经了解了包,模块,类方法和`BEGIN`块的obtuse行为和语义,那么我可以解释一下这种极为常见的`use`函数

以下三个语句：

```perl
use Caterpillar ("crawl", "pupate");
use Caterpillar ();
use Caterpillar;
```

分别等效于：

```perl
BEGIN {
    require Caterpillar;
    Caterpillar->import("crawl", "pupate");
}
BEGIN {
    require Caterpillar;
}
BEGIN {
    require Caterpillar;
    Caterpillar->import();
}
```

+ 这三个示例的顺序都正确,只是Perl愚蠢.
+ `use`是伪装的`BEGIN`块.相同的警告适用. `use`语句必须始终放置在文件的顶部,绝不能放在条件语句中.
+ `import()`不是内置的`Perl`函数.它是用户定义的类方法.`Caterpillar`程序包的程序员必须定义或继承``import()``,理论上该方法可以接受任何内容作为参数并对这些参数执行任何操作.`use Caterpillar;`什么都可以做. 请查阅`Caterpillar.pm`的文档以确切了解将要发生的情况.
+ 请注意,`require Caterpillar`是如何加载名为`Caterpillar.pm`的模块的,而`Caterpillar->import()`会调用在`Caterpillar`软件包中定义的`import()`子例程.希望模块和软件命名相同.

### Exporter

定义`import()`方法的最常见方法是从`Exporter`模块继承它.`Exporter`是Perl编程语言的核心模块,也是事实上的核心功能.
在`Exporter`的`import()`实现中,您传入的参数列表被解释为子例程名称列表.子例程`import()`时,该子例程在当前程序包及其原始程序包中都可用.

使用示例最容易理解此概念.这是`Caterpillar.pm`的样子：

```perl
use strict;
use warnings;

package Caterpillar;

# Inherit from Exporter
use parent ("Exporter");

sub crawl  { print "inch inch";   }
sub eat    { print "chomp chomp"; }
sub pupate { print "bloop bloop"; }

our @EXPORT_OK = ("crawl", "eat");

return 1;
```

包变量`@EXPORT_OK`应包含子例程名称的列表.

然后,另一段代码通常可以使用`use`语句按名称`import()`这些子例程：

```perl
use strict;
use warnings;
use Caterpillar ("crawl");

crawl(); # "inch inch"
```

在这种情况下,当前包为`main`,因此`crawl()`调用实际上是对`main::crawl()`的调用,由于它是导入的,因此映射到`Caterpillar::crawl()`.

注意：无论`@EXPORT_OK`的内容如何,​​每个方法都可以始终通过` longhand`调用：

```perl
use strict;
use warnings;
use Caterpillar (); # no subroutines named, no import() call made

# and yet...
Caterpillar::crawl();  # "inch inch"
Caterpillar::eat();    # "chomp chomp"
Caterpillar::pupate(); # "bloop bloop"
```

`Perl`没有私有方法.通常,以一两个下划线开头表示私用.

### @EXPORT

`Exporter`模块还定义了一个名为`@EXPORT`的程序包变量,也可以使用子例程名称列表填充该变量.

```perl
use strict;
use warnings;

package Caterpillar;

# Inherit from Exporter
use parent ("Exporter");

sub crawl  { print "inch inch";   }
sub eat    { print "chomp chomp"; }
sub pupate { print "bloop bloop"; }

our @EXPORT = ("crawl", "eat", "pupate");

return 1;
```

如果在根本不带参数的情况下调用`import()`,则将导出`@EXPORT`中命名的子例程,这是在此处发生的情况：

```bash
use strict;
use warnings;
use Caterpillar; # calls import() with no arguments

crawl();  # "inch inch"
eat();    # "chomp chomp"
pupate(); # "bloop bloop"
```

但是请注意,我们又回到那个问题,在没有其他线索的情况下,要告诉我们最初定义`crawl()`的位置可能并不容易.这个故事的寓意是双重的：

1. 创建使用`Exporter`的模块时,默认情况下切勿使用`@EXPORT`导出子例程.始终使用户`longhand`的调用子例程或使用`import()`明确地调用(例如`use Caterpillar ("crawl")`),这是在`Caterpillar.pm`中寻找`crawl()`的强大线索).
2. 当`use`含有`Exporter`的模块时,请始终明确给出要导入的子例程.如果您不想`import()`任何子例程并希望`longhand`的引用它们,则必须提供一个明确的空列表: `use Caterpillar ()`.
 
## 杂记

+ 核心模块`Data::Dumper`可用于将任意标量输出到屏幕.这是必不可少的调试工具.
+ 还有另一种语法`qw{ }`,用于声明数组.这在`use`语句中很常见：

```perl
use Account qw{create open close suspend delete};
```

还有[many other quote-like operators](https://perldoc.perl.org/perlop#Quote-and-Quote-like-Operators).

+ 在`=~ m//`和`=~ s///`操作中,可以使用大括号而不是斜杠作为正则表达式定界符.如果您的正则表达式包含很多斜杠(否则需要使用反斜杠转义),这将非常有用.
例如,`=~ m{///}`匹配三个文字正斜杠,而`=~ s{^https?://}{}`删除`URL`的协议部分.
+ `Perl`确实具有`CONSTANTS`.现在不鼓励这样做,但并非总是如此.常量实际上只是带有省略括号的子例程调用.
+ 有时人们会忽略哈希键周围的引号,而用`$hash{key}`代替`$hash{"key"}`.他们可以避免使用它,因为在这种情况下,裸的`key`以字符串`"key"`的形式出现,而不是子例程调用`key()`.
+ 如果您看到包裹在带有双chevrons 形定界符中的未格式化代码块,例如`<<EOF`,它们其实是`here-doc`.

警告! 许多内置函数可以不带参数调用,它们对`$_`进行操作.希望这将帮助您理解如下形式：

```perl
print foreach @array;
```

和

```perl
foreach ( @array ) {
    next unless defined;
}
```

我不喜欢这种形式,因为它在重构时可能导致问题.
