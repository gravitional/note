# ruby 教程

[20分钟体验 Ruby](https://www.ruby-lang.org/zh_cn/documentation/quickstart/)

### Ruby 的互动性

打开 `IRB`

+ 如果您使用 `macOS` 打开 `Terminal` 然后输入 `irb`, 回车
+ 如果您使用 `Linux`, 打开一个 `Shell`, 然后输入 `irb`, 回车
+ 如果您使用 `Windows`, 打开 `开始菜单 -> Ruby -> Interactive Ruby`

输入: `"Hello World"`

```ruby
irb(main):001:0> "Hello World"
=> "Hello World"
```

第二行显示的只是 `IRB` 给我们的上一个命令的返回值.
如果我们要打印 `"Hello World"` 的话, 还需要更多一点代码:

```ruby
irb(main):002:0> puts "Hello World"
Hello World
=> nil
```

`puts` 是 `Ruby` 语言里用来打印的基本命令. 那 `=> nil` 是什么? 其实是命令的`返回值`.
`puts` 命令永远返回 `nil`, `nil` 也是 `Ruby` 的`空值`.

## 您的免费计算器

简单的数学运算

```ruby
irb(main):003:0> 3+2
=> 5
```

您可以在原来命令的基础上进行修改.  试着按一下 `上方键` 看看是不是打出了原来输入的 `3+2`.
如果能正常显示出的话, 您可以用`左方键`来移动光标直到 `+`, 然后按退格删除它 并输入 `*`.

```ruby
irb(main):004:0> 3*2
=> 6
irb(main):005:0> 3**2
=> 9
```

在 `Ruby` 里 `**` 是 `次方` 的意思. 但如果您想开根号呢?

```ruby
irb(main):006:0> Math.sqrt(9)
=> 3.0
```

我们来仔细看一下. 第一, 什么是 `Math`?

## 模块, 给代码分组

`Math` 是一个自带的`数学模块`.
`模块`在 Ruby 里有两个作用. 第一:  把功能相似的函数放到同一个名称下. `Math` 模块还有 `sin()` 和 `tan()` 这样的函数.

接下来的是一个`点`, `点`是用来告诉一个接收者它所要接受的信息.
在这个例子里面,` 信息`就是 `sqrt(9)`, 意思就是调用 `sqrt` 函数,  并给它 `9` 作为参数.
当然 `sqrt` 就是 "square root" 的缩写.

这个函数的返回值是 `3.0`. 这是因为大多数情况下, 开方的结果并不是整数, 所以 `sqrt` 始终会返回浮点数.

如果我们想记住运算结果呢? 存到`变量`里吧.

```ruby
irb(main):007:0> a = 3 ** 2
=> 9
irb(main):008:0> b = 4 ** 2
=> 16
irb(main):009:0> Math.sqrt(a+b)
=> 5.0
```

尽管这是个非常好的计算器, 我们已经要从基本的 Hello World 程序向更有意思的领域迈进了.

## 函数

如果您想说很多次`"Hello"`, 却不想累酸手指的话, 是时候定义一个`函数`啦!

```ruby
irb(main):010:0> def h
irb(main):011:1> puts "Hello World!"
irb(main):012:1> end
=> nil
```

`def h` 定义一个`函数`. 它告诉 `Ruby` 我们的函数名字是 `h`.
下一行是函数的内容,  正是我们前面看到过的那行代码: `puts "Hello World!"`. 最后的一行 `end` 告诉 `Ruby` 函数的定义完成了.
我们来试着把这个函数调用几次:

```ruby
irb(main):013:0> h
Hello World!
=> nil
irb(main):014:0> h()
Hello World!
=> nil
```

很方便吧. 在 Ruby 里调用`函数`就像提起 `Ruby` 的名字一样简单.
如果`函数`不需要接受`参数`,  您只要提到它就够了. 您可以加一对括号 `()`, 但不是必需的.

如果您想对一个人而不是全世界说您好呢? 只要让 `h 函数`接受一个参数就可以了.

```ruby
irb(main):015:0> def h(name)
irb(main):016:1> puts "Hello #{name}!"
irb(main):017:1> end
=> nil
irb(main):018:0> h("Matz")
Hello Matz!
=> nil
```

和期待的一样. 让我们再仔细看看究竟发生了什么.

### 在字符串中预留位置

啥是 `#{name}` 啊? 这是 `Ruby` 用来往`字符串`中插入信息的方法.
大括号 `{}` 里面的代码会被替换为运算后的`字符串` (如果他们还不是`字符串`的话)然后插入到包含大括号的原始`字符串`中去.
您可以用这个方法将人名`大写`:

```ruby
irb(main):019:0> def h(name = "World")
irb(main):020:1> puts "Hello #{name.capitalize}!"
irb(main):021:1> end
=> nil
irb(main):022:0> h "chris"
Hello Chris!
=> nil
irb(main):023:0> h
Hello World!
=> nil
```

这里还有几个小窍门. 第一是我们又一次省略了函数的括号. 如果我们的命令看起来意图很明显的话,  函数的括号是可以省略的.
另一个是函数缺省的参数值是 `World`. 意思就是说 "如果 `name` 参数没有给出的话,  `name` 的缺省值就设置为 `"World"`.

## 如何更有礼貌

让我们更有礼貌一些, 不光记住您的名字, 还在您到来的时候欢迎您, 并且始终彬彬有礼.
您可以开始使用`对象`了. 我们先建立 `Greeter` (有礼貌的人) 类.

```ruby
irb(main):024:0> class Greeter
irb(main):025:1>   def initialize(name = "World")
irb(main):026:2>     @name = name
irb(main):027:2>   end
irb(main):028:1>   def say_hi
irb(main):029:2>     puts "Hi #{@name}!"
irb(main):030:2>   end
irb(main):031:1>   def say_bye
irb(main):032:2>     puts "Bye #{@name}, come back soon."
irb(main):033:2>   end
irb(main):034:1> end
=> nil
```

新的关键字 `class`! 这个关键字定义了一个新的类 `Greeter` 和它的一些`函数`.  特别留意一下 `@name`, 这是一个`实例变量`.
`类`里面的任何函数都可以使用`实例变量`.  您可以看到 `say_hi` 和 `say_bye` 函数都使用了它.

下面我们要带着 `Greeter` 类出来活动活动了.

### 建立对象

我们来建立一个 `greeter` 对象然后使用它:

```ruby
irb(main):035:0> g = Greeter.new("Pat")
=> #<Greeter:0x16cac @name="Pat">
irb(main):036:0> g.say_hi
Hi Pat!
=> nil
irb(main):037:0> g.say_bye
Bye Pat, come back soon.
=> nil
```

当 g 对象被建立后, 它就记住了名字属性的值 `Pat`. Hmm...  如果我们想直接读取`名字`的`值`呢?

```ruby
irb(main):038:0> g.@name
SyntaxError: compile error
(irb):52: syntax error
        from (irb):52
```

晕, 做不到.

### 揭开对象的面纱

`实例变量`是被隐藏起来的, 但他们并不是被完全的隐藏起来.
当您检查一个对象的时候还是可以看到他们的. `Ruby` 采用了面向对象的思想, 将`内部属性`保护了起来.

到底 `Greeter` 有哪些函数呢?

```ruby
irb(main):039:0> Greeter.instance_methods
=> ["method", "send", "object_id", "singleton_methods",
    "__send__", "equal?", "taint", "frozen?",
    "instance_variable_get", "kind_of?", "to_a",
    "instance_eval", "type", "protected_methods", "extend",
    "eql?", "display", "instance_variable_set", "hash",
    "is_a?", "to_s", "class", "tainted?", "private_methods",
    "untaint", "say_hi", "id", "inspect", "==", "===",
    "clone", "public_methods", "respond_to?", "freeze",
    "say_bye", "__id__", "=~", "methods", "nil?", "dup",
    "instance_variables", "instance_of?"]
```

哇. . . 有这么多! 可我们只定义了两个啊, 怎么回事?
这里列出的其实是 `Greeter` 对象包含的 `所有` 的函数, 当然也就包括了它所`继承`的类的函数了.
如果我们只希望列出 `Greeter` 自己的函数, 可以提供一个 `false` 参数给 `instance_methods`, 表示我们不希望列出祖先类的函数.

```ruby
irb(main):040:0> Greeter.instance_methods(false)
=> ["say_bye", "say_hi"]
```

看起来好多了. 我们来看看 `greeter` 会对哪些函数作出回应:

```ruby
irb(main):041:0> g.respond_to?("name")
=> false
irb(main):042:0> g.respond_to?("say_hi")
=> true
irb(main):043:0> g.respond_to?("to_s")
=> true
```

它知道 `say_hi` 和 `to_s` (意思是把什么东西转换成`字符串`, 这是每个`对象`都有的功能),  但是它不知道 `name`.

### 改变类吧, 永远都不晚

假如您想获取甚至改变`名字属性`呢? `Ruby` 提供了一个简单的方法来访问属性.

```ruby
irb(main):044:0> class Greeter
irb(main):045:1>   attr_accessor :name
irb(main):046:1> end
=> nil
```

在 `Ruby` 里, 您可以把一个`类`打开然后改变它.
这些改变会对以后生成的甚至是已经生成的`对象`产生即时效果.
下面我们来建一个新的 `Greeter` 对象, 然后看一看它的 `@name` 属性.

```ruby
irb(main):047:0> g = Greeter.new("Andy")
=> #<Greeter:0x3c9b0 @name="Andy">
irb(main):048:0> g.respond_to?("name")
=> true
irb(main):049:0> g.respond_to?("name=")
=> true
irb(main):050:0> g.say_hi
Hi Andy!
=> nil
irb(main):051:0> g.name="Betty"
=> "Betty"
irb(main):052:0> g
=> #<Greeter:0x3c9b0 @name="Betty">
irb(main):053:0> g.name
=> "Betty"
irb(main):054:0> g.say_hi
Hi Betty!
=> nil
```

`attr_accessor` 会自动为我们定义两个新的`函数`,  `name` 用来读取变量的值,  `name=` 用来给变量赋值.

### 见面熟的 MegaGreeter!

已经建立好的这个 `greeter` 不是那么有新意, 它一次只能向一个人问好.
如果我们有一个 `MegaGreeter` 可以同时向世界, 一个人, 甚至向一群人问好, 那该多好啊?

我们不再使用互动 `Ruby` 的解析器 `IRB` 了, 而是把`代码`写到一个`文件`里.

输入 `exit` 或者按下 `Ctrl-D` 退出 `IRB`.

```ruby
#!/usr/bin/env ruby

class MegaGreeter
  attr_accessor :names

  # 创建对象
  def initialize(names = "World")
    @names = names
  end

  # Say hi to everybody
  def say_hi
    if @names.nil?
      puts "..."
    elsif @names.respond_to?("each")
      # @names  是某种类型的列表, 迭代!
      @names.each do |name|
        puts "Hello #{name}!"
      end
    else
      puts "Hello #{@names}!"
    end
  end

  # Say bye to everybody
  def say_bye
    if @names.nil?
      puts "..."
    elsif @names.respond_to?("join")
      # Join the list elements with commas
      puts "Goodbye #{@names.join(", ")}.  Come back soon!"
    else
      puts "Goodbye #{@names}.  Come back soon!"
    end
  end

end

if __FILE__ == $0
  mg = MegaGreeter.new
  mg.say_hi
  mg.say_bye

  # Change name to be "Zeke"
  mg.names = "Zeke"
  mg.say_hi
  mg.say_bye

  # Change the name to an array of names
  mg.names = ["Albert", "Brenda", "Charles",
    "Dave", "Engelbert"]
  mg.say_hi
  mg.say_bye

  # Change to nil
  mg.names = nil
  mg.say_hi
  mg.say_bye
end
```

把这个文件储存到 `ri20min.rb`,  然后在命令行输入 `ruby ri20min.rb` 来运行它.  您应该可以看到: :

```bash
Hello World!
Goodbye World.  Come back soon!
...
Come back soon!
...
...
```

这个例子里有很多新鲜的代码, 我们还是来仔细的瞧瞧.

请注意由 (`#`) 开始的第一行.  在 `Ruby` 里面, 任何代码中`井字符`后面的内容都会被解释器忽略.  
而第一行有点特别, 因为在 `Unix` 操作系统下,  井字符开头的第一行告诉了系统的 `Shell` 如何执行这个文件. (称为 shebang)
其他 `井字符` 引导的注释只是起说明的作用.

我们熟悉的 `say_hi` 函数也了有点变化:

```ruby
# Say hi to everybody
def say_hi
  if @names.nil?
    puts "..."
  elsif @names.respond_to?("each")
    # @names is a list of some kind, iterate!
    @names.each do |name|
      puts "Hello #{name}!"
    end
  else
    puts "Hello #{@names}!"
  end
end
```

它现在会根据 `@names` 参数的不同而采取不同的行动. 如果参数是 `nil`, 它会打印 `...`. 没有理由和空气问好对吗?

## 循环, 迭代

如果 `@names` 对象可以回应 `each` 函数, 那它就是可以被`迭代的`,  于是我们对它做`迭代`, 向每个人问好. 
最后如果 `@names` 是其他的类,  就把它转化为`字符串`, 用默认的方式问好.

下面看一看这个迭代结构

```ruby
@names.each do |name|
  puts "Hello #{name}!"
end
```

`each` 是一个可以接受`代码块`的`函数`. 它在迭代每一个`元素`时都会调用一次之前所接受到的`代码块`.  
`代码块`像是一个不需要命名的`函数`, 和 `lambda` 类似.  在 `| |` 之间的就是传输给`代码块`的`参数`.

具体来说就是在每一次`循环`中, `name` 首先被赋值为 `list` 里面一个对应`元素`的值,  然后作为参数传递到 `puts "Hello #{name}!"` 这句命令里.

大多数编程语言都是用 `for` 循环来完成迭代的, 比如在 `C` 里面:

```ruby
for (i=0; i<number_of_elements; i++)
{
  do_something_with(element[i]);
}
```

这样也成, 不过不那么优美. 您需要一个没什么意思的 `i` 来监控`列表长度`和检测循环退出的判断.  
`Ruby` 的方法对比来看就更清爽, 所有的清理工作都被隐藏在 `each` 函数里了,  您只需要告诉它您想做什么. 
在 `each` 函数内部, 实际上会去自动调用 `yield "Albert"`, `yield "Brenda"`, `yield "Charles"`, 等等.

## 代码块, 让 Ruby 闪亮

`代码块`最有用的地方是用来处理比`迭代列表`更繁琐的工作. 
除了一般家务活之外,  您可以用它来自动安装卸载或处理运行错误. 真正做到让用户省心, 放心.

```ruby
# Say bye to everybody
def say_bye
  if @names.nil?
    puts "..."
  elsif @names.respond_to?("join")
    # Join the list elements with commas
    puts "Goodbye #{@names.join(", ")}.  Come back soon!"
  else
    puts "Goodbye #{@names}.  Come back soon!"
  end
end
```

`say_bye` 函数并没有用到 `each`, 而是查看 `@names` 是否支持 `join` 函数. 如果是的话就调用, 否则就简单的将变量转化为`字符串`.  
这种不在乎类的作风就是常说的"鸭子型", 意思就是说"如果它走起来像鸭子, 叫起来像鸭子,  那它一定是鸭子". 
这种思想的好处就是不限制函数所支持的`参数类别`.  如果有人写了一个新的类, 只要它像其他列表类一样回应 `join` 这个函数,  那它就可以被相应的其他函数所使用.

## 让脚本跑起来

这就是 `MegaGreeter` 类的所有内容了. 剩下的代码就只是调用一下这个类的`函数`.  还有最后一点`小技术`在这里:

```ruby
if __FILE__ == $0
```

`__FILE__` 是一个魔法值, 它存有`现在运行的脚本文件`的名字, 也就是`ri20min.rb`. 
`$0` 是`启动脚本`的名字.  也就是调用者的名字.
代码里的比较结构的意思是 `如果这是启动脚本的话...` .

这允许 这个脚本作为`库`, 被别人调用的时候不运行`启动代码`, 而在作为`执行脚本`的时候调用`启动代码`.

## 就这么多啦

Ruby 20分钟体验已经结束了, 但 Ruby 还有无数值得探索的地方: 代码块与 `yield`,  模块与 `mixins`, 等等. 
在简短的体验了 `Ruby` 语言后, 希望您愿意进一步接触它.

如果您希望进一步了解 `Ruby`, 可以到我们的 [文档](https://www.ruby-lang.org/zh_cn/documentation/) 部分.  
那里汇集了更多的手册和介绍, 全部免费的哦.


