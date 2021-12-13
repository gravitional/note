# Options 模块

[Phil Christensen/Options-1.5.2/Options](https://metacpan.org/pod/Options)

## 名称

Options - 又一个 `Perl` 模块, 为命令行选项解析和使用方法提供支持.

## 说明

```perl
use Options;

$options = new Options(params => [
                            ['port',   'p', undef,       'The port to connect to.'],
                            ['host',   'h', 'localhost', 'The host to connect to.']
                       ],
                       flags =>  [
                            ['secure', 's', 'Use SSL for encryption.'],
                            ['quit',   'q', 'Quit after connecting.'],
                            ['help',   'h', 'Display this usage guide.'],
                       ]);

# 解析默认选项来源 (@ARGV)
%results = $options->get_options();

# 提供使用方法
if($options->get_result('help')){
    $options->print_usage();
    exit(1);
}
```

## 描述

`Options` 是为了模仿 Twisted Python 的用法库的语法而创建的.
它为命令行选项提供了解析器, 并集成了`自动用法生成器`.
支持 `flags` 和 `parameters`, 包括 `长` 和 `短` 的形式, `必要参数` (required)和 `默认参数`(default).

## 获取选项

### `new Options()`

+ 创建 `Options类` 的新实例. 要做到这一点, 需要向 `构造函数` 传递两个 `可选的` 命名参数(arguments).
`params` 是带有 `参数` 的命令行开关, 而 `flags` 是布尔开关. (duh.)
+ `每个 argument` 都由`匿名数组引用`组成, 其中包含你希望支持的, 每个选项的`匿名数组`.
+ `Params数组` 必须有四个元素, 由开关的 `长形式` 和 `短形式`, `默认值`, 以及将在使用指南中打印的`描述`组成.
如果 `默认值` 被指定为 `undef`, 它将成为 `必须值`, 没有它程序将不会继续运行.
没有 `默认值` 的选项, 可以指定 `空字符串`(`""`)来表示省略默认值.
+ `flags 数组` 更简单, 省略 `默认元素`.

### `$options->get_options()`

这个方法被调用时 `不用参数`, 它开始解析全局变量 `@ARGV`, 或者作为 `首参数` 传递给函数的`数组`. 完
成后, 它返回 `哈希值`, 其中`键`是`长选项名称`, 而`值`是解析的结果,
即 `字符串` 代表参数, `布尔值`(实际上是`1`或`0`)代表`标志型`选项.

如果 `解析器` 遇到 `未知标志`, 或者是 `裸词` 即前面没有 `已识别开关`, 它们会按照被发现的顺序留在 `$options->{'unrecognized'}` 数组中.

如果没有传入 `数组`(即 `@ARGV` 被解析了), 未识别项目将被留在 `@ARGV` 中, 以便脚本可以做额外的处理.

如果结果缺少 `必要参数`, 该模块会打印出 `用法表`, 并调用 `exit(1)`.

### `$options->get_result(option)`

尽管 `get_options()` 返回 `哈希值`, 而且这是可以接受的使用结果的方式,
但在处理选项时, 这个函数提供了某种程度的便利, 当在`列表上下文中`调用时, 它将返回`结果列表`, 即使只提供了`一个参数`.

而在这种情况下, `$options->get_options()` 将返回对应此 `option 解析结果的列表` 的`引用`.

然而, 当有`多个参数`时, 在 `标量上下文中` 调用它将是令人失望的.

### `$options->print_usage($optional_message)`

如果省略了`必要参数`, 选项将自动显示使用信息, 但是这个方法可以用来实现 `--help` 参数.

## 高级用法

`Options.pm` 有几个隐藏的钩子, 当你以不同的方式使用该模块时, 可能会发现它们很有用.

第一个允许你控制发生错误时的 `默认行为`.

```perl
$options->{'exit'} = 0;
```

当这个 `标志` 被设置为 `0` 时,  `get_options()` 在发生错误时, 将不再先打印用法后调用 `exit(1)`.
相反, 它将简单地调用 `die($reason)`, 你可以在 `eval` 中捕获它.

第二个(也是更有趣的)钩子也允许你指定 `子例程引用`, 以便在 `get_options()` 中发生错误时执行.
例如, 下面的代码将复制 `get_options()` 的默认行为:

```perl
$options->{'error_handler'} = sub {
   # 选项实例
    $self = shift;

   # 导致错误的原因
    $error_msg = shift;

    # 在这里做需要的事, 可能包括调用print_usage()

    # 返回 `true` 将忽略所有的错误, 并试图尽可能多地进行解析,
    # 而返回 false将立即退出, 错误代码为1
    return 0;
};
```

最后, 如果你想对 `print_usage()` 的输出做一些其他的事, `你可以让它发送到你手边的任一个filehandle` 对象.

例如你在Perl 5.8.6上, 你可以使用 `StringIO` 服务来获取 `字符串版本` 的用法文本.

```perl
open(STRINGIO, '+>', \$usage_text) or die $!;
$options->{'usage_fh'} = \*STRINGIO;
...
...
...
$options->get_options();
@usage_lines = <$options->{'usage_fh'}>;

# 不要忘记关闭 handle
close($options->{'usage_fh'});
```
