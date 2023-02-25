# Clipp OverView

关于每个主题的详细解释, 请参见 示例部分.

为了提高可读性, 所有的例子中都省略了 `命名空间`(namespace) 修饰符.
所有实体都定义在命名空间 `clipp` 中.

### 基本设置

```cpp
int main(int argc, char* argv[]) {
    using namespace clipp;

    auto cli = ( /* CODE DEFINING COMMAND LINE INTERFACE GOES HERE */ );
    parse(argc, argv, cli);    //excludes argv[0]

    //if you want to include argv[0]
    //parse(argv, argv+argc, cli);
}
```

命令行界面有两种构建模块: `parameters` and `groups`(`参数`和`组`).
命名方便的 `工厂函数` 产生 `参数` 或 `组`, 并应用所需的设置.

参数(标志串, 命令, 位置值, 所需标志, 可重复参数)
Parameters (flag strings, commands, positional values, required flags, repeatable parameters)

```cpp
bool a = false, f = false;
string s; vector<string> vs;
auto cli = (                             // matches  required  positional  repeatable
    command("push"),                     // exactly      yes       yes         no
    required("-f", "--file").set(f),     // exactly      yes       no          no
    required("-a", "--all", "-A").set(a),  // exactly      no        no          no

    value("file", s),                    // any arg      yes       yes         no
    values("file", vs),                  // any arg      yes       yes         yes
    opt_value("file", s),                // any arg      no        yes         no
    opt_values("file", vs),              // any arg      no        yes         yes

    //"catch all" parameter - useful for error handling
    any_other(vs),                       // any arg      no        no          yes
    //catches arguments that fulfill a predicate and aren't matched by other parameters
    any(predicate, vs)                   // predicate    no        no          yes
);
```

上述函数是便利工厂:

```cpp
bool f = true; string s;
auto v1 = values("file", s);
// is equivalent to:
auto v2 = parameter{match::nonempty}.label("file").blocking(true).repeatable(true).set(s);

auto r1 = required("-f", "--file").set(f);
// is equivalent to:
auto r2 = parameter{"-f", "--file"}.required(true).set(f);
```

+ `必要参数` 必须至少与一个命令行参数相匹配; required parameter
+ `可重复参数` 可以匹配任何数量的参数; repeatable parameter
+ `非位置性`(=`非阻塞性`)参数可以按任何顺序匹配参数; non-positional (=non-blocking)

+ `位置性(阻塞性)参数` 定义了一个 `停止点`,
也就是说, 在它匹配之前, 它后面的所有参数都不允许匹配;
一旦它匹配了, 它前面的所有参数(在当前组内)都将变得不可达(unreachable); positional (blocking)

## Flags + Values, 标志和值

如果你想让参数按顺序匹配(in sequence),
你可以用操作符 `&` 或者分组函数 `in_sequence` 把它们绑在一起.

```cpp
int n = 1; string s; vector<int> ls;
auto cli = (
    //option with required value
    option("-n", "--repeat") & value("times", n),

    //required flag  with optional value
    required("--file") & opt_value("name", s),

    //option, 带且仅带两个 values
    option("-p", "--pos") & value("x") & value("y"),

    //same as before                   v            v
    in_sequence( option("-p", "--pos") , value("x") , value("y") ),

    //option, 最少一个value (可以更多)
    option("-l") & values("lines", ls)
);
```

### Filtering Value Parameters

值参数使用一个 `过滤器函数`(filter function), 来测试它们是否被允许匹配某个 `参数字符串`. `value`, `values`, `opt_value` 和 `opt_values` 使用的默认过滤器
`match::nonempty`将匹配任何非空的参数字符串.
你可以提供其他 `过滤函数/函数对象` 作为 `value`, `values` 等的第一个参数,
或者使用这些内置的 `快捷工厂函数` 之一, 涵盖最常见的情况:

```cpp
string name; double r = 0.0; int n = 0;
auto cli = (
    value("user", name),   // matches any non-empty string
    word("user", name),    // matches any non-empty alphanumeric string
    number("ratio", r),    // matches string representations of numbers
    integer("times", n)    // matches string representations of integers
);
```

与 `value`, `opt_value` 等类似, 也有 `words` 和 `opt_word` 函数等.

使用自定义过滤器的 `值参数`
[Custom Filters](https://github.com/muellan/clipp#custom-value-filters)

```cpp
auto is_char = [](const string& arg) { return arg.size() == 1 && std::isalpha(arg[0]); };

char c = ' ';
                             // matches       required  positional  repeatable
value(is_char, "c", c);      // one character  yes       yes         no
```

### Groups

+ 用 `圆括号` 和 `逗号` 将相互兼容的参数 [分组](https://github.com/muellan/clipp#grouping).

```cpp
auto cli = ( option("-a"), option("-b"), option("-c") );
```

+ 使用操作符 `|` 或 `one_of` 将相互排斥的参数分组为备选参数:

```cpp
auto cli1 = ( value("input_file") | command("list") | command("flush") );

auto cli2 = one_of( value("input_file") , command("list") , command("flush") );
```

+ 使用操作符 `&` 或 `in_sequence` 对参数进行分组, 使其必须依次匹配.

```cpp
double x = 0, y = 0, z = 0;
auto cli1 = ( option("-pos") & value("X",x) & value("Y",y) & value("Z",z) );

auto cli2 = in_sequence( option("-pos") , value("X",x) , value("Y",y) , value("Z",z) );
```

注意周围的组不受此影响,
所以 -`a` 和 `-b` 可以按任何顺序匹配, 而 `-b` 和值 `X` 必须按顺序匹配.

```cpp
bool a = false, b = false; int x = 0;
auto cli = (  option("-a").set(a),  option("-b").set(b) & value("X",x)  );
```

+ `组` 可以被嵌套和组合, 形成任意复杂的界面
见[这里1](https://github.com/muellan/clipp#nested-alternatives)
和 [这里2](https://github.com/muellan/clipp#complex-nestings).

```cpp
auto cli = ( command("push") | ( command("pull"), option("-f", "--force") )  );
```

+ `组` 也可以是可重复的.

```cpp
auto cli1 = repeatable( command("flip") | command("flop") );
```

+ 在一组 `标志` 上强制使用 `共同前缀`.

```cpp
int x = 0;
auto cli1 = with_prefix("-", option("a"), option("b") & value("x",x), ... );
                          // =>     -a           -b     ^unaffected^

auto cli2 = with_prefix_short_long("-", "--", option("a", "all"), option("b"), ... );
                                               // => -a  --all           -b
```

+ 在一组 `标志` 上强制使用 `共同后缀`.

```cpp
int x = 0;
auto cli1 = with_suffix("=", option("a") & value("x",x), ... );
                          // =>      a=    ^unaffected^

auto cli2 = with_suffix_short_long(":", ":=", option("a", "all"), option("b"), ... );
                                               // =>  a:   all:=          b:
```

+ 使一组标志 `可连接`, [joinable](https://github.com/muellan/clipp#joinable-flags)

```cpp
auto cli1 = joinable( option("-a"), option("-b"));  //will match "-a", "-b", "-ab", "-ba"

//works also with arbitrary common prefixes:
auto cli2 = joinable( option("--xA0"), option("--xB1"));  //will also match "--xA0B1" or "--xB1A0"
```

### Interfacing With Your Code

The easiest way to connect the command line interface to the rest of your code is to bind object values or function (object) calls to parameters (see also here):

```cpp
bool b = false; int i = 5; int m = 0; string x; ifstream fs;
auto cli = (
    option("-b").set(b),                      // "-b" detected -> set b to true
    option("-m").set(m,2),                    // "-m" detected -> set m to 2
    option("-x") & value("X", x),             // set x's value from arg string
    option("-i") & opt_value("i", i),         // set i's value from arg string
    option("-v").call( []{ cout << "v"; } ),  // call function (object) / lambda
    option("-v")( []{ cout << "v"; } ),       // same as previous line
    option("-f") & value("file").call([&](string f){ fs.open(f); })
);
```

In production code one would probably use a settings class:

```cpp
struct settings { bool x = false; /* ... */ };

settings cmdline_settings(int argc, char* argv[]) {
    settings s;
    auto cli = ( option("-x").set(s.x), /* ... */ );
    parse(argc, argv, cli);
    return s;
}
```

Note that the target must either be:

    a fundamental type (int, long int, float, double, ...)
    a type that is convertible from const char*
    a callable entity: function, function object / lambda that either has an empty parameter list or exactly one parameter that is convertible from const char*

Generating Documentation (see also here)

Docstrings for groups and for parameters can either be set with the member function doc or with operator %:

```cpp
auto cli = (
    (   option("x").set(x).doc("sets X"),
        option("y").set(y) % "sets Y"
    ),
    "documented group 1:" % (
        option("-g").set(g).doc("activates G"),
        option("-h").set(h) % "activates H"
    ),
    (   option("-i").set(i) % "activates I",
        option("-j").set(j) % "activates J"
    ).doc("documented group 2:")
);
```

### Usage Lines:

```cpp
cout << usage_lines(cli, "progname") << '\n';

//with formatting options
auto fmt = doc_formatting{}
           .first_column(3)
           .last_column(79);
cout << usage_lines(cli, "progname", fmt) << '\n';

Detailed Documentation:

cout << documentation(cli) << '\n';

//with formatting options
auto fmt = doc_formatting{}
           .first_column(7)
           .doc_column(15)
           .last_column(99);
cout << documentation(cli, fmt) << '\n';
```

### Man Pages:

```cpp
auto cli = ( /*CODE DEFINING COMMAND LINE INTERFACE GOES HERE*/ );
cout << make_man_page(cli, "progname") << '\n';

//with formatting options
auto fmt = doc_formatting{}
           .first_column(7)
           .doc_column(15)
           .last_column(99);
cout << make_man_page(cli, "progname", fmt) << '\n';
```

(Error) Event Handlers (see here, and here)

Each parameter can have event handler functions attached to it. These are invoked once for each argument that is mapped to the parameter (or once per missing event):

```cpp
string file = "default.txt";
auto param = required("-nof").set(file,"") |
             required("-f") & value("file", file)
                              // on 2nd, 3rd, 4th,... match (would be an error in this case)
                              .if_repeated( [] { /* ... */ } )
                              // if required value-param was missing
                              .if_missing( [] { /* ... */ } )
                              // if unreachable, e.g. no flag "-f" before filename
                              .if_blocked( [] { /* ... */ } )
                              // if match is in conflict with other alternative "-nof"
                              .if_conflicted( [] { /* ... */ } );
```

The handler functions can also take an int, which is set to the argument index at which the event occurred first:

```cpp
string file = "default.txt";
auto param = required("-nof").set(file,"") |
             required("-f") & value("file", file)
                              .if_repeated  ( [] (int argIdx) { /* ... */ } )
                              .if_missing   ( [] (int argIdx) { /* ... */ } )
                              .if_blocked   ( [] (int argIdx) { /* ... */ } )
                              .if_conflicted( [] (int argIdx) { /* ... */ } );
```

## Special Cases

If we give -f -b or -b -f -a as command line arguments for the following CLI, an error will be reported, since the value after -f is not optional:

```cpp
auto cli = (  option("-a"),  option("-f") & value("filename"),  option("-b")  );
```

This behavior is fine for most use cases. But what if we want our program to take any string as a filename, because our filenames might also collide with flag names? We can make the value parameter greedy with operator !. This way, the next string after -f will always be matched with highest priority as soon as -f was given:

```cpp
auto cli = (  option("-a"),  option("-f") & !value("filename"),   option("-b")  );
                                        //  ^~~~~~
```

Be very careful with greedy parameters!

## Parsing Result Analysis

```cpp
auto cli = ( /* your interface here */ );
auto res = parse(argc, argv, cli);

if(res.any_error()) { /* ... */ }

//aggregated errors
if(res.unmapped_args_count()) { /* ... */ }
if(res.any_bad_repeat()) { /* ... */ }
if(res.any_blocked())    { /* ... */ }
if(res.any_conflict())   { /* ... */ }

for(const auto& m : res.missing()) {
    cout << "missing " << m.param() << " after index " << m.after_index() << '\n';
}

//per-argument mapping
for(const auto& m : res) {
    cout << m.index() << ": " << m.arg() << " -> " << m.param();
    cout << " repeat #" << m.repeat();
    if(m.blocked()) cout << " blocked";
    if(m.conflict()) cout << " conflict";
    cout << '\n';
}
```

## Writing Your Own Convenience Factories

Sometimes it can make your CLI code more expressive and increase maintainability, if you create your own factory functions for making parameters:

```cpp
//value that can only connect to one object with automatic default value documentation
template<class Target>
clipp::parameter
documented_value(const std::string& name, Target& tgt, const std::string& docstr) {
    using std::to_string;
    return clipp::value(name,tgt).doc(docstr + "(default: " + to_string(tgt) + ")");
}

//value that only matches strings without prefix '-'
template<class Target, class... Targets>
clipp::parameter
nodash_value(std::string label, Target&& tgt, Targets&&... tgts) {
    return clipp::value(clipp::match::prefix_not{"-"}, std::move(label),
               std::forward<Target>(tgt), std::forward<Targets>(tgts)...);
}
```
