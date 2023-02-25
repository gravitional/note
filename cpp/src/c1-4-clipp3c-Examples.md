# clipp 值过滤, Value Filters

If a parameter doesn't have flags, i.e. it is a value-parameter, a filter function will be used to test if it matches an argument string. The default filter is clipp::match::nonempty which will match any non-empty argument string. If you want more control over what is matched, you can use some other predefined filters or you can write your own ones (see here).

Usage:   exec [-n <times>] [-l <line>...] [-b <ratio>] [-f <term>]

```cpp
int n = 1;
std::vector<int> lines;
double r = 1.0;
string term, name;
auto cli = (
    option("-n", "--repeat") & integer("times", n),
    option("-l", "--line")   & integers("#", lines),
    option("-r", "--ratio)   & number("ratio", r),
    option("-f", "--find")   & word("term", term)
);
```

### Predefined Filtering Value-Parameters

```cpp
auto cli = (
    value("x"),    //non-empty string
    word("x"),     //alphanumeric string
    number("x"),   //string representing integer or floating point number
    integer("x")   //string representing integral number
);
```

Note that there are additional functions for

    optional parameters: opt_value, opt_word, ...
    repeatable parameters: values, words, ...
    repeatable, optional parameters: opt_values, opt_words, ...

## Using Filters Explicitly

Two kinds of filters are supported right now that can be passed as first argument of value, values, opt_value or opt_values as well as argument of the constructor parameter::parameter(Filter&&)

    Predicates (const string&) -> bool which should return true if and only if an argument is an exact match.

    Substring matchers (const string&) -> subrange which in case of a match also indicate the position and length of the matched substring within a command line argument.

string s;
value( match::length{1,5}, "str", s);

```cpp
//or using the parameter class directly
auto p = parameter{ match::length{1,5} }
         .positional(true).required(true)
         .label("str").set(s);
```

There are a couple of predefined filters in namespace clipp::match, but you can of course write your own ones (see here).

Here is another example that makes sure we don't catch any value starting with "-" as a filename:

```cpp
auto cli = (
    option("-a")
    option("-f") & value(match::prefix_not("-"), "filename"),
    option("-b")
);

namespace clipp {
namespace match {

  //simple predicates
  bool none (const string&);
  bool any (const string&);
  bool nonempty (const string&);
  bool alphabetic (const string&);
  bool alphanumeric (const string&);

  //filters with settings and substring matching
  class numbers {
      explicit numbers(char decimalPoint = '.', char digitSeparator = ',', char exponentSeparator = 'e');
      subrange operator () (const string& arg);
  };

  class integers {
      explicit integers(char digitSeparator = ',');
      subrange operator () (const string& arg);
  };

  class substring {
      explicit substring(const string& str);
      subrange operator () (const string& arg);
  };

  class prefix {
      explicit prefix(const string& prefix);
      subrange operator () (const string& arg);
  };

  class prefix_not {
      explicit prefix_not(const string& prefix);
      subrange operator () (const string& arg);
  };

  class length {
      explicit length(size_t exact);
      explicit length(size_t min, size_t max);
      subrange operator () (const string& arg);
  };

} }
```

## Greedy Parameters

By default, the parser tries to identify a command line argument (in that order) as

    a single flag
    a concatenation of multiple, joinable flags (in any order)
    a concatenation of a joinable flag sequence in the order defined in the CLI code
    a single value parameter
    a concatenation of a joinable flag/value sequence in the order defined in the CLI code
    a concatenation of joinable flags & values in no particular order

If no match was found, the parser tries the same list again without any restrictions imposed by blocking (positional) parameters, conflicting alternatives, etc. If this leads to any match, an error will be reported. This way, potential, but illegal matches can be found and, e.g., conflicting alternatives can be reported.

Consider this CLI:

```cpp
auto cli = (  option("-a"),  option("-f") & value("filename"),  option("-b")  );
```

If we give -f -b or -b -f -a as command line arguments, an error will be reported, since the value after -f is not optional.

This behavior is fine for most use cases. But what if we want our program to take any string as a filename, because our filenames might also collide with flag names? We can make the filename value parameter greedy, so that the next string after -f will always be matched with highest priority as soon as -f was given:

```cpp
auto cli = (  option("-a"),  option("-f") & greedy(value("filename")),  option("-b")  );
```

or using operator !:

```cpp
auto cli = (  option("-a"),  option("-f") & !value("filename"),   option("-b")  );
```

Now, every string coming after an -f will be used as filename.

If we don't want just any kind of match accepted, but still retain a higher priority for a value parameter, we could use a value filter:

```cpp
auto cli = (
    (   command("A"),
        option("-f") & !value(match::prefix_not("-"), "filename"),
        option("-b")
    ) |
    (   command("B"),
        option("-x")
    )
);
```

This way, the command line arguments A -f B will set the filename to "B" and produce no conflict error between the alternative commands A and B but A -f -b will still give an error.

Note, that there is an inherent decision problem: either we want the filename value to match no matter what, or we won't get proper error handling if someone forgets to specify a filename and gives A -f -b Also, there might be interfaces where we really want to catch something like A -f B as a command conflict.
Generalized Joinable Parameters

Not only flags, but arbitrary combinations of flags and values can be made joinable. This feature is especially powerful if combined with repeatable groups. Important: in order for an argument to be matched by such expressions, no parameter requirements must be violated during the matching. Also, no partial argument matches are allowed.
Example 1: Counting Letters

Usage:   counter [a|b]...

```cpp
int as = 0, bs = 0;

auto cli = joinable( repeatable(
        option("a").call([&]{++as;}) |
        option("b").call([&]{++bs;})
    ) );

if(parse(argc, argv, cli))
    cout << "as: " << as << "\nbs: " << bs << '\n';
else
    cout << "Usage:\n" << usage_lines(cli, argv[0]) << '\n';
```

### Valid input includes:

$ ./counter a
$ ./counter b
$ ./counter ab
$ ./counter abba
$ ./counter a b baba
$ ./counter a babba abab abbbbba b a ba a
    ...

Example 2: Listing Numbers

Usage:   numbers ([,] [<number>])...

```cpp
std::vector<double> nums;

auto cli = joinable(repeatable( option(",") , opt_number("number", nums) ));

if(parse(argc, argv, cli)) {
    cout << "numbers:\n";
    for(auto n : nums) cout << n << '\n';
} else {
    cout << "Usage:\n" << usage_lines(cli, argv[0]) << '\n';
}
```

Valid input includes:

```bash
$ ./numbers 1
$ ./numbers 1 2 3
$ ./numbers 1 , 2
$ ./numbers 1 , 2 , 3
$ ./numbers 1, 2, 3
$ ./numbers 1 ,2 ,3
$ ./numbers 1,2
$ ./numbers 1,2,3
$ ./numbers 1.1 , 2
$ ./numbers 1,2.3,4.5
$ ./numbers 1,2,3 4.2 5,6 2 7.1,8.23,9
```

Warning: Be careful with joinable and repeatable parameters! The resulting command line interface might be a lot less intuitive to use than you think. It can also be hard to get the "grammar" of complex parsing expressions right. The following definition for example, contains a subtle pitfall:

```cpp
auto cli = joinable(repeatable( option(",") , number("number", nums) ));
//                                            ^^^ non-optional
```

This will not match arguments like "1,". This is, because, if the repeat group is 'hit' by any of its child parameters, all non-optional parameters must also match within the current 'repeat cycle'. So, if the parser hits the "," it expects to find a number arg as well, because it is blocking (positional) and required. Only after seeing this number can it enter the next repeat cycle. Thus, the argument will not be matched, since joined matches are only valid if no error occured. Making the number optional solves the problem.
Custom Value Filters

Two kinds of filters are supported right now that can be passed as first argument of value, values, opt_value or opt_values as well as argument of the constructor parameter::parameter(Filter&&):

    Predicates (const string&) -> bool which should return true if and only if an argument is an exact match.

    Substring matchers (const string&) -> subrange which in case of a match also indicate the position and length of the matched substring within a command line argument.

## Simple Predicate Example

Usage:   annotate auto | (label <character>)

```cpp
auto is_char = [](const string& arg) { return arg.size() == 1 && std::isalpha(arg[0]); };

char lbl = ' ';
auto cli = (  command("auto") | ( command("label"), value(is_char, "character", lbl) )  );
```

## Substring Matcher Example

Let's write a program that takes strings and lists all tag names (<tag>) contained in them:

Usage:   tagnames <string>...

```cpp
//custom filter
auto tag_name = [] (const string& arg) {
    if(arg.size() < 3) return subrange{}; //too short
    auto i = arg.find("<");
    if(i == string::npos) return subrange{}; //no tag start found
    auto j = arg.find(">", i+1);
    if(j == string::npos) return subrange{}; //didn't find end of tag
    return subrange{i,j-i+1}; //partial match {start, length}
};

std::set<string> tags;
auto cli = joinable(
    values(tag_name, "string",
           [&](const string& arg){ if(arg[1] != '/') tags.insert(arg);})
);

if(parse(argc, argv, cli)) {
    cout << "tag names:\n";
    for(const auto& t : tags) cout << t << '\n';
} else {
    cout << "Usage:\n" << usage_lines(cli, "tagnames") << '\n';
}
```

```bash
$ ./tagnames "<cee><d><e></e></d></cee>" "<a><bo></bo></a>"
tag names:
<a>
<bo>
<cee>
<d>
<e>
```

## Sanity Checks

Check, if no flag occurs as prefix of any other flag (identical flags will be ignored):

```cpp
auto cli = ( /* command line interface definition */);
assert( cli.flags_are_prefix_free() );
```

Check common prefix of all flags, like for example "-" (or "/" on Windows):

```cpp
auto cli = ( /* command line interface definition */);
assert( cli.common_flag_prefix() == "-" );
```

## Basic Error Handling

Each parameter can have error handler functions/lambdas/function objects for different fail cases attached to it:

    if_repeated is raised each time an argument is mapped to a parameter regardless of that parameter's repeatability setting
    if_missing is raised if a required parameter has no argument associated with it
    if_conflicted is raised if two or more arguments are mapped to more than one parameter of a group of alternatives
    if_blocked is raised if an argument can only be mapped to a parameter that was not reachable at the time (e.g. because a positional value was expected before that parameter or the parameter was in a non-active alternative branch)

Example:

Usage:   send <file> -t <target>... [--http|--ftp]

```cpp
string filename;
vector<string> targets;
vector<string> wrong;
bool http = true;

auto istarget = match::prefix_not("-");

auto cli = (
    value("file", filename)
        .if_missing([]{ cout << "You need to provide a source filename!\n"; } )
        .if_repeated([](int idx){ cout << "Only one source file allowed! (index " << idx << ")\n"; } )
    ,
    required("-t") & values(istarget, "target", targets)
        .if_missing([]{ cout << "You need to provide at least one target filename!\n"; } )
        .if_blocked([]{ cout << "Target names must not be given before the source file name!\n"; })
    ,
    option("--http").set(http,true) |
    option("--ftp").set(http,false) % "protocol, default is http"
        .if_conflicted([]{ cout << "You can only use one protocol at a time!\n"; } )
    ,
    any_other(wrong)
);

if(parse(argc, argv, cli) && wrong.empty()) {
    cout << "OK\n";
    /* ... */
} else {
    for(const auto& arg : wrong) cout << "'" << arg << "' is not a valid argument\n";
    cout << "Usage:" << usage_lines(cli,argv[0]) << '\n';
}
```

An error handler can either have an empty parameter list or take an int which is set to the command line argument index where the error occured first.

The catch-all parameter made by any_other is used to catch command line arguments that are not supported.

The value parameter target will only match command line arguments that do not begin with "-", so that wrongly spelled options cannot be parsed as target value.
Parsing

```cpp
auto cli = (
    command("make"),
    value("file")           % "name of file to make",
    option("-f", "--force") % "overwrite existing file"
);

//excludes argv[0]
parse(argc, argv, cli);

//if you want to include argv[0]
parse(argv, argv+argc, cli);

parse({"make", "out.txt"}, cli);

auto args = std::vector<std::string> {"make", "out.txt", "-f"};
parse(args, cli);
```

The parse functions return an object of parsing_result which can be used for detailed analysis and will (explicitly) convert to false if any error occured during parsing.

```cpp
auto result = parse(argc, argv, cli);

auto doc_label = [](const parameter& p) {
    if(!p.flags().empty()) return p.flags().front();
    if(!p.label().empty()) return p.label();
    return doc_string{"<?>"};
};

cout << "args -> parameter mapping:\n";
for(const auto& m : result) {
    os << "#" << m.index() << " " << m.arg() << " -> ";
    auto p = m.param();
    if(p) {
        os << doc_label(*p) << " \t";
        if(m.repeat() > 0) {
            os << (m.bad_repeat() ? "[bad repeat " : "[repeat ")
               <<  m.repeat() << "]";
        }
        if(m.blocked())  os << " [blocked]";
        if(m.conflict()) os << " [conflict]";
        os << '\n';
    }
    else {
        os << " [unmapped]\n";
    }
}

cout << "missing parameters:\n";
for(const auto& m : result.missing()) {
    auto p = m.param();
    if(p) {
        os << doc_label(*p) << " \t";
        os << " [missing after " << m.after_index() << "]\n";
    }
}
```
