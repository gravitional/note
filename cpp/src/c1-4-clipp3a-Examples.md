# Clipp 例子

Note that namespace qualifiers are omitted from all examples for better readability. The repository folder "examples" contains code for most of the following examples.

## Options

```man
SYNOPSIS
       switch [-a] [-b] [-c] [--hi]

OPTIONS
       -a          activates a
       -b          activates b
       -c, --noc   deactivates c
       --hi        says hi
```

```cpp
bool a = false, b = false, c = true; //target variables

auto cli = (
    option("-a").set(a)                  % "activates a",
    option("-b").set(b)                  % "activates b",
    option("-c", "--noc").set(c,false)   % "deactivates c",
    option("--hi")([]{cout << "hi!\n";}) % "says hi");

if(parse(argc, argv, cli))
    cout << "a=" << a << "\nb=" << b << "\nc=" << c << '\n';
else
    cout << make_man_page(cli, "switch");
```

This will set a to true, if -a is found, b to true, if -b is found, c to false,
if -c or --noc are found and prints "hi" if --hi is found in argv. In case of parsing errors a man page will be printed.

## Coding Styles

If you like it more verbose use set to set variables, call to call functions and doc for docstrings. The sequence of member function calls doesn't matter.

```cpp
auto cli = (
    option("-b").set(b).doc("activates b"),
    option("-c", "--noc").set(c,false).doc("deactivates c"),
    option("--hi").call([]{cout << "hi!\n";}).doc("says hi") );
```

You can also use operator >> and operator << to define actions

```cpp
auto cli = (
    option("-b")          % "activates b"   >> b,
    option("-c", "--noc") % "deactivates c" >> set(c,false),
    option("--hi")        % "says hi"       >> []{cout << "hi!\n";} );

auto cli = (
    option("-b")          % "activates b"   >> b,
    option("-c", "--noc") % "deactivates c" >> set(c,false),
    option("--hi")        % "says hi"       >> []{cout << "hi!\n";} );

auto cli = (
    b                    << option("-b")          % "activates b",
    set(c,false)         << option("-c", "--noc") % "deactivates c",
    []{cout << "hi!\n";} << option("--hi")        % "says hi" );

auto cli = (
    "activates b"   % option("-b")          >> b,
    "deactivates c" % option("-c", "--noc") >> set(c,false),
    "says hi"       % option("--hi")        >> []{cout << "hi!\n";} );
```

Note that % has a higher precedence than << and >> which means that you either have to keep the docstrings closer to the command line parameters than the actions or use parentheses.

You should also have a look at actions for more details.
Step-by-step configuration of parameters:

```cpp
int n = 1;

auto optN = parameter{"-n", "-N", "--iterations", "--repeats"}.required(true);

auto valN = parameter{match::any}
    .label("times")
    .set(n)
    .call([](string s) { if(!str::represents_number(s)) throw runtime_error{"invalid value for 'times'"}; })
    .if_missing([]{ cout << "value 'times' not found!\n"; })
    .doc("number of iterations (default = " + std::to_string(n) + ")");

auto cli = group{};
cli.push_back(std::move(optN));
cli.push_back(std::move(valN));

// or:
auto cli = group{std::move(optN), std::move(valN)};
```

## Flag Strings

There are no limitations regarding formatting and you can have an arbitrary number of flags per command line parameter.

```cpp
bool onetwo = false;
auto myopt = option("-1", "-2", "1", "2", ":1", ":2", "--no1", "--no2").set(onetwo);
             //     ^----- will match any one of these strings ------^
```

Same prefix for all flags

```cpp
bool a = false, b = false;

auto cli = with_prefix("-",
    option("a").set(a),     // -a
    option("b").set(b)      // -b
);
```

Same prefix for all flags: single vs. multiple character(s)

Single-letter flags will get the first prefix, flags with more than one letter will get the second one.

```cpp
bool a = false, b = false;

auto cli = with_prefixes_short_long("-", "--",
    option("a", "all").set(a),      // -a, --all
    option("b", "bottom").set(b)    // -b, --bottom
);
```

Same suffix for all flags

```cpp
bool a = false, b = false;

auto cli = with_suffix(":",
    option("a").set(a),     // a:
    option("b").set(b)      // b:
);
```

Same suffix for all flags: single vs. multiple character(s)

Single-letter flags will get the first suffix (empty in this example), flags with more than one letter will get the second one.

```cpp
int a = 0, b = 0;

auto cli = with_suffixes_short_long("", "=",
    option("a", "all") & value("A", a),      // -a, --all=
    option("b", "bottom") & value("B", b)    // -b, --bottom=
);
```

Make Sure No Flag Occurs As Prefix Of Another Flag

```cpp
auto cli = ( /* your command line interface here */ );
assert(cli.flags_are_prefix_free());
```

Note that identical flags will not trigger an error.

## Grouping

Groups can be nested (see here) and have their own documentation string. The statement auto cli = ( ... ); creates a group, if there are more than two parameters/groups declared inside the parentheses.

```man
SYNOPSIS
        myprogram [x] [y] [a] [b] [-c] [-d] [-e] [-f]

OPTIONS
        x      sets X
        y      sets Y

        documented group 1:
        a      activates A
        b      activates B

        documented group 2:
        -c     activates C
        -d     activates D

        -e, -f activates E or F
```

```cpp
bool x = false, y = false, a = false, b = false;
bool g = false, h = false, e = false, f = false;

auto cli = (
    (   option("x").set(x) % "sets X",  //simple group
        option("y").set(y) % "sets Y"
    ),
    (   option("a").set(a) % "activates A",
        option("b").set(b) % "activates B"
    ) % "documented group 1:"           //docstring after group
    ,
    "documented group 2:" % (           //docstring before group
        option("-g").set(g) % "activates G",
        option("-h").set(h) % "activates H"
    ),
    "activates E or F" % (
        option("-e").set(e),        //no docstrings inside group
        option("-f").set(f)
    )
);

cout << make_man_page(cli, "myprogram");
```

The above example is in fact shorthand for this:

```cpp
group cli{
    group{
        parameter{"x"}.set(x).doc("sets X"),
        parameter{"y"}.set(y).doc("sets Y")
    },
    group{
        parameter{"a"}.set(a).doc("activates A"),
        parameter{"b"}.set(b).doc("activates B")
    }.doc("documented group 1:")
    ,
    group{
        parameter{"-g"}.set(g).doc("activates G"),
        parameter{"-h"}.set(h).doc("activates H")
    }.doc("documented group 2:")
    ,
    group{
        parameter{"-e"}.set(e),
        parameter{"-f"}.set(f)
    }.doc("activates E or F")
};

cout << make_man_page(cli, "myprogram");
```

You can of course also fill groups one-by-one:

```cpp
group cli;
cli.push_back(option("x").sets(x).doc("sets X"));
//...
```

## Required Positional Values

```man
SYNOPSIS
        myprogram <infile> <outfile> [-s]

OPTIONS
        infile        input filename
        outfile       output filename
        -s, --split   split files
```

```cpp
string ifile, ofile;
bool split = false;
auto cli = (
    value("infile", ifile)             % "input filename",
    value("outfile", ofile)            % "output filename",
    option("-s", "--split").set(split) % "split files" );
```

## Alternative Value Mapping Style

```cpp
auto cli = (
     value("infile")         % "input filename"  >> ifile,
     value("outfile")        % "output filename" >> ofile,
     option("-s", "--split") % "split files"     >> split  );
```

See here for more on possible mapping styles.

## Options With Values

Parameters can be sequenced using operator & or the function in_sequence. Sequenced parameters can only be matched one after the other. This mechanism can be used to attach a value parameter to an option.

```man
SYNOPSIS
       simplify [-n <count>] [-r <ratio>] [-m [<lines=5>]]

OPTIONS
       -n, --count <count>     number of iterations
       -r, --ratio <ratio>     compression ratio
       -m <lines=5>            merge lines (default: 5)
```

```cpp
int n = 0;
bool domerge = false;
long m = 5;
auto print_ratio = [](const char* r) { cout << "using ratio of " << r << '\n'; };

auto cli = (
    (option("-n", "--count") & value("count", n))           % "number of iterations",
    (option("-r", "--ratio") & value("ratio", print_ratio)) % "compression ratio",
    (option("-m").set(domerge) & opt_value("lines=5", m))   % "merge lines (default: 5)"
);
```

## Alternative Value Mapping Styles

```cpp
auto cli = (
    (option("-n", "--count") & value("count") >> n                 ) % "number of iterations",
    (option("-r", "--ratio") & value("ratio") >> print_ratio       ) % "compression ratio",
    (option("-m"           ) & opt_value("lines=5") >> m >> domerge) % "merge lines (default: 5)"
);

auto cli = (
    (option("-n", "--count") & value("count").set(n))         % "number of iterations",
    (option("-r", "--ratio") & value("ratio")(print_ratio))   % "compression ratio",
    (option("-m").set(domerge) & opt_value("lines=5").set(m)) % "merge lines (default: 5)"
);
```

See here for more on coding styles.

## Options With Multiple Values

Parameters can be sequenced using operator & or the function in_sequence. Sequenced parameters can only be matched one after the other. This mechanism can be used to attach multiple values to an option.

```man
SYNOPSIS
       transform <geometry file> [-translate <x> <y> <z>] [-rotate <azimuth> <polar>]
```

```cpp
string infile;
bool tr = false, rot = false;
double x = 0, y = 0, z = 0;
double phi = 0, theta = 0;

auto cli = (
    value("geometry file", infile),
    option("-translate").set(tr) & value("x", x) & value("y", y) & value("z", z),
    option("-rotate").set(rot) & value("azimuth", phi) & value("polar", theta)
);
```

Note that the following interface definition is equivalent to the above. Since value is positional we can list it with ,, but we have to make sure that the groups of values will only be matched after the options, hence the &.

```cpp
auto cli = (
    value("geometry file", infile),
    option("-translate").set(tr) & ( value("x", x), value("y", y), value("z", z) ),
    option("-rotate").set(rot)   & ( value("azimuth", phi) , value("polar", theta) )
);
```

## Required Flags

Required flags are usually used together with non-optional values. Note that -i and -o are not positional in the following example, i.e., the relative order in which command line arguments for -i, -o and -r are provided is irrelevant.

```man
SYNOPSIS
        myprogram [-r] -i <input dir> -o <output dir>

OPTIONS
        -r, --recursive
                search in subdirectories
        -i, --in <input dir>
                path to input directory
        -o, --out <output dir>
                path to output directory
```

```cpp
bool recurse = false;
string inpath, outpath;

auto cli = (
    option("-r", "--recursive").set(recurse)                 % "search in subdirectories",
    (required("-i", "--in" ) & value("input dir", inpath))   % "path to input directory",
    (required("-o", "--out") & value("output dir", outpath)) % "path to output directory"
);
```

## Repeatable Parameters

```man
SYNOPSIS
        simplify <file>... [-c] [-i <line>...]

OPTIONS
        <file>...               input files
        -c, --compress          compress results
        -i, --ignore <line>...  lines to be ignored
```

```cpp
vector<string> files;
vector<int> lines;
bool zip = false;
auto cli = (
    values("file", files)                                % "input files",
    option("-c", "--compress").set(zip)                  % "compress results",
    (option("-i", "--ignore") & integers("line", lines)) % "lines to be ignored"
);
```

The call values("v") is shorthand for value("v").repeatable(true).

Note, that the value parameter line is repeatable, but the flag --ignore is not. So something like

    simplify file1 file2 --ignore 1 2 --ignore 3 4 -c

is taken to be an error. However, it is possible if you make the group of --ignore and line itself repeatable:
Repeatable Groups of Options with Repeatable Values

```man
SYNOPSIS
        simplify <file>... [-c] [-i <line>...]...

OPTIONS
        <file>...               input files
        -c, --compress          compress results
        -i, --ignore <line>...  lines to be ignored
```

```cpp
vector<string> files;
vector<int> lines;
bool zip = false;
auto cli = (
    values("file", files)                            % "input files",
    option("-c", "--compress").set(zip)              % "compress results",
    repeatable(  // <-----
        option("-i", "--ignore") & integers("line", lines)
    )                                                % "lines to be ignored"
);
```

Now both the option --ignore and the value parameter value are repeatable. In all of the following examples lines will be set to {1,2,3,4} and c will be set to true:

    simplify file1 file2 -c --ignore 1 2 3 4
    simplify file1 file2 --ignore 1 2 3 4 -c
    simplify file1 file2 --ignore 1 -c --ignore 2 3 4
    simplify file1 file2 --ignore 1 2 --ignore 3 4 -c
    simplify file1 file2 --ignore 1 --ignore 2 -c --ignore 3 --ignore 4
    simplify file1 file2 -c --ignore1 --ignore2 --ignore3 --ignore4
