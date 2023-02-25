# clipp 动作,Actions

Actions are executed if a parameter matched an argument string in the command line arguments list. Actions are defined using the following member functions or operators:

    parameter::call(f) or parameter::operator () (f): call a callable entity f (function, lambda, custom function object) for each one of the matched argument strings. If f accepts exactly one parameter that is convertible from const char*, the command line argument is passed to it. If the parameter list is empty, it is simply called without argument.

    parameter::set(target, value): assign a fixed value to a target object; note that the assignment target = value; must be a valid statement

    parameter::set(target):
        A bool target is set to true if the flag/value is present and left unchanged otherwise.
        A target object of fundamental type T (int, long, float, double, ...) will be assigned the result of converting the argument string to type T.
        Targets of type std::vector<T> are appended with a value for each matched argument string. Note that T must either be (explicitly) convertible from const char* or a fundamental type.

    operator << or operator >> assign arg strings to lvalues or call callable entities. Which kind of action will be performed is automatically determined through overload resolution.

Predefined Functions

```cpp
int x = 0;                   // equivalent to:
option("-x")(set(x))         // option("-x").set(x)
option("-x")(set(x,2))       // option("-x").set(x,2)
option("-x")(increment(x))   // option("-x")([&]{++x;})
option("-x")(decrement(x))   // option("-x")([&]{--x;})

bool b = false;              // equivalent to:
option("-b")(flip(b))        // option("-x")([&]{b = !b;})

Some Examples

bool a = false, b = false;
int i = 1, n = 0, m = 0;
float x = 0.0f;

auto cli = (                           //INFORMAL description
    option("-a").set(a),               //if(found("-a")) a = true;
    option("-b") >> b,                 //if(found("-b")) b = true;
    option("--toggle").call(flip(b)),  //if(found("--toggle")) flip(b);

    value("n").set(n),                 //n = std::atoi(arg);
    option("-i") & value("#",i),       //if(found("-i arg")) i = std::atoi(arg);
    option("-1").set(m,1),             //if(found("-1")) m = 1;
    option("-2").set(m,2),             //if(found("-2")) m = 2;

    //if(found("-z")) call_lambda_with_arg("-z");
    option("-z").call([](const char* s) { cout << s; }),

    //using 'operator()' instead of 'call'
    //if(found("bob")) call_lambda_with_arg("bob");
    option("bob")([](const std::string& s) { cout << s; }),

    //for_each_occurence("-x arg", call_lambda_with_arg(arg));
    repeatable( option("-x") & value("X")([&](const char* s) { x = std::atof(s); }) ),

    option("--all") >> []()              { cout << "found --all\n"; }
                    >> [](const char* s) { cout << "found flag " << s << '\n'; };
);
```

## Joinable Flags

SYNOPSIS
        edit <file> [-rbs] ([:vim] [:st3] [:atom] [:emacs])

OPTIONS
        -r      open read-only
        -b      use backup file
        -s      use swap file

        :vim, :st3, :atom, :emacs
                editor(s) to use; multiple possible

```cpp
std::string file;
bool readonly = false, usebackup = false, useswap = false;
enum class editor {vim, sublime3, atom, emacs};
std::vector<editor> editors;
auto add = [&](editor e){ return [&]{ editors.push_back(e); }; };

auto cli = (
    value("file", file),
    joinable(
        option("-r").set(readonly)  % "open read-only",
        option("-b").set(usebackup) % "use backup file",
        option("-s").set(useswap)   % "use swap file"
    ),
    joinable(
        option(":vim")    >> add(editor::vim),
        option(":st3")    >> add(editor::sublime3),
        option(":atom")   >> add(editor::atom),
        option(":emacs")  >> add(editor::emacs)
    ) % "editor(s) to use; multiple possible"
);
```

    Flags can be joined regardless of their length (second group in the example).
    If the flags have a common prefix (- or : in the example) it must be given at least once as leading prefix in the command line argument.
    Allowed args for the first group are: -r, -b, -s, -rb, -br, -rs, -sr, -sb, -bs, -rbs, -rsb, ...
    Allowed args for the second group are: :vim, :vim:atom, :emacs:st3, :vimatom, ...

More Examples
joinable flags  valid args
a, b  ab, ba, a, b
-a, -b  -ab, -ba, -a, -b, -a-b, -b-a
--a, --b  --ab, --ba, --a, --b, --a--b, --b--a
,a, ,b  ,ab, ,ba, ,a, ,b, ,a,b, ,b,a
Xab, Xcd  Xabcd, Xcdab, XabXcd, XcdXab, Xab, Xcd
x:ab, x:cd  x:abcd, x:cdab, x:abx:cd, x:cdx:ab, x:ab, x:cd
Alternatives

SYNOPSIS
        find <file>... -s <expr> [any|all]

OPTIONS
        <file>...  input filenames
        -s <expr>  string to look for
        any        report as soon as any matches
        all        report only if all match

```cpp
vector<string> files;
string expr;
bool ifany = false, ifall = false;

auto cli = (
    values("file", files)                  % "input filenames",
    (required("-s") & value("expr", expr)) % "string to look for",
    option("any").set(ifany)               % "report as soon as any matches" |
    option("all").set(ifall)               % "report only if all match"
);
```

If you like it more verbose you can use the function one_of instead of operator |:

```cpp
auto cli = (
    values("file", files)                  % "input filenames",
    (required("-s") & value("expr", expr)) % "string to look for",
    one_of( option("any").set(ifany)       % "report as soon as any matches",
            option("all").set(ifall)       % "report only if all match" )
);
```

## gcc-style switches

SYNOPSIS
        format [-o <output file>] [-falign|-fnoalign)]

OPTIONS
        -o, --out <file>
                output filename

        -falign, -fnoalign
                control alignment

```cpp
string outfile = "a.out";
bool align = false;

auto cli = (
    (option("-o", "--out") & value("output file", outfile)) % "output filename",
    ( option("-falign"  ).set(align,true) |
      option("-fnoalign").set(align,false) )                % "control alignment"
);
```

Note, that the documentation string is attached to the group of parameters for better readability.
non-redundant prefix specification

```cpp
//has the same meaning as the code above
string outfile = "a.out";
bool align = false;

auto cli = (
    (option("-o", "--out") & value("output file", outfile)) % "output filename",
    with_prefix("-f", option("align"  ).set(align,true) |
                      option("noalign").set(align,false) )  % "control alignment"
);
```

merge alternatives with common prefixes in documentation

Usage:   format [-o <output file>] [-f(align|noalign)]

```cpp
auto fmt = doc_formatting{}.merge_alternative_flags_with_common_prefix(true);
cout << usage_lines(cli, "format", fmt) << '\n';
```

## Commands

= positional, required flags

SYNOPSIS
        make_doc new <filename> [-e <enc>]

OPTIONS
        -e, --encoding  'utf8' or 'cp1252', default is UTF-8

```cpp
std::string fname;
std::string enc = "utf8";

auto cli = (
    command("new"),
    value("filename", fname),
    option("-e", "--encoding") & value("enc", enc).doc("'utf8' or 'cp1252', default is " + enc)
);
```

## Nested Alternatives

SYNOPSIS
        image-find help
        image-find build (new|add) <file>... [-v] [-b [<size=1024>]] [--init|--no-init]
        image-find query <infile> -o <outfile> [-f <format>]

OPTIONS
        -v, --verbose
                print detailed report

        -b, --buffer [<size=1024>]
                sets buffer size in KiByte

        --init, --no-init
                do or don't initialize

        -f, --out-format <format>
                determine output format

Value handling actions are omitted; see examples/nested_alternatives.cpp for a fully functional demo.

```cpp
auto cli = (
    command("help")
    | ( command("build"),
        ( command("new") | command("add")),
        values("file"),
        option("-v", "--verbose")                           % "print detailed report",
        (option("-b", "--buffer") & opt_value("size=1024")) % "sets buffer size in KiByte",
        ( option("--init") | option("--no-init") )          % "do or don't initialize"
    )
    | ( command("query"),
        value("infile"),
        required("-o", "--out") & value("outfile"),
        (option("-f", "--out-format") & value("format"))  % "determine output format"
    )
);
```

Note:

```cpp
doc_formatting::split_alternatives(bool)             //default: true
doc_formatting::alternatives_min_split_size(int)     //default: 3
```

control if the usage is split up into several lines if any group inside an alternative exceeds a given minimum size.
Complex Nestings

The combination of blocking parameters, alternatives and grouping makes it possible to define interfaces with decision trees/DAGs of arbitrary complexity.

SYNOPSIS
    complex_nesting [-v] [-i] (copy|move) [--all] [--replace] [-f] <files>... [-r] [-h]
    complex_nesting [-v] [-i] compare (date|content) [-b] [-q] <files>... [-r] [-h]
    complex_nesting [-v] [-i] merge (diff|patch) -o <outdir> [--show-conflicts] <files>... [-r] [-h]
    complex_nesting [-v] [-i] merge content [--git-style] [-m <marker>] -o <outdir> [--show-conflicts] <files>... [-r] [-h]
    complex_nesting [-v] [-i] list <files>... [-r] [-h]

OPTIONS
    user interface options:
        -v, --verbose      show detailed output
        -i, --interactive  use interactive mode

    copy mode:
        --all              copy all
        --replace          replace existing files
        -f, --force        don't ask for confirmation

    compare mode:
        -b, --binary       compare files byte by byte
        -q, --quick        use heuristics for faster comparison

    merge mode:
        diff               merge using diff
        patch              merge using patch
        content            merge based on content

        content based merge options:
            --git-style    emulate git's merge behavior
            <marker>       merge marker symbol

        <outdir>           target directory for merge result
        --show-conflicts   show merge conflicts during run

    mode-independent options:
        <files>...         input files
        -r, --recursive    descend into subdirectories
        -h, --help         show help

Actions and target variables are omitted in the code.

```cpp
auto copyMode = "copy mode:" % (
    command("copy") | command("move"),
    option("--all")         % "copy all",
    option("--replace")     % "replace existing files",
    option("-f", "--force") % "don't ask for confirmation"
);

auto compareMode = "compare mode:" % (
    command("compare"),
    (command("date") | command("content")),
    option("-b", "--binary") % "compare files byte by byte",
    option("-q", "--quick")  % "use heuristics for faster comparison"
);

auto mergeAlgo = (
    command("diff")  % "merge using diff"  |
    command("patch") % "merge using patch" |
    (   command("content") % "merge based on content",
        "content based merge options:" % (
          option("--git-style") % "emulate git's merge behavior",
          option("-m", "--marker") & value("marker") % "merge marker symbol"
        )
    )
);

auto mergeMode = "merge mode:" % (
    command("merge"),
    mergeAlgo,
    required("-o") & value("outdir") % "target directory for merge result",
    option("--show-conflicts")       % "show merge conflicts during run"
);

auto firstOpt = "user interface options:" % (
    option("-v", "--verbose")     % "show detailed output",
    option("-i", "--interactive") % "use interactive mode"
);
auto lastOpt = "mode-independent options:" % (
    values("files")             % "input files",
    option("-r", "--recursive") % "descend into subdirectories",
    option("-h", "--help")      % "show help"
);

auto cli = (
    firstOpt,
    copyMode | compareMode | mergeMode | command("list"),
    lastOpt
);

if(parse(argc, argv, cli)) {
     // program logic...
} else {
    auto fmt = doc_formatting{}.doc_column(31);
    cout << make_man_page(cli, argv[0], fmt) << '\n';
}
```

You could of course write down everything as one big expression (docstrings are omitted)...:

```cpp
auto cli = (
    option("-v", "--verbose"),
    option("-i", "--interactive"),
    (
        (   (command("copy") | command("move")),
            option("--all"), option("--replace"),
            option("-f", "--force")
        )
        | ( command("compare"),
            (command("date") | command("content")),
            option("-b", "--binary"), option("-q", "--quick")
        )
        | ( command("merge"),
            (
                ( command("content"),
                  option("--git-style"),
                  option("-m", "--marker") & value("marker")
                )
                | command("diff")
                | command("patch")
            ),
            required("-o") & value("outdir"),
            option("--show-conflicts")
        )
        | command("list")
    ),
    values("files"),
    option("-r", "--recursive"),
    option("-h", "--help")
);
```

...but it is probably more readable and maintainable if you break up the CLI definition into logical parts.

Note:

```cpp
doc_formatting::split_alternatives(bool)             //default: true
doc_formatting::alternatives_min_split_size(int)     //default: 3
```

control whether the usage is split up into several lines if any group inside an alternative exceeds a given minimum size.

## An Example From docopt

Naval Fate.

Usage:
  naval_fate ship new <name>...
  naval_fate ship <name> move <x> <y> [--speed= <kn>]
  naval_fate ship shoot <x> <y>
  naval_fate mine (set|remove) <x> <y> [--moored|--drifting]
  naval_fate -h | --help
  naval_fate --version

Options:
  --speed= <kn>  Speed in knots [default: 10].
  --moored       Moored (anchored) mine.
  --drifting     Drifting mine.
  -h, --help     Show this screen.
  --version      Show version.

This code defines the command line interface, handles the parsing result and generates the above man page snippet.

```cpp
int x = 0, y = 0;
float speed = 0.0f;
bool drift = true;
vector<string> names;
enum class mode { none, help, shipnew, shipmove, shipshoot, mineset, minerem};
mode selected = mode::none;

//define command line interface
auto coordinates = ( value("x", x), value("y", y) );

auto shipnew  = ( command("new").set(selected,mode::shipnew),
                  values("name", names) );

auto shipmove = (
    value("name", names),
    command("move").set(selected,mode::shipmove), coordinates,
    option("--speed=") & value("kn",speed) % "Speed in knots [default: 10]");

auto shipshoot = ( command("shoot").set(selected,mode::shipshoot),
                   coordinates );

auto mines = (
    command("mine"),
    (command("set"   ).set(selected,mode::mineset) |
     command("remove").set(selected,mode::minerem) ),
    coordinates,
    (option("--moored"  ).set(drift,false) % "Moored (anchored) mine." |
     option("--drifting").set(drift,true)  % "Drifting mine."          )
);

auto navalcli = (
    ( command("ship"), ( shipnew | shipmove | shipshoot ) )
    | mines,
    | command("-h", "--help").set(selected,mode::help)     % "Show this screen."
    | command("--version")([]{ cout << "version 1.0\n"; }) % "Show version."
);

parse(argc, argv, navalcli);

//handle results
switch(m) {
    case mode::none:
        break;
    case mode::help: {
        auto fmt = doc_formatting{}
            .first_column(2).doc_column(16)
            .max_flags_per_param_in_usage(4);

        cout << "Naval Fate.\n\nUsage:\n"
             << usage_lines(navalcli, "naval_fate", fmt)
             << "\n\nOptions:\n"
             << documentation(navalcli, fmt) << '\n';
        }
        break;
    }
    //...
```
