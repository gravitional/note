# clipp 文档生成

Generate usage lines and documentation from parameters:

```cpp
auto cli = ( /* command line interface definition */ );

//used default formatting
cout << "Usage:\n" << usage_lines(cli, "progname")
     << "\nOptions:\n" << documentation(cli) << '\n';
```

... or generate an entire man page in one go:

```cpp
auto cli = ( /* command line interface definition */ );

cout << make_man_page(cli, "progname")
        .prepend_section("DESCRIPTION", "    The best thing since sliced bread.")
        .append_section("LICENSE", "    GPL3");
```

### Example

DESCRIPTION
    Builds a database of words from text files.

SYNOPSIS
    worddb help
    worddb build (new|add) <file>
    worddb query -i <infile> [-p]
    worddb info space
    worddb info statistics (words|chars)
    worddb remove (any|all) <regex>
    worddb modify [-c] [-u] [-m <size>]

OPTIONS
    build commands
        new                  make new database
        add                  append to existing database

    query settings
        <infile>             input file
        -p, --pretty-print   human friendly output

    database info modes
        space                detailed memory occupation analysis

        statistics analysis
            words            word frequency table
            chars            character frequency table

    remove mode
        <regex>              regular expression filter

    modification operations
        -c, --compress       compress database in-memory
        -u, --unique         keep only unique entries
        -m, --memlimit       max. size in RAM

LICENSE
    GPL3

The full code:

```cpp
auto cli = (
    command("help") |
    ( command("build"),
        "build commands" %
        (   command("new")  % "make new database"
          | command("add")  % "append to existing database"
        ),
        value("file")
    ) |
    ( command("query"),
        "query settings" %
        (   required("-i", "--input") & value("infile") % "input file",
            option("-p", "--pretty-print") % "human friendly output")
    ) |
    ( command("info"),
        "database info modes" % (
            command("space") % "detailed memory occupation analysis" |
            (
                command("statistics"),
                "statistics analysis" % (
                    command("words") % "word frequency table" |
                    command("chars") % "character frequency table"
                )
            )
        )
    ) |
    "remove mode" % (
        command("remove"),
        "modify" % ( command("any") | command("all") ),
        value("regex") % "regular expression filter"
    ) |
    ( command("modify"),
        "modification operations" % (
            option("-c", "--compress") % "compress database in-memory",
            option("-u", "--unique")   % "keep only unique entries",
            option("-m", "--memlimit") % "max. size in RAM" & value("size")
        )
    )
);

auto fmt = doc_formatting{} .first_column(4) .doc_column(28) .last_column(80);

cout << make_man_page(cli, "worddb", fmt)
    .prepend_section("DESCRIPTION", "    Builds a database of words from text files.")
    .append_section("LICENSE", "    GPL3") << '\n';
```

## Formatting Options

```cpp
//all formatting options (with their default values)
auto fmt = doc_formatting{}
    .first_column(8)                           //left border column for text body
    .doc_column(20)                            //column where parameter docstring starts
    .last_column(100)                          //right border column for text body
    .indent_size(4)                            //indent of documentation lines for children of a documented group
    .line_spacing(0)                           //number of empty lines after single documentation lines
    .paragraph_spacing(1)                      //number of empty lines before and after paragraphs
    .flag_separator(", ")                      //between flags of the same parameter
    .param_separator(" ")                      //between parameters
    .group_separator(" ")                      //between groups (in usage)
    .alternative_param_separator("|")          //between alternative flags
    .alternative_group_separator(" | ")        //between alternative groups
    .surround_group("(", ")")                  //surround groups with these
    .surround_alternatives("(", ")")           //surround group of alternatives with these
    .surround_alternative_flags("", "")        //surround alternative flags with these
    .surround_joinable("(", ")")               //surround group of joinable flags with these
    .surround_optional("[", "]")               //surround optional parameters with these
    .surround_repeat("", "...")                //surround repeatable parameters with these
    .surround_value("<", ">")                  //surround values with these
    .empty_label("")                           //used if parameter has no flags and no label
    .max_flags_per_param_in_usage(1)           //max. # of flags per parameter in usage
    .max_flags_per_param_in_doc(32)            //max. # of flags per parameter in detailed documentation
    .split_alternatives(true)                  //split usage into several lines for large alternatives
    .alternatives_min_split_size(3)            //min. # of parameters for separate usage line
    .merge_alternative_flags_with_common_prefix(false)  //-ab(cdxy|xy) instead of -abcdxy|-abxy
    .merge_joinable_flags_with_common_prefix(true)     //-abc instead of -a -b -c
    .ignore_newline_chars(false)               //ignore '\n' in docstrings
    ;

cout << "Usage:\n" << usage_lines(cli, "progname", fmt)
     << "\nOptions:\n" << documentation(cli, fmt) << '\n';

//or generate entire man page in one go
cout << make_man_page(cli, "progname", fmt)
        .prepend_section("DESCRIPTION", "This program lets you format text.")
        .append_section("LICENSE", "GPLv3");
```

## Documentation Filtering

The documentation generator class documentation can also take an additional third constructor argument that allows to filter parameters according to their properties.

```cpp
int main(int argc, char* argv[]) {
    auto cli = (
        value("input file"),
        option("-r", "--recursive").set(rec).doc("convert files recursively"),
        option("-o") & value("output format", fmt),
        option("-utf16").set(utf16).doc("use UTF-16 encoding")
    );

    auto fmt = doc_formatting{}.doc_column(28);

    auto filter = param_filter{}.prefix("--");

    cout << "Usage:\n" << usage_lines(cli, argv[0]) << "\n\n"
         << "Parameters:\n" << documentation(cli, fmt, filter) << '\n';}
```

Which results in:

Usage:
        convert <input file> [-r] [-o <output format>] [-utf16]

Parameters:
        -r, --recursive    convert files recursively

## Parameter Filters

Any function/lambda that maps a parameter to a bool can be used as filter predicate. CLIPP also comes with a default parameter filter class:

```cpp
//all param_filter options (with their default values)
auto filter = param_filter{}
    .prefix("")               //only parameters with given prefix
    .required(tri::either)    //required parameters
    .blocking(tri::either)    //blocking/positional parameters
    .repeatable(tri::either)  //repeatable parameters
    .has_doc(tri::yes)        //parameters with/without docstrings
    ;
```

which uses a dedicated tristate type:

```cpp
namespace clipp {
   enum class tri { no, yes, either };
}
```
