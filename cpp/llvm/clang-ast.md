# clang 查看 ast

[How to view Clang AST?](https://stackoverflow.com/questions/18560019/how-to-view-clang-ast)

## ans1

The method with `-cc1` invocation [will have problem with includes](https://clang.llvm.org/docs/FAQ.html#id2) and recognizing C++.

For full-featured parsing, use:

```bash
clang -Xclang -ast-dump file.cpp
```

## ans2

Clang supports showing the AST with Graphviz's `dotty` --
you can grab the temporary `.dot` file generated
 (name is printed out) to get the graph source.

```bash
clang -cc1 -ast-view your_file.c
```

You can also print to the command line with:

```bash
clang -cc1 -ast-dump your_file.c
```

or:

```bash
clang -cc1 -ast-print your_file.c
```

or in 3.3:

```bash
clang -cc1 -ast-dump-xml your_file.c
```

but this [was removed](http://llvm.org/viewvc/llvm-project?view=revision&revision=127141) later as pointed by Lukas Kubanek in the comment.
