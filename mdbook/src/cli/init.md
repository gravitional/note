# init命令

有一些最小的`模板`(boilerplate), 对每一本新书都是一样的.
正是为了这个目的, `mdBook` 包括一个 `init` 命令.

`init` 命令是这样使用的.

```bash
mdbook init
```

第一次使用 `init` 命令时, 有几个文件会被设置好.

```conf
book-test/
└──book
└── src
    ├── chapter_1.md
    └── SUMMARY.md
```

+ `src`目录是你用 `markdown` 写书的地方. 它包含所有的`源文件`, `配置文件`等.
+ `book` 目录是你的书被`渲染`的地方. 所有的输出都准备好了, 可以上传到`服务器`上让你的读者看到.
+ `SUMMARY.md` 是你的书的`骨架`(skeleton), 将在另一章详细讨论.

>提示: 从SUMMARY.md生成章节

当 `SUMMARY.md` 文件已经存在时, `init`命令会首先解析它, 并根据 `SUMMARY.md` 中使用的路径生成缺少的文件. 
这允许你思考并创建你的书的`整个结构`, 然后让 `mdBook` 为你生成文件结构.

#### 指定一个目录

`init` 命令可以接受一个`目录`作为参数, 作为图书的`根目录`, 而不是`当前工作目录`.

```bash
mdbook init path/to/book
```

#### --主题

当你使用 `--theme` 标志时, 默认的`主题`将被复制到源目录下一个名为 `theme` 的目录中, 以便你修改它.

主题是有选择地被`覆盖`的, 这意味着如果你不想覆盖某个特定的文件, 只要删除它, `mdBook` 就会使用`默认文件`.

#### --标题

为书指定一个`标题`. 如果没有提供, 交互式的提示, 将要求提供一个 `标题`.

```bash
mdbook init --title="My amazing book"
```

#### --ignore

创建一个`.gitignore` 文件, 配置为, 在[构建](https://rust-lang.github.io/mdBook/cli/build.html)图书时忽略创建的`book`目录.
如果不提供, 会有一个交互式的提示, 询问是否应该创建它.