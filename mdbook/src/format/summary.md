# SUMMARY.md

`mdBook` 通过 `摘要文件`(summary) 知道要包括哪些章节, 它们应该以什么顺序出现, 它们的层次结构是什么, 以及源文件在哪里.
没有这个文件, 就没有书籍排版.

这个 `markdown` 文件必须命名为 `SUMMARY.md`. 它的格式非常严格, 必须遵循下面列出的`结构`, 以便于`解析`(parsing). 
任何没有在下面指定的`元素`, 不管是格式化相关的(formatting)还是文本的(textual), 最好都不要给出, 或者在试图编译书本时导致`错误`.

### 结构

1. ***标题*** -- 虽然是`可选的`, 但通常的做法是以`标题`开始, 一般是<code   class="language-markdown"># Summary</code>.
然而, `解析器`会忽略这一行, 因此可以省略.

  ```markdown
   # Summary
   ```

1. ***前缀章节*** -- 在编号的主要章节之前, 可以添加`前缀章节`, 这些章节不会被编号. 
这对`前言`, `引言`等很有用. 然而, 有一些限制. 前缀的章节`不能嵌套`; 它们应该都在`根一级`(root level).
而且, 一旦你添加了`有编号`的章节, 你就不能再添加`前缀章节`.

   ```markdown
   [A Prefix Chapter](relative/path/to/markdown.md)

   - [First Chapter](relative/path/to/markdown2.md)
   ```

1. ***部分标题***-- 可以作为下面`编号的章节`的标题. 这可以用来在逻辑上分隔本书的不同部分. 
`标题`被渲染成`不可点击`的文本. 标题是`可选的`, 编号的章节可以根据需要分成许多部分.

   ```markdown
   # My Part Tile

   - [First Chapter](relative/path/to/markdown.md)
   ```

1. ***编号的章节***-- 编号的章节概述了本书的主要内容, 并且可以`嵌套`, 从而形成一个漂亮的`层次结构`(hierarchy, `章节`, `子章节`等).

  ```markdown
   # Title of Part

   - [First Chapter](relative/path/to/markdown.md)
   - [Second Chapter](relative/path/to/markdown2.md)
      - [Sub Chapter](relative/path/to/markdown3.md)

   # Title of Another Part

   - [Another Chapter](relative/path/to/markdown4.md)
   ```

编号的章节可以用 `-` 或 `*` 来表示(不要混合使用`分隔符`).

1. ***后缀章***-- 和`前缀章`一样, `后缀章`也是没有编号的, 但它们是在有编号的章节之后.

  ```markdown
   - [Last Chapter](relative/path/to/markdown.md)

   [Title of Suffix Chapter](relative/path/to/markdown2.md)
   ```

1. ***草稿章节*** -- `草稿章节`是没有文件, 从而也没有内容的章节. 
`草稿章节`的目的是标记未来要写的章节. 或者在还在布置书的结构时, 避免在你还在大量改变书的结构时创建文件.
`草稿章节`将在 `HTML` 渲染器中被渲染成目录中的`无效链接`(disabled links), 正如你在左边的目录中看到的下一章. 
`草稿章节`的写法与普通章节一样, 但不写文件的`路径`.

   ```markdown
   - [Draft Chapter]()
   ```

1. ***分隔符*** -- `分隔符`可以加在任何其他元素之前, 之间和之后. 它们会在建立的大纲列表中产生一个`HTML渲染`的`行`. 
`分隔符`是只包含破折号的行, 且至少有三个: `---`.

   ```markdown
   # My Part Title
   
   [A Prefix Chapter](relative/path/to/markdown.md)

   ---

   - [First Chapter](relative/path/to/markdown2.md)
   ```

### 例子

下面是本指南的 `SUMMARY.md` 的 `markdown` 源, 以及左边呈现的目录. 

```markdown
{{#include ../SUMMARY.md}}
```
