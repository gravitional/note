# markdown.md

reference: [Simplelified Markdown Syntax](https://www.appinn.com/markdown/#backslash)
[CommonMark specification ](https://commonmark.org/)

## 删除线

```markdown
~~这是删除线~~~~
```

~~这是删除线~~~~

## 链接 常用

```markdown
I get 10 times more traffic from [Google][] than from
[Yahoo][] or [MSN][].

  [google]: http://google.com/        "Google"
  [yahoo]:  http://search.yahoo.com/  "Yahoo Search"
  [msn]:    http://search.msn.com/    "MSN Search"
```

[Google][]
And then define the link:

[Google]: http://google.com/

## 链接 详细

[original context][]

[original context]: https://www.appinn.com/markdown/#link "original context"

>Markdown 支持两种形式的链接语法:  行内式和参考式两种形式.
>不管是哪一种, 链接文字都是用`[方括号]`来标记.

### 行内式

>要建立一个 行内式的链接, 只要在方块括号后面紧接着圆括号并插入网址链接即可,
>如果你还想要加上链接的 title 文字, 只要在网址后面, 用双引号把 title 文字包起来即可, 例如:

```markdown

This is [an example](http://example.com/ "Title") inline link.

[This link](http://example.net/) has no title attribute.
```

This is [an example](http://example.com/ "Title") inline link.

[This link](http://example.net/) has no title attribute.

>如果你是要链接到同样主机的资源, 你可以使用相对路径:

```markdown
See my [About](/about/) page for details.
```

### 参考式

>参考式的链接是在链接文字的括号后面再接上另一个方括号, 而在第二个方括号里面要填入用以辨识链接的标记:

```markdown
This is [an example][id] reference-style link.
```

你也可以选择性地在两个方括号中间加上一个空格:

```markdown
This is [an example] [id] reference-style link.
```

接着, 在文件的任意处, 你可以把这个标记的链接内容定义出来:

```markdown
[id]: http://example.com/  "Optional Title Here"
```

链接内容定义的形式为:

方括号(前面可以选择性地加上至多三个空格来缩进), 里面输入链接文字
接着一个冒号
接着一个以上的空格或制表符
接着链接的网址
选择性地接着 title 内容, 可以用单引号, 双引号或是括弧包着

>下面这三种链接的定义都是相同:

```markdown
[foo]: http://example.com/  "Optional Title Here"
[foo]: http://example.com/  'Optional Title Here'
[foo]: http://example.com/  (Optional Title Here)
```

>请注意: 有一个已知的问题是 Markdown.pl 1.0.1 会忽略单引号包起来的链接 title.
>链接网址也可以用方括号包起来:

```markdown
[id]: <http://example.com/>  "Optional Title Here"
```

>你也可以把 title 属性放到下一行, 也可以加一些缩进, 若网址太长的话, 这样会比较好看:

```markdown
[id]: http://example.com/longish/path/to/resource/here
    "Optional Title Here"
```

>**网址定义只有在产生链接的时候用到, 并不会直接出现在文件之中. **
>链接辨别标签可以有字母, 数字, 空白和标点符号, 但是并不区分大小写, 因此下面两个链接是一样的:
>
```markdown
[link text][a]
[link text][A]
```

### 隐式链接

隐式链接标记功能让你可以省略指定链接标记, 这种情形下, 链接标记会视为等同于链接文字, 要用隐式链接标记只要在链接文字后面加上一个空的方括号, 如果你要让 "Google" 链接到 google.com, 你可以简化成:

```markdown
[Google][]
```

然后定义链接内容:

```markdown
[Google]: http://google.com/
```

由于链接文字可能包含空白, 所以这种简化型的标记内也许包含多个单词:

```markdown
Visit [Daring Fireball][] for more information.
```

然后接着定义链接:

```markdown
[Daring Fireball]: http://daringfireball.net/
```

链接的定义可以放在文件中的任何一个地方, 我比较偏好直接放在链接出现段落的后面, 你也可以把它放在文件最后面, 就像是注解一样.

下面是一个参考式链接的范例:

```markdown
I get 10 times more traffic from [Google] [1] than from
[Yahoo] [2] or [MSN] [3].

  [1]: http://google.com/        "Google"
  [2]: http://search.yahoo.com/  "Yahoo Search"
  [3]: http://search.msn.com/    "MSN Search"
```

如果改成用链接名称的方式写:

```markdown
I get 10 times more traffic from [Google][] than from
[Yahoo][] or [MSN][].

  [google]: http://google.com/        "Google"
  [yahoo]:  http://search.yahoo.com/  "Yahoo Search"
  [msn]:    http://search.msn.com/    "MSN Search"
```

上面两种写法都会产生下面的 HTML.

```html
<p>I get 10 times more traffic from <a href="http://google.com/"
title="Google">Google</a> than from
<a href="http://search.yahoo.com/" title="Yahoo Search">Yahoo</a>
or <a href="http://search.msn.com/" title="MSN Search">MSN</a>.</p>
```

下面是用行内式写的同样一段内容的 Markdown 文件, 提供作为比较之用:

```markdown
I get 10 times more traffic from [Google](http://google.com/ "Google")
than from [Yahoo](http://search.yahoo.com/ "Yahoo Search") or
[MSN](http://search.msn.com/ "MSN Search").
```

参考式的链接其实重点不在于它比较好写, 而是它比较好读, 比较一下上面的范例,
使用参考式的文章本身只有 81 个字符, 但是用行内形式的却会增加到 176 个字元,
如果是用纯 HTML 格式来写, 会有 234 个字元, 在 HTML 格式中, 标签比文本还要多.

使用 Markdown 的参考式链接, 可以让文件更像是浏览器最后产生的结果,
让你可以把一些标记相关的元数据移到段落文字之外, 你就可以增加链接而不让文章的阅读感觉被打断.

## 代码区块 常用

```markdown
这是一个普通段落:

    这是一个代码区块.
```

$$  \iint _a^b \frac a b $$

### 代码区块 详细

[original context][]

和程序相关的写作或是标签语言原始码通常会有已经排版好的代码区块,
通常这些区块我们并不希望它以一般段落文件的方式去排版, 而是照原来的样子显示,
Markdown 会用 `<pre>` 和 `<code>` 标签来把代码区块包起来.

要在 Markdown 中建立代码区块很简单, 只要简单地缩进 4 个空格或是 1 个制表符就可以, 例如, 下面的输入:

```markdown
这是一个普通段落:

    这是一个代码区块.
```

Markdown 会转换成:

```html
<p>这是一个普通段落: </p>

<pre><code>这是一个代码区块.
</code></pre>
```

这个每行一阶的缩进(4 个空格或是 1 个制表符), 都会被移除, 例如:

```markdown
Here is an example of AppleScript:

    tell application "Foo"
        beep
    end tell
```

会被转换为:

```html
<p>Here is an example of AppleScript:</p>

<pre><code>tell application "Foo"
    beep
end tell
</code></pre>
```

一个代码区块会一直持续到没有缩进的那一行(或是文件结尾).

在代码区块里面,  `&` ,  `<` 和 `>` 会自动转成 `HTML` 实体,
这样的方式让你非常容易使用 Markdown 插入范例用的 HTML 原始码,
只需要复制贴上, 再加上缩进就可以了, 剩下的 Markdown 都会帮你处理, 例如:

```html
    <div class="footer">
        &copy; 2004 Foo Corporation
    </div>
```

会被转换为:

```html
<pre><code>&lt;div class="footer"&gt;
    &amp;copy; 2004 Foo Corporation
&lt;/div&gt;
</code></pre>
```

代码区块中, 一般的 Markdown 语法不会被转换, 像是星号便只是星号,
这表示你可以很容易地以 Markdown 语法撰写 Markdown 语法相关的文件.

## 反斜杠转义

Markdown 可以利用反斜杠来插入一些在语法中有其它意义的符号,
例如: 如果你想要用星号加在文字旁边的方式来做出强调效果(但不用 `<em>` 标签), 你可以在星号的前面加上反斜杠:

```markdown
\*literal asterisks\*
```

Markdown 支持以下这些符号前面加上反斜杠来帮助插入普通的符号:

```markdown
\   反斜线
`   反引号
*   星号
_   底线
{}  花括号
[]  方括号
()  括弧
#   井字号
+   加号
-   减号
.   英文句点
!   惊叹号
```

## 行内代码

如果要标记一小段行内代码, 你可以用反引号把它包起来``(`)``, 例如:

```markdown
Use the `printf()` function.
```

会产生:

```html
<p>Use the <code>printf()</code> function.</p>
```

如果要在代码区段内插入反引号, 你可以用多个反引号来开启和结束代码区段:

```markdown
``There is a literal backtick (`) here.``
```

## 添加图片

[MarkDown添加图片的三种方式](https://www.jianshu.com/p/280c6a6f2594)

插图最基础的格式就是:

```markdown
![替代文本](图片链接 "鼠标悬浮文字")
```

+ `替代文本`: 图片的`Alt`标签, 用来描述图片的关键词, 可以不写. 本意是当图片因为某种原因不能被显示时而出现的替代文字.
后来又被用于`SEO`, 可以方便搜索引擎根据`Alt text`里面的关键词搜索到图片.
+ `图片链接`: 可以是图片的本地地址或者是网址.
+ `"鼠标悬浮文字"`: 鼠标悬置于图片上会出现的标题文字, 可以不写.

## markdownlint 语法提示

默认规则配置禁用`MD013/line-length`, 因为许多文件包含的行长超过`80`个字符

```json
{
    "MD013": false
}
```

注意: 默认情况下禁用`MD002/first-heading-h1`, 因为markdownlint库中已不推荐使用它.

可以在项目的任何目录中, 通过创建名为 `.markdownlint.json` or `.markdownlintrc` 的`JSON`文件, 或名为`markdownlint.yaml` or `.markdownlint.yml` 的`YAML`文件来启用, 禁用和自定义规则. 这些自定义的规则适用于同一目录中的`Markdown`文件以及没有配置规则的任何子目录.

注意: `.markdownlint{.json,.yaml,.yml,rc}`仅在打开项目时使用.
当没有打开文件夹或文件不属于当前项目时, 将应用常规用户和工作区设置(请参见下文).
如果同一目录中存在多个这些文件, `.markdownlint.json`优先于`.markdownlint`,
`.markdownlint.yml`优先于`.markdownlintrc`.

自定义配置通常由项目根目录中的`.markdownlint.json`文件定义:

```json
{
    "default": true,
    "MD003": { "style": "atx_closed" },
    "MD007": { "indent": 4 },
    "no-hard-tabs": false
}
```

要从另一个配置文件开始, 任何配置文件都可以使用`extends`属性提供相对路径:

```json
{
    "extends": "../.markdownlint.json",
    "no-hard-tabs": true
}
```

通过扩展引用的文件不必是当前项目的一部分(但通常是).

也可以使用`VS Code`的用户和工作区`settings`来配置规则, 在`VS Code`的用户设置中设置如下内容:

```json
{
    "editor.someSetting": true,
    "markdownlint.config": {
        "default": true,
        "MD003": { "style": "atx_closed" },
        "MD007": { "indent": 4 },
        "no-hard-tabs": false
    }
}
```

从`user settings`引用的文件路径是相对于用户的主目录解析的(例如: 在`Windows`上为`%USERPROFILE%`, 在`macOS/Linux`上为`$HOME`).
配置位置具有以下优先级(以降序排列):

+ `.markdownlint{.json,.yaml,.yml,rc}` file in the same directory
+ `.markdownlint{.json,.yaml,.yml,rc}` file in a parent directory
+ `.markdownlint{.json,.yaml,.yml,rc}` file in the root of the project
+ `Visual Studio Code` user/workspace settings
+ `Default configuration` (see above)

找到配置后, 较低优先级的位置将被忽略.  保存到任何位置的更改将立即生效.  通过扩展引用的文件不会监视更改.
仅最后两个位置适用于项目外部的文件.

## 字体,图片排版

[Markdown进阶](https://blog.csdn.net/heimu24/article/details/81189700)
[Markdown 简明语法参考](http://whuhan2013.github.io/blog/2015/09/19/markdown-simple-grammar/)

### 更改字体

Markdown 更改字体的语法:

<font face="黑体">我是黑体字</font>
<font face="微软雅黑">我是微软雅黑</font>
<font face="STCAIYUN">我是华文彩云</font>
<font color=red>我是红色</font>
<font color=#008000>我是绿色</font>
<font color=Blue>我是蓝色</font>
<font size=5>我是尺寸</font>
<font face="黑体" color=green size=5>我是黑体, 绿色, 尺寸为5</font>

#### 为文字添加背景色

由于 `style` 标签, 和标签的 `style` 属性不被支持, 所以这里能是借助 `table`, `tr`, `td` 等表格标签的 `bgcolor` 属性来实现背景色.
故这里对于文字背景色的设置, 只是将那一整行看作一个表格, 更改了那个格子的背景色(`bgcolor`)

Markdown 语法:

```markdown
<table><tr><td bgcolor=yellow>背景色yellow</td></tr></table>
```

### 图片大小和排版

如果是单纯插入来自图床的图片外链:

+ 设置设置图片百分比

```markdown
<img src="http://pic11.photophoto.cn/20090626/0036036341009653_b.jpg" width="50%" height="50%">
```

+ 固定图片宽度/高度:

```markdown
<img src="http://xxx.jpg" title="Logo" width="100" />
```

宽度是 `Width`, 高度是 `High`. `Title` 为图片描述.

+ 单张居中显示:

```markdown
<center>
    <img src="http://dreamofbook.qiniudn.com/Zero.png">
</center>
或者
<figure>
    <img src="http://xxx.jpg">
</figure>
```

+ 设置图片居右

```markdown
<div align=right><img src="http://pic11.photophoto.cn/20090626/0036036341009653_b.jpg" width="50%" height="50%"></div>
```

+ 两张并排显示:

```markdown
<figure class="half">
    <img src="http://xxx.jpg"> <img src="http://yyy.jpg"> </figure>
```

三张并排显示:

```markdown
<figure class="third">
    <img src="http://xxx.jpg">
    <img src="http://yyy.jpg">
    <img src="http://zzz.jpg">
</figure>
```

使用时复制相应的代码粘贴, 然后替换 `""`(英文引号) 内的链接,
也就是把 `http://xxx.jpg` 链接替换自己上传图片的外链(一般只有图床的图才有这样的链接).

Tips: `jpg`/`png` 都是常用文件格式,
jpg 格式压缩效率高, 相对文件质量低, 占用空间少, 在页面显示时加载会快一些.
png 格式的图片更清晰.

## Markdown 表格

[Markdown 表格](https://markdown.com.cn/extended-syntax/tables.html)

要添加表, 请使用 `三个` 或多个连字符(---)创建每列的标题,
并使用管道(|)分隔每列. 您可以选择在表的任一端添加管道.

```markdown
| Syntax    | Description |
| --------- | ----------- |
| Header    | Title       |
| Paragraph | Text        |
```

呈现的输出如下所示:

| Syntax    | Description |
| --------- | ----------- |
| Header    | Title       |
| Paragraph | Text        |

单元格宽度可以变化, 如下所示. 呈现的输出将看起来相同.

| Syntax    | Description |
| --------- | ----------- |
| Header    | Title       |
| Paragraph | Text        |

Tip: 使用连字符和管道创建表可能很麻烦.
为了加快该过程, 请尝试使用 [Markdown Tables Generator](https://www.tablesgenerator.com/markdown_tables).
使用图形界面构建表, 然后将生成的Markdown格式的文本复制到文件中.

## Markdown 定义列表

一些Markdown处理器允许您创建术语及其对应定义的定义列表.
要创建定义列表, 请在第一行上键入术语. 在下一行, 键入一个冒号, 后跟一个空格和定义.

```markdown
First Term
: This is the definition of the first term.

Second Term
: This is one definition of the second term.
: This is another definition of the second term.
```

HTML看起来像这样:

```html
<dl>
  <dt>First Term</dt>
  <dd>This is the definition of the first term.</dd>
  <dt>Second Term</dt>
  <dd>This is one definition of the second term. </dd>
  <dd>This is another definition of the second term.</dd>
</dl>
```

呈现的输出如下所示:

First Term
: This is the definition of the first term.

Second Term
: This is one definition of the second term.
: This is another definition of the second term.

## Markdown 脚注

[Markdown 脚注](https://markdown.com.cn/extended-syntax/footnotes.html)

脚注使您可以添加注释和参考, 而不会使文档正文混乱.
当您创建脚注时, 带有脚注的上标数字会出现在您添加脚注参考的位置.
读者可以单击链接以跳至页面底部的脚注内容.

要创建脚注参考, 请在方括号([^1])内添加插入符号和标识符.
标识符可以是数字或单词, 但不能包含空格或制表符.
标识符仅将脚注参考与脚注本身相关联-在输出中, 脚注按顺序编号.

在括号内使用另一个插入符号和数字添加脚注, 并用冒号和文本([^1]: My footnote.).
您不必在文档末尾添加脚注.
您可以将它们放在除列表, 块引号和表之类的其他元素之外的任何位置.

Here's a simple footnote,[^1] and here's a longer one.[^bignote]

[^1]: This is the first footnote.

[^bignote]: Here's one with multiple paragraphs and code.

    Indent paragraphs to include them in the footnote.

    `{ my code }`

    Add as many paragraphs as you like.

## Markdown 标题编号 {#custom-id}

许多Markdown处理器支持标题的 `自定义ID` - 一些Markdown处理器会自动添加它们.
添加自定义ID允许您直接链接到标题并使用CSS对其进行修改. 要添加自定义标题ID,
请在与标题相同的行上用大括号括起该 `自定义ID`.

```markdown
### My Great Heading {#custom-id}
```

HTML看起来像这样:

```html
<h3 id="custom-id">My Great Heading</h3>
```

### 链接到标题ID (#headid)

通过创建带有数字符号(#)和自定义标题ID的 标准链接,
可以链接到文件中具有自定义ID的标题.

Markdown HTML 预览效果

`[Heading IDs](#heading-ids)  <a href="#heading-ids">Heading IDs</a>`

[Heading IDs](#custom-id)

其他网站可以通过将自定义标题ID添加到网页的完整URL
例如 `[Heading IDs](https://markdown.com.cn/extended-syntax/heading-ids.html#headid)`, 来链接到标题.

## 使用 Emoji 表情

[Markdown 使用 Emoji 表情](https://markdown.com.cn/extended-syntax/emoji.html)
