# INSPIRE 搜索

[Search tips for using INSPIRE ](https://help.inspirehep.net/knowledge-base/inspire-paper-search/)

## 搜索基础知识: 基于关键词的搜索 vs. 自由格式的搜索

`INSPIRE` 的自定义 `查询分析器` 既支持基于 `关键词` 的精确搜索, 也支持类似谷歌的一般自由格式的搜索.

### 关键字 style

搜索使用熟悉的`关键词` 术语. 例如:

+ 搜索作者 `a e cremmer`
+ 隶属机构搜索 `aff ecole normale superieure`

```python
a e cremmer and aff ecole normale superieure
```

### 自由格式(谷歌式)

例如, 记录中的任何包含 `quark` 的信息(`标题`, `作者`, `期刊` 等),
可以与 `关键词式` 搜索相结合, 例如, `author:parke`.

```python
lockyer top quark
witten black hole
quark author:s.j.parke.1
quark and a s.j.parke.1
```

### 标题

`标题搜索`: 可以搜索独立的 `标题词语`, 或使用 `双引号`, 搜索 `整个短语`.

`单引号搜索` 执行 `子字符串搜索`.
例如, `t 'fusion'` 可以找到标题中含有 `diffusion` 或 `fusions` 的文件.
请注意, 当引用几个词的时候, 这可能会产生意想不到的结果.

```python
t black hole information
t "black hole information"
t black hole not t "black hole" not t "black holes"
t 'fusion' not t fusion
```

### 作者姓名

灵活而复杂的 `SPIRES 作者搜索` 在 `INSPIRE中` 也适用.
你可以把作者姓名写成 `j ellis` 或 `ellis, j`, 它们会得到相同的结果.
然而, 如果 `姓氏`(surname)是 `复姓`(compound), 如 `Llewellyn Smith`, 这就不起作用了.
在这种情况下, 搜索应始终使用逗号, 即以 `姓, 名`(family names, given name) 的形式进行.
搜索 `find a llewellyn Smith` 时, 除了所需的结果外, 还会匹配列出为 `Smith, L.` 的作者.
使用句号(`.`)对搜索没有影响.

为了理解 `姓名搜索` 的工作原理, 请考虑在有四位物理学家的世界里,
表格上的名字为

    John R. Ellis, Jane Q. Ellis, Peter James Ellis 和 Ronald James Ellis

包括 `中间名的首字母`, 将使搜索只匹配含有 `中间名字` 的 `记录`.
包括 `全名` 将使搜索只匹配与 `首字母` 或 `确切名字` 相符的记录.
在 `INSPIRE中` 首字母的排序没有区别(`J.R.` 与 `R.J.`).

例子列表
搜索 找到姓名如下的论文

+ `j ellis`;    `"J. Ellis"`, `"J.R. Ellis"`, `"J.Q. Ellis"`, `"Jane Ellis"`, `"John Ellis"`,
`"Jane Q. Ellis"`, `"John R. Ellis"`, `"P.J. Ellis"`, `"Peter J. Ellis"`,
`"Peter James Ellis"`, `"R.J. Ellis"`, `"Ronald J. Ellis"`
`"Ronald James Ellis"`

+ `j r ellis`; `"J.R. Ellis"`, `"John R. Ellis"`, `"R.J. Ellis"`, `"Ronald J. Ellis"`, `"Ronald James Ellis"`

+ `john ellis`; `"J. Ellis"`, `"J.R. Ellis"`, `"J.Q. Ellis"`, `"John Ellis"`, `"John R. Ellis"`, `"P.J. Ellis"`,
`"Peter J. Ellis"`, `"R.J. Ellis"`, `"Ronald J. Ellis"`, `"Ronald James Ellis"`

+ `fa` 专门用于搜索 `第一作者`(first-author).

+ `精确作者`(exact-author)搜索 `ea`,要求 `搜索` 与 `INSPIRE记录` 中的姓名 `完全一致`. 这种搜索要求使用 `逗号`.

```python
a ellis
a john ellis
a j r ellis
a ellis, j
a llewellyn smith, c
fa beacom
ea ellis, j
```

### 更多关于 `作者搜索`

当你找到一篇 `论文`, 并点击 `作者` 的名字时, 你会被带到作者的[出版简介页面](https://inspirehep.net/authors/1010819).

每个 `作者简介页` 都与由 `首字母`, `姓氏` 和 `数字` 组成的 `签名` 相联系
例如, `j.r.ellis.1` 代表 `John Ellis`), 该 `签名` 也可用于搜索.

这些页面由`程序`创建, 该程序根据 `姓名`, `合作者`, `隶属关系` 和其他 `元数据` 来识别作者的身份.
对于大多数作者来说, 它做得相当好. 对于其他作者, 如 `John Smith`, 它做得不太理想.

如果作者已经清点了他们的 `出版清单`, 使用 `作者出版资料页面` 的 `签名` 进行搜索, 会得到最准确的结果.

```python
a j.r.ellis.1
author:j.r.ellis.1
```

### 按`作者人数`搜索

这个 `关键词` 对于将搜索范围缩小到:
通常少于 `5名` 作者的 `理论文章`, 或由 `全体合作者` 撰写的 `实验文章` 很有用.
关于后一点的更多信息可以在[我们的博文中找到](https://blog.inspirehep.net/2012/05/working-with-collaborations/)

```python
a j ellis and ac 1
a j ellis and ac 1->10
cn cdf and ac 100+
cn cms and ac 1000+
```

## 逻辑运算符: 优先级, 小括号和截断

逻辑运算符: `and`(`+`), `or`(`|`), `not`(`-`).

`逻辑运算符` 可以以 `文字` 或 `数学符号` 的形式使用.

```python
a l everett not t higgs and j phys.rev.lett. and primarch hep-ph
a l everett - t higgs + j phys.rev.lett. + primarch hep-ph
```

### 逻辑运算符: 优先和括号

```python
a gaiotto or t defect and date 2016
(a gaiotto or t defect) and date 2016
```

当在一次搜索中使用几个 `and`, `or` 和 `not` 运算符时, 操作会从右边开始`归组`

例如, `a gaiotto or t defect and date 2016` 可以找到所有由 `Gaiotto` 撰写的, 或 `2016年` 发表的 `标题` 含有 `defect` 的论文.

这个顺序往往不是我们想要的, 所以可以添加 `小括号` 来手动控制操作的顺序.
有了括号, 上述搜索与 `a gaiotto or (t defect and date 2016)` 相同,
但与 `(a gaiotto or t defect) and date 2016` 不同(也许此搜索更有用).

### 逻辑运算符: 使用`运算符`和`:`来划定搜索条件

```python
a witten and t jones # 查找有作者 Witten 和标题 Jones 的论文.

a witten t jones # 查找作者姓名与 Witten T. Jones 相匹配的论文,
# 因此若无人名称为 Witten , 搜索结果中的作者姓名将是 W.<姓名以T开头>Jones
# 或 <姓名以T开头>W. Jones.

a witten title jones # 查找作者姓名与 Witten Title Jones 相匹配的论文,
# 因此若无人名称为 Witten 或 Title, 搜索结果中的作者姓名将是 W.T. Jones 或 T.W. Jones.

a:witten title:jones # 使用 : 调用 indexed keywords, 找到作者为 Witten, 标题为 Jones 的论文.
```

### Truncation

```python
a o*aigh and t alge*
```

`星号通配符` 可以用在搜索词的任何地方, 就像在本例中, 我们搜索 `O'Raifeartaigh` 的论文那样.

如果搜索变得过于笼统, 例如 `find t a*` 可能会超时.

## 期刊

要找到一篇特定的文章, 请使用 `期刊名称`, `卷`(包括 `字母`)和 `第一页编号`(或 `文章ID`).
在 `INSPIRE` 中搜索 `期刊` 需要使用期刊名称的 `标准简称`, 如 `Phys.Rev.`.

```python
j phys.rev.lett.,62,1825 # 逗号两边不能有空格
j phys.rev.lett.,112,025001
j Phys.Lett.B,33,305
Phys.Rev.Lett. 112 (2014) 025001
```

您还可以搜索某期刊的所有文章:

```python
j Phys.Rev.Lett.
```

搜索特定的 `volume series`

```python
j phys.lett.b and a witten and t anomaly
# 要搜索一个特定的卷, 请使用搜索词 `vol`
j phys.rev.d and vol 85
```

## 日期

日期搜索可能有些不精确, 因为日期可能是论文 `首次出现` 的日期, 也可能是期刊的 `出版日期`(包括任何可能的 `勘误`).

+ 要搜索论文 `首次出现` 的日期, 请使用 `de` (最早的日期).
+ 要搜索某 `期刊` 的 `出版年份`, 请使用 `jy`(期刊年).

+ 所有日期必须是 `ISO格式`, `yyyy(-mm(-dd))`, 例如 `2003`, `2003-06`, `2003-06-27`.
+ 诸如 `today`, `yesterday`, `last month`(距今天正好一个月)和 `this month`(与今天同月)等日期也可以使用.
+ 使用 `<` 或 `>` 的日期搜索, 例如 `find de > 2000`, 将假设最早的一天, 实际搜索 `find de > 2000-01-01`.

+ 要将搜索范围限制在论文 `首次出现` 的日期(即 `INSPIRE记录` 中最早的日期), 请使用 `de`(date-earliest).
+ 另外还有 `date-added`(`da` 或 `dadd`)和 `date-updated`(`du dupd`)的关键词.

```python
a maldacena and de 1997-11
de today
de last month
a maldacena and de > 2019
de today-1
a quinn and jy 1979
dadd 2014-07-15
du 2021-04-21
a j.r.ellis.1 and d 1990->1999
da today and title neutrino*
da today - 2 and primarch hep-th
```

## Eprints

`Eprint 编号` 书写时带不带 `arXiv:` 都行.

你可以用 `primarch`(primary archive, 相对于 cross-lists)来搜索 eprint 类型.

```python
eprint arxiv:0711.2908 or 0705.4298 or hep-ph/0504227
a unruh or t cauchy not t problem and primarch gr-qc
```

## 报告编号, Report number

您可以使用 `完整的` 报告编号或 `截断的` 形式:

```python
r CERN-TH-3368 or ADP-21-3/T1150
r fermilab-thesis-*
```

## 实验/合作, Experiments/collaborations

可以使用合作名称进行搜索:

```python
cn babar
cn atlas and ac 1000+
```

使用 `author-count`, [上述的 ac][] 可以帮助将 `结果集` 缩小到完整的合作论文(并排除会议论文).

关于INSPIRE中实验的更多信息可以在 [实验数据库][] 中找到, 包括 [出版物的完整统计][].

[上述的 ac]: https://help.inspirehep.net/knowledge-base/inspire-paper-search/#author_count
[实验数据库]: https://inspirehep.net/experiments?sort=mostrecent&size=25&page=1
[出版物的完整统计]: https://inspirehep.net/experiments/1108541

## 所属机构

使用 [机构数据库][] 中机构名称的 `确切形式` 来查找来自特定机构的论文, 如第一个例子所示:

```python
aff beijing, inst. high energy phys.
```

该 `机构数据库` 还包含机构的详细信息, 包括 [出版物的统计][].

[机构数据库]: https://inspirehep.net/institutions?sort=mostrecent&size=25&page=1
[出版物的统计]: https://inspirehep.net/institutions/903123

## 会议论文

关于搜索 `实验合作组` 的 `会议记录` 的更多信息, 可以在我们[2012年12月的博文中找到](https://blog.inspirehep.net/2012/12/httpblog-inspirehep-net201212confronting-those-confounding/).

搜索某个特定会议的论文, 你也可以从 [会议集合][] 开始.

[会议集合]: https://inspirehep.net/conferences?sort=dateasc&size=25&page=1&start_date=upcoming

## 引用次数

你可以使用搜索词 `topcite` 来搜索 `任何引用数量` 的论文:
任何数量的引用都可以用来搜索. 你也可以搜索范围, 如 `topcite 60->173`:

```python
a m albrow and j phys.rev.lett. and t quark* and topcite 137+
a bando and topcite 100->500
```

## 计算 h-index

你可以键入任何搜索, 并拨动 `Show Citation Summary` 的开关. `h-index` 将被显示出来.

```python
a s coleman
```

## 参考`某篇论文`或`某位作者`的文章

Articles that `refer to` a particular paper or author : 指向 xxx

引用作者 `Witten` 的 `S.J. Parke` 论文

```python
author:s.j.parke.1 refersto:author:e.witten.1
```

请注意, 该搜索需要使用 `Parke` 和 `Witten` 的名字的 `作者ID` 形式.

引用 [论文1262571][] 的论文.

```python
refersto:recid:1262571
```

`Refersto` 是一个有用的工具, 可以找到最近对你的工作的引用.
请注意, `refersto` 操作符可以找到引用 `某组论文` 的论文数量. 这与计算这些论文的总引用次数是不同的.
`refersto` 的结果包括那些, 引用 `目标论文集` 中的多篇论文的论文, 并且引用方只列出一次.
如果你想用传统的方法计算一组论文的 `总引文数`, 请使用上述的 `citesummary` 格式.

[论文1262571]: https://inspirehep.net/literature/1262571

## 文件类型/类型代码,Document-Type / Type-Code

+ `b`;  书籍
+ `c`;  会议文件
+ `core`;   关于HEP的工作
+ `i`;  介绍性的
+ `l`;  讲座
+ `note`;   实验说明
+ `p`;  已发表(在有参考价值的杂志上)
+ `proceedings`;    会议论文集
+ `r`;  评论
+ `t`;  论文(Thesis)

这些信息也可以在 `INSPIRE` 结果页面左侧的分面中找到.

```python
t qgp and tc p
t string theory and tc r
t top quark and tc c
t qcd and tc l
t cms and tc t
t qed and tc b
t higgs and tc i
```

## 数据集

`INSPIRE` 显示到 `HEPData` 数据集的链接. 带有相关数据集的论文可以通过 `external_system_identifiers` 搜索找到.

```python
external_system_identifiers.schema:HEPData
```

## 面图,Facets

在任何 INSPIRE 搜索结果的左手边, 人们可以通过当前页描述的许多特征, 轻松缩小搜索范围:

+ 作者人数
+ 是否包括 `PDG RPP` 的版本
+ 文件类型/类型代码
+ 作者
+ 主题, Subject
+ arXiv类别(包括交叉列表, cross-list)
+ Collaboration

## 查询解析器, ElasticSearch和我们的API

任何搜索命令都可以被分析, 以验证高级搜索是否被解释为预期.

虽然, 一般来说不提供对解释这些响应的支持,
但具有充分理由的错误报告, 请发送到 [feedback@inspirehep.net][].

更多关于INSPIRE的搜索功能的内部运作的信息,
包括 `ElasticSearch`, 可以在我们的 [INSPIRE REST API 文档][] 中找到.

如果你对这些搜索有疑问或意见, 或者你想在这个帮助页面上添加任何东西, 请联系 [feedback@inspirehep.net][].

[INSPIRE REST API 文档]: https://github.com/inspirehep/rest-api-doc
[feedback@inspirehep.net]: mailto:feedback@inspirehep.net
