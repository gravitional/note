# 介绍

`Guile` 是 `Scheme` 编程语言的一个实现.
Scheme(http://schemers.org/)是一种优雅的, 概念简单的 `Lisp方言`,
由 Guy Steele 和 Gerald Sussman 发起, 并由被称为RnRS(Scheme的修订报告)的一系列报告演变而来.
与Python或Perl等不同, Scheme没有仁慈的独裁者(dictator.).
有许多Scheme的实现, 它们有不同的特点, 并有围绕它们的社区和学术活动, 而语言的发展是这些相互作用的结果.
Guile的特殊性在于:

+ 它很容易与其他用 `C语言` 编写的代码相结合
+ 它与 `GNU项目` 有着历史和持续的联系
+ 它强调 `交互式` 和 `渐进式` 编程 (incremental programming)
+ 它实际上支持几种语言, 而不仅仅是Scheme.

接下来的几节将解释我们对这些观点的理解.
之后的章节包括如何获得和安装Guile, 以及我们在本手册中使用的排版约定.
