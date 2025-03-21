#import "@preview/physica:0.9.4": *
#import "ymy-style.typ": *
#show: apply-template
#set text(font: "Noto Sans CJK SC")

= 抽象代数

#link("https://zh.wikipedia.org/wiki/模")[wiki-模]
#link("https://en.wikipedia.org/wiki/Algebra_over_a_field#Generalization:_algebra_over_a_ring")[代数 (环论)]
#link("https://zhuanlan.zhihu.com/p/166100315")[域上的代数]

在数学的抽象代数中，环上的模(module over a ring)的概念是对向量空间概念的推广，
这里不再要求向量空间里的标量的代数结构是域，进而放宽标量可以是环。
因此，模同向量空间一样是加法交换群；在环元素和模元素之间定义了乘积运算，
并且环元素和模元素的乘积是符合结合律的[注 1]和分配律的。
模非常密切的关联于群的表示理论。它们还是交换代数和同调代数的中心概念，
并广泛的用于代数几何和代数拓扑中。

== 定义

假设$R$是环(ring)且 $1_R in R$, $1_R$ 是其乘法运算的单位元，
则 *左R-模* 包括一个交换群$(M,+)$，以及一个映射（或运算）：$R times M -> M$（叫做标量乘法或数积），
通常把此运算的值$(r,x)$ 记作$r x$或是$r dot.c x$，$r in R$且$x in M$ 并且满足以下条件