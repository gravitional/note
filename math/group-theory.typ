#import "my_ytableau.typ": *
#import "ymy-style.typ": *
#set text(font: "Noto Sans CJK SC")
#set math.equation(numbering: "(1)")
#let myTitle = [群论]
#align(center, text(17pt)[*#myTitle*])

= 第四章 置换群

置换群在物理和数学上的重要意义：
- 置换群描写全同粒子体系的置换对称性
- 所有有限群都同构于置换群的子群
- 杨算符能明确描写张量指标间的复杂对称性

== 置换群的一般性质

=== 置换

#mydef[置换的定义][qq][ww][
  $n$个客体排列次序的变换称为 #mybl[置换]；
  $n$个客体共有$n!$个不同的置换
]

$
  R = mat(
  1,2,3, cdots,j,cdots,n;
  r_1,r_2,r_3,cdots,r_j,cdots,r_n;
  )
$<my1>

#myrem[置换的另一种定义][
  第$j$位置的客体，经过置换后变成了($arrD$)，原先 $r_j$位置上的客体，与当前的定义互逆
]


例如：
$
  mat(
  1,2,3;
  2,3,1) (varphi_1 varphi_2 varphi_3)=
  (varphi_3 varphi_1 varphi_2)
$<my2>

对一给定的置换，各列的排列次序无关紧要，
重要的是每一列上下两个数字间的对应关系。

#myexa[置换的乘积][
  两个#mybl[置换的乘积]定义为相继做两次置换
  #let s = $mat(3,4,5,2,1;2,4,5,1,3)$
  #let s2 = $mat(1,2,3,4,5;2,1,2,4,5)$
  #let r = $mat(1,2,3,4,5;3,4,5,2,1)$
  #let r2 = $mat(5,4,1,2,3;1,2,3,4,5)$
  $
    &S=#s=#s2\
  & R=#r=#r2\
  & S R = #s #r =mat(1,2,3,4,5;2,4,5,1,3)\
  & S R = #s2 #r2 =mat(5,4,1,2,3;3,1,2,4,5)\
  $
]
$S R$ 可以理解为，把$R$置换的第二行数字作$S$置换，
或者把$S$置换的第一行数字作$R^(-1)$ 置换。
- 置换用矩阵来描写，但置换的乘积不服从矩阵乘积规则。

#myexa[置换群][$n$ 个客体的$n!$个置换的集合满足群的四个条件，构成群，称为 #mybl[$n$个客体置换群] 记作 #mybl[$S_n$].
]
- 恒元
$
  & E = mat(
  1,2,3, cdots,n;
  1,2,3, cdots,n;
  ) wide E R=R E =R\
  & R R^(-1) = R^(-1) = E
$

$n$个客体中$m$个客体的所有置换变换构成置换群$S_m$，显然$S_m$是$S_n$的子群（$m<=n$）
#mysupp[置换群的子群链][
  $
    S_n supset S_(n-1) supset S_(n-2) cdots supset S_1 =E
  $
]

=== 轮换和对换

#mydef[轮换][
  轮换是一类特殊的置换：$n-ell$个客体保持不变，
  余下的$ell$个客体顺序变换，形成一个循环；
  $ell$称为#mybl[轮换长度]
  $
    mat(a_1,a_2,cdots,a_ell)=mat(
    a_1, cdots, a_(ell-1), a_ell, ..myeqbl(b_1, cdots, b_(n-ell));
    a_2, cdots, a_(ell), a_1, ..myeqbl(b_1, cdots, b_(n-ell));
    )
  $
]


- 两个没有公共客体的轮换，乘积次序可以交换
#let a = $mat(a_1, a_2, cdots, a_ell)$
#let b = $mat(b_1, b_2, cdots, b_ell)$
$
  #a thin #b =#b thin #a
$
- 轮换的逆

#ydiag(
  3,
  (1, 2, 3),
  (4, 5),
)
