#import "@preview/physica:0.9.4": *
#import "ymy-style.typ": *
#show: apply-template
#set text(font: "Source Han Serif SC")


= 微分几何与广义相对论

== 拓扑空间简介

== 流形和张量场

=== 微分流形

=== 切矢和切矢场

=== 流形上的矢量场

#myexa[例1][
在2维欧氏空间中，笛卡尔系${x,y}$的$x$及$y$坐标线是互相正交的两组平行直线，极坐标系$r,phi$的$phi$坐标线是以原点为心得无数同心圆，$r$坐标线是从原点出发的无数半直线。
]

== 黎曼（内禀）曲率张量

== 李导数、killing场和超曲面

== 微分形式及其积分

=== 用标架计算曲率张量

$
  tensor((e_tau),+b) grad_b tensor((e_mu),+a)
  =tensor(gamma,+sigma,-mu,-tau) tensor((e_sigma),+a)
$
其中展开系数 $tensor(gamma,+sigma,-mu,-tau)$ 称为*联络系数(connection coefficients)*，可看作 $grad_a$ 借基底场${tensor((e_mu),a)}$ 的体现。

$
  (pdv(,x^tau))^b grad_b (pdv(,x^mu))^a=
  tensor(Gamma,+sigma,-mu,-tau)
$<5-7-1>
式#ref(<5-7-1>)与对偶基矢$(e^nu)_a$ 缩并给出 $tensor(gamma,+sigma,-mu,-tau)$ 的显表达式.


#mytheo[嘉当(Cartan)第一结构方程][Important][Exam][other][
  $
    dd(vb(e)^nu) = - vb(e)^mu and tensor(vb(omega),-nu,+nu)
  $<5-7-6>
]

#mytheo[嘉当第二结构方程][Important][
  $
    tensor(vb(R),-mu,+nu)=
    tensor(dd(vb(omega)),-mu,+nu)+
    tensor(vb(omega),-mu,+lambda) and
    tensor(vb(omega),-lambda,+nu)
  $<5-7-8>
]

式@5-7-8 等价于式(3-4-20')，分别是曲率定义（曲率与联络的关系）在标架和坐标基底的分量表达式。


$ g_(mu nu) = g_(a b) (e_mu)^a (e_nu)^b $
$ g^(mu nu) = g^(a b) (e^mu)_a (e^nu)_b $
引进以下两个记号
$
  (a) quad tensor((e_mu),-a) equiv g_(a b) tensor((e_mu),+b),wide
  (b) quad tensor((e^mu),+a) equiv g^(a b) tensor((e^mu),-b),
$
则有
$
  (a) quad tensor((e^mu),-a) equiv g^(mu nu) tensor((e_nu),-a),wide
  (b) quad tensor((e_mu),+a) equiv g_(mu nu) tensor((e^nu),+a),
$

#myeqs(
  $ F &= sum $, // 编号为 (1-1.a)
  $ = x $, // 编号为 (1-1.b)
  $ = 1 / 2m v^2 $, // 编号为 (1-1.c)
)
