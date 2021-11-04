# FeynRules

[FeynRules](http://feynrules.irmp.ucl.ac.be/#no1)
[FeynRules 2.0](https://arxiv.org/abs/1310.1921)

`FeynRules` 是一个 Mathematica 软件包, 允许在动量空间为任何 QFT 物理模型计算 `Feynman` 规则.
用户需要向 FeynRules 提供描述新模型所需的最小信息, 这些信息包含在所谓的`模型文件`中.
这些信息随后被用来计算与`拉格朗日量`相关的费曼规则集.

然后, 代码计算出的费曼规则可以用来将新的物理模型实现到其他现有的工具中, 如 `MC生成器`.
这是通过一组接口完成的, 这些接口是由相应的 `MC作者` 共同开发和维护的.
