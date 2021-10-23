# FeynCalc

## 输出格式

恢复被 `FeynCalc` 更改默认输出格式:

```mathematica
SetOptions[EvaluationNotebook[],  CommonDefaultFormatTypes -> {"Output" -> StandardForm}]
```

## FeynArts

[FeynArts in FeynCalc](https://github.com/FeynCalc/feyncalc/wiki/FeynArts)

如果你使用自动安装程序安装`FeynCalc`的稳定版或开发版, 你会被问到是否应该下载和修补最新版本的`FeynArts`.
因此, 不需要额外的步骤.
然而, 你可能会想更新你的`FeynArts`版本而不重新安装`FeynCalc`. 在这种情况下, 请遵循以下步骤.

下载最新版本的`FeynArts`, 并将`tarball`解压到

```mathematica
<< FeynCalc`
Print[$FeynArtsDirectory]
```

启动 `Mathematica` 并输入

```mathematica
$LoadFeynArts = True;
<<FeynCalc`;
```

将出现一个对话框, 询问您是否要对`FeynArts`打补丁. 点击确定, 等到修补过程结束.
重启`Mathematica`内核, 并尝试运行一些示例代码(点击`FeynCalc`加载时出现的横幅上的示例链接).
确保一切都能正确运行, 没有任何警告和错误.

## 圈积分

### 费曼参数化

`FeynmanParametrize[exp,k]`
