# asymptote

[error-in-asymptote-using-upacity-to-draw-a-lemniscate](https://tex.stackexchange.com/questions/473893/error-in-asymptote-using-upacity-to-draw-a-lemniscate)

## 基本

`//` 用作单行注释, `/**/`用作多行注释

示例代码1: --弦图

```asymptote
// 网格
import math; //实用数学功能
add( scale(1cm) * grid(7, 7, gray) ); // 添加格子到图形中
add( shift(0,3cm) * rotate(-aTan(3/4)) * scale(1cm) * grid(5, 5, gray) );
// 弦图主体
fill( box((3cm,3cm), (4cm,4cm)), opacity(0.5)+yellow ); //填充工具
filldraw( (4cm,0) -- (4cm,3cm) -- (0,3cm) -- cycle //填充并且描绘
^^ (7cm,4cm) -- (4cm,4cm) -- (4cm,0) -- cycle
^^ (3cm,7cm) -- (3cm,4cm) -- (7cm,4cm) -- cycle
^^ (0,3cm) -- (3cm,3cm) -- (3cm,7cm) -- cycle,
fillpen=opacity(0.1)+red, drawpen=red+0.5mm  //指定填充用的笔和描绘用的笔
);
```

保存到`test.asy`文件中, 通过 `asy -V -f pdf test` 运行, 生成 `pdf` 格式的图像.
如果脚本中含有指定透明度的语句, 如`opacity(0.5)`, 可能会报错, 原因是`ghostscripts`的安全选项, 通过使用加上`-nosafe`来禁止安全选项即可:

```asymptote
asy -V  -nosafe -f pdf test
```
