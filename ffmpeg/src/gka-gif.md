# paraView 导出动画

[paraView做动画(终极教程)](https://blog.csdn.net/a22635798/article/details/103822186)

效果最好的方法是:
`file` 菜单下的 `save animation` 按钮, 将每一针都输出成图片,把导出的图片连成动画

推荐使用: EnVe和Demo3D VideoMaker

其他方法对比分析:

+ 网上有方法说, View -> Animation View, 但是此方法只能在paraView中查看动画, 无法导出.
+ file菜单下的save animation 按钮, 也可以导出avi动画, 但是动画效果非常差

## gka 帧动画生成工具

[帧动画生成工具](https://gka.js.org/#/?id=gka-1)

### 安装

```bash
npm i gka -g
```

开始使用
只需一行命令, 快速生成动画文件, 支持效果预览.

```bash
gka E:\img  # 对 E:\img 目录中的图片进行处理
```
