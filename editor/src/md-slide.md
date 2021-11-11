# Markdown Preview Enhanced

[markdown-preview-enhanced](https://shd101wyy.github.io/markdown-preview-enhanced/#/zh-cn/presentation)
[幻灯片制作](https://shd101wyy.github.io/markdown-preview-enhanced/#/zh-cn/presentation)
[用 Markdown 做幻灯片](https://zhuanlan.zhihu.com/p/108697301)

Markdown Preview Enhanced 是为 Atom 及 Visual Studio Code 编辑器上的 Markdown 插件.
这款插件意在让你拥有飘逸的 Markdown 写作体验.

## 安装MPE

+ 通过 VS Code 安装(推荐):
打开 vscode 编辑器, 在插件页搜索 markdown-preview-enhanced, 接着点击 Install 按钮.
+ 通过 GitHub 安装:
从 [Releases][] 下载 `markdown-preview-enhanced-*.vsix` 文件.
打开 `vscode`, 运行 `Extension: Install from VSIX` 命令, 然后选择你刚刚下载好的 `*.vsix` 文件.

[Releases] :https://github.com/shd101wyy/vscode-markdown-preview-enhanced/releases

vscode 中 MPE 默认预览主题的条目为:

    markdown-preview-enhanced.previewTheme

## 制作幻灯片

Markdown Preview Enhanced 使用 [reveal.js](https://github.com/hakimel/reveal.js) 来渲染漂亮的幻灯片.

[点击这里][] 查看相关的介绍.

[点击这里]: https://rawgit.com/shd101wyy/markdown-preview-enhanced/master/docs/presentation-intro.html

### Front-Matter

你可以通过 `front-matter`(扉页) 来设置你的幻灯片.
你需要将你的设置写在 `presentation` 部分下. 例如:

```Markdown
---
presentation:
  width: 800
  height: 600
---

<!-- slide -->

在这里编写你的幻灯片. . .
```

这个幻灯片将会拥有 `800x600` 的大小.

### 设置

```yaml
---
presentation:
  # presentation 主题
  # === 可选的主题 ===
  # "beige.css"
  # "black.css"
  # "blood.css"
  # "league.css"
  # "moon.css"
  # "night.css"
  # "serif.css"
  # "simple.css"
  # "sky.css"
  # "solarized.css"
  # "white.css"
  # "none.css"
  theme: white.css

  # 当演示文稿被缩放以适应不同的分辨率时, 演示文稿的 "正常 "尺寸, 长宽比将被保留下来
  # 可以用百分比单位指定.
  width: 960
  height: 700

  # 在内容周围空白边框的尺寸
  margin: 0.1

  # 适用于内容的最小/最大缩放的上下限
  minScale: 0.2
  maxScale: 1.5

  # 在右下角显示控件
  controls: true

  # 显示一个演示进度条
  progress: true

  # 显示当前幻灯片的页码
  slideNumber: false

  # 将切换幻灯片的变化推送到浏览器历史记录中
  history: false

  # 启用导航的键盘快捷方式
  keyboard: true

  # 启用幻灯片概览模式
  overview: true

  # 幻灯片垂直居中
  center: true

  # 在有触摸输入的设备上启用触摸导航功能
  touch: true

  # 循环播放演示
  loop: false

  # 改变演示方向为 Right To Left
  rtl: false

  # 每次加载演示文稿时, 随机调整幻灯片的顺序
  shuffle: false

  # 在全局范围内打开或关闭 fragments
  fragments: true

  # Flags, 标志演示是否运行在嵌入式模式中
  # 即包含在屏幕的有限部分中
  embedded: false

  # 标志着当问号键被按下时, 我们是否应该显示一个帮助覆盖
  help: true

  # 标志着演讲者的笔记是否应该对所有观众可见
  showNotes: false

  # 自动进入下一张幻灯片的毫秒数.
  # 设置为0 表示禁用自动切换,
  # 在你的幻灯片上使用data-autoslide属性可以覆盖该值
  autoSlide: 0

  # 在用户输入后停止自动切换
  autoSlideStoppable: true

  # 通过鼠标滚轮启用幻灯片导航
  mouseWheel: false

  # 在移动设备上隐藏地址栏
  hideAddressBar: true

  # 在 iframe 预览覆盖层中打开链接
  previewLinks: false

  # 转场风格
  transition: 'default' # 其他值: none/fade/slide/convex/concave/zoom

  # 过渡速度
  transitionSpeed: 'default' # default/fast/slow

  # 整张幻灯片背景的过渡样式
  backgroundTransition: 'default' # none/fade/slide/convex/concave/zoom

  # 悬浮视图中,当前幻灯片之外的可见数量
  viewDistance: 3

  # 视差(Parallax)背景图片
  parallaxBackgroundImage: '' # 例如: "'https://s3.amazonaws.com/hakim-static/reveal-js/reveal-parallax-1.jpg'"

  # 视差背景尺寸,目前只支持像素单位, 不要使用%或auto
  parallaxBackgroundSize: '' # CSS语法, 例如 "2100px 900px"

  # 每张幻灯片要移动视差背景的像素数
  # - 自动计算, 除非指定
  # - 设置为0以禁止沿某一轴线移动
  parallaxBackgroundHorizontal: 200
  parallaxBackgroundVertical: 50

  # 视差背景图片
  parallaxBackgroundImage: '' # 例如: "https://s3.amazonaws.com/hakim-static/reveal-js/reveal-parallax-1.jpg"

  # 每张幻灯片要移动视差背景的像素数
  # - 除非指定, 否则会自动计算
  # - 设置为0以禁止沿某一轴线移动
  parallaxBackgroundHorizontal: 200
  parallaxBackgroundVertical: 50

  # 启用 Speake Notes
  enableSpeakerNotes: false
---
```

### 自定义幻灯片样式

[CSS系列--前端进阶之路: 初涉Less](https://www.cnblogs.com/landeanfen/p/6047031.html)

你可以添加 `id` 以及 `class` 到一个特定的幻灯片:

```Markdown
<!-- slide id="my-id" class="my-class1 my-class2" -->
```

或者你也可以自定义第 `nth` 个幻灯片, 编写你的 `less` 如下:

```less
.markdown-preview.markdown-preview {
  // 自定义 presentation 样式
  .reveal .slides {
    // 修改所有幻灯片
  }

  // 自定义 presentation 样式
  .slides > section:nth-child(1) {
    // 修改 `第 1 个幻灯片`
  }
}
```

### 输出

`Ctrl+Shift+p` 输入`m p e`可以看到它的大部分配置入口.
按下快捷键`Ctrl+k v` 可以打开渲染预览窗口.

在渲染预览窗口右击, 菜单中选择 `HTML->HTML(offline)`. 
将会输出幻灯片为 `HTML` 格式, 使用浏览器即可播放. 
如果担心在别的电脑上无法正常显示，可以选择 `HTML->HTML(cdn hosted)`
