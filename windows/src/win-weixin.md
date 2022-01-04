# 公众号排版

"小可爱" 微信不支持 `SM.MS` 这种图床,可能是读取速度有点慢就直接拒绝了.码的纸张.

转自: [Typora公众号写作与排版 ](https://sspai.com/post/40524#!)

利用 `Markdown` 排版公众号文章. 所需工具如下:

+ `Typora`. [typora 编辑器](https://www.typora.io/)
+ `PicGo` 图床 app [ PicGo](https://picgo.github.io/PicGo-Doc/zh/guide/)
+ `CSS`的基本知识, 稍微了解即可.

>`CSS`, 层叠样式表(英文全称: Cascading Style Sheets)是一种用来表现 `HTML` (准通用标记语言的一个应用)或 `XML`(准通用标记语言的一个子集)等文件样式的计算机语言.
>`CSS` 不仅可以静态地修饰网页, 还可以配合各种脚本语言动态地对网页各元素进行格式化. [MDN: 什么是CSS](https://developer.mozilla.org/zh-CN/docs/Learn/CSS/First_steps/What_is_CSS)

可以解决以下问题:

+ 图片上传: 在编辑器中写好文章, 上传平台的时候还得手动粘贴
+ `Markdown` 编辑器中的文章上传至公众号之前, 还得各种转码.

做好相关配置后, 写作和排版能够同步完成, 直接粘贴至公众号编辑器中即可.

排版的基本思路是:

网页上的`内容`和`排版`是分开的, 内容编辑好以后, 再使用 `CSS` 样式文件完成字号, 行间距, 背景, 颜色等排版.
所以说, 配置好 `CSS` 文件就相当于一劳永逸的完成了排版工作, 下次只要套用 `CSS` 就好了.

我们使用`Typora`编辑器, [让 Markdown 写作更简单, 免费极简编辑器: Typora](https://sspai.com/post/30292)

+ `Typora` 的编辑逻辑是`所见即所得`, 输入 `Markdown` 标记后, 会根据选择的 `主题` 即时渲染成相应格式.
+ `主题` 是使用 `CSS` 文档定义的, 只要修改 `CSS` 文档中的对应参数, 即可修改主题的样式.
+ 所以本质上, `Typora` 就是一个 `HTML/CSS` 的渲染器.
+ 在`Typora`中, 通过按下`Ctrl+/`, 或者`视图->源代码模式`, 可以在渲染视图和源代码视图之间切换.

![所见即所得](https://cdn.sspai.com/2017/08/20/882f2dc69f0223330d31a09a64313c0e.gif)

借助`Typora`, 我们可以非常方便的完成排版:

+ 在 `Markdown` 中写作, 自动上传本地图片到图床;
+ 应用调整好的`CSS` 格式, 粘贴`富文本`格式到微信中.

## 插件配置

+ 使用 `Typora` 编写 `Markdown` , 通过配置`图床`插件, 自动将本地图片转成在线图片.
+ 先安装合适的`Typora`插件.  英文好的同学可以查看 [Typora 的官方参考](https://support.typora.io/Upload-Image/#configuration) :
    + `mac` 用户: [iPic + Typora, 方便快捷地在 Markdown 中插图](https://sspai.com/post/36275).
    + `windows` 用户: 安装 [PicGo app](https://picgo.github.io/PicGo-Doc/zh/guide/), 下载地址在 [PicGo.Github](https://github.com/Molunerfinn/PicGo/releases), 打开链接, 按下`Ctrl+F` 搜索`x64.exe` 安装包.如果网络原因不能访问`Github`, 可以下载 [网盘备份](https://www.aliyundrive.com/s/kSzsKeQRHB5).
    + 下载之后,选择你喜欢的路径安装, 运行. 点击侧栏中的`SM.MS图床`, 下一步 `设定Token`.
+ 注册图床账号, 例如 [sm.ms](https://sm.ms/). 这一步我不是很熟悉,也许图床不注册也能用.
这里以 `sm.ms` 为例, 注册, 验证邮箱之后, 网站右上角点击 `User->Dashboard`, -> 侧边栏选择 `API Token`, 点击 `Generate Secret Token-> 确定`, 文本框中会生成一段乱码,复制到上一步 `PicGo` 的输入栏里.
+ 测试一下: 在 `PicGo` 侧栏选择`上传区`. 随便拖张图片, 到 `PicGo` 的上传提示框, 应该会自动复制 `图片链接` 到 `剪贴板` . 上传框下面的`链接格式`可以更改生成链接的格式, 我们先保持默认的`Markdown` 就好.
把生成的图片链接直接复制到`Typora`编辑界面中, 看能否预览.
图片链接的`Markdown`格式应该类似于:

        ![](https://i.loli.net/2021/10/03/Lz3nNVyDjrGfXkb.png)

+ 在`Typora`的`文件->偏好设置->侧栏:图像->上传服务设定`中设置:

        上传服务 ->PicGo(app), PicGo 路径-> 你的安装路径.

这里有一个`验证图片选项`, 我验证失败了,但是不影响使用, 暂且不管.
现在使用`Typora`编辑`Markdown`的时候,直接把图片拖放到`Typora`中,它会自动上传图片.

我们需要做的只是找到 `CSS` 文件的目录, 修改目标格式, 写好文章然后复制粘贴.
使用`文件->偏好设置->侧边栏:外观->打开主题文件夹`, 即可打开 `CSS` 文件目录.

![a](https://i.loli.net/2021/10/03/y59ekXcoV4fIpt1.png)
![b](https://i.loli.net/2021/10/03/Lz3nNVyDjrGfXkb.png)

参考[sspai老哥](https://sspai.com/post/40524#!),
>在目录中预设了很多主题, 我个人比较喜欢 `Github` 的样式, 所以我的排版样式是基于 `Github` 修改的.
(好吧, 其实是我的水平太低, 让我重写一个 `CSS` 还不如去死)
> 我在目录内新建了名为`WeChat`的 `CSS` 文件. >在电脑上我还是倾向于使用原生 `GitHub` 主题, 在公众号文章中才会选择自定义的格式.`GitHub` 原来的样式已经不错了, 只是在手机端浏览时, 行距, 页边距, 字号等不太合适.
> 另外, 我修改了部分颜色, 看起来不是那么单调.

复制`github.css -> wechat.css`, 然后根据个人喜好调整:

```css
/* 修改正文部分, 页边距为 0.5em, 行高增加至 1.5em. p 表示段落 , 参考
https://developer.mozilla.org/zh-CN/docs/Learn/Getting_started_with_the_web/CSS_basics */
p {
    margin: 0.8em 0.5em;
    line-height: 1.5em;
}
/* 修改标题及引用部分的边线颜色 */
h2 {
   padding-bottom: .3em;
    font-size: 1.5em;
    line-height: 1.225;
    border-bottom: 1px solid #FFBF00;
    text-align: center
}
blockquote {
    border-left: 4px solid #FFBF00;
    padding: 0 15px;
    color: #777777;
}
```

[wechat.css 示例 网盘链接](https://www.aliyundrive.com/s/fsGbvVLZ1zH)

此外, 在公众号文章页面, 按下`F12`进入开发者模式,可以查看相关的`CSS` 样式表

![CSS style sheet](https://i.loli.net/2021/10/03/8jObYiqfgodmxeB.png)

## 查看修改后的样式

`Typora` 可以自行选择用于渲染的 `CSS` 文件, 在电脑写作时, 我会选择`Github`,
在发布前, 我会选择`菜单栏:主题->WeChat`, 然后粘贴到公众号编辑器中.

修改前和修改后的样式如下:

![GithubTheme](https://i.loli.net/2021/10/03/64aM9yRwGPXpE7O.png)

![WeChatTheme](https://i.loli.net/2021/10/03/voMjHNIh48OP7D9.png)

文章写完, 选择好想要的样式, `Ctrl+A,Ctrl+C` 复制到公众号编辑器中就 `OK` 了!
`Typora` 中使用的排版样式, 会完整的复制到公众号文章中, 真正的所见即所得.

![最终样式](https://i.loli.net/2021/10/03/Xkpfw3EGFIsVUtm.png)

## 其他参考

[使用 Markdown + CSS 搞定公众号的排版规范 ](https://sspai.com/post/59091)
[Markdown + CSS 实现微信公众号排版](https://cloud.tencent.com/developer/article/1051711)
[用CSS样式为微信公众号排版](https://zhuanlan.zhihu.com/p/23809384)

[wechat-mp-article](https://github.com/ufologist/wechat-mp-article)
[中文网页重设与排版.css](https://github.com/sofish/typo.css)
[可配置的,更适合阅读的中文文章样式库 ](https://github.com/zmmbreeze/Entry.css)

`typora` 官方安装配置命令行版 `PicGo` 教程.

[Install PicGo-Core via npm](https://support.typora.io/Upload-Image/#image-uploaders)

通过 `node` 包管理器安装 `PicGo-Core`(需要 `NodeJS`运行时).
如果你安装了 `node` 或 `yarn`,你可以在终端运行以下命令.

```bash
npm install picgo -g
# 或
yarn global add picgo
```

然后你可以在终端输入 `which picgo` 来获得它的实际安装位置,
然后选择 `自定义命令` 作为 `图片上传器` 功能,并输入`[ node.js 的路径] [picgo-core路径] upload` 作为命令.
如果安装了 `node` 和 `picgo` 到系统`PATH`,你也可以直接填写 `picgo upload` 作为自定义命令.

## 配置PicGo-Core

+ 选项1: 编辑配置文件.  请在以下位置编辑 `config.json`

+ Linux / macOS ->  `~/.picgo/config.json`.
+ Windows -> `C:\Users\[您的用户名]\.picgo\config.json`.

细节可以参考, [配置文件](https://picgo.github.io/PicGo-Core-Doc/zh/guide/config.html) (Chinese Only).

`picgo` 需要配置文件来启动. 当你未指定配置文件的时候, `picgo` 将会使用默认配置文件来启动.

通常来说你只需要配置 `Uploader` 即可,所以你可以通过 `picgo set uploader` 来进入交互式命令行,
配置成功后会自动生成配置文件,无需复制粘贴! 其他更多的命令可以参考 `CLI` 命令 一章.
