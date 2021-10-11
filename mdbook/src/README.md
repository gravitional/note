# 介绍

`mdBook` 是一个`命令行工具`, 也是一个 `Rust crate`, 可以用 `Markdown` 创建书籍.
它的输出类似于 `Gitbook` 这样的工具, 是创建 `产品` 或 `API文档`, `教程`, `课程材料`
或任何需要 `简洁`,  `易于浏览` 和 可定制的 `演示文稿`的理想选择.
`mdBook` 是用 `Rust` 编写的; 它的性能和简单性使它成为通过自动化直接发布到托管网站(如GitHub Pages)的理想工具.
事实上, 本指南既是 `mdBook` 的文档, 也是 `mdBook` 产生的一个很好的例子.

`mdBook`包括内置的对 `Markdown` 的预处理, 和产生 `HTML` 以外的其他格式的渲染器的支持.
这些设施还可以实现其他功能, 如`验证`. 搜索 `Rust` 的 `crates.io` 是一个发现更多扩展的好方法.

## API文档

除了上述功能外, mdBook还有一个 `Rust API`.
这使得你可以编写你自己的预处理器或渲染器, 以及将 `mdBook` 的功能纳入其他应用程序.
本指南的 `For Developers` 部分包含更多的信息和一些例子.

## Markdown

`mdBook`的 `解析器` 遵循 [CommonMark规范](https://commonmark.org/).
你可以参加一个快速教程, 或者实时试用CommonMark. 如果想获得更深入的体验, 请查看Markdown指南.

## 贡献

`mdBook`是免费和开源的. 你可以在 [GitHub](https://github.com/rust-lang/mdBook)上找到源代码, 问题和功能请求可以在GitHub问题跟踪器上发布.
`mdBook`依靠社区来修复错误和增加功能: 如果你想贡献, 请阅读贡献指南并考虑打开一个拉动请求.

## 许可证

`mdBook`的源代码和文档是在 `Mozilla Public License v2.0` 下发布的.
