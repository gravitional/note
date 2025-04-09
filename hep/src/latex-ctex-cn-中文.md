# latex 中文, cn, ctex

## 设置中文字体

在 LaTeX 的 ctex 宏包中, 可以通过以下几种方式设置中文字体:

1. 使用 `\setCJKmainfont`, `\setCJKsansfont` 和 `\setCJKmonofont` 命令
    这些命令分别用于设置正文, 无衬线和等宽字体. 例如:
    ```latex
    \setCJKmainfont{SimSun}[BoldFont=SimHei, ItalicFont=KaiTi] % 设置宋体为正文字体, 粗体为黑体, 斜体为楷体
    \setCJKsansfont{Microsoft YaHei}[BoldFont={* Bold}] % 设置微软雅黑为无衬线字体, 粗体为微软雅黑粗体
    \setCJKmonofont{FangSong} % 设置仿宋为等宽字体
    ```

2. 使用 fontset 选项
    ctex 宏包提供了 fontset 选项, 可以快速切换预定义的字体配置. 例如:
    ```latex
    \documentclass[fontset=windows]{ctexart} % 使用 Windows 默认字体配置
    ```

    常见的 fontset 选项包括:
    windows: 使用 Windows 默认字体(宋体, 黑体等).
    linux: 使用 Linux 默认字体(如 Fandol 字体).
    founder: 使用方正字体.

3. 自定义字体配置
    如果需要完全自定义字体, 可以使用 fontset=none 选项, 然后手动设置字体. 例如:
    ```latex
    \documentclass[fontset=none]{ctexart}
    \setCJKmainfont{FZShuSong-Z01}[BoldFont={Source Han Serif SC SemiBold}, ItalicFont=FZKai-Z03]
    \setCJKsansfont{FZHei-B01}[BoldFont={Source Han Sans SC Medium}]
    \setCJKmonofont{FZFangSong-Z02}
    ```

1. 使用 \ctexset 命令
    可以通过 \ctexset 命令在文档类中设置字体. 例如:
    ```latex
    \documentclass{ctexart}
    \ctexset{fontset=adobe}
    ```

### 注意事项

确保系统中已安装所需的字体.
使用 XeLaTeX 或 LuaLaTeX 编译引擎, 因为它们支持系统字体.
如果需要使用 pdfLaTeX, 可以考虑使用其他宏包(如 xeCJK 或 luatexja).
通过以上方法, 可以灵活地设置 LaTeX 文档中的中文字体.

## 在 Windows 上查看字体名称, 在 LaTeX 中使用

1. 通过字体文件夹查看
打开"此电脑", 定位到 C:\Windows\Fonts 文件夹, 这里保存着系统所有的字体文件.
在字体文件夹中, 右键点击字体文件, 选择"属性", 在弹出的窗口中可以看到"字体名称".

2. 通过字体设置查看
按 Windows + S 打开搜索框, 输入 字体设置, 然后选择 字体设置.
在"字体"设置页面中, 浏览所有已安装的字体, 点击你想要的字体, 记下字体的确切名称.

3. 使用 CMD 命令查看
在管理员身份下运行以下命令:
```bash
fc-list :lang=zh-cn > C:\font_zh-cn.txt
```

运行后, 可以在 C 盘下找到一个名为 font_zh-cn.txt 的文本文件, 其中包含了所有中文字体的名称.

4. 使用字体查看器
打开"字体"应用, 找到你想要查看的字体, 右键点击选择"属性", 在弹出的窗口中查看"字体名称".

### 注意事项

在 LaTeX 中使用字体时, 字体名称必须与系统中显示的名称一致.
如果字体名称包含空格或特殊字符, 需要用大括号 `{}` 包裹.
如果需要使用非系统自带字体, 确保字体已安装到 `C:\Windows\Fonts` 文件夹中.
通过以上方法, 你可以轻松找到并使用 Windows 系统中的字体名称,
以便在 LaTeX 中进行设置.