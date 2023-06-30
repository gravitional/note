# LaTeX 代码展示

[Latex最漂亮高亮代码制造方法](https://blog.csdn.net/weixin_50295745/article/details/124662167)
[LaTeX笔记 | 代码环境之 listings 宏包](https://zhuanlan.zhihu.com/p/346099539)
[不一样的 LaTeX 教程: 使用 listings 宏包美化代码](https://zhuanlan.zhihu.com/p/464141424)

```latex
lstlisting 环境设置
环境设置参数:

\lstset{
 basicstyle=\small\ttfamily, % 基本样式
  keywordstyle=\color{blue}, % 关键词样式
  commentstyle=\color{gray!50!black!50},    % 注释样式
  stringstyle=\rmfamily\slshape\color{red},  % 字符串样式
 backgroundcolor=\color{gray!5},     % 代码块背景颜色
 frame=leftline,      % 代码框形状
 framerule=12pt,%
  rulecolor=\color{gray!90},      % 代码框颜色
 numbers=left,    % 左侧显示行号往左靠, 还可以为right , 或none, 即不加行号
  numberstyle=\footnotesize\itshape, % 行号的样式
  firstnumber=1,
  stepnumber=1,                   % 若设置为2, 则显示行号为1,3,5
  numbersep=7pt,                % 行号与代码之间的间距
 aboveskip=.25em,    % 代码块边框
 showspaces=false,                % 显示添加特定下划线的空格
 showstringspaces=false,          % 不显示代码字符串中间的空格标记
 keepspaces=true,
 showtabs=false,                  % 在字符串中显示制表符
 tabsize=2,                       % 默认缩进2个字符
 captionpos=b,                    % 将标题位置设置为底部
 flexiblecolumns=true,    %
 breaklines=true,                 % 设置自动断行
 breakatwhitespace=false,         % 设置自动中断是否只发生在空格处
 breakautoindent=true,   %
 breakindent=1em,    %
 title=\lstname,    %
 escapeinside=``,     % 在``里显示中文
 xleftmargin=1em,  xrightmargin=1em,     % 设定listing左右的空白
 aboveskip=1ex, belowskip=1ex,
 framextopmargin=1pt, framexbottommargin=1pt,
        abovecaptionskip=-2pt,belowcaptionskip=3pt,
 % 设定中文冲突, 断行, 列模式, 数学环境输入, listing数字的样式
 extendedchars=false, columns=flexible, mathescape=true,
 texcl=true,
 fontadjust
}%
```
