# perl 安装包自带说明

什么是Strawberry Perl?

+ `Perl` 是一种编程语言, 适用于编写简单的脚本和复杂的应用程序, 以及复杂的应用程序.
见  [perlintro](http://perldoc.perl.org/perlintro.html)

+ `Strawberry Perl` 是一个适用于微软Windows 的perl环境,
它包含了你运行和开发perl应用程序所需的一切.
你需要运行和开发perl应用程序. 它被设计成尽可能接近UNIX系统上的perl环境.
见 [strawberryperl](http://strawberryperl.com/)

+ 如果你对perl完全陌生, 可以考虑访问 [learnperl](http://learn.perl.org/)

## 安装说明

只对于 `.ZIP` 分发, `.MSI`安装程序 无需这么复杂

+ 如果从 `.zip` 文件中安装这个版本,
你必须将其解压到一个 目录, 其中不能有空格 - 例如 `c:\myperl\`
然后运行一些命令并手动设置一些环境变量.

```cmd
c:\myperl\relocation.pl.bat ...这是必须的!
c:\myperl\update_env.pl.bat ... 这是可选的
```

你可以在 `update_env.pl.bat` 后面指定 `--nosystem`,
它只为当前用户安装 Strawberry  Perl的环境变量.

+ 如果固定的安装路径不适合你,
可以试试 [Strawberry Perl  便携版](http://strawberryperl.com/releases.html)

## 如何使用 Strawberry Perl?

+ 在命令提示符窗口中, 你可以运行任何perl脚本

    ```cmd
    c:\> perl c:\path\to\script.pl
    ```

+ 通过以下方式从http://www.cpan.org/ 安装额外的perl模块(库):

    ```cmd
     c:\> cpan Module::Name
    ```

+ 运行包括在 `Strawberry Perl` 中的其他工具, 如: `perldoc`, `gcc`, `gmake` ...

你需要文本编辑器来创建perl脚本.
编辑器并不包括在 Strawberry Perl 中. 有几个选择:

+ Padre(可以通过运行  "cpan Padre "来安装)
+ 和 [Notepad3](https://sourceforge.net/projects/notepad3/),

这两个软件都包括语法高亮功能语法高亮的perl脚本. 如果你愿意, 你甚至可以使用记事本.
