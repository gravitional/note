# perl tidy 使用

[perltidy - a perl script indenter and reformatter](https://metacpan.org/dist/Perl-Tidy/view/bin/
perltidy)
[在vim中使用perltidy美化perl代码](https://www.cnblogs.com/itech/archive/2013/02/18/2915279.html)

```bash
// "-q", "-et=4", "-t", "-ce", "-l=0", "-bar", "-naws", "-blbs=2", "-mbl=2"
// "--profile=${env:UserProfile}.perltidyrc",
//-gnu, // GNU Coding Standards
//-pbp, // --perl-best-practices
//-i=2, // Use 2 columns per indentation level
//-ci=2, // Continuation indentation 在长行断开时使用的额外缩进空格.
//-ce, // else 和 elsif 紧跟在关闭上一个代码块的大括号之后
//-nbl, // 将开头括号与引入该括号的关键字放在同一行
//-bt=2, // braces not tight
//-sbt=2, // square brackets not tight
//-pt=2, // Tightness of curly braces, parentheses, and square brackets.
//-nsfs, // --space-for-semicolon
//-nsak=s, // --nospace-after-keyword=s removes keywords.
//-dws, // --delete-old-whitespace
```

使用 `-b` 作原地修改, `-bext='/'` 不做备份

## vscode json 预定义变量, magic 变量

[Debugging current file in VS Code](https://stackoverflow.com/questions/38419779/debugging-current-file-in-vs-code)
[$HOME folder on visual studio code debugger](https://stackoverflow.com/questions/42114267/how-to-specify-home-folder-on-visual-studio-code-debugger)

```bash
${env:HOME}
${env:UserProfile}
```
