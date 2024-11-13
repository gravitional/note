# diff2html

[rtfpessoa/diff2html](https://github.com/rtfpessoa/diff2html#distributions)
[diff2html-cli](https://www.npmjs.com/package/diff2html-cli)

## 安装

[npm]: https://docs.npmjs.com/downloading-and-installing-node-js-and-npm

[npm 是干什么的]([Node Package Manager](https://zhuanlan.zhihu.com/p/24357770))

使用 [npm][] 进行安装(Node Package Manager)

```bash
npm install -g diff2html-cli
```

## 用法

```bash
diff2html [ flags and/or options ] -- [git diff passthrough flags and options]
```

flag    别名     描述     可选项     默认

+ `-s`;     `--style`;      输出格式;     `line, side`;     `line`
+ `--sc`;       `--synchronisedScroll`;     同步水平滚动;    `true, false`     `true`
+ `--hc`;       `--highlightCode`;      高亮代码;     `true, false`;     `true`
+ `--su`;       `--summary`;        展示文件总结;     `closed, open, hidden`;     `closed`
+ `-d`;     `--diffStyle`;      Diff 格式;     `word, char`;     `word`
+ `--lm`;       `--matching`;       Diff line 匹配类型;     `lines, words, none`;     `none`
+ `--lmt`;      `--matchWordsThreshold`;        Diff line 匹配词阈值;         `0.25`
+ `--lmm`;      `--matchingMaxComparisons`;     Diff line matching 一个修改块的最大比较行数;    `2500`
+ `--hwt`;      `--htmlWrapperTemplate`;        当使用html输出格式时, 要渲染的自定义模板的路径;     `[string]`
+ `-f`;     `--format`;     输出格式;     `html, json`     `html`
+ `-i`;     `--input`;      Diff 输入源;    `file, command, stdin`;     `command`
+ `-o`;     `--output`;     输出目标;     `preview, stdout`;     `preview`
+ `-u`;     `--diffy`;      Upload to diffy.org;     `browser, pbcopy, print`
+ `-F`;     `--file`;       发送输出到文件(覆盖输出选项);     `[string]`
+ `--ig`;       `--ignore`;     diff 时忽略特定文件;     `[string]`
+ `-v`;     `--version`;        显示版本号
+ `-h`;     `--help`;       显示帮助

### Exit 状态码

+ 🎉 0: 成功
+ 😵 1: 一般错误
+ 😰 3: diff 输入是空的
+ 👮 4: `--hwt | --htmlWrapperTemplate` 的值不是有效的文件

### 自定义HTML wrapper 模板

模板基本上是几个占位符的简单替换 [master/src/cli.ts#L40](https://github.com/rtfpessoa/diff2html-cli/blob/master/src/cli.ts#L40).
要提供一个自定义的模板, 你需要确保你的HTML中有以下 `comments` 和 `imports`, 与这里的完全一致:

+ 在 `<head>` tag 中

    ```html
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.9.0/styles/github.min.css" />
    <!--diff2html-css-->
    <!--diff2html-js-ui-->
    <script>
      document.addEventListener('DOMContentLoaded', () => {
        const targetElement = document.getElementById('diff');
        const diff2htmlUi = new Diff2HtmlUI(targetElement);
        //diff2html-fileListToggle
        //diff2html-synchronisedScroll
        //diff2html-highlightCode
      });
    </script>
    ```

+ 在 <body> tag 中

    ```html
    <div id="diff">
      <!--diff2html-diff-->
    </div>
    ```

## 例子

+ `diff` 最后一次`commit`, 逐行比较, 行与行之间比较单词, 在浏览器中预览, 输入来自 `git diff` 命令;

        diff2html -s line -f html -d word -i command -o preview -- -M HEAD~1

+ 读取文件中的输入;

        diff2html -i file -- my-file-diff.diff

+ 从 `stdin` 读取 `diff`;

        diff -u file1.txt file2.txt | diff2html -i stdin

+ 打印 `json` 格式到 `stdout`;

        diff2html -f json -o stdout -- -M HEAD~1

+ 打印到文件;

        diff2html -F my-pretty-diff.html -- -M HEAD~1

+ 使用自定义 `markup` 模板打印到文件可以包括以下变量

        diff2html -F my-pretty-diff.html --hwt my-custom-template.html -- -M HEAD~1

+ 在生成的 `diff` 中忽略 `package-lock.json` 和 `yarn.lock`.

        diff2html --ig package-lock.json --ig yarn.lock

>注意例子中的 `--`.

## `git diff [<options>] <commit> <commit>…​ <commit> [--] [<path>…​]`

此形式用于查看 merge commit 的结果.
第一个列出的 `<commit>` 必须是合并本身;
其余两个或更多 commit 应是其父级提交.
快捷方式是使用后缀 `^@` 和 `^!`,
如果 A 是 merge commit, 那么 `git diff A A^@`, `git diff A^!`, `git show A`
都给出相同的 combined diff.

## `git diff [<options>] <commit>..<commit> [--] [<path>…​]`

这与之前的形式(不带 `..`)同义,
用于查看两个任意 `<commit>` 之间的改动.
如果省略了一边的 `<commit>`, 效果与使用 `HEAD` 相同. 
例如查看从 `HEAD首父节点` 到 `HEAD` 的更改

```bash
git diff HEAD^..
```

## `git diff [<options>] <commit>...<commit> [--] [<path>…​]`

[gitrevisions[7]]: https://git-scm.com/docs/gitrevisions
[git-merge[1]]: https://git-scm.com/docs/git-merge

此形式用于查看 changes on the branch 至多包含第二个 `<commit>`, 从两个 `<commit>` 的公共祖先开始.

`git diff A...B` 等同于 `git diff $(git merge-base A B) B`.
您可以省略 `<commit>` 中的任何一个, 效果与使用 `HEAD` 相同.

为了以防万一, 需要注意的是, 除了 `--merge-base` 和最后两种使用 `..` 符号的形式外,
上述描述中的所有 `<commit>` 都可以是任何 `<tree>`.
我们感兴趣的是名为 `AUTO_MERGE` 的 `ref` 所指向的树,
它是由 `ort` 合并策略在遇到合并冲突时写入的, 参见 [git-merge[1]][].
将工作树与 `AUTO_MERGE` 进行比较,
可以看到你迄今为止为解决文本冲突所做的改动(见下面的示例).

关于 `<commit>` 的更多拼写方式,
请参阅 [gitrevisions[7]][] 中的 "SPECIFYING REVISIONS" 部分.
不过, `git diff` 是比较两个端点(endpoints), 而不是范围(ranges),
而范围符号(`<commit>...<commit>` 和 `<commit>...<commit>`)
并不意味着 [gitrevisions[7]][] 中 "SPECIFYING RANGES "一节所定义的范围.
