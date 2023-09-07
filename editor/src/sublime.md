# sublime 使用

## intro

[sublime Introduction](https://docs.sublimetext.io/guide/#history)

## font face

[Font Settings](https://www.sublimetext.com/docs/font.html)

在 "偏好设置" -> "设置 "的右侧窗格中添加以下几行, 即可更改字体:

```json
"font_face":  "Courier New",
"font_size":  10
```

然后根据需要进行更改. 保存时字体将发生变化.
浏览首选项 -> 设置的左侧窗格, 你还可以看到其他需要更改的设置.

## sublime commands

[How to list all commands in Sublime Text 3](https://stackoverflow.com/questions/48656430/how-to-list-all-commands-in-sublime-text-3)

[社区文档](https://docs.sublimetext.io/),
尤其是[命令列表部分](https://docs.sublimetext.io/reference/commands.html)
提供了一份相当完整的 Sublime 核心命令列表.
不过, 这并不能帮助你了解第三方软件包和插件可能添加的命令.

在你的问题中, 你提到知道如何获取某个命令,
但不知道在其他地方使用该命令时可能会用到什么.
如果你知道如何调用某个命令(按键, 命令调板, 菜单),
但不知道该命令是什么, Sublime 可以帮你解决这个问题.

如果使用 `` Ctrl+` `` 或 `视图`>`显示控制台` 打开 Sublime 控制台, 可以输入以下命令:

```python
sublime.log_commands(True)
```

现在, 每当你执行任何操作时, Sublime 都会在控制台中记录正在执行的命令,
以及该命令可能包含的任何参数.
例如, 如果你打开日志记录并依次按下每个方向键, 控制台就会显示如下内容:

```python
command: move {"by": "lines", "forward": false}
command: move {"by": "lines", "forward": true}
command: move {"by": "characters", "forward": false}
command: move {"by": "characters", "forward": true}
```

使用该工具, 您可以找出各种操作所执行的命令, 以便在其他地方使用它们.
这也是一种方便的诊断方法, 如果 键盘快捷键 没有按照预期工作.
用 `False` 而不是 `True` 调用相同的命令(或重启 Sublime), 即可关闭日志记录.

如果你对每条可能的命令的内部细节都很感兴趣, 那么类似下面这样的命令也是可行的.
它实现了一个名为 `list_all_commands` 的命令, 运行该命令后,
所有类型的可用命令都会被列到一个新的 scratch buffer 中.

请注意, 并非所有实现的命令都是供外部使用的; 插件有时会定义自己使用的 辅助命令.
这意味着, 虽然这里会告诉你所有存在的命令, 但并不意味着所有这些命令都是供你使用的.

此外, 虽然这里列出了 `command` 类的 `run` 方法(Sublime 执行命令时会执行该方法)所需的大致参数,
但有些命令的参数列表可能并不明确.

```python
import sublime
import sublime_plugin

import inspect

from sublime_plugin import application_command_classes
from sublime_plugin import window_command_classes
from sublime_plugin import text_command_classes

class ListAllCommandsCommand(sublime_plugin.WindowCommand):
    def run(self):
        self.view = self.window.new_file()
        self.view.set_scratch(True)
        self.view.set_name("Command List")

        self.list_category("Application Commands", application_command_classes)
        self.list_category("Window Commands", window_command_classes)
        self.list_category("Text Commands", text_command_classes)

    def append(self, line):
        self.view.run_command("append", {"characters": line + "\n"})

    def list_category(self, title, command_list):
        self.append(title)
        self.append(len(title)*"=")

        for command in command_list:
            self.append("{cmd} {args}".format(
                cmd=self.get_name(command),
                args=str(inspect.signature(command.run))))

        self.append("")

    def get_name(self, cls):
        clsname = cls.__name__
        name = clsname[0].lower()
        last_upper = False
        for c in clsname[1:]:
            if c.isupper() and not last_upper:
                name += '_'
                name += c.lower()
            else:
                name += c
            last_upper = c.isupper()
        if name.endswith("_command"):
            name = name[0:-8]
        return name
```
