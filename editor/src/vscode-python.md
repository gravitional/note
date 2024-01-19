# vscode python 调试

[python解释器设置](https://code.visualstudio.com/docs/python/debugging#_python)

## python

指向调试所用 Python 解释器的完整路径.
如果未指定, 默认情况下会使用为工作区选择的解释器,
相当于使用 `${command:python.interpreterPath}` 的值.
要使用其他解释器, 请在调试配置的 `python` 属性中指定其路径.

或者, 也可以使用在每个平台上定义的自定义环境变量,
其中包含要使用的 Python 解释器的完整路径, 这样就不需要其他文件夹路径了.
如果需要向 `Python` 解释器传递参数, 可以使用 `pythonArgs` 属性.

例子如下:

```json
// python run script
        {
            "name": "Python_debug",
            "type": "debugpy",
            "request": "launch",
            "program": "${file}",
            "console": "integratedTerminal",
            "stopOnEntry": false,
            "cwd": "${fileDirname}",
            "python": "C:/Users/yd/AppData/Local/Programs/Python/Python311/python.exe"
        }
```
