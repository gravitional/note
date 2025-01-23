# juypter lab

[jupyterlab-cli](https://nocomplexity.com/documents/jupyterlab/notebooks/jupyterlab-cli.html)

`--notebook-dir=<Unicode>`; The directory to use for notebooks and kernels.
`--preferred-dir`; Preferred starting directory to use for notebooks and kernels.

```bash
#Windows Example
jupyter lab --notebook-dir=E:/ --preferred-dir E:/Documents/Somewhere/Else
#Linux Example
jupyter lab --notebook-dir=/var/ --preferred-dir /var/www/html/example-app/
```

## notebook vscode

vscode 运行 notebook 需要安装 ipykernel 包,

```bash
python -m pip install ipykernel -U --user
```

`A` 在上方插入cell
`B` 在下方插入cell

`M` 将cell类型改成 Markdown
`Y` 将cell类型改成 code

## lab 配置文件

[Jupyter’s Common Configuration Approach](https://docs.jupyter.org/en/latest/use/config.html)

要生成 cofig 文件, 运行

```bash
jupyter {application} --generate-config # 例如
jupyter-lab --generate-config
```

生成的文件其名为 `jupyter_application_config.py`.

书写配置可以使用 python 语法, 例如

```python
import pathlib as path

_user_root_dir = path.Path('~').expanduser()
_jupyter_test_dir = _user_root_dir / 'jupyterTest/'

c = get_config()  #noqa
# 配置启动目录
c.FileContentsManager.preferred_dir = _jupyter_test_dir.as_posix()
# 配置 lab 的根目录, 也就是限制 lab 的访问范围
c.ServerApp.root_dir = _user_root_dir.as_posix()
```