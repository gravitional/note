import subprocess as subp
import time
import pathlib as plb

_t1 = time.perf_counter()

cwd = plb.Path(__file__).parent
name_lst = [
    'case2',
    'case3',

    'case4-2d',
    'case4-3d',
    'case4b',
    'case4c',

    'case5',
    'case5b',
    'case6',

    'case7',
    'case8',
    'case9',
    'case10',
]
path_lst = [cwd/plb.Path(x) for x in name_lst]

out_path = 'Runable.7z'
out_path = cwd/plb.Path(out_path)

exe_7z = 'C:/Program Files/7-Zip/7z.exe'
cmd_7z = 'a'
# 列表必须有 逗号, 不然会解析成 字符串拼接
exclude = [
    r"-xr!result",
    r"-xr!__pycache__",
    r"-xr!result_backup",
    r"-xr!result - 副本",
]
# print(exclude)


def my_7zip():
    _t1 = time.perf_counter()
    subp.run([exe_7z, cmd_7z, out_path, *exclude, *path_lst], check=True)
    _t2 = time.perf_counter()
    _ttt = (_t2-_t1)*1
    print(f'zip time cost: {_ttt} seconds')


if __name__ == '__main__':
    my_7zip()
