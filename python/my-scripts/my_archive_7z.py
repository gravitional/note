import subprocess as subp
import time
import pathlib as ptl
import os

_t1 = time.perf_counter()
cwd = ptl.Path(__file__).parent
path_lst = [
    'case4b',  # post convert fail
    'case6',  # post convert fail
    'case4c',  # post convert fail
    'case2',
    'case3',
    'case4-2d',
    'case4-3d',
    'case4-中文路径-case4',
    'case5',
    'case5b',
    'case7',
    'case8',
    'case9',
    'case10',
    'case11',
    'case12',
    'case13',
    'case14-timer'  # 分段步长
]

path_lst = [cwd / ptl.Path(x) for x in path_lst]

out_path = 'Runable.7z'
out_path = cwd / ptl.Path(out_path)
if out_path.exists():
    print('<<<<< remove old archive')
    os.remove(out_path)
    print('<<<<< remove done')

exe_7z = 'C:/Program Files/7-Zip/7z.exe'
cmd_7z = 'a'
# 列表必须有 逗号, 不然会解析成 字符串拼接
exclude = [
    r"-xr!result",
    r"-xr!cache",
    r"-xr!__pycache__",
    r"-xr!result_backup",
    r"-xr!result - 副本",
]
# print(exclude)


def my_7zip():
    print('<<<<< start process 7z a:')
    _t1 = time.perf_counter()
    # subp.run([exe_7z, cmd_7z, out_path, *exclude, *path_lst], check=True)
    p = subp.Popen([exe_7z, cmd_7z, out_path, *exclude, *path_lst], shell=False)
    p.communicate()
    _t2 = time.perf_counter()
    _ttt = (_t2 - _t1) * 1
    print(f'zip time cost: {_ttt} seconds')


if __name__ == '__main__':
    my_7zip()
