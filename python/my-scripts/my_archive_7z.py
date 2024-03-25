import argparse as agp
from datetime import datetime as dtt
import subprocess as subp
import time
import pathlib as ptl
import os

_t1 = time.perf_counter()
_cwd = ptl.Path(__file__).parent
_arc_dft_out_name = 'casesZip_' + dtt.now().strftime('%y%m%d_%H%M%S')
_exe_7z = 'C:/Program Files/7-Zip/7z.exe'
_cmd_7z = 'a'


def prt_sep(istr):
    print('<<<[ymy]: ', istr)


def exe():
    parser = agp.ArgumentParser(
        prog='my_archive_7z',
        description='''将给定的 folders 添加到 xxx.7z压缩文档, 并排除 result 等结果目录''',
        formatter_class=agp.RawDescriptionHelpFormatter,
        epilog='')  # epilog='"是不小心, 还是故意的"')
    parser.add_argument('folder',
                        metavar='folder',
                        type=str,
                        nargs='+',
                        help='要加入压缩包的目录, 可以有多个')
    parser.add_argument(
        '-o',  # 指定 .7z
        '--output',
        metavar='name',
        dest='output',
        action='store',
        default=_arc_dft_out_name,
        help='指定输出的 .7z 文档的名称, 不带.7z后缀; 默认自动创建名称')
    parser.add_argument(
        '-x',  # 开关参数
        '--exclude',
        dest='exclude',
        action='store_true',  # 存储 True or False,
        help='开关, 是否排除 result 等结果目录')
    parser.add_argument(
        '-ep',  # 开关参数
        '--exclude-pattern',
        metavar='name',
        dest='ex_patt',
        type=str,
        nargs='+',
        help='模式列表, 要排除的其他目录的模式')
    args = parser.parse_args()

    # 要加入压缩文档的 目录 或文件 列表
    path_lst = []
    path_lst_str = []
    for fd in args.folder:
        ifd = ptl.Path(fd).expanduser()  # 要压缩的文件目录
        ifd_str = ifd.as_posix()
        path_lst.append(ifd)
        path_lst_str.append(ifd_str)

    arch_base = ptl.Path('~/Desktop').expanduser()  #打包到桌面
    arch_path: ptl.Path = arch_base / (args.output + '.7z')  # 压缩文件名称
    prt_sep(f'打包到压缩文件: {arch_path}')

    if arch_path.exists():
        print('<<<<< remove old archive')
        os.remove(arch_path)
        print('<<<<< remove done')

    # 列表必须有 逗号, 不然会解析成 字符串拼接
    exclude = [
        # r"-xr!result",
        r"-xr!cache",
        r"-xr!__pycache__",
        r"-xr!result_backup",
        r"-xr!result - 副本",
    ]
    # print(exclude)

    print('<<<<< start process 7z a:')
    _t1 = time.perf_counter()
    # subp.run([exe_7z, cmd_7z, out_path, *exclude, *path_lst], check=True)
    p = subp.Popen([_exe_7z, _cmd_7z, arch_path, *exclude, *path_lst_str],
                   shell=False)
    p.communicate(timeout=86400)  # 时间限制为 86400s
    _t2 = time.perf_counter()
    _ttt = (_t2 - _t1) * 1
    print(f'zip time cost: {_ttt} seconds')


if __name__ == '__main__':
    exe()
