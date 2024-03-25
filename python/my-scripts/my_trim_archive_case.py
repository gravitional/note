import argparse as agp
import shutil as shu
import pathlib as ptl
import subprocess as subp
import re
from datetime import datetime as dtt

_exe_7z = r'C:/Program Files/7-Zip/7z.exe'
_args_7z = ['a']
_arc_dft_out_name = 'case_' + dtt.now().strftime('%y%m%d_%H%M%S')


def prt_sep(istr):
    print('<<<[ymy]: ', istr)


# 输入为ibe 对应的目录
def clean_case(ipath: ptl.Path):
    # 删除 ibe 对应的 项目目录
    if ipath.exists() and ipath.is_dir():
        prt_sep(f'以下目录被处理的很干净: {ipath}')
        shu.rmtree(ipath)
    # 删除多余的 result 文件
    # ip_parent = ipath.parent
    # ip_result = ip_parent / 'result'
    # if ip_result.exists() and ip_result.is_dir():
    #     shu.rmtree(ip_result)


def clean_none(ipath: ptl.Path):
    prt_sep(f'以下目录将保留一点原始的味道: {ipath}')


def exe():
    parser = agp.ArgumentParser(prog='my_trim_archive_case',
                                description='''例如某个IBE工程在目录 abc 中:
abc
 |-- case.ibe
 |-- case
      |-- Solving/SolvingDomain/control.json
          ....
将 {control.json, mesh.h5} 等文件移动到 abc 下面,
并清除 Solving/SolvingDomain 等冗余目录''',
                                formatter_class=agp.RawDescriptionHelpFormatter,
                                epilog='')  # epilog='"是不小心, 还是故意的"')

    parser.add_argument('folder',
                        metavar='folder',
                        type=str,
                        nargs='+',
                        help='要精简的目录, xxx.ibe 所在的父目录, 可以有多个')
    parser.add_argument('-c',
                        '--clean',
                        dest='clean',
                        action='store_const',
                        const=clean_case,
                        default=clean_none,
                        help='清理工程目录, 例如上面的 case/Solving/SolvingDomain')
    parser.add_argument(
        '-a',  # 开关参数
        '--archive',
        dest='archive',
        action='store_true',  # 存储 True or False,
        help='开关, 是否打包到桌面 .7z 文档')

    parser.add_argument(
        '-w',  # 开关参数
        '--overwrite',
        dest='overwrite',
        action='store_true',  # 存储 True or False,
        help='开关, 是否覆盖 abc目录中 已经存在的 control.json 等文件')

    parser.add_argument(
        '-o',  # 指定 .7z
        '--output',
        metavar='name',
        dest='output',
        action='store',
        default=_arc_dft_out_name,
        help='指定输出的 .7z 文档的名称, 不带.7z后缀; 默认自动创建名称')

    args = parser.parse_args()

    #============================ 合并控制文件到 项目根目录, 清理 result
    # 传入的是 .ibe 所在的父目录
    folder_exist = []  # 输入中存在的目录
    for fd in args.folder:
        ifd = ptl.Path(fd).expanduser()  # ibe 父目录
        ifd_stem = ifd.stem  # 父目录的 stem 名称
        ibe_folder = ''  # .ibe 对应的工程目录
        if ifd.exists() and ifd.is_dir():
            folder_exist.append(ifd.as_posix())
            ibes = sorted(ifd.glob('*.ibe'))  # ibe 文件名称
            ibe_stem = None
            if ibes:  # 如果列表不为空
                ibe_stem = ibes[0].stem  # ibe 的 stem 名称
            ibe_folder = ibe_stem if ibe_stem else ifd_stem
        else:
            raise f'所给路径不存在, 或者不是目录: {ifd}'
        ibe_folder_path = ifd / ibe_folder
        dir_solving_domain = ibe_folder_path / 'Solving' / 'SolvingDomain/'
        dir_solving = ibe_folder_path / 'Solving'
        if dir_solving_domain.exists():  # 检查 solvingDomain 是否存在
            for sd_p in dir_solving_domain.glob('*'):
                re_result = re.compile(r'result', flags=re.I)
                if re_result.match(sd_p.stem):  # 排除 resultxxx 文件夹
                    continue
                # 根目录下是否有同名文件, 默认不执行覆盖; 一般是 修改过的 control.json 文件
                ifd_term = ifd / sd_p.name
                if args.overwrite:
                    shu.copy2(sd_p, ifd)
                elif ifd_term.exists():  # 默认, 不拷贝已经存在的文件
                    # prt_sep(f'Does not copy already exist {ifd_term}')
                    continue
            # 清除 ibe 工程目录; 默认什么也不做
            args.clean(ibe_folder_path)
        else:
            prt_sep(f'处理的很干净: {ifd}')

    #============================ 打包
    if args.archive:  # 如果需要打包到 .7z
        arch_base = ptl.Path('~/Desktop').expanduser()  #打包到桌面
        arch_path: ptl.Path = arch_base / (args.output + '.7z')  # 压缩文件名称
        prt_sep(f'打包到压缩文件: {arch_path}')
        _args_7z.append(arch_path.as_posix())
        _args_7z.extend(folder_exist)  # 存在的 项目路径
        p = subp.Popen([_exe_7z, *_args_7z], shell=False)
        p.communicate()
    else:
        prt_sep('没有给出 `压缩文档` 的名称, 不制作 .7z 了')


if __name__ == '__main__':
    exe()
