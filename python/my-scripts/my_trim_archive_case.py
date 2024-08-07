#-------- 清理 IBE 工程, 移动控制文件, 删除 result文件夹
import argparse as agp
import shutil as shu
import pathlib as ptl
import subprocess as subp
import re
from datetime import datetime as dtt

_exe_7z = r'C:/Program Files/7-Zip/7z.exe'
_cmd_7z = 'a'
_arc_dft_out_name = 'case_' + dtt.now().strftime('%y%m%d_%H%M%S')
#----- 7z 参数, 要排除的文件夹
_exclude = [
    # r"-xr!result",
    r"-xr!cache",
    r"-xr!__pycache__",
    r"-xr!result_backup",
    r"-xr!result - 副本",
]


def prt_sep(istr):
    print('<<<[ymy]: ', istr)


#=================================== delete result 文件夹
#--- 输入 案例文件夹
def delete_result(ipath: ptl.Path):
    # 各种可能的 result 文件夹
    result_dir = sorted(ipath.glob('**/Solving/SolvingDomain/result'))
    result_dir += sorted(ipath.glob('**/result_backup'))
    result_dir += sorted(ipath.glob('**/result - 副本'))
    for ret_dir in result_dir:
        if ret_dir.exists and ret_dir.is_dir():
            prt_sep(f'删除 result 目录: {ret_dir}')
            shu.rmtree(ret_dir)


def delete_none(ipath: ptl.Path):
    prt_sep(f'result 文件夹保留原状: {ipath}')


#===================================  清理 inner ibe 工程目录
# 输入为ibe 对应的目录
def clean_case(ipath: ptl.Path):
    # 删除 ibe 对应的 项目目录
    if ipath.exists() and ipath.is_dir():
        prt_sep(f'IBE冗余目录 已删除: {ipath}')
        shu.rmtree(ipath)
    # 删除多余的 result 文件
    # ip_parent = ipath.parent
    # ip_result = ip_parent / 'result'
    # if ip_result.exists() and ip_result.is_dir():
    #     shu.rmtree(ip_result)


def clean_none(ipath: ptl.Path):
    prt_sep(f'保留 IBE 冗余目录: {ipath}')


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
    parser.add_argument('-C',
                        '--clean',
                        dest='clean',
                        action='store_const',
                        const=clean_case,
                        default=clean_none,
                        help='清理工程目录, 例如上面的 case/Solving/SolvingDomain')
    parser.add_argument(
        '-D',  # 开关参数
        '--delete-result',
        dest='delete_result',
        action='store_const',
        const=delete_result,
        default=delete_none,
        help='清理 result 目录')
    parser.add_argument(
        '-a',  # 开关参数
        '--archive',
        dest='archive',
        action='store_true',  # 存储 True or False,
        help='开关, 是否打包到桌面 .7z 文档')
    parser.add_argument(
        '-M',  # 开关参数
        '--move-model',
        dest='movemodel',
        action='store_true',
        help='开关, 覆盖 abc目录中 已经存在的 control.json 等文件')

    parser.add_argument(
        '-o',  # 指定 .7z
        '--output',
        metavar='name',
        dest='output',
        action='store',
        default=_arc_dft_out_name,
        help='指定输出的 .7z 文档的名称, 不带.7z后缀; 默认自动创建名称')

    args = parser.parse_args()

    #============================================== 合并控制文件到 项目根目录, 清理 result
    # 传入的是 .ibe 所在的父目录
    folder_outter_exist = []  # 输入中存在的目录
    # re_Solving = re.compile(r'Solving', flags=re.I)
    # re_Solving_SolvingDomain = re.compile(r'Solving/SolvingDomain', flags=re.I)
    re_result = re.compile(r'result', flags=re.I)  # regex, result 目录
    for fd in args.folder:
        folder_outter = ptl.Path(fd).expanduser()  # ibe 父目录, 即外层目录名称
        # -------------------------------------- 遍历 所有 ibe 工程
        if folder_outter.exists() and folder_outter.is_dir():
            args.delete_result(folder_outter)  # 删除 result 文件夹
            folder_outter_exist.append(folder_outter.as_posix())
            solving_domains = sorted(
                folder_outter.glob('**/Solving/SolvingDomain'))
            #==================================== 检查 solvingDomain 是否存在, 移动模型输入到 top level
            if len(solving_domains) == 0:
                prt_sep(f'不存在 IBE冗余目录: {folder_outter}')
                continue
            if len(solving_domains) > 1:
                prt_sep(f'存在多个IBE工程目录, 不 move 控制文件: {folder_outter}')
                continue
            else:
                for solving_domain in solving_domains:
                    for item in solving_domain.glob('*'):
                        if re_result.match(item.stem):  # copy 时略过 resultxxx 文件夹
                            continue
                        # 根目录下是否有同名文件, 默认不执行覆盖; 一般是 修改过的 control.json 文件
                        correspong_outter_file = folder_outter / item.name
                        if args.movemodel:
                            shu.copy2(item, folder_outter)
                        elif correspong_outter_file.exists():  # 默认, 不拷贝已经存在的文件
                            # prt_sep(f'Does not copy already exist {ifd_term}')
                            continue
                    #========================================== 清除 ibe 工程目录 Solving; 默认什么也不做
                    if solving_domain.exists():
                        args.clean(solving_domain.parent)
        else:
            raise f'所给路径不存在, 或者不是目录: {folder_outter}'

    #============================ 打包
    if args.archive:  # 如果需要打包到 .7z
        arch_base = ptl.Path('~/Desktop').expanduser()  #打包到桌面
        arch_path: ptl.Path = arch_base / (args.output + '.7z')  # 压缩文件名称
        prt_sep(f'打包到压缩文件: {arch_path}')
        p = subp.Popen(
            [_exe_7z, _cmd_7z, arch_path, *_exclude, *folder_outter_exist],
            shell=False)
        p.communicate()
    else:
        prt_sep('没有给出 `压缩文档` 的名称, 不制作 .7z 了')


if __name__ == '__main__':
    exe()
