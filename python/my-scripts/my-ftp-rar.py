from ftplib import FTP
import pathlib as ptl
import subprocess as subp
import re

# ======================== 配置变量
# 求解器 solver/sdk 目录
# solver_sdk_dir = ptl.Path('~/Downloads/ttta').expanduser()
solver_sdk_dir = ptl.Path('C:/Solver/sdk').expanduser()
# winRAR 可执行文件路径
exe_rar = 'C:/Program Files/WinRAR/Rar.exe'
# sdk.rar 的下载目录
down_dir = ptl.Path('~/Downloads/').expanduser()
# ========================= 检查变量
if not solver_sdk_dir.exists():
    raise RuntimeError('目标文件夹不存在')
if not solver_sdk_dir.is_dir():
    raise RuntimeError('目标路径不是文件夹')
solver_sdk_dir = solver_sdk_dir.as_posix()
if not solver_sdk_dir.endswith('\\'):
    solver_sdk_dir += '\\'
print(f'求解器 sdk 目录为 {solver_sdk_dir}')

sdk_download_path: str = ''


def ftp_sdk():
    global sdk_download_path
    # ========================== ftp part
    # 连接到 ftp, 跳转目录
    sdk_ftp = FTP('xxxx')
    sdk_ftp.login('xxx', 'xxx')
    sdk_ftp.cwd('/SDK/SolverSDK/develop/')
    # 获取 solverSDK 列表
    f_list = list(sdk_ftp.mlsd())
    # 按照时间信息排序
    f_list.sort(key=lambda x: x[1]['modify'], reverse=True)
    # 获取最新的 sdk 文件
    sdk_new = ''
    for f in f_list:
        if re.search(r'solversdk_develop_.+\.rar', f[0]):
            sdk_new = f[0]
            break
    print(f'最新的 sovler sdk 为 {sdk_new}')
    # ftp RETR 命令
    retr_cmd = 'RETR ' + sdk_new
    # sdk 下载路径
    sdk_download_path = down_dir.joinpath(sdk_new).as_posix()
    print(f'sdk.rar 下载路径 {sdk_download_path}')
    with open(sdk_download_path, 'wb') as fp:
        sdk_ftp.retrbinary(retr_cmd, fp.write)


def winrar_sdk():
    global sdk_download_path
    # ======================= windows rar part
    args_file = sdk_download_path
    # 获取 sdk.rar 文件的内容
    args_rar_list = ['lb', args_file]
    # 获取 sdk.rar 的 base directory
    file_list_str = subp.check_output([exe_rar, *args_rar_list],
                                      shell=False).decode('utf-8')
    file_list: list[str] = file_list_str.splitlines()
    base_dir: str = ptl.Path(file_list[0]).parts[0]
    base_patten: str = base_dir + '\*'
    # rar 解压缩目录
    args_rar_extract = [
        'x', '-y', args_file, '-ep1', base_patten, solver_sdk_dir
    ]
    # 执行解压缩
    p = subp.Popen([exe_rar, *args_rar_extract], shell=False)
    p.communicate()


if __name__ == '__main__':
    ftp_sdk()
    winrar_sdk()
    input('输入任意字符, 并按Enter结束:')
