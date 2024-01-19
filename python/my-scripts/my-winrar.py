from ftplib import FTP
import pathlib as ptl
import subprocess as subp

iftp = FTP('172.30.100.34')
# login after securing control channel
# ftps.auth()
iftp.login('autotest', 'autotest')
# print(ftps.pwd())
# ftps.retrlines('LIST')
iftp.cwd('/CI/Simdroid/develop/')
iftp.dir()
with open('develop_Release.zip', 'wb') as fp:
    iftp.retrbinary('RETR develop_Release.zip', fp.write)

# 解压文件
down_dir = ptl.Path('~/Downloads/ttaa/').expanduser()

dest_dir = ptl.Path('~/Downloads/ttaa/').expanduser()
print(dest_dir)
if not dest_dir.exists():
    raise RuntimeError('目标文件夹不存在')
if not dest_dir.is_dir():
    raise RuntimeError('目标路径不是文件夹')
dest_dir = dest_dir.as_posix()
if not dest_dir.endswith('\\'):
    dest_dir += '\\'
print(f'dest_dir is {dest_dir}')
#
#
args_file = down_dir.joinpath('develop.rar').as_posix()
#
exe_rar = 'C:\Program Files\WinRAR\Rar.exe'
# 打印文件内容
args_rar_list = ['lb', args_file]

#
# find base directory in rar file
file_list_str = subp.check_output([exe_rar, *args_rar_list],
                                  shell=False).decode('utf-8')
file_list: list[str] = file_list_str.splitlines()
base_dir: str = ptl.Path(file_list[0]).parts[0]
base_patten: str = base_dir + r'\*'
args_rar_extract = ['x', '-y', args_file, '-ep1', base_patten, dest_dir]
#
p = subp.Popen([exe_rar, *args_rar_extract], shell=False)
p.communicate()
