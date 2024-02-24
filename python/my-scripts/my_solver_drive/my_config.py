import pathlib as ptl

## ======================== 配置变量 of ftp, sdk
# 求解器 solver/sdk 目录
cfg_solver_sdk_dir = ptl.Path('C:/Solver/sdk').expanduser()
cfg_exe_rar = 'C:/Program Files/WinRAR/Rar.exe'  # winRAR.exe 的路径
cfg_down_dir = ptl.Path('~/Downloads/').expanduser()  # sdk.rar 的临时下载目录
# --------------------------- sdk ftp 地址配置
cfg_sdk_ftp_address = 'x'
cfg_sdk_user = 'x'
cfg_sdk_passwd = 'x'
cfg_sdk_remote_dir = '/SDK/SolverSDK/develop/'  # 远程目录

## ======================== 配置变量 of git, cmake
cfg_solver_dir = 'C:/solver'  # solver 代码本地路径
cfg_solver_build_dir = 'C:/solver/build/'  # solver build 路径
cfg_exe_git = 'C:/Program Files/Git/bin/git.exe'  # git.exe 路径
cfg_exe_cmake = 'C:/Program Files/CMake/bin/cmake.exe'  # cmake.exe 路径

## ======================== 配置环境 visual studio, msbuild
# msbuild.exe 的路径
cfg_exe_msbuild = r'C:/Program Files (x86)/Microsoft Visual Studio/2019/Professional/MSBuild/Current/Bin/MSBuild.exe'
# solver.sln 的路径
cfg_sln_solver = r'C:/Solver/build/solver.sln'
# common/Base 项目的路径; for 强制链接, 解决 IBE validator 问题
cfg_sln_common_Base = r'C:/Solver/build/src/common/Common/Base/Base.vcxproj'


## ======================== functions
def prt_sep(istr: str = ''):
    print('<' * 35 + '[ymy] ' + istr, flush=True)
