import subprocess as subp
import pathlib as plb

# ----------------- exe 路径
# 求解器路径
solver_dir = 'C:/solver'
solver_build_dir = 'C:/solver/build/'
# git 路径
exe_git = 'C:/Program Files/Git/bin/git.exe'
# cmake 路径
exe_cmake = 'C:/Program Files/CMake/bin/cmake.exe'

# ------------------- 命令参数
args_git = ['-C', solver_dir, 'pull']
exe_git_sub = exe_git
# 无需担心 命令行转义, 下面使用 shell=False
args_git_sub = [
    '-C', solver_dir, 'submodule', 'foreach', '--recursive',
    'echo "========================================";git pull || true'
]

args_cmake = [
    '-DRELEASE_WITH_DEBUG_INFO=OFF', '-S', solver_dir, '-B', solver_build_dir
]


def init():
    print('================================= pull from repo: solver\n')
    p = subp.Popen([exe_git, *args_git], shell=False)
    p.communicate()

    print('\n================================= sub modules:\n')
    p = subp.Popen([exe_git_sub, *args_git_sub], shell=False)
    p.communicate()

    print('\n================================= running CMake\n')
    p = subp.Popen([exe_cmake, *args_cmake], shell=False)
    p.communicate()


if __name__ == '__main__':
    init()
    input('输入任意字符, 并按Enter结束:')
