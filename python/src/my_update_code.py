import subprocess as subp
import pathlib as plb

# ----------------- exe 路径
# 求解器路径
solver = 'C:/solver'
solver_build = 'C:/solver/src/build/'
# git 路径
cmd2 = 'C:/Program Files/Git/bin/git.exe'
# cmake 路径
cmd4 = 'C:/Program Files/CMake/bin/cmake.exe'

# ------------------- 命令参数
arg2 = ['-C', solver, 'pull']
cmd3 = cmd2
# 无需担心 命令行转义, 下面使用 shell=False
arg3 = ['-C', solver, 'submodule', 'foreach', '--recursive',
        'echo "========================================";git pull || true']


arg4 = ['-DRELEASE_WITH_DEBUG_INFO=OFF',
        '-S', solver, '-B', solver_build]


def init():
    print('================================= pull from repo solver repo\n')
    p = subp.Popen([cmd2, *arg2], shell=False)
    p.communicate()

    print('\n================================= sub modules:\n')
    p = subp.Popen([cmd3, *arg3], shell=False)
    p.communicate()

    print('\n================================= Running Cmake\n')
    p = subp.Popen([cmd4, *arg4], shell=False)
    p.communicate()


if __name__ == '__main__':
    init()
