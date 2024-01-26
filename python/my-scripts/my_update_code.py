import subprocess as subp
import pathlib as plb


def prt_sep(istr: str = ''):
    print('<' * 35 + '[ymy] ' + istr, flush=True)


# ----------------- exe 路径
# 求解器路径
_solver_dir = 'C:/solver'
_solver_build_dir = 'C:/solver/build/'
# git 路径
_exe_git = 'C:/Program Files/Git/bin/git.exe'
# cmake 路径
_exe_cmake = 'C:/Program Files/CMake/bin/cmake.exe'

# ------------------- 命令参数
_args_git = ['-C', _solver_dir, 'pull']
_exe_git_sub = _exe_git
# 无需担心 命令行转义, 下面使用 shell=False
_args_git_sub = [
    '-C', _solver_dir, 'submodule', 'foreach', '--recursive',
    'echo "========================================";git pull || true'
]

_args_cmake = [
    '-DRELEASE_WITH_DEBUG_INFO=OFF', '-S', _solver_dir, '-B', _solver_build_dir
]


def pull_and_cmake():
    prt_sep('Pull from repo: solver\n')
    p = subp.Popen([_exe_git, *_args_git], shell=False)
    p.communicate()

    prt_sep('Sub modules:\n')
    p = subp.Popen([_exe_git_sub, *_args_git_sub], shell=False)
    p.communicate()

    prt_sep('Running CMake\n')
    p = subp.Popen([_exe_cmake, *_args_cmake], shell=False)
    p.communicate()


if __name__ == '__main__':
    pull_and_cmake()
    input('更新 solver 代码成功, 按Enter结束:')
