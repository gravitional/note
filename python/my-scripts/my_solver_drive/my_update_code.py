import subprocess as subp
from my_config import prt_sep
from my_config import cfg_solver_dir, cfg_solver_build_dir, cfg_exe_git, cfg_exe_cmake

# ------------------- 命令参数
_args_git = ['-C', cfg_solver_dir, 'pull']
_exe_git_sub = cfg_exe_git
# 无需担心 命令行转义, 下面使用 shell=False
_args_git_sub = [
    '-C', cfg_solver_dir, 'submodule', 'foreach', '--recursive',
    'echo "========================================";git pull || true'
]

_args_cmake = [
    '-DRELEASE_WITH_DEBUG_INFO=OFF', '-S', cfg_solver_dir, '-B',
    cfg_solver_build_dir
]


def pull_and_cmake():
    prt_sep('Pull from repo: solver\n')
    p = subp.Popen([cfg_exe_git, *_args_git], shell=False)
    p.communicate()

    prt_sep('Sub modules:\n')
    p = subp.Popen([_exe_git_sub, *_args_git_sub], shell=False)
    p.communicate()

    prt_sep('Running CMake\n')
    p = subp.Popen([cfg_exe_cmake, *_args_cmake], shell=False)
    p.communicate()


if __name__ == '__main__':
    pull_and_cmake()
    input('更新 solver 代码成功, 按Enter结束:')
