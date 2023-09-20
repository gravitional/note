import subprocess as proc
from pathlib import Path

solver = r'C:/Solver/bin/Release/solver.exe'
solver_args = r"-tdep8"
path_lst = [
    'case4b',  # post convert fail
    'case6',  # post convert fail
    'case4c',  # post convert fail

    'case2',
    'case3',

    'case4-2d',
    'case4-3d',

    'case5',
    'case5b',

    'case7',
    'case8',
    'case9',
    'case10',
]

cwd = Path(__file__).parent  # 案例目录, 脚本放在案例仓库目录
path_lst_psx = [(cwd/x).as_posix() for x in path_lst]


def run_solver():
    for p in path_lst_psx:
        print(f'>> Run solver on {p} ', 'status: ')
        proc.run([solver, solver_args, p], check=True)


def copy_solu():
    pass


if __name__ == '__main__':
    run_solver()
