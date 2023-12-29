import subprocess as proc
from pathlib import Path
from time import strftime, localtime, perf_counter

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
    'case4-中文路径-case4',
    'case5',
    'case5b',
    'case7',
    'case8',
    'case9',
    'case10',
    'case11',
    'case12',
    'case13',
    'case14-timer'  # 分段步长
]

cwd = Path(__file__).parent  # 案例目录, 脚本放在案例仓库目录
path_lst_psx = [(cwd / x).as_posix() for x in path_lst]
time_stamp = strftime('%Y-%d-%H-%M-%S', localtime())
logfile = f'ec_test_log_{time_stamp}.log'
logfile = (cwd / logfile).as_posix()


def run_solver():
    with open(logfile, 'w+') as flog:
        t_tot1 = perf_counter()
        for p in path_lst_psx:
            print(f'>> Run solver on {p} ', 'status: ')
            # run solver on path
            t_sub1 = perf_counter()
            proc.run([solver, solver_args, p], check=True)
            t_sub2 = perf_counter()
            log_prt_time(flog, t_sub1, t_sub2)
            flog.flush()
        t_tot2 = perf_counter()
        flog.write('=========== total time ===========' + '\n')
        log_prt_time(flog, t_tot1, t_tot2)


def copy_solu():
    pass


# log and print time
def log_prt_time(flog, t1: float, t2: float):
    time_str = []
    time_str.append(f'程序运行时间:{(t2-t1)/60.0} Min')
    time_str.append(f'程序运行时间:{(t2-t1)} Sec')
    for t_str in time_str:
        print(t_str)
        flog.write(t_str + '\n')


if __name__ == '__main__':
    run_solver()
