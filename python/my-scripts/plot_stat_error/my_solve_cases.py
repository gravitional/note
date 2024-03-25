# 批量运行 solver.exe on 工程案例; 工程案例路径 相对于 本文件所在目录
import subprocess as subp
import pathlib as ptl
from time import strftime, localtime, perf_counter
#--- 求解器路径
solver = r'C:/Solver/bin/Release/solver.exe'
solver_args = r"-tdep8"
#--- 案例目录的名称
path_lst = [
    'case1-staticEF-mesh1-0.0001', 'case1-staticEF-mesh1-0.0002',
    'case1-staticEF-mesh1-0.0003', 'case1-staticEF-mesh1-0.0004',
    'case1-staticEF-mesh1-0.0005'
]
cwd = ptl.Path(__file__).parent  # 本文件的父目录
path_lst_psx = [(cwd / x).as_posix() for x in path_lst]
#--- 求解时间记录
time_stamp = strftime('%Y-%d-%H-%M-%S', localtime())
logfile = 'AMREF_test_log.txt'
logfile_with_time = f'AMREF_test_log_{time_stamp}.log'
logfile = (cwd / logfile).as_posix()


def run_solver():
    flog = open(logfile, 'w+')
    t_tot1 = perf_counter()
    for ip in path_lst_psx:
        # run solver on path
        log_msg = f'>> Run solver on {ip}'
        print(log_msg, ' status: ')
        flog.write(log_msg + '\n')
        t_sub1 = perf_counter()
        p = subp.Popen([solver, solver_args, ip], shell=False)
        p.communicate(timeout=86400)
        t_sub2 = perf_counter()
        log_prt_time(flog, t_sub1, t_sub2)
        flog.flush()
    t_tot2 = perf_counter()
    flog.write('=========== total time ===========' + '\n')
    log_prt_time(flog, t_tot1, t_tot2)
    flog.close()


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
    flog.write('\n')


if __name__ == '__main__':
    run_solver()
