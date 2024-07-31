# 批量运行 solver.exe on 工程案例; 工程案例路径 相对于 本文件所在目录
import subprocess as subp
import pathlib as ptl
from time import strftime, localtime, perf_counter, sleep
#--- 求解器路径
solver = r'C:/Solver/bin/Release/solver.exe'
solver_args = r"-tdep8"
#--- 案例目录的名称
# 2D 案例, wyc
path_lst0 = [
    'case0-staticEF-2D-plating-coarse',
    'case0-wyc-staticEF-2D-square',
]
# 2D 案例, EF
path_lst1 = [
    'case3-staticEF-2D-PyFunDisp3-wide',
    'case3-staticEF-2D-PyFunVel3',
    'case3-staticEF-2D-Swing',
    'case3-staticEF-2D-Swing-L2',
    'case3-staticEF-2D-Swing-L3',
    'case3-staticEF-2D-Trans',
    'case3-staticEF-2D-Trans-joint',
    'case3-staticEF-2D-ring',
    'case3-staticEF-2D-ring-L2',
]
# 3D 案例, EF
path_lst2 = [
    'case5-staticEF-3D-RotVar',
    'case5-staticEF-3D-RotVar-fine',
    'case5-staticEF-3D-Trans',
]
# 案例, MF
path_lst3 = [
    'case4-staticMF-2D-ring',
    'case4-staticMF-2D-ring-cube',
    'case4-staticMF-2D-ring-excitation',
    'case4-staticMF-2D-ring-wide',
    'case4-staticMF-motion-2D-ring',
]
#------ merge
path_lst = [
    *path_lst0,
    *path_lst1,
    *path_lst2,
    *path_lst3,
]
# 本文件的父目录
cwd = ptl.Path(__file__).parent
path_lst_psx = [(cwd / x).as_posix() for x in path_lst]
#--- 求解时间记录
time_stamp = strftime('%Y-%d-%H-%M-%S', localtime())
logfile = 'my_solver_log.txt'
logfile_with_time = f'my_solver_log_{time_stamp}.log'
logfile = (cwd / logfile).as_posix()


def run_solver():
    flog = open(logfile, 'w+')
    t_tot1 = perf_counter()
    t_sleep: float = 0.0
    for ip in path_lst_psx:
        # run solver on path
        log_msg = f'>> Run solver on {ip}'
        print(log_msg, ' status: ')
        flog.write(log_msg + '\n')
        t_sub1 = perf_counter()
        p = subp.Popen([solver, solver_args, ip], shell=False)
        try:
            outs, errs = p.communicate(timeout=86400)
        except subp.TimeoutExpired:
            p.kill()
            outs, errs = p.communicate()
        #------- return code branch
        ret_code = p.returncode
        if ret_code == 0:
            msg = f'Success with return code {ret_code}'
            print(msg)
            flog.write(f'<< {msg}\n')
        else:
            msg = f'Failed with return code {ret_code}'
            print(msg)
            flog.write(f'<< {msg}\n')
        t_sub2 = perf_counter()
        log_prt_time(flog, t_sub1, t_sub2)
        flog.flush()
        sleep(0)  # 防止 由于 ip guard 卡住
        t_sleep += 0
    t_tot2 = perf_counter()
    t_tot2 -= t_sleep
    flog.write('=========== total time of this test ===========\n')
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
