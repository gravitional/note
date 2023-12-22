import subprocess as proc
from pathlib import Path
from time import strftime, localtime, perf_counter

solver = r'C:/Solver/bin/Release/solver.exe'
solver_args = r"-fp8"
path_lst = [
    # 1127x474mm; tet10; dyna curr; 20s/10s;
    'anodization/anodization/Solving/SolvingDomain',
    # 41378x17396mm; tet10; static curr;
    'case ship/ship/Solving/SolvingDomain',
    # 37227x15651mm; tet10; dyna curr; time: 20s/10s
    'cathodic_protection/cathodic_protection/Solving/SolvingDomain',

    # 60x25mm; tet10; static curr;
    'plating/PCB/PCB1/PCB Cu plating static/Solving/SolvingDomain',
    # 109x46mm; tet10; static curr;
    'plating/PCB/PCB1/PCB Cu plating-shade/Solving/SolvingDomain',

    # 20x8mm; tet10; static curr;
    'plating/PCB/PCB2/PCB2 Cu plating static/Solving/SolvingDomain',
    # 48x20mm; tet10; dyna curr; 180s/60s
    'plating/PCB/PCB3/PCB3 Cu plating static/Solving/SolvingDomain',
    # 1770x707mm; tet10; static curr
    'plating/plating/Plating 3D APP/Solving/SolvingDomain',
    # 1225x725mm; tet10; dyna curr 12000s/4000s
    'plating/XXX/xxxx/Solving/SolvingDomain',

    # 54x23mm; tri6; dyna curr; 5s/1s
    'primary cell 2D/Primary-Cell-2D-1011/Primary Cell-2D/Solving/SolvingDomain',
    # 122x51mm; tet10; dyna curr; 5s/1s
    'primary cell 2D/Primary-Cell-3D/Primary-Cell-3D/Solving/SolvingDomain'
]

cwd = Path(__file__).parent  # 案例目录, 脚本放在案例仓库目录
path_lst_psx = [(cwd / x).as_posix() for x in path_lst]
time_stamp = strftime('%Y-%d-%H-%M-%S', localtime())
logfile = f'ec_test_log_{time_stamp}.log'
logfile = (cwd / logfile).as_posix()
_total_header = '=========== total time ===========\n'


def run_solver():
    with open(logfile, 'w+') as flog:
        t_tot1 = perf_counter()

        for p in path_lst_psx:
            print(f'>> Run solver on {p} ', 'status: ')
            # run solver on path
            flog.write(f"Run case: {p}\n")
            flog.flush()

            t_sub1 = perf_counter()
            proc.run([solver, solver_args, p], check=True)
            t_sub2 = perf_counter()

            log_prt_time(flog, t_sub1, t_sub2)
            flog.flush()

        t_tot2 = perf_counter()

        print(_total_header)
        flog.write(_total_header)

        log_prt_time(flog, t_tot1, t_tot2)
        print("----------------------\n")
        flog.flush()


def copy_solu():
    pass


# log and print time
def log_prt_time(flog, t1: float, t2: float):
    time_str = []
    time_str.append(f'程序运行时间:{(t2-t1)/60.0:>-#.10G} Min')
    time_str.append(f'程序运行时间:{(t2-t1):>-#.10G} Sec')
    for t_str in time_str:
        print(t_str)
        flog.write(t_str + '\n')
    flog.write('\n')


if __name__ == '__main__':
    run_solver()
