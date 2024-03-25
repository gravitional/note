# 提取 solver.log 中的 error output

import re
import glob
import pathlib as ptl

solver_log = '**/solver.log'
ret_log = 'mesh_error.txt'
stat_log = '_mesh_error.py'

cwd = ptl.Path(__file__).parent
cwd_str = cwd.as_posix()
ret_log = (cwd / ret_log).as_posix()

cat1 = re.compile(
    r'^Need adaptive mesh refinemet at error (\d+\.\d+).+(\d+\.\d+)')
cat2 = re.compile(r'.+Adaptive mesh refinement: node change from.+')
cat3 = re.compile(r'.+Adaptive mesh refinement: cell change from.+')


# 统计数据
def match_stat(line: str):
    return cat1.match(line)


def match_others(line):
    return cat2.match(line) or cat3.match(line)


sv_logs = glob.iglob(pathname=solver_log,
                     root_dir=cwd_str,
                     dir_fd=None,
                     recursive=True)


def grep_error():
    fh_stat = open(stat_log, 'w')
    refine_step_lst = [0]  # 细化步数列表
    case_numbers: int = 0  # 案例数目
    fh_stat.write('''import numpy as np
global_aver_errors=np.array([
''')
    fh_ret = open(ret_log, 'w')
    fh_ret.write('# mesh compare log\n')

    refine_step: int = 0
    for sv in sv_logs:
        case_numbers += 1
        sv_path = ptl.Path(sv)
        sv_prt = sv_path.parent.name
        with open(sv, 'r') as fh:
            fh_ret.write(f'\n## {sv_prt}\n\n')
            for line in fh:
                if ret := match_stat(line):
                    refine_step += 1
                    errs = ret.groups()
                    errs = tuple(map(float, errs))
                    fh_stat.write(f'{errs},\n')
                    fh_ret.write(line)
                elif match_others(line):
                    fh_ret.write(line)
            refine_step_lst.append(refine_step)

    fh_stat.write('],dtype=float)\n\n')
    fh_stat.write(f'''case_numbers:int={case_numbers}
refine_step_lst:int={refine_step_lst}\n''')

    fh_stat.close()
    fh_ret.close()


if __name__ == '__main__':
    grep_error()
