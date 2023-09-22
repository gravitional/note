import subprocess as proc
from pathlib import Path

post_input = r"postConverter-auto.json"
post_cvt_exe = r'C:/Solver/bin/Release/postconverter.exe'
path_lst = [
    'case4b',  # fail
    'case6',  # fail
    'case4c',  # fail

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
post_input_psx = Path(cwd/post_input).as_posix()
path_lst_psx = [(cwd/p).as_posix() for p in path_lst]


def gen_json(case_path):
    f_str = f'''{{
    "Input": {{
        "Mesh": "{case_path}/mesh.h5",
        "Result": "{case_path}/result/result.h5"
    }},
    "Output": {{
        "Format": "Cache",
        "File": "{case_path}/result/result.cache.h5"
    }}
}}
'''
    with open(post_input_psx, 'w') as file:
        file.writelines(f_str)


def runTest():
    for p in path_lst_psx:
        print(f'>> Run post converter on {p} ', 'status: ')
        gen_json(p)
        proc.run([post_cvt_exe, post_input_psx], check=True)


if __name__ == '__main__':
    runTest()
