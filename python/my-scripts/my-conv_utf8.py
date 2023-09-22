# -*- coding:utf-8 -*-
from pathlib import Path
import sys
__author__ = 'Thomas'

'''
将 GBK 编码的 solver.log 转换成 utf-8 编码
'''

pattern = 'solver.log'
coding0 = 'gbk'  # 原本的编码
coding1 = 'utf-8'  # 要转换到的编码


def convert(filename, in_enc="GBK", out_enc="UTF8"):
    try:
        filenew = filename+'.bak'
        print(f'convert {filename} to {filenew}')
        with open(filename, 'r', encoding=coding0) as f:
            with open(filenew, 'w', encoding=coding1) as f2:
                while (line := f.readline()):
                    f2.write(line)
        print("done")
    except IOError as e:
        # except:
        print("error")


def explore(dir):
    p = Path(dir)
    # 递归搜索名字为 pattern 的文件
    for txt in list(p.glob(f'**/{pattern}')):
        convert(txt.as_posix())


def main():
    args = sys.argv[1:]
    if (not len(args)):
        explore('.')
    else:
        for path in args:
            if Path(path).is_file():
                convert(path)
            elif Path(path).is_dir():
                explore(path)


if __name__ == "__main__":
    main()
