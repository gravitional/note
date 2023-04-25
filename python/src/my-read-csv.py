import numpy as np
import csv

# 分隔符默认是空格，或者连续空格 delimiter=None,
def readData1(name: str, cols, skip=0, /, dtype=float):
    with open(name, newline='') as csvfile:
        mdata = np.loadtxt(csvfile, dtype=dtype,
                           delimiter=None, usecols=cols, skiprows=skip)
        return mdata


def readData2(name: str, cols, skip=0, /, dtype=float):
    with open(name, newline='') as csvfile:
        mdata = np.genfromtxt(csvfile, dtype=dtype,
                              usecols=cols, skip_header=skip)
        return mdata


def readData3(name: str):
    with open(name, newline='') as csvfile:
        spamreader = csv.reader(csvfile, delimiter=' ',
                                skipinitialspace=True, quotechar='|')
        for row in spamreader:
            print(', '.join(row))
