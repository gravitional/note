#!/usr/bin/python
# -*- coding: UTF-8 -*-

import h5py
import numpy as np


def main():
    # ===========================================================================
    # 本地保存文件的名称
    file_name: str = "result.h5"
    # Read HDF5 file.
    f = h5py.File(file_name, "r")  # mode = {'w', 'r', 'value'}

    # Print the keys of groups and datasets under '/'.
    print(f.filename, ":")
    print([key for key in f.keys()], "\n")

    print("iter through the {}\n".format(file_name))
    my_iter(file_name, f, 0)
    # Save and exit the file
    f.close()


# 递归函数,  遍历所有节点
def my_iter(name_str, d, count):
    if dataq(d):  # 如果是 dataset 类型
        print("{}{} is Dataset:".format("--" * count, name_str))  # 打印当前节点名称
        print("  " * count, d[:])  # 打印数据详情
        if len(d.attrs.keys()) > 0:
            print("{}{} has attributes:".format("--" * (count + 1), name_str))
            for key in d.attrs.keys():  # 打印属性
                print("**" * (count + 1), key, ":", d.attrs[key])
        else:
            print("{}{} has No attributes".format("**" * (count + 1), name_str))
        print()
    elif hasattr(d, "keys") and len(d.keys()) > 0:  # 如果是 group 类型递归调用
        print("\n{}{} is Group, it has:".format("<<" * count, name_str))
        for key in d.keys():
            my_iter("{}/{}".format(d.name, key), d[key], count + 1)
    else:
        print("\n{}{} is Group, it has Nothing".format("<<" * count, name_str))


def dataq(data):
    return hasattr(data, "size")


if __name__ == "__main__":
    main()
