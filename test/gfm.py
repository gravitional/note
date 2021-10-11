# -*- coding: utf-8 -*-
"""
使用通配符，获取所有文件，或进行操作。
"""

__author__ = '飞鸽传说, modified by thomas'

import glob,os,shutil

# 给出当前目录下的文件的generator
def files(curr_dir = '.', ext = '*.aux'): # 指定默认变量，目录和拓展名
    """给出当前目录下的文件"""
    for i in glob.glob(os.path.join(curr_dir, ext)):
        yield i

def all_files(rootdir, ext):
    """给出当前目录下以及子目录的文件"""
    for name in os.listdir(rootdir):
        if os.path.isdir(os.path.join(rootdir, name)):#判断是否为文件夹
            try:
                for sub_file in all_files(os.path.join(rootdir, name), ext):
                    # 递归地给出所有子文件夹中的文件
                    yield sub_file
            except:
                pass
    for cwd_file in files(rootdir, ext):
        yield cwd_file

def dirs(rootdir, ext):
    """给出所有文件夹"""
    for name in os.listdir(rootdir):
        if os.path.isdir(os.path.join(rootdir, name)):#判断是否为文件夹
            yield name

# 默认不显示
def rm(rootdir, ext, show = False):
    """删除rootdir目录下的符合的文件"""
    for cwd_file in files(rootdir, ext):
        if show:
            print(cwd_file)
        if os.path.isdir(cwd_file):#判断是否为文件夹
            shutil.rmtree(cwd_file)
        elif  os.path.isfile(cwd_file):
            os.remove(cwd_file)

# 默认不显示
def rma(rootdir, ext, show = False):
    """删除rootdir目录下以及子目录下符合的文件"""
    for rec_file in all_files(rootdir, ext):
        if show:
            print(rec_file)
        if os.path.isdir(rec_file):#判断是否为文件夹
            shutil.rmtree(rec_file)
        elif  os.path.isfile(rec_file):
            os.remove(rec_file)

# os.rmdir(path) 删除path指定的空目录，如果目录非空，则抛出一个OSError异常。
# os.remove(path) 删除路径为path的文件。如果path 是一个文件夹，将抛出OSError;
# os.removedirs(path) 递归删除目录。如果非空，将抛出OSError;
# shutil.rmtree(目录路径) `shutil.rmtree(目录路径)` #移除整个目录，无论是否空

# 如果在命令行运行，Python解释器把一个特殊变量__name__置为__main__
#if __name__ == '__main__':
    #这一行删除预设的文件
#    rma('.', '*.aux', show = True)
    # remove_all_files('.', '*.exe', show = True)
#    rm('.', '*.aux', show = True)
    # for i in files('.','*.c'):
        # print i