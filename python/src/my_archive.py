# import zipfile as zpf
import pathlib as ptl
import shutil as shu
import time

# 目标压缩文件的路径
archive_name = ptl.Path('~/test/myarchive').expanduser().as_posix()
# 源文件的路径
source_dir = ptl.Path('~/.ssh').expanduser()
# 压缩文件格式: zip, tar, gztar, bztar,  xztar
zip_format = 'xztar'

# 源文件的 parent 目录
root_dir = source_dir.parent.as_posix()
# 源文件 目录名称
base_dir = source_dir.stem
print(f'compressing {base_dir} under {root_dir}')
_t1 = time.perf_counter()
shu.make_archive(archive_name, zip_format,
                 root_dir=root_dir, base_dir=base_dir)
_t2 = time.perf_counter()
print(f'time cost by compressing(s): {_t2-_t1}')
