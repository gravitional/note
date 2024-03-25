import os
import glob
import pathlib as ptl
# ==================== 导入配置变量
from my_config import prt_sep
from my_config import cfg_down_dir, cfg_save_last_files, cfg_sdk_pattern


# ------------ 删除旧的文件夹
def clean_cache():
    cache_sdks = glob.iglob(cfg_sdk_pattern,
                            root_dir=cfg_down_dir,
                            dir_fd=None,
                            recursive=False)
    sdk_lst = []
    for f in cache_sdks:
        ipath = ptl.Path(cfg_down_dir, f)
        entry = (os.stat(ipath, dir_fd=None).st_ctime, ipath)
        # print(entry)
        sdk_lst.append(entry)
    sdk_lst.sort(key=lambda x: x[0], reverse=True)
    # print(sdk_lst)
    if len(sdk_lst) > cfg_save_last_files:
        sdk_lst = sdk_lst[cfg_save_last_files:]
        prt_sep("以下文件将被删除:")
        for f in sdk_lst:
            print(f[1])
            os.remove(f[1], dir_fd=None)
    elif len(sdk_lst) > 0:
        prt_sep("当前缓存文件:")
        for f in sdk_lst:
            print(f[1])
    else:
        prt_sep("当前无缓存文件.")


if __name__ == "__main__":
    clean_cache()
