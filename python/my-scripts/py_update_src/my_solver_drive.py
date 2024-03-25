from my_config import prt_sep
from my_update_code import pull_and_cmake
from my_clean_cache import clean_cache
from my_ftp_rar import ftp_sdk, winrar_sdk
from my_msbuild_solver import msbuild_solver

if __name__ == '__main__':
    # update solver/sdk
    prt_sep('Now: 删除 solver/sdk 缓存\n')
    clean_cache()
    prt_sep('Success: 删除 solver/sdk 缓存\n')

    prt_sep('Now: 更新 solver/sdk\n')
    ftp_sdk()
    winrar_sdk()
    prt_sep('Success: 更新 solver/sdk')

    # update solver code, then cmake
    prt_sep('Now: 更新 solver 代码仓库\n')
    pull_and_cmake()
    prt_sep('Success: 更新 solver 代码仓库')

    #
    prt_sep('Now: Build solver 代码\n')
    msbuild_solver()
    prt_sep('Success: Build solver 代码')

    #
    prt_sep()
    input('所有操作成功, 按 Enter 结束')
