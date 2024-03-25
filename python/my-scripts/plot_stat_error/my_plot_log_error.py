# 根据生成的 _mesh_error.py 文件, 画出 误差 -- 网格细化次数 的折线图

from matplotlib import pyplot as plt
import numpy as np
from _mesh_error import global_aver_errors, case_numbers, refine_step_lst
from my_conf import Count

cnt = Count()

#------ 初始网格的参数
xlst = list(range(len(refine_step_lst)))
mesh_label = list(range(case_numbers))
mesh_label_str = list(map(lambda x: f'mesh {x}', mesh_label))
arr_shape = (refine_step_lst, case_numbers)

# 误差; 例如 6 个细化step(包括初始步); 5 种 初始网格
err_global = global_aver_errors[:, 0]
err_average = global_aver_errors[:, 1]

fig, axs = plt.subplots(2, 1)
# plot y_data 的每一列数据
for ii in range(case_numbers):
    icnt = cnt()
    ref_a = refine_step_lst[ii]
    ref_b = refine_step_lst[ii + 1]
    err = err_global[ref_a:ref_b]
    # 总误差
    xlst = list(range(len(err)))
    axs[0].plot(xlst, err, label=f'case {icnt+1}')
    # 平均误差
    err = err_average[ref_a:ref_b]
    axs[1].plot(xlst, err, label=f'case {icnt+1}')

axs[0].set_title('global error')
axs[0].legend(handlelength=4)
axs[1].legend(handlelength=4)
axs[1].set_title('average error')
# plt.tight_layout()
plt.show()
