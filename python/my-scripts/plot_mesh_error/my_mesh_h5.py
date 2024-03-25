#!/usr/bin/env python3
"""
从 mesh.h5 文件中, 读取 node, cell, boundary 信息
"""
import h5py
import numpy as np

mesh_data = 'mesh.h5'
# 输出文件名称
fs_node_py = '_nodes.py'
fs_Vele_py = '_Velements.py'
fs_Sele_py = '_Selements.py'

# 节点路径
p_node_coord = '/Mesh/Node/Coord'
#=========================== volume 单元路径
p_cell = '/Mesh/Cell'
# 单元类型, 例如 203, 表示 2维, 3个节点
p_cell_type = '/Mesh/Cell/Type'
# volume单元对应的 node, 以map的形式给出, {index, value}
p_cell_node_idx = '/Mesh/Cell/Nodes.Index'
p_cell_node_val = '/Mesh/Cell/Nodes.Value'
#=========================== surf 单元路径
p_Scell = '/Mesh/Surface'
p_Scell_type = '/Mesh/Surface/Type'
# surf 单元对应的 node, 以map的形式给出, {index, value}
p_Scell_node_idx = '/Mesh/Surface/Nodes.Index'
p_Scell_node_val = '/Mesh/Surface/Nodes.Value'
# surf 的 owner
p_Scell_owner = '/Mesh/Surface/Owner'
# surf 在 owner 中的局域编号
p_Scell_owner_face_idx = '/Mesh/Surface/OwnerFaceIndex'
dim3: int = 3  #节点坐标始终按 x,y,z 存储, 即使是二维案例

print('parsing mesh data...\n>>>>>>>>>')


def print_name(name):
    print(name)


def prepare_mesh():
    with h5py.File(mesh_data, "r") as f_mesh:
        # f.visit(print_name)
        #---------------------------- node 坐标列表
        node_coord = f_mesh[p_node_coord]
        crd_sz: int = dim3
        ilen = node_coord.shape[0] // crd_sz  # node list 长度
        crd_arr = np.zeros((ilen, crd_sz + 1), dtype=float)  # 格式为 {编号, x,y,z}
        for i in range(ilen):
            crd_arr[i, 0] = i  # 节点编号, 0-based
            crd_arr[i, 1:] = node_coord[crd_sz * i:crd_sz * i + crd_sz]  # 节点坐标

        ele_arr_str = [
            '(' + ','.join([f'{x:.12g}'
                            for x in line]) + '),\n'
            for line in crd_arr
        ]
        with open(fs_node_py, 'w') as f_ele:
            f_ele.write(f'''# 编号, x, y, z 坐标
import numpy as np
in_nodes_coord=np.array([
''')
            f_ele.writelines(ele_arr_str)
            f_ele.write('],dtype=float)\n')

        #------------------------- volume cell list
        cell_type = f_mesh[p_cell_type]
        type1: int = cell_type[0]  # 单元类型, 例如四面体是 304
        cell_sz: int = type1 % 100  # 单元的节点数
        # 体单元的 构成节点表 {idx->val}
        cell_node_idx = f_mesh[p_cell_node_idx]
        cell_node_val = f_mesh[p_cell_node_val]
        # cell list 长度; index == cell list 长度+1
        ilen = cell_node_idx.shape[0] - 1
        ele_arr = np.zeros((ilen, cell_sz + 1), dtype=int)  # 格式为 {编号, 构成节点ID}
        for i in range(ilen):
            ele_arr[i, 0] = i  # 编号
            ele_nd_ia = cell_node_idx[i]
            ele_nd_ib = cell_node_idx[i + 1]
            ele_arr[i, 1:] = cell_node_val[ele_nd_ia:ele_nd_ib]  # ele的构成节点编号
        # 写出到文件
        ele_arr_str = [
            '(' + ','.join([f'{x:d}'
                            for x in line]) + '),\n'
            for line in ele_arr
        ]
        with open(fs_Vele_py, 'w') as f_ele:
            f_ele.write(f'''# 编号, 构成node的ID
import numpy as np
in_Velements=np.array([
''')
            f_ele.writelines(ele_arr_str)
            f_ele.write('],dtype=int)\n')

        #------------------------- surface cell list
        cell_type = f_mesh[p_Scell_type]
        type1: int = cell_type[0]  # 单元类型, 例如 线段是 102
        cell_sz: int = type1 % 100  # 单元的节点数
        # 体单元的 构成节点表 {idx->val}
        cell_node_idx = f_mesh[p_Scell_node_idx]
        cell_node_val = f_mesh[p_Scell_node_val]
        cell_node_own = f_mesh[p_Scell_owner]
        cell_node_own_face_idx = f_mesh[p_Scell_owner_face_idx]
        # cell list 长度; index == cell list 长度+1
        ilen = cell_node_idx.shape[0] - 1
        # 格式为 {编号,own,loc idx, 构成节点ID}
        Sele_arr = np.zeros((ilen, cell_sz + 3), dtype=int)
        for i in range(ilen):
            Sele_arr[i, 0] = i
            Sele_arr[i, 1] = cell_node_own[i]
            Sele_arr[i, 2] = cell_node_own_face_idx[i]
            ele_nd_ia = cell_node_idx[i]
            ele_nd_ib = cell_node_idx[i + 1]
            Sele_arr[i, 3:] = cell_node_val[ele_nd_ia:ele_nd_ib]
        # 写出到文件
        Sele_arr_str = [
            '(' + ','.join([f'{x:d}'
                            for x in line]) + '),\n'
            for line in Sele_arr
        ]
        with open(fs_Sele_py, 'w') as f_ele:
            f_ele.write(f'''# 编号, own, local index, 构成node的ID
import numpy as np
in_Selements=np.array([
''')
            f_ele.writelines(Sele_arr_str)
            f_ele.write('],dtype=int)\n')

        print(f'node: {node_coord.shape}')
        print(f'volume element: {ele_arr.shape}')
        print(f'surface element: {Sele_arr.shape}')

    # ============================================== 合并
    # # 打开当前目录下的result.txt文件，如果没有则创建
    # fs_mesh = open(fs_mesh, 'w', encoding='utf8')
    # s_names = [fs_node, fs_ele]
    # # 向文件中写入字符; 先遍历文件名
    # for s in s_names:
    #     # 遍历单个文件，读取行数
    #     for line in open(s, encoding='utf8'):
    #         fs_mesh.writelines(line)
    # # 关闭文件
    # fs_mesh.close()


if __name__ == '__main__':
    prepare_mesh()
