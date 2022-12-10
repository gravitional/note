#!/usr/bin/env python3
import re

print('let\'s find out the data! \n>>>>>>>>>')

mesh_data = 'mesh.dat'
tagList = []
tagStart = []
parse = []
parseh = {}

termOrd = 0
with open(mesh_data, 'r') as mesh_fh:
    lnum = 0
    for line in mesh_fh:
        if m := re.search(r'(\w+) +\{', line):  # 若匹配到 xxx {, 例如 node {
            term = f'{m.group(1)}-{termOrd}'  # group(1): 匹配到的正则分组1, 例如 node
            termOrd += 1  # 编号自增
            print(f'find: {term} @ {lnum}')    # lnum 表示读取到的文件中的行号
            tagList.append(term)      # 记录起始标志node, 压入列表顶端
            tagStart.append(lnum)  # 记录起始行号
        if m := re.search(r'\}', line):  # 若匹配到结束符 }
            print(f'''find: `}}'  that end {tagList[-1]} @ {lnum}''')
            # 制作映射表, (node , [开始行数，结束行数])
            parse.append((tagList.pop(), [tagStart.pop(), lnum]))
        lnum += 1

parseh = dict(parse)    # 转换成 python dict

# for 迭代遍历key, 打开 输出Handle
for node, range in parseh.items():
    with open(f'{node}.txt', 'w') as res_fh:  # 存储结果的 txt
        with open(mesh_data, 'r') as mesh_fh:     # 从 mesh.dat 中提取数据
            lnum = 0
            for line in mesh_fh:
                if lnum > range[0] and lnum < range[1]:
                    line = re.sub(r'[\[\],]', '', line)  # 删除多余的字符
                    line = re.sub(r'^\s+', '', line)  # 删除多余的空白
                    res_fh.write(line)  # 输出结果到对应的 txt
                lnum += 1
