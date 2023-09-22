def c3MRO(cls):
    if cls is object:
        # 讨论假设顶层基类为object, 递归终止
        return [object]

    # 构造C3-MRO算法的总式, 递归开始
    mergeList = [c3MRO(baseCls) for baseCls in cls.__bases__]
    mergeList.append(list(cls.__bases__))
    mro = [cls] + merge(mergeList)
    return mro


def merge(inLists):
    if not inLists:
        # 若合并的内容为空, 返回空list
        # 配合下文的排除空list操作, 递归终止
        return []

    # 遍历要合并的mro
    for mroList in inLists:
        # 取head
        head = mroList[0]
        # 遍历要合并的mro(与外一层相同), 检查尾中是否有head
        ### 此处也遍历了被取head的mro, 严格地来说不符合标准算法实现
        # 但按照多继承中地基础规则(一个类只能被继承一次),
        ### head不可能在自己地尾中, 无影响, 若标准实现, 反而增加开销
        for cmpList in inLists[inLists.index(mroList) + 1:]:
            if head in cmpList[1:]:
                break
        else:
            # 筛选出好head
            nextList = []
            for mergeItem in inLists:
                if head in mergeItem:
                    mergeItem.remove(head)
                if mergeItem:
                    # 排除空list
                    nextList.append(mergeItem)
            # 递归开始
            return [head] + merge(nextList)
    else:
        # 无好head, 引发类型错误
        raise TypeError


class A(object):
    pass


class B(object):
    pass


class C(object):
    pass


class E(A, B):
    pass


class F(B, C):
    pass


class G(E, F):
    pass


if __name__ == '__main__':
    print([i.__name__ for i in c3MRO(G)])
    ## ['G', 'E', 'A', 'F', 'B', 'C', 'object']
    print(G.mro())
