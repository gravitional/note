# python list

## 列表展开

[Python 将两层列表展开平铺成一层的5种方法](https://blog.csdn.net/weixin_40539892/article/details/79103290)
[How do I make a flat list out of a list of lists?](https://stackoverflow.com/questions/952914/how-do-i-make-a-flat-list-out-of-a-list-of-lists)

Given a list of lists `l`,

```python
flat_list = [item for sublist in l for item in sublist]
```

or, 可以使用 `itertools.chain()`:

```python
import itertools
list2d = [[1,2,3], [4,5,6], [7], [8,9]]
merged = list(itertools.chain(*list2d))
```

Or you can use `itertools.chain.from_iterable()`,
which doesn't require unpacking the list with the `*` operator:

```python
import itertools
list2d = [[1,2,3], [4,5,6], [7], [8,9]]
merged = list(itertools.chain.from_iterable(list2d))
```
