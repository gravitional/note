# 容器

[容器](https://zh.cppreference.com/w/cpp/named_req/Container)

## 容器 (Container)

容器 (Container) 是用于存储其他 `对象`, 并照顾管理其所容纳的对象 `所用的内存` 的对象.

## 知分配器容器

[知分配器容器](https://zh.cppreference.com/w/cpp/named_req/AllocatorAwareContainer)

`知分配器容器` (AllocatorAwareContainer) 是 `容器` (Container) ,
其保有一个分配器 (Allocator) `instance`,
并在所有成员函数中用该实例来 allocate 及 deallocate 内存,
并在此内存中 construct  及 destroy 对象
(这种对象可以是容器元素, nodes, 或对于无序容器为 bucket arrays),

>除了 `std::basic_string` 特化不用分配器构造/析构其元素 (C++23 起).

### 下列规则适用于容器的构造

+ `AllocatorAwareContainers` 的复制构造函数, 通过在正被复制的容器的 `allocator` 上调用 `std::allocator_traits<allocator_type>::select_on_container_copy_construction`
获得自己的 `allocator` 实例.
+ 移动构造函数通过从属于旧容器的 `allocator` 进行移动构造, 获得其自己的分配器实例.
+ 所有其他构造函数均接收一个 `const allocator_type&` 形参.

仅有的替换 `分配器` 的方式是进行 `移动赋值`, `复制赋值` 及 `交换`:

+ 仅当 `std::allocator_traits<allocator_type>::propagate_on_container_copy_assignment::value` 为 `true` 时, 复制赋值才会替换分配器
+ 仅当 `std::allocator_traits<allocator_type>::propagate_on_container_move_assignment::value` 为 `true` 时, 移动赋值才会替换分配器
+ 仅当 `std::allocator_traits<allocator_type>::propagate_on_container_swap::value` 为 `true` 时, `交换` 才会替换分配器.
特别是它将通过对非成员函数 `swap` 的无限定的调用来交换 `分配器` 实例, 见可交换 (Swappable) .

>注: 若 `propagate_on_container_swap` 为 `false`, 则交换两个拥有 `unequal allocators` 的容器是未定义行为.

+ accessor `get_allocator()` 获得 `allocator ` 的副本.
副本来自 `构造容器` 时所用的, 或最近的 `分配器替换` 操作所安装.

## 序列容器

[SequenceContainer](https://zh.cppreference.com/w/cpp/named_req/SequenceContainer)

序列容器 (SequenceContainer) 是在 `线性排列` 中存储 `相同类型` 对象的容器 (Container) .

要求满足下列容器类型的某些公共操作
`std::basic_string`
`std::array`
`std::deque`
`std::list`
`std::vector`

## 连续容器

[ContiguousContainer](https://zh.cppreference.com/w/cpp/named_req/ContiguousContainer)

`连续容器`(ContiguousContainer) 是在 `连续的内存位置` 中存储对象的容器 (Container) .

### 要求

以下情况下, 类型 `X` 满足连续容器:

类型 X 满足 `容器` (Container)
类型 X 支持 `老式随机访问迭代器` (LegacyRandomAccessIterator)
成员类型 `X::iterator` 与 `X::const_iterator` 是
`老式连续迭代器` (LegacyContiguousIterator) (C++20 前)
contiguous_iterator (C++20 起)

### 标准库中的连续容器

+ `basic_string`; 存储并操作字符序列, (类模板)
+ `array`(C++11); 静态的连续数组 (类模板)
+ `vector`; 动态的连续数组(类模板)

## 可逆容器

[ReversibleContainer](https://zh.cppreference.com/w/cpp/named_req/ReversibleContainer)

`可逆容器`(ReversibleContainer) 作为容器 (Container),
它的迭代器需要满足 `老式双向迭代器` (LegacyBidirectionalIterator) 或
`老式随机访问迭代器` (LegacyRandomAccessIterator)的要求.
这些 `迭代器` 允许对 `可逆容器` 进行逆序迭代.

### 要求

+ `X` 容器类型
+ `T` 元素类型
+ `a` X 类型对象

## 类型

+ `X::reverse_iterator`
+ `X::const_reverse_iterator`

### 方法

+ `a.rbegin()`;  reverse_iterator;
+ `a.rend()`;    reverse_iterator; 对常数 a 为 const_reverse_iterator
+ `a.crbegin()`; const_reverse_iterator
+ `a.crend()`;   const_reverse_iterator

### 要求

```cpp
#include <vector>
#include <iostream>

int main()
{
    std::vector<int> v = {3, 1, 4, 1, 5, 9};

    for(std::vector<int>::reverse_iterator i = v.rbegin(); i != v.rend(); ++i) {
        std::cout << *i << '\n';
    }
}
```
