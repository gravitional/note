# STL容器底层数据结构

[C++ STL容器底层数据结构总结](https://www.jianshu.com/p/834cc223bb57)

总的来说, STL包括几个部分: 容器, 算法(泛型算法), 迭代器三个主要部分(当然还包含仿函数, 适配器等其他部分),

![STL三个主要部分的关系示意图](https://upload-images.jianshu.io/upload_images/10118224-ee23dafe96babb08.png?imageMogr2/auto-orient/strip|imageView2/2/w/778/format/webp)

本篇文章主要是对于STL中的常用容器的底层数据结构进行总结整理.

## vector

### vector底层数据结构

vector是我们用到最多的数据结构, 其底层数据结构是 `数组`,
由于数组的特点, `vector` 也具有以下特性:

+ O(1)时间的快速访问;
+ 顺序存储, 所以插入到非尾结点位置所需时间复杂度为O(n), 删除也一样;

+ 扩容规则: 当我们新建一个vector的时候, 会首先分配给他一片连续的内存空间,
如 `std::vector<int> vec`, 当通过push_back向其中增加元素时,
如果初始分配空间已满, 就会引起vector扩容, 其扩容规则在gcc下以2倍方式完成:
首先重新申请一个2倍大的内存空间, 然后将原空间的内容拷贝过来;
最后将原空间内容进行释放, 将内存交还给操作系统;

测试代码如下:

```cpp
#include<iostream>
#include<vector>
using namespace std;

void mycapacity(const vector<int>& vec)
{
    cout << "分配总空间大小为: " << vec.capacity() << endl;
}

void mysize(const vector<int>& vec)
{
    cout << "已用空间大小为: " << vec.size() << endl;
}

void myprint(const vector<int>& vec)
{
    for (int i = 0; i < vec.size(); ++i)
        cout << vec[i] << ",";
    cout << endl;
}

int main()
{
    vector<int> vec;
    cout << "起始状态: " << endl;
    mycapacity(vec);
    mysize(vec);
    cout << "========================" << endl;

    for (int i = 0; i < 10; ++i) {
        vec.push_back(i);
        cout << "压入第" << i+1 << "个元素之后: " << endl;
        myprint(vec);
        mycapacity(vec);
        mysize(vec);
        cout << "========================" << endl;
    }

    return 0;
}
```

![测试输出结果](https://upload-images.jianshu.io/upload_images/10118224-e59cafd511df1adc.png?imageMogr2/auto-orient/strip|imageView2/2/w/516/format/webp)

从输出结果中的三个红色箭头可以看出 `vector` 的扩容规则.

### 注意事项

根据 `vector` 的插入和删除特性,
以及扩容规则, 我们在使用 `vector` 的时候要注意,
在插入位置和删除位置之后的所有迭代器和指针引用都会失效,
同理, 扩容之后的所有迭代器指针和引用也都会失效.

## map & multimap & unordered_map & unordered_multimap

### map与multimap底层数据结构

map与multimap是STL中的关联容器, 提供一对一 `key-value` 的数据处理能力;
map与multimap的区别在于, multimap允许关键字重复, 而map不允许重复.

这两个关联容器的底层数据结构均为红黑树,
关于红黑树的理解可以参考[教你透彻了解红黑树](https://github.com/julycoding/The-Art-Of-Programming-By-July-2nd/blob/master/ebook/zh/03.01.md)一文.

根据红黑树的原理, map与multimap可以实现O(lgn)的查找, 插入和删除.

### unordered_map 与unordered_multimap底层数据结构

unordered_map与unordered_multimap 对比2.1中的两种map,
在于其2.1中的两个容器实现了以key为序排列, 也就是说map与multimap为有序的.
而unordered_map与unordered_multimap中key为无序排列, 其底层实现为hash table,
因此其查找时间复杂度理论上达到了 `O(1)`,
之所以说理论上是因为在理想无碰撞的情况下, 而真实情况未必如此.

[insert_or_assign](https://en.cppreference.com/w/cpp/container/unordered_map/insert_or_assign)

## set & multiset & unordered_set & unordered_multiset

这四种容器也都是关联容器,
set系与map系的区别在于map中存储的是`<key-value>`,
而set可以理解为关键字即值, 即只保存关键字的容器.

### set & multiset底层数据结构

set与multiset有序存储元素, 这两种容器的底层实现与map一样都是红黑树,
所以能实现O(lgn)的查找, 插入, 删除操作.

set与multiset的区别在于是否允许重复;

### unordered_set & unordered_multiset

与unordered_map & unordered_multimap相同,
其底层实现为hash table;

## priority_queue

### priority_queue

优先级队列相当于一个有权值的单向队列queue, 在这个队列中, 所有元素是按照优先级排列的.

priority_queue根据堆的处理规则来调整元素之间的位置, 关于堆的原理, 可以参考堆;

根据堆的特性, 优先级队列实现了取出最大最小元素时间复杂度为O(1),对于插入和删除, 其最坏情况为O(lgn).

## 其他数据结构

list的底层数据结构为双向链表, 特点是支持快速的增删.
queue为单向队列, 为先入先出原则.
deque为双向队列, 其对比queue可以实现在头尾两端高效的插入和删除操作.
