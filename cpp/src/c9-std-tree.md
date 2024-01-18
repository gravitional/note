# c++ std tree 库

[Why does the C++ STL not provide any "tree" containers?](https://stackoverflow.com/questions/205945/why-does-the-c-stl-not-provide-any-tree-containers)

使用树状结构有两个原因:

您想使用树状结构来反映问题:
为此, 我们有 [boost graph library](https://www.boost.org/doc/libs/1_84_0/libs/graph/doc/index.html)

或者你想要一个具有树状访问特性的容器 为此我们有

+ [std::map](https://en.cppreference.com/w/cpp/container/map),
和 [std::multimap](https://en.cppreference.com/w/cpp/container/multimap)
+ [std::set](https://en.cppreference.com/w/cpp/container/set),
和 [std::multiset](https://en.cppreference.com/w/cpp/container/multiset)

基本上, 这两个容器的特性决定了它们实际上必须使用树来实现
(尽管这实际上并不是必须的).

另请参阅本问题:
[C树的实现](https://stackoverflow.com/questions/181630/whats-a-good-and-stable-c-tree-implementation)
