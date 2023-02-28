# c++正则表达式

[正则表达式库](https://zh.cppreference.com/w/cpp/regex)
[regex_iterator](https://zh.cppreference.com/w/cpp/regex/regex_iterator)
[正则表达式库](https://zh.cppreference.com/w/cpp/regex)
[C/C++ 正则表达式 regex库介绍(详细版)](https://blog.csdn.net/weixin_50964512/article/details/124933873)
[匹配结果](https://zh.cppreference.com/w/cpp/regex/match_results)
[匹配结果的第n个捕获组](https://zh.cppreference.com/w/cpp/regex/match_results/str)

## 使用迭代器

如果你认为 `regex_search` 用起来比较麻烦, 则可以使用迭代器, 用法如下:

```cpp
regex r("-(.*?)-");
string s = "yushi-csdn--yushi-csdn";
sregex_iterator beg(s.begin(),s.end(),r);
sregex_iterator end;
for (; beg != end; beg++) {
    cout << beg->str(1) << endl;
}
```

由于 `sregex_iterator` 默认构造函数为指向最后一个元素之后,
所以对 `end` 没有进行任何处理, 只是作为一个结束标志
成员函数 `str` 可以返回指定捕获组的字符串, 不传入数字则代表全部匹配内容
