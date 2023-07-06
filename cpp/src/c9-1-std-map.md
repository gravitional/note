# c++ std::map

## 最大元素

[C++ How to find the biggest key in a std::map?](https://stackoverflow.com/questions/1660195/c-how-to-find-the-biggest-key-in-a-stdmap)

The end:

```cpp
m.rbegin();
```

Maps(和sets)是被排序的, 所以第一个元素是最小的, 而最后一个元素是最大的.
默认情况下, maps 使用 `std::less`, 但是你可以切换比较器, 这当然会改变最大元素的位置.
例如, 使用 `std::greater` 会将最大值放在 `begin()` 处.

请记住 `rbegin` 返回迭代器.
要获得实际的键, 使用 `m.rbegin()->first`.
为了清楚起见, 你可以把它包装成一个函数, 尽管我不确定这是否值得:

```cpp
template <typename T>
inline const typename T::key_type& last_key(const T& pMap)
{
    return pMap.rbegin()->first;
}

typedef std::map</* types */> map_type;

map_type myMap;
// populate

map_type::key_type k = last_key(myMap);
```
