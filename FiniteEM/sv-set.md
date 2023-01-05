# solver Set 类

采用压缩格式的id集合

```cpp
class Set: public IDNameObject{}
```

获取 `setID` 或者 `setIDs` 引用的 `element IDs`.

```cpp
std::vector<int> getIDbySet(int setID);
std::vector<int> getIDbySet(const std::vector<int>& setIDs);
```
