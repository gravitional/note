# 载荷, 激励类 Load

```cpp
class FE_API Load : public Component {
public:
    
    // 设置构件号, 如是加到所有单元，则设为0，取时也用0
    void SetCompID(int id){_compID=id;}
    // 添加集合号
    void AddSetID(int id);
    void AddSetIDs(vector<int> &ids);
    // 取构件号, 如果没有 comID 则取 setID

    // 设置波函数, 即单变量函数
    // 复制波函数
    
    // 更新幅值
    // 设置幅值
    // 取幅值
    // 取导数
    // 取函数ID
    
public:
    int locID; //局域坐标系

protected:
    int _compID;
    vector<int> _setIDs;
    VarValue _waveAmpl;
}
```
