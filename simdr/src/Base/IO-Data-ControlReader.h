#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include "Enum.h"
#define BASE_API

// 求解器控制文件全局单例类

using namespace std;

namespace common
{
    class JsonIO;
}

class BASE_API ControlReader : public Singleton<ControlReader>
{
public:
    ControlReader(token);
    ~ControlReader();
    //---- 整体控制
    // 设置文件路径
    void SetFile(const string &path) { _path = string; }
    // 如果未读过或者已更新则读取
    Read();

    //----
    // 回到根节点
    void Read();
    // 回到父节点
    void GoParent();
    // 前进到子节点, exception为true 且未找到关键字时会抛出异常
    bool GoChild(const string &key, bool exception = true);
    // 前进到绝对路径的特定节点，exception 为true 且未找到关键字时会抛出异常
    boll GoTo(const vector<string> &keyPath, bool exception = true);

    // 保存当前路径
    void Store();
    // 恢复上一个保存的路径
    void Restore();

    //----读取数据
    // 取建
    bool HasChild(const string &key) const;
    const vector<string> GetChildren() const;
    string GetCurrentNode() const;
    bool IsChildArray(const string &key) const;

    // 取单值，出错时返回默认值
    template <typename T>
    bool ReadValue(const string &key, T &value, const T &defaultValue);
    bool ReadValue(const string &key, string &value, const char *defaultValue);
    // 取单值，出错时会抛出异常
    // 取数组，出错时返回默认值
    // 取固定长度数组，出错时会返回异常
    template <typename T>
    void ReadValues(const string &key, T *value, int count);
    //---
    // 取名字，并转成 id，出错时会抛出异常
    void ReadIDByName(const string &key, int &value);
    void ReadIDByName(const string &key, vector<int> &values);
    int ReadIDByName(const string &key);
    // 取名字，并转成id，未找到时返回默认值
    void ReadIDByName(const string &key, int &value, int defaultValue);
    void ReadIDByName(const string &key, vector<int> &values, int defaultValue);

    //--------- 取单值或id
    void ReadValueOrIDByName(const string &key, double &value, int &id);

    //----- 取枚举值，出错时返回默认值
    template <typename EnumType>
    bool ReadEnum(const string &key, EnumType &value, EnumType defaultValue);
    // 取枚举值，出错时会抛出异常, fun-style
    template <typename EnumType>
    bool ReadEnum(const string &key, EnumType &value);
    // x=y style
    template <typename EnumType>
    EnumType ReadEnum(const string &key);

    //---- 取给定范围字符串
    string ReadValidString(const string &key, unordered_set<string> &validValues);

    //---错误处理
    void Error(const string &info);
    void Error(const string &key, const string &info);

    //--- 字符串与ID 映射
    // 根据名称取id
    int GetIDByName(const string &name);
    int GetIDByName(const string &name, int defaultValue);
    // 根据名称设置 id, fun-style
    void SetIDByName(const string &name, int id);

public:
private:
    common::JsonIO *_reader;             // Json 对象
    string _path;                        // 文件路径
    int _modifyTag;                      // 文件修改时间标识
    unordered_map<string, int> _nameMap; // 名称到索引的映射, 不同reader共享
};

// 全局函数, 控制条目读取单例
inline ControlReader *controlReader()
{
    return &ControlReader::GetInstance();
}