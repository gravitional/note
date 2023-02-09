#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include "Enum.h"

// 求解器控制文件全局单例类

using namespace std;

namespace common
{
    class JsonIO;
}

class BASE_API ControlReader : public Singleton<ControlReader>
{
public:
    //---- 整体控制

public:
private:
    common::JsonIO *_reader;
    string _path;                        // 文件路径
    int _modifyTag;                      // 文件修改时间标识
    unordered_map<string, int> _nameMap; // 名称到索引的映射, 不同reader共享
};

// 全局函数, 分析单例
inline ControlReader *controlReader()
{
    return &ControlReader::GetInstance();
}