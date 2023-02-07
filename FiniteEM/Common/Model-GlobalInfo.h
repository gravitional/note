// 全局信息类

enum class SptialType
{
    DIM3,
    DIM2,
    AXIS2
};

class COMMON_API GlobalInfo : public Singleton<GlobalInfo>
{

    //-------- 从文件读取信息
public:
    void Read();

    //---------路径
public:
    // 设置工程路径
    // 取工程路径
    // 取结果路径
    // 创建结果文件夹

private:
    std::string _pathProject; // 工程文件路径
    std::string _pathResult;  // 结果文件路径

    //---- 静态信息
public:
    // 求解器名称
    // 文件戳
    // 取空间类型
    // 取空间维度
private:
    std::string _solver;
    std::string _stamp;
    int _dim;

    //--- 动态信息，适合存储少量的全局常量
public:
    // 设置参数名称
    // 取整数参数
    // 设置浮点数参数
    // 取浮点数参数
    // 设置标记
    void SetFlag(const std::string &key, bool value = true);
    // 取标记
    void GetFlag(const std::string &key) const;

private:
    std::unordered_map<std::string, int> _ints;
    std::unordered_map<std::string, double> _doubles;
    std::unordered_set<std::string> _flags;
};

// 全局函数, 分析单例
inline GlobalInfo *blobalInfo()
{
    return &GlobalInfo::GetInstance();
}