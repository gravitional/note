// 分析管理类

class Timer;
class AnalysisBase;

class COMMON_API AnalysisControl : public Singleton<AnalysisControl>
{
public:
    //-------- 许可证
    // 检查许可
    // 检查规模
    // 是否为注册版本

    //---------分析管理
public:
    // 添加一个分析至末尾
    // 去下一个分析
    // 取当前分析号
    // 当前分析是否是续算
    // 是否暂停
private:
    std::queue<AnalysisBase> _analyses; // 分析任务列表
    AnalysisBase *_curAnalysis;         // 当前分析任务

    //-----------当前分析信息
public:
    // 设置标记, 当前分析后清除
    // 取标记
private:
    std::unordered_set<std::string> _flags;

    //---计时器
public:
    // 取计时器，默认使用独立计时器
    // 设置当前分析使用连续计时器
    // 根据名称取计时器
private:
    Timer *_curTimer;         // 当前计时器
    Timer *_continuousTimer;  // 连续计时器
    Timer *_independentTimer; // 独立计时器

    //---输出变量控制
public:
    // 从文件读入
    // 当前分析是否输出给定变量
    // 当前分析是否输出给定变量的任一, Any
    // 取当前分析的输出变量
    std::set<std::string> &GetOutFields() { return _curFields; }

private:
    std::vector<std::string> _userFields; // 用户指定输出变量
    std::set<std::string> _curFields;     // 结合用户指定的当前分析输出变量

    //---temporary 纷纷西部名称
public:
private:
    std::unordered_map<std::string, std::string> _caseAnalysisName;
};

// 全局函数, 分析单例
inline AnalysisControl *anlsCtrl()
{
    return &AnalysisControl::GetInstance();
}
inline Timer *timer()
{
    return anlsCtrl()->GetTimer();
}