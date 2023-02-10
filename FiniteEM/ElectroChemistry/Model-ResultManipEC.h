#include "FieldData.h"
#include <complex>
#include "EMBaseHeader.h"
#include <map>

using namespace std;

class ResultManipEC
{
public:
    //---- 单例模式
    static ResultManipEC *GetInstance() { return &_instance; }

private:
    static ResultManipEC _instance;

    //----解操作
public:
    common::RefArray<double> AddSolution(int StoreNO); // 添加一个解的快照
    common::RefArray<double> GetSolution(Int StoreNO); // 得到某工况下的解的快照

    void AccumSoluData(int baseNO, int addNO);                  // 将两种序号的方程的解的结果相加
    void CopySoluData(int copy, int from, double factor = 1.0); // 赋值方程解
    void SetSolution(int storeNo, double solu[]);               // 存入方程解
    void ZeroSolution(int seoreNo);                             // 清空

    //--- 导入导出
    bool DumpSave(ofstream &fout); // 保存二进制文件
    bool DumpLoad(ifstream &fin);  // 加载二进制文件

    //--- 单元场变量
    static int GetFieldComponentNum(FieldType type);
    void InitFieldData(int storeNo);
    vector<FieldType> GetoutputFields();
    FieldType GetFieldType(const string &name);
    FieldData &GetEleData();
    FieldData &GetEleNdData();
    FieldData &GetEleGsData();
    FieldData &GetSoluData();

    //--- 节点和约束的映射
    void AddNodeID2Constraint(const int JobNO, int theID, double Consvalue);
    void GetNodeID2Constraint(const int JobNO, int theID, double &Consvalue);
    void SetNodeID2Constraint(int CopyNO, int FromNO);

protected:
    map<int, map<int, double>> NodeID2Constraint;
};

// 全局函数
inline ResultManipEC *GetResultManipEC()
{
    return ResultManipEC::GetInstance();
}
