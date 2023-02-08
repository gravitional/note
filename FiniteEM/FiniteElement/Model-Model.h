#include <unorderer_map>
#include <set>
class CompNode;
class CompEdge;
class CompElement;
class CompMaterial;

// 模型数据
class FE_API Model : public Singleton<Model>
{
    //---构件
public:
    // 添加一个构件
    template <typename T>
    void AddComponent(T *component);
    // 添加一个构件，自动分配ID
    // 判断构件是否存在
    //  通过ID获取某类（存储组件）的指针（不同类型的）
    template <typename T>
    T *GetComponent(int id = 1);
    // 取构件T及子类的指针向量
    template <typename T>
    void GetCompPtrs(vector<T *> &ptrs);
    template <typename T>
    vector<T *> GetCompPtrs();

private:
    CompStorageGeneral _components;

    //---节点
public:
    // 增加节点
    // 判断节点是否存在
    // 根据ID删除节点
    // 根据ID取节点
    // 取节点数
    // 取节点最大ID
    // 获得所有指针
    // 获得所有ID

    //---边, edge
    // 增加边
    // 判断边是否存在
    // 根据ID删除边
    // 根据ID取边
    // 取边数目
    // 取边最大ID
    // 获得所有指针, 边
    // 获得所有ID
private:
    CompStorageNumerous<CompEdge> _edges;

    //---单元
public:
    // 增加单元
    // 增加单元，自动分配ID
    // 是否存在该ID的单元
    // 根据ID删除单元
    // 取单元最大ID
    // 设置最大单元ID
    // 根据ID取单元
    // 取单元并转换类型
    template <typename T>
    T *GetElement(int id);
    // 取单元数目
    // 获得所有单元指针
    // 获得所有ID
    // 获得某类单元及其子类的数量及其ID编号
    // 获得大类的单元总个数与ID号
private:
    // 获得所有指针, 边
    // 获得所有ID
protected:
    int _compID;
    vector<int> _setIDs;
    VarValue _waveAmpl;
}
