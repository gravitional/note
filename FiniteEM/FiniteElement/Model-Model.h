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
protected:
    int _compID;
    vector<int> _setIDs;
    VarValue _waveAmpl;
}
