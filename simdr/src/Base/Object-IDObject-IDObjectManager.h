/*!
 * @file IDOBjectManager.h
 * @brief ID 对象管理类, 0-base
 */

#include "BaseHeader.h"
#include "Singleton.h"
#include <vector>
#include <unordered_map>
#define BASE_API

class IDObject;

class BASE_API IDOBjectManager : public Singleton<IDOBjectManager>
{
public:
    IDOBjectManager(token);
    virtual ~IDOBjectManager()
    {
        for (auto &item : _objects)
        {
            for (auto obj : item.second)
            {
                delete obj;
            }
        }
    };

    // 添加一个对象，自动赋ID;
    template <typename T>
    int Add(T *object);

    // 取一个对象指针
    template <typename T>
    T *Get(ind id);

    // 取一个类型的所有对象指针
    template <typename T>
    std::vector<T *> GetPtrs(ind id);

private:
    std::unordered_map<std::string, std::vector<IDObject>> _objects; // 名称到ID对象的 map
};

// 全局单例
inline IDOBjectManager *idObjects()
{
    return &IDOBjectManager::GetInstance();
}