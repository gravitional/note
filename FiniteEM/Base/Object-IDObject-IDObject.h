/*!
 * @file IDOBject.h
 * @brief ID 对象类
 */

#include "BaseHeader.h"
#include <string>
#define BASE_API

class BASE_API IDOBject
{
public:
    IDOBject()
    {
        _id = -1;
    }
    virtual ~IDOBject();

    // 存取ID
    void SetID(int id) { _id = id; }
    int GetID() const { return _id; }

private:
    // 禁止复制与赋值
    IDOBject(const IDOBject &) = delete;
    IDOBject operator=(const IDOBject &) = delete;
    IDOBject operator=(const IDOBject &&) = delete;

private:
    int _id;
};

// ID对象，带名称，类
class BASE_API IDNameOBject : public IDOBject
{
public:
    IDNameOBject() = default;
    virtual ~IDOBject() = default;

    // 存取Name
    void SetName(const std::string &name) { _name = name; }
    const std::string &GetName() const { return _name; }

private:
    std::string _name; // 对象名称
};