// https://blog.csdn.net/weixin_39445116/article/details/127860743

#define vtkSetMacro(name, type)       \
    virtual void Set##name(type _arg) \
    {                                 \
                                      \
        if (this->name != _arg)       \
        {                             \
            this->name = _arg;        \
        }                             \
    }
#define vtkSetMacroOverride(name, type) \
    void Set##name(type _arg) override  \
    {                                   \
                                        \
        if (this->name != _arg)         \
        {                               \
            this->name = _arg;          \
        }                               \
    }

//
// Get built-in type.  Creates member Get"name"() (e.g., GetVisibility());
//
#define vtkGetMacro(name, type)    \
    virtual type Get##name() const \
    {                              \
                                   \
        return this->name;         \
    }

#include <iostream>
#include <string>

class Test1
{
public:
    vtkSetMacro(a, std::string);
    vtkGetMacro(a, std::string);
    std::string a;
};

class Test1Child : public Test1
{
    vtkSetMacroOverride(a, std::string);
};

int main()
{
    Test1 t;
    t.Seta("10220");

    std::cout << t.Geta();

    return 0;
}
