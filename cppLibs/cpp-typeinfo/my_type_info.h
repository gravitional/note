#include <type_traits>
// https://learn.microsoft.com/zh-cn/cpp/standard-library/is-lvalue-reference-class?view=msvc-170
#include <concepts>

//如何打印表达式的类型信息------------------
template <typename T>
std::string type_to_string()
{
#if defined(_MSC_VER)
    std::string type_name{__FUNCSIG__};
    // class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >
    //    __cdecl type_to_string<int&>(void)
    auto start_pos = type_name.find_first_of('<',
                                             std::string(typeid(std::string).name()).size()) +
                     1;
    auto end_pos = type_name.find_last_of('>');
    return type_name.substr(start_pos, (end_pos - start_pos));
#elif defined(__clang__)
    std::string type_name{__PRETTY_FUNCTION__};
    auto start_pos = type_name.find_first_of('=') + 2;
    auto end_pos = type_name.find_first_of(']', start_pos);
    return type_name.substr(start_pos, (end_pos - start_pos));

#elif defined(__GNUC__)
    std::string type_name{__PRETTY_FUNCTION__};
    // std::__cxx11::string type_to_string() [with T = int&; std::__cxx11::string = std::__cxx11::basic_string<char>]
    auto start_pos = type_name.find_first_of('=') + 2;
    auto end_pos = type_name.find_first_of(';', start_pos);
    return type_name.substr(start_pos, (end_pos - start_pos));
#endif
}

//----------------------- 判断左值和将亡值, 不属于它们中任意一个的就是纯右值
// 如何在Visual Studio中启用C++ 20模块支持
// https://www.bilibili.com/read/cv13482817/
template <typename T>
concept is_lvalue = std::is_lvalue_reference_v<T>;

template <typename T>
concept is_xvalue = std::is_rvalue_reference_v<T>;

template <typename T>
concept is_prvalue = !(is_lvalue<T> || is_xvalue<T>);

template <typename T>
concept is_rvalue = is_prvalue<T> || is_xvalue<T>;

template <typename T>
concept is_glvalue = is_lvalue<T> || is_xvalue<T>;
// 使用时传入的模板参数是decltype((a)) 这样的形式.
// 注意: 上述concept只能用于判定值类型, 无法实际在模板中使用, 因为模板中传入的是类型参数.
//定义一个宏, 方便使用
#define value_type(value)                                        \
    do                                                           \
    {                                                            \
        std::cout << #value " is lvalue : "                      \
                  << is_lvalue<decltype((value))> << std::endl;  \
        std::cout << #value " is xvalue : "                      \
                  << is_xvalue<decltype((value))> << std::endl;  \
        std::cout << #value " is prvalue : "                     \
                  << is_prvalue<decltype((value))> << std::endl; \
        std::cout << #value " is rvalue : "                      \
                  << is_rvalue<decltype((value))> << std::endl;  \
        std::cout << #value " is glvalue : "                     \
                  << is_glvalue<decltype((value))> << std::endl; \
    } while (0)