#include <iostream>
#include <boost/shared_ptr.hpp>
#include <boost/filesystem.hpp>

// Boost库是为C++语言标准库提供扩展的一些C++程序库的总称,
//由Boost社区组织开发,维护.
// Boost库可以与C++标准库完美共同工作, 并且为其提供扩展功能.

int main(int argc, char *argv[])
{
    std::cout << "Hello Third Party Include!" << std::endl;

    // use a shared ptr
    boost::shared_ptr<int> isp(new int(4));

    // trivial use of boost filesystem
    boost::filesystem::path path = "/usr/share/cmake/modules";
    if (path.is_relative())
    {
        std::cout << "Path is relative" << std::endl;
    }
    else
    {
        std::cout << "Path is not relative" << std::endl;
    }

    return 0;
}
