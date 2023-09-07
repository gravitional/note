# c++ file, path lib

## c++17, std::filesystem 库

[c++ combine path of file as a string with file name](https://stackoverflow.com/questions/28669329/c-combine-path-of-file-as-a-string-with-file-name)

从 目录与文件名称 得到全路径:

```cpp
#include <filesystem> //C++ 17
#include <iostream>

namespace fs = std::filesystem;
using namespace std;

void main()
{
    string dir("c:\\temp");
    string fileName("my_file.txt");

    fs::path fullPath = dir;
    fullPath /= fileName;
    cout << fullPath.c_str() << endl;
}
```

## c++11, c++14 上的 filesystem 库

[std::filesystem::path::append](https://en.cppreference.com/w/cpp/filesystem/path/append)
[关于filesystem在c++11, 14, 17中的使用问题](https://blog.csdn.net/shellching/article/details/123133540)

+ 在C++11时, 该类库定义在 `std::tr2::sys` 命名空间

```cpp
#include <filesystem>
using namespace std;
using namespace std::tr2::sys;

// 示例
void RemoveFile()
{
    directory_iterator end, ite("aaa\\");

    for (; ite != end; ++ite)
    {
        auto &thePath = ite->path();
        if (!is_directory(thePath))
        {
            const string &sName = ite->path().filename();
            if (sName == "test.txt")
            {
                remove(thePath);
            }
        }
    }
}
```

上述代码在 `C++14` 中编译不过, 已经不存在 `tr2` 命名空间
`filesystem` 放在 `std::experimental::filesystem` 空间

```cpp
#include <experimental/filesystem>
using namespace std::experimental::filesystem;
```

+ 但是, 在 (VS2019, C++14) 编译时可能会报错C1189, 提示该头文件已被取代, 建议使用新的
如果不想根据提示改用 `C++17`, 那么我们可以坚持自己的想法, 在项目配置中加预处理宏定义即可
`_SILENCE_EXPERIMENTAL_FILESYSTEM_DEPRECATION_WARNING`

+ 上述代码到了C++17仍能编译通过, 但我们可以使用新版本的库文件

```cpp
#include <filesystem>
using namespace std;
using namespace filesystem;

void Test()
{
    string filepath = "aaa\\";
    directory_iterator end, ite(filepath);

    for (; ite != end; ++ite)
    {
        auto &thePath = ite->path();
        // string strFullPath = pp.string() / "\\" / thePath.relative_path().string();

        path fp(std::filesystem::current_path() / thePath);
        bool bl = filesystem::exists(fp);

        if (!is_directory(fp))
        {
            // const string& sName = thePath.filename().c_str();
            file_status fst(status(fp));
            file_type ftype = fst.type();
            directory_entry de(fp);
            perms fpem = fst.permissions();
            size_t sz = file_size(fp);
            rename(fp, fp);
            auto sName = thePath.filename().string();
            auto duration(last_write_time(fp).time_since_epoch());
            time_t tt = chrono::duration_cast<chrono::seconds>(duration).count();
            // remove(path(strFullPath));
        }
    }

    path dir(filepath);
    directory_iterator di(dir);
    for (const auto &entry : di)
    {
        cout << entry.path().filename() << endl;
        if (entry.is_directory())
        {}
        else if (entry.is_regular_file())
        {}
        else
        {}
    }

    // 深度优先遍历
    recursive_directory_iterator rdi(dir);
    for (const auto &entry : di)
    {
        cout << entry.path() << endl;
    }

    create_directories(dir);       // 创建中间缺失目录
    create_directory(dir / "dd");  // 仅末级目录
    remove(dir / "bbb" / "c.txt"); // 删除单个
    remove_all(dir);               // 递归删除所有
    const auto options = copy_options::recursive | copy_options::update_existing;
    copy(dir, dir, options);
}
```
