# learn.cpp.x.md

## 编译

如果是多个 `C++` 代码文件, 如 `runoob1.cpp, runoob2.cpp`, 编译命令如下：

```bash
g++ runoob1.cpp runoob2.cpp -o runoob
```

`g++` 有些系统默认是使用 `C++98`, 我们可以指定使用 `C++11` 来编译 `main.cpp` 文件：

```bash
g++ -g -Wall -std=c++11 main.cpp
```
