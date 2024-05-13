# eigen

## 下载安装

[eigen 主页](https://eigen.tuxfamily.org/index.php?title=Main_Page)
下载解压后, 进入 `eigen-x.x.x` 目录

```powershell
#
mkdir build-msvc
cd build-msvc

# cmake 构建 项目
cmake -B . -S .. -DCMAKE_INSTALL_PREFIX='C:/cppLibs/eigen' -G 'Visual Studio 17 2022'
# cmake build 项目, in parallel
cmake --build . --config=release -j
# cmake 安装到指定位置
cmake --install . --config=release
```

## 文档

[eigen 中文文档](https://runebook.dev/zh/docs/eigen3/-index-)
[eigen api 文档](https://eigen.tuxfamily.org/dox/index.html)

## cmake

```cmake
# 设置 eigen cmake 所在目录
set(my_project_dir ${CMAKE_SOURCE_DIR})
set(Eigen3_DIR "${my_project_dir}/dependencies/eigen/share/eigen3/cmake")
# find eigen
message("<<<eigen dir: " ${Eigen3_DIR})
find_package(Eigen3 3.3 REQUIRED NO_MODULE)
```
