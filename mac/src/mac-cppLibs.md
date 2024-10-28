# mac 安装 cpp libs

## boost

克隆并解压仓库, 然后运行

```bash
./bootstrap.sh --prefix=/Users/tom/myLibs/boost
./b2 install
```

## openMPI

```bash
tar xf openmpi-<version>.tar.bz2
cd openmpi-<version>
./configure --prefix=/Users/tom/myLibs/openmpi 2>&1 | tee config.out
# <... lots of output ...>

#-- Use an integer value of N for parallel builds
make -j 8 all 2>&1 | tee make.out
# ...lots of output...

#--- Depending on the <prefix> chosen above,
# you may need root access for the following:
make install 2>&1 | tee install.out
# ...lots of output...
```

### clang 编译 openmp 程序

[Mac c++ compile openmp](https://zhuanlan.zhihu.com/p/659351908)

### clang 不支持 `std::execution::par` `std::execution::seq` 等等

https://godbolt.org/z/cY67PT7Gx
