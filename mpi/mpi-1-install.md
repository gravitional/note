# MPI的安装与配置

## ms mpi, windows 安装

[Microsoft MPI](https://learn.microsoft.com/en-us/message-passing-interface/microsoft-mpi?redirectedfrom=MSDN)

在 windows 上需要额外安装 mpi 的二进制文件, 使用上面的url.
将安装完成后得到的 `mpiexec.exe` 复制到 msys 的PATH中, 
例如 `~/bin`目录下.
`mpi.h`, `libmpi.dll` 等开发依赖可以通过 pacman 安装,
无需使用 msmpisdk.msi 中提供的, 可以免去配置头文件搜索路径的麻烦.

安装后的路径:

```bash
# exe文件, 如 mpiexec.exe
C:/Program Files/Microsoft MPI/Bin 
# include 和 lib 文件
C:/Program Files (x86)/Microsoft SDKs/MPI/Include
C:/Program Files (x86)/Microsoft SDKs/MPI/Lib/x64 
```

### 安装msmpi, MSYS2 ucrt64

使用 pacman 安装如下包

```bash
pacman -S mingw-w64-ucrt-x86_64-msmpi
```

然后使用 cmake 测试脚本[mpi-cmake-test](./mpi-cmake.cmake) 查看是否安装成功.
查看输出的 mpi include 目录和 lib 目录信息.

## mpich 源码安装, linux

[mpich install guide](https://www.mpich.org/documentation/guides/)

```bash
# 解压 src 目录
tar xfz mpich-4.2.0rc1.tar.gz

# 创建构建目录
mkdir build && cd build

# 运行 configure 脚本, 默认安装到 /usr/local/bin
../configure -prefix='c:/cppLibs' CC=gcc.exe CXX=g++.exe -disable-f77 FC=gfortran.exe |& tee c.txt
```

### MPI与OpenMPI和MPICH等的关系

`MPI`(Message Passing Interface),
由其字面意思也可些许看出, 是一个信息传递接口.
可以理解为是一种独立于语言的信息传递标准.
而 `OpenMPI` 和 `MPICH` 等是对这种标准的具体实现.
也就是说, `OpenMPI` 和 `MPICH` 这类库是具体用代码实现浏MPI标准.
因此我们需要安装 `OpenMPI`
或者 `MPICH` 去实现我们所学的 `MPI` 的信息传递标准.

`MPICH` 和 `OpenMPI` 等是采用 `MPI` 标准的通信协议.
本文将选择 `MPICH` 的安装作为示范,
一步一步讲解如何配置 `MPI` 的环境.
`OpenMPI` 的安装方法也同理.

## MPI的下载与安装

[https://www.mpich.org/downloads/]: https://www.mpich.org/downloads/

在开始安装之前, 先检查一下是否已经安装好了相应的编译器.

```bash
which gcc
which gfortran
```

当检查完编译器之后,
去 [https://www.mpich.org/downloads/][] 选择合适的版本下载,
对于没有图形界面的服务器, 也可使用wget命令下载.

MPI库通常采用的是源码安装, `cd`进入到解压后的文件夹中,
使用`./configure` 进行安装前的设置与检查,
由于我们只需要更改一下安装的路径,
因此在`--prefix`这一参数中, 设置你想要`安装路径`即可.

```bash
wget http://www.mpich.org/static/downloads/3.3.2/mpich-3.3.2.tar.gz
tar -zxvf mpich-3.3.2.tar.gz #解压下载的压缩包
cd mpich-3.3.2 #进入解压后的文件夹内
./configure  --prefix=/usr/local/mpich-3.3.2
# --prefix这一参数是设置安装的路径, 根据需要设置合适的路径即可, 但需要记住安装的位置
make
make install
如果没有报错, 就说明顺利安装完成了.
```

## 环境变量的配置

安装完成后, 可以去之前--prefix设置的路径去看一下安装结果.
安装好了之后, 还需要告诉系统mpi 库的路径地址,
这样当你调用mpi的命令时, 系统才知道你在干什么.

在用户的根目录下, 有一个 .bashrc 的文本文件
(默认使用的是bash, 如果是zsh等自行查阅资料,
我在这里避免信息过多导致初学者疑惑).
这个文件可以理解为, 每次打开终端时都会加载的启动项.
`~` 即为家目录, 也就是用户的根目录.

```bash
vim ~/.bashrc
```

通过vim打开当前用户下所对应的.bashrc文件,
在其中加入一行(建议添加在最下面一行)

```bash
export PATH="/usr/local/mpich-3.3.2/bin:$PATH"
```

保存退出之后 , 使用source这一命令执行一下就把新加的命令执行了.
前面说过, .bashrc文件是每次开启终端后的类似加载启动项文件.
也就是说, 如果你不想手动source来加载的话,
也可以通过新打开一个终端让它开启时自动加载.

```bash
source ~/.bashrc
which mpicc
which mpif90
```

之后, 用which来检验下配置的环境变量是否正确.
如果显示了其路径, 则说明安装顺利完成了.
这时候, 进入到最开始解压的文件夹中,
到解压的文件夹内的examples文件夹中, 测试一下hello是否能顺利运行.

```bash
mpirun -np 4 ./hello
```

若可运行说明顺利完成安装.

## VisualStudio2019配置MPI

[VisualStudio2019配置MPI](https://blog.csdn.net/Jacamox/article/details/112563361)
