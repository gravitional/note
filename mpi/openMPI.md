# open MPI

## 安装

[Linux下安装OpenMPI](https://blog.csdn.net/liu_feng_zi_/article/details/107429347)

+ 下载openmpi: https://www.open-mpi.org/, 解压

```bash
tar xzf openmpi-xxx.tar.gz
```

+ 配置安装路径, 编译并安装, 安装路径自定义

```bash
./configure --prefix=/home/tom/myLibs/openmpi
make -j
make install
```

+ 设置环境变量, 路径为自己安装的路径

```bash
MPI_HOME=/usr/local/openmpi
export PATH=${MPI_HOME}/bin:$PATH
export LD_LIBRARY_PATH=${MPI_HOME}/lib:$LD_LIBRARY_PATH
export MANPATH=${MPI_HOME}/share/man:$MANPATH
```

+ 验证安装成功, 测试一下自带的例子

```bash
cd openmpi-4.0.4/examples
make
mpirun -np 4 hello_c
```
