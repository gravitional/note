# solver 程序调用

## solver.exe 运行参数

+ `CASE_PATH`; 工程路径
+ `--help`; `-h`; 打印帮助信息
+ `--threads`; `-p`; 并行线程数目, 调试时建议为`1`
+ `--cuda`; `-c`; 启用 cuda;
+ `--no-warning`; `-w`; 不输入日志中的 warning 信息
+ `--debug`; `-d`; 会输出日志中的`debug`信息, 平时建议加上
+ `--test`; `-t`; 会输出日志中的 test 信息, 用于自动化测试, 平时建议加上
+ `--profile`; `-f`; 会额外打印时间内存信息
+ `--restart`; `-r`; 续算
+ `--ensight`; `-e`; 会输出 ensight 格式结果, 平时建议加上.
+ `--license-server-ip`; license服务器IP
+ `--license-server-port`; license服务器端口

调用示例

```powershell
C:\Sim-dev\Release\bin\Solver\IBECAE\solver.exe --ensight --debug --test --threads=4 xxx\BianYaQi\Solving\SolvingDomain
```

## 求解器报错

### 分解过程错误

```bash
[Warning] 分解过程错误: -9,
```

网格匹配要和周期边界主从一致
网格的 `网格匹配` 的 `主从顺序`, 要和 `周期边界` 那边的设置一致

## 打印机使用

ibesupport, 8888
添加用户名和地址.
待扫描文件放到顶端 `斜坡输入口`, 选择 `扫描后发送` 功能

## 批量运行工程, 测试solver

[测试 solver 的脚本](solver-test.ps1)
