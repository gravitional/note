# MatLab Basic

## clear

[clear](https://ww2.mathworks.cn/help/matlab/ref/clear.html#btd0uqt-4)

提示:

+ 调用 `clear all`, `clear classes` 和 `clear functions` 会降低代码性能, 且通常没有必要. 
    + 要从当前工作区中清除一个或多个特定变量, 请使用 `clear name1 ... nameN`. 
    + 要清除当前工作区中的所有变量, 请使用 `clear` 或 `clearvars`. 
    + 要清除所有全局变量, 请使用 `clear global` 或 `clearvars –global`. 
    + 要清除特定类, 请使用 `clear myClass`. 
    + 要清除特定函数或脚本, 请使用 ``clear functionName``. 
    + 要清除所有 `MEX` 函数, 请使用 `clear mex`. 

+ `clear` 函数可以删除您指定的变量. 
要删除除几个指定变量之外的所有变量, 请改用 `clearvars`. 

+ 如果您清除图窗或图形对象的句柄, 该对象自身将不会删除. 
可使用 delete 删除对象. 另一方面, 删除对象并不会删除用于存储其句柄的变量(如果有). 

+ `clear` 函数不会清除 `Simulink` 模型. 请改用 `bdclose`. 
+ `clear` 函数不会清除局部函数或嵌套函数中的 `持久变量`. 
+ 在 `UNIX` 系统中, ``clear`` 不会影响分配给 `MATLAB` 进程的内存量. 
