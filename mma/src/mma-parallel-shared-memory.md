# 虚拟共享内存

## 同步

一个简单的实现`deadlock`的例子.

```mathematica
Parallelize[
 {
      (*获得锁a*)
  CriticalSection[{lcka},
   CriticalSection[{lckb},
    Pause[Random[]];
    Print["A done"]
    ]
   ],
   (*获得锁b*)
  CriticalSection[{lckb},
   CriticalSection[{lcka},
    Pause[Random[]];
    Print["B done"]
    ]
   ]
  }
 ]
```
