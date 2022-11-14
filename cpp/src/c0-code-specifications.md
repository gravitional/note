# linux 内核编码风格

[Linux 内核代码风格](https://www.kernel.org/doc/html/latest/translations/zh_CN/process/coding-style.html)
[Linux 内核编码风格](https://zhuanlan.zhihu.com/p/330280764)

### 括号

左括号紧跟在语句的最后, 与语句在相同的一行. 而右括号要另起一行, 作为该行的第一个字符.
如果接下来的部分是相同语句的一部分, 那么右括号就不单独占一行.

```c
if ... {
              if ... {
              ...
              } else {
              ...
                     }
       }
return ... ;

do {
...
} while ( ...);
```

函数采用以下的书写方式:

```c
static inline int rt_policy(int policy)
{
              ...
}
```

最后不需要一定使用括号的语句可以忽略它:

```c
if (a==b)
              return 0;
return 1;
```

### 每行代码的长度

要尽可能地保证代码长度不超过80个字符, 如果代码行超过`80`应该折到下一行.
将参数分行输入, 在开头简单地加入两个标准tab:

```c
static int wait_noreap_copyout ( a, b, c, ...
              d,e,f )
{

}
```

### 命名规范

名称中不允许使用混合的大小写字符.
局部变量如果能够清楚地表明它的用途, 那么选取`idx`甚至是i这样的名称都是可行的.
而像`theLoopIndex`这样冗长反复的名字不在接受之列.  -- 匈牙利命名法(在变量名称中加入变量的类别)危害极大.

### 函数

根据经验函数的代码长度不应该超过两屏, 局部变量不应该超过十个.

+ 一个函数应该功能单一并且实现精准.
+ 将一个函数分解成一些更短小的函数的组合不会带来危害.  -- 如果你担心函数调用导致的开销, 可以使用inline关键字.

### 注释

一般情况下, 注释的目的是描述你的代码要做什么和为什么要做, 而不是具体通过什么方式实现的. 怎么实现应该由代码本身展现.
注释不应该包含谁写了那个函数, 修改日期和其他那些琐碎而无实际意义的内容. 这些信息应该集中在文件最开头地方.
重要信息常常以`XXX:`开头, 而`bug`通常以`FIXME`开头.