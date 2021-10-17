# learn.powershell.5.md

[收集和分享 Windows PowerShell 相关教程,技术和最新动态](https://www.pstips.net/)
版权归原作者所有

## Powershell数组和哈希表

## Powershell错误处理

## Powershell文本和正则表达式

### 使用反向引用

例如, 怎样更改正则表达式, 让它只替换名字Miller和Meyer.

```powershell
"Mr. Miller, Mrs. Meyer and Mr. Werner"-replace
"(Mr.|Mrs.)\s*(Miller|Meyer)", "Our client"
# Our client, Our client and Mr. Werner
```

输出结果看起来有点奇怪, 但是确实是和搜索模式匹配的. 被替换掉的仅仅是`Mr.` or `Mrs. Miller` 与 `Mr.` or `Mrs. Meyer`.
词语"Mr. Werner"没有被替换.
遗憾的是结果没道理替换掉整个模式, 至少人名应当保留. 这可能吗?
此时反向引用应当登场了.
在正则表达式中, 不论你什么时候使用圆括号, 圆括号中的结果都是分开被运算的.
你可以在你的"替换串"中使用这些分离出来的结果.
第一个子表达式的结果总是"Mr."或者"Mrs.".
第二个子表达式总是返回人名.
这些子表达式分别用"$1"和 "$2″来表示
(更多的的子表达式你可以使用"$3″等等).

```powershell
"Mr. Miller, Mrs. Meyer and Mr. Werner"-replace "(Mr.|Mrs.)\s*(Miller|Meyer)", "Our client $2"
# Our client , Our client and Mr. Werner
```

奇怪的是, 第一个反向引用似乎并没有工作.
当然原因也非常明显:
"$1"and "$2″看起来是PowerShell 变量, 但是实际上它们应当是操作符`-replace`的正则表达式词语.
导致此结果的是你把"替换串"放在了双引号中了, PowerShell会将变量替换成具体的值, 而这个值一般情况下应当为空.
所以要是反向引用在"替换串"中起作用, 你必须将"替换串"放置在单引号中, 这样让`$`变成普通字符, 这样PowerShell就不会把它识别为自己的变量了, 并完成替换功能:

```powershell
# 替换串文本必须放置单引号中, 反向引用才能工作,
# $2才会替换成子表达式返回的值
"Mr. Miller, Mrs. Meyer and Mr. Werner"-replace "(Mr.|Mrs.)\s*(Miller|Meyer)", 'Our client $2'
# Our client Miller, Our client Meyer and Mr. Werner

# 另外也可以使用转义字符 `$来标记$:
"Mr. Miller, Mrs. Meyer and Mr. Werner"-replace "(Mr.|Mrs.)\s*(Miller|Meyer)", "Our client `$2"
# Our client Miller, Our client Meyer and Mr. Werner
```

## PowerShell处理XML

## PowerShell注册表
