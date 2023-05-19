# 浮点数制(IEEE-754)的转换 struct

[Python实现浮点数制(IEEE-754)的转换——struct](https://zhuanlan.zhihu.com/p/541829376)
[struct --- 将字节串解读为打包的二进制数据](https://docs.python.org/zh-cn/3/library/struct.html)

## 字节顺序

![字节顺序](https://pic3.zhimg.com/80/v2-8dad00d3ebc098ffc550659a0bfe6ef6_720w.webp)

`struct`可以通过上表规定转换的顺序, 只介绍常用的 `<` 和 `>`

举两个例子:

```python
struct.pack('<f',3.5)  #这个意思是将3.5这个浮点数(f是float的意思)按照小端模式转换
struct.pack('>I',5), # 将5这个整数(I是unsigned int的意思)按照大端模式转换.
```

默认不加 `<>` 是按照当前机器的大小端转换.
可以通过 `sys.byteorder` 查看当前机器的是大端还是小端

## 格式字符

这里的格式字符都是按照与C语言对应的, 见下表
比较常用的是前三个, 与IEEE-754相关.

![format](https://pic2.zhimg.com/80/v2-06c8e26f62ccc80b536a1d9dcd134b5d_720w.webp)

tips: 格式字符之前可以带有整数重复计数.
例如, 格式字符串'4f'的含义与'ffff'完全相同;

## 浮点数制的转换

## IEEE-754浮点数 转为 10进制float

通常 `IEEE-754` 的浮点数都是 `16进制` 表示的, 需要经过如下步骤

+ `16进制` -> `无符号10进制`
+ `无符号10进制` -> pack
将pack之后的数unpack为d,f或者e, 分别对应binary-64/binary-32/binary-16

### binary-32的`0xbfad50` 转为 十进制浮点数

```python
import struct
x = 'bfad50'
y = int(x, 16)
z = struct.pack('<I', y)
print(f"struct pack {z}")
a = struct.unpack('<f', z)
print(a)
# 输出
# 1.7602752576441485e-38
```

![下面是IEEE在线工具的结果](https://pic1.zhimg.com/80/v2-178f794c887fb53083ad1330ea5de860_720w.webp)

### binary-16的`0xbfad` 转为 十进制浮点数

```python
import struct
x='bfad'
y=int(x,16)
z=struct.unpack('<e',struct.pack('<H',y)) # 这里需要使用H, 2bytes
print(z[0])
# 输出
# -1.9189453125
```

![img](https://pic3.zhimg.com/80/v2-cc63b574cb08f0e807e841dcd1e9087a_720w.webp)

## 10进制float转为 IEEE-754 浮点数

通常将转换后的数用16进制表示, 步骤如下:

将10进制浮点数, 按照d,f,e(binary-64, binary-32,binary-16) pack
将pack的结果按照无符号数unpack
将无符号数用16进制表示

### 例如: 将10进制浮点数 `3.5` 转换为IEEE-754 binary-16

```python
import struct
x=3.5
y=struct.unpack('<h',struct.pack('<e',x)) # 将浮点数按照2byte float转换
z=hex(y[0])
print(z)
# 输出结果
# 0x4300
```

### 将10进制浮点数 `35.8` 转换为IEEE-754 binary-32

```python
import struct
x=3.5
y=struct.unpack('<I',struct.pack('<f',x)) # 将浮点数按照4byte float转换
z=hex(y[0])
print(z)
# 输出结果
# 0x40600000
```

![img](https://pic3.zhimg.com/80/v2-3d7fb94ee92db56c70a330d4d08358ee_720w.webp)
