# base64-二进制打印

`Base64`是一种用 `64` 个字符来表示任意二进制数据的方法.

用记事本打开`exe`, `jpg`, `pdf`这些文件时, 我们都会看到一大堆乱码,
因为二进制文件包含很多无法显示和打印的字符,
所以, 如果要让记事本这样的文本处理软件能处理二进制数据,
就需要一个二进制到字符串的转换方法.
`Base64`是一种最常见的二进制编码方法.

`Base64`的原理很简单, 首先, 准备一个包含 64 个字符的数组:

```python
['A', 'B', 'C', ... 'a', 'b', 'c', ... '0', '1', ... '+', '/']
```

然后, 对二进制数据进行处理, 每3个字节一组, 一共是`3x8=24bit`, 划为`4`组, 每组正好`6`个`bit`

+ `1byte` = `8bit`
+ `16` 进制字符(`\x`) = `4bit`
+ `1byte` = `2 \x`
+ `base64` 编码: `64=2^6`, `6` 和 `8` 的最小公倍数是 `24`,
即 `3` 个 `byte`, 等于 `4` 个 `6bit` 组.

## base64-encode

这样我们得到4个数字作为索引, 然后查表, 获得相应的4个字符, 就是编码后的字符串.

所以, `Base64`编码会把`3`字节的二进制数据编码为`4`字节的文本数据, 长度增加`33%`, 好处是编码后的文本数据可以在邮件正文, 网页等直接显示.

如果要编码的二进制数据不是`3`的倍数, 最后会剩下`1`个或`2`个字节怎么办? `Base64`用`\x00`字节在末尾补足后, 再在编码的末尾加上`1`个或`2`个`=`号, 表示补了多少字节, 解码的时候, 会自动去掉.

Python内置的`base64`可以直接进行`base64`的编解码:

```python
>>> import base64
>>> base64.b64encode(b'binary\x00string')
b'YmluYXJ5AHN0cmluZw=='
>>> base64.b64decode(b'YmluYXJ5AHN0cmluZw==')
b'binary\x00string'
```

由于标准的`Base64`编码后可能出现字符`+`和`/`, 在`URL`中就不能直接作为参数, 所以又有一种"`url safe`"的`base64`编码, 其实就是把字符`+`和`/`分别变成`-`和`_`:

```python
>>> base64.b64encode(b'i\xb7\x1d\xfb\xef\xff')
b'abcd++//'
>>> base64.urlsafe_b64encode(b'i\xb7\x1d\xfb\xef\xff')
b'abcd--__'
>>> base64.urlsafe_b64decode('abcd--__')
b'i\xb7\x1d\xfb\xef\xff'
```

还可以自己定义`64`个字符的排列顺序, 这样就可以自定义`Base64`编码, 不过, 通常情况下完全没有必要.

`Base64`是一种通过查表的编码方法, 不能用于加密, 即使使用自定义的编码表也不行.
`Base64`适用于小段内容的编码, 比如数字证书签名, Cookie的内容等.
由于=字符也可能出现在`Base64`编码中, 但`=`用在URL, Cookie里面会造成歧义, 所以, 很多`Base64`编码后会把`=`去掉:

```python
# 标准Base64:
'abcd' -> 'YWJjZA=='
# 自动去掉=:
'abcd' -> 'YWJjZA'
```

去掉`=`后怎么解码呢? 因为`Base64`是把3个字节变为4个字节, 所以, `Base64`编码的长度永远是`4`的倍数,
因此, 需要加上`=`把`Base64`字符串的长度变为4的倍数, 就可以正常解码了.

## 小结-base64

Base64是一种任意二进制到文本字符串的编码方法, 常用于在URL, Cookie, 网页中传输少量二进制数据.

## 练习-base64

请写一个能处理去掉`=`的`base64`解码函数:

以`Unicode`表示的`str`通过`encode()`方法可以编码为指定的`bytes`, 例如:

```python
>>> 'ABC'.encode('ascii')
b'ABC'
>>> '中文'.encode('utf-8')
b'\xe4\xb8\xad\xe6\x96\x87'
>>> '中文'.encode('ascii')
```

```python
# -*- coding: utf-8 -*-
import base64

def safe_base64_decode(s):
    eq_length=4-(len(s)%4)
    full_byte=s+b'='*eq_length
    return base64.b64decode(full_byte)

# 测试:
assert b'abcd' == safe_base64_decode(b'YWJjZA=='), safe_base64_decode('YWJjZA==')
assert b'abcd' == safe_base64_decode(b'YWJjZA'), safe_base64_decode('YWJjZA')
print('ok')
```
