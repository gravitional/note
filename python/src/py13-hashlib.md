# hashlib-哈希算法

摘要算法简介

Python的hashlib提供了常见的摘要算法, 如`MD5`, `SHA1`等等.

什么是摘要算法呢? 摘要算法又称哈希算法, 散列算法. 它通过一个函数,
把任意长度的数据转换为一个长度固定的数据串(通常用16进制的字符串表示).

举个例子, 你写了一篇文章, 内容是一个字符串`'how to use python hashlib - by Michael'`,
并附上这篇文章的摘要是`'2d73d4f15c0db7f5ecb321b6a65e5d6d'`. 如果有人篡改了你的文章,
并发表为`'how to use python hashlib - by Bob'`, 你可以一下子指出Bob篡改了你的文章,
因为根据`'how to use python hashlib - by Bob'`计算出的摘要不同于原始文章的摘要.
可见, 摘要算法就是通过摘要函数f()对任意长度的数据`data`计算出固定长度的摘要`digest`,
目的是为了发现原始数据是否被人篡改过.

摘要算法之所以能指出数据是否被篡改过, 就是因为摘要函数是一个单向函数, 计算f(`data`)很容易,
但通过`digest`反推`data`却非常困难. 而且, 对原始数据做一个`bit`的修改, 都会导致计算出的摘要完全不同.

我们以常见的摘要算法MD5为例, 计算出一个字符串的`MD5`值:

```python
import hashlib

md5 = hashlib.md5()
md5.update('how to use md5 in python hashlib?'.encode('utf-8'))
print(md5.hexdigest())
```

计算结果如下:

```python
d26a53750bc40b38b65a520292f69306
```

如果数据量很大, 可以分块多次调用`update()`, 最后计算的结果是一样的:

```python
import hashlib

md5 = hashlib.md5()
md5.update('how to use md5 in '.encode('utf-8'))
md5.update('python hashlib?'.encode('utf-8'))
print(md5.hexdigest())
```

试试改动一个字母, 看看计算的结果是否完全不同.

`MD5`是最常见的摘要算法, 速度很快, 生成结果是固定的`128 bit`字节,
通常用一个`32`位的`16`进制字符串表示.

另一种常见的摘要算法是`SHA1`, 调用`SHA1`和调用MD5完全类似:

```python
import hashlib

sha1 = hashlib.sha1()
sha1.update('how to use sha1 in '.encode('utf-8'))
sha1.update('python hashlib?'.encode('utf-8'))
print(sha1.hexdigest())
```

`SHA1`的结果是`160 bit`字节, 通常用一个40位的16进制字符串表示.
比`SHA1`更安全的算法是`SHA256`和`SHA512`, 不过越安全的算法不仅越慢, 而且摘要长度更长.

有没有可能两个不同的数据通过某个摘要算法得到了相同的摘要?
完全有可能, 因为任何摘要算法都是把无限多的数据集合映射到一个有限的集合中.
这种情况称为碰撞, 比如Bob试图根据你的摘要反推出一篇文章`'how to learn hashlib in python - by Bob'`,
并且这篇文章的摘要恰好和你的文章完全一致, 这种情况也并非不可能出现, 但是非常非常困难.

## 摘要算法应用

摘要算法能应用到什么地方? 举个常用例子:

任何允许用户登录的网站都会存储用户登录的用户名和口令.
如何存储用户名和口令呢? 方法是存到数据库表中:

| name | password |
| ----- | ----- |
| michael | 123456 |
| bob | abc999 |
| alice | alice2008 |

如果以明文保存用户口令, 如果数据库泄露, 所有用户的口令就落入黑客的手里.
此外, 网站运维人员是可以访问数据库的, 也就是能获取到所有用户的口令.

正确的保存口令的方式是不存储用户的明文口令, 而是存储用户口令的摘要, 比如`MD5`:

| username | password |
| ----- | ----- |
| `michael` | `e10adc3949ba59abbe56e057f20f883e` |
| `bob` | `878ef96e86145580c38c87f0410ad153` |
| `alice` | `99b1c2188db85afee403b1536010c2c9` |

当用户登录时, 首先计算用户输入的明文口令的`MD5`,
然后和数据库存储的MD5对比,
如果一致, 说明口令输入正确, 如果不一致, 口令肯定错误.

## 练习-hashlib

根据用户输入的口令, 计算出存储在数据库中的`MD5`口令:

```python
def calc_md5(password):
    pass
```

存储`MD5`的好处是即使运维人员能访问数据库, 也无法获知用户的明文口令.
设计一个验证用户登录的函数, 根据用户输入的口令是否正确, 返回`True`或`False`:

```python
# -*- coding: utf-8 -*-
import hashlib

db = {
    'michael': 'e10adc3949ba59abbe56e057f20f883e',
    'bob': '878ef96e86145580c38c87f0410ad153',
    'alice': '99b1c2188db85afee403b1536010c2c9'
}

def login(user, password):
    md5_new = hashlib.md5()
    md5_new.update(str(password).encode('utf-8'))
    return md5_new.hexdigest()==db[str(user)]

# 测试:
assert login('michael', '123456')
assert login('bob', 'abc999')
assert login('alice', 'alice2008')
assert not login('michael', '1234567')
assert not login('bob', '123456')
assert not login('alice', 'Alice2008')
print('ok')
```

采用`MD5`存储口令是否就一定安全呢? 也不一定. 假设你是一个黑客,
已经拿到了存储`MD5`口令的数据库, 如何通过`MD5`反推用户的明文口令呢?
暴力破解费事费力, 真正的黑客不会这么干.

考虑这么个情况, 很多用户喜欢用`123456`, `888888`, `password`这些简单的口令,
于是, 黑客可以事先计算出这些常用口令的`MD5`值, 得到一个反推表:

```python
'e10adc3949ba59abbe56e057f20f883e': '123456'
'21218cca77804d2ba1922c33e0151105': '888888'
'5f4dcc3b5aa765d61d8327deb882cf99': 'password'
```

这样, 无需破解, 只需要对比数据库的`MD5`, 黑客就获得了使用常用口令的用户账号.
对于用户来讲, 当然不要使用过于简单的口令. 但是, 我们能否在程序设计上对简单口令加强保护呢?
由于常用口令的`MD5`值很容易被计算出来,
所以要确保存储的用户口令不是那些已经被计算出来的常用口令的`MD5`,
这一方法通过对原始口令加一个复杂字符串来实现, 俗称"加盐":

```python
def calc_md5(password):
    return get_md5(password + 'the-Salt')
```

经过`Salt`处理的MD5口令, 只要`Salt`不被黑客知道, 即使用户输入简单口令,
也很难通过`MD5`反推明文口令.但是如果有两个用户都使用了相同的简单口令比如`123456`,
在数据库中, 将存储两条相同的`MD5`值, 这说明这两个用户的口令是一样的.
有没有办法让使用相同口令的用户存储不同的`MD5`呢?

如果假定用户无法修改登录名, 就可以通过把登录名作为`Salt`的一部分来计算`MD5`,
从而实现相同口令的用户也存储不同的`MD5`.

## 练习-hashlib-2

根据用户输入的登录名和口令模拟用户注册, 计算更安全的`MD5`:

```python
db = {}

def register(username, password):
    db[username] = get_md5(password + username + 'the-Salt')
```

然后, 根据修改后的`MD5`算法实现用户登录的验证:

```python
# -*- coding: utf-8 -*-
import hashlib, random

def get_md5(s):
    return hashlib.md5(s.encode('utf-8')).hexdigest()

class User(object):
    def __init__(self, username, password):
        self.username = username
        self.salt = ''.join([chr(random.randint(48, 122)) for i in range(20)])
        self.password = get_md5(password + self.salt)
db = {
    'michael': User('michael', '123456'),
    'bob': User('bob', 'abc999'),
    'alice': User('alice', 'alice2008')
}
```

```python
# -*- coding: utf-8 -*-
import hashlib, random

def get_md5(s):
    return hashlib.md5(s.encode('utf-8')).hexdigest()

class User(object):
    def __init__(self, username, password):
        self.username = username
        self.salt = ''.join([chr(random.randint(48, 122)) for i in range(20)])
        self.password = get_md5(password + self.salt)

db = {
    'michael': User('michael', '123456'),
    'bob': User('bob', 'abc999'),
    'alice': User('alice', 'alice2008')
}

def login(username, password):
    user = db[username]
    return user.password == get_md5(password+user.salt)

# 测试:
assert login('michael', '123456')
assert login('bob', 'abc999')
assert login('alice', 'alice2008')
assert not login('michael', '1234567')
assert not login('bob', '123456')
assert not login('alice', 'Alice2008')
print('ok')
```

# hmac-赋key哈希算法

通过哈希算法, 我们可以验证一段数据是否有效, 方法就是对比该数据的哈希值,
例如, 判断用户口令是否正确, 我们用保存在数据库中的 `password_md5` 对比计算md5(password)的结果,
如果一致, 用户输入的口令就是正确的.

为了防止黑客通过彩虹表根据哈希值反推原始口令, 在计算哈希的时候,
不能仅针对原始输入计算, 需要增加一个salt来使得相同的输入也能得到不同的哈希,
这样大大增加了黑客破解的难度.

如果salt是我们自己随机生成的, 通常我们计算MD5时采用md5(message + salt).
但实际上, 把salt看做一个"口令", 加salt的哈希就是: 计算一段message的哈希时,
根据不通口令计算出不同的哈希. 要验证哈希值, 必须同时提供正确的口令.

这实际上就是Hmac算法: Keyed-Hashing for Message Authentication.
它通过一个标准算法, 在计算哈希的过程中, 把key混入计算过程中.

和我们自定义的加salt算法不同, Hmac算法针对所有哈希算法都通用, 无论是MD5还是SHA-1.
采用Hmac替代我们自己的salt算法, 可以使程序算法更标准化, 也更安全.

Python自带的hmac模块实现了标准的Hmac算法. 我们来看看如何使用hmac实现带key的哈希.

我们首先需要准备待计算的原始消息message, 随机key, 哈希算法, 这里采用MD5, 使用hmac的代码如下:

```python
import hmac
message = b'Hello, world!'
key = b'secret'
h = hmac.new(key, message, digestmod='MD5')
# 如果消息很长, 可以多次调用h.update(msg)
h.hexdigest()
#>> 'fa4ee7d173f2d97ee79022d1a7355bcf'
```

可见使用hmac和普通hash算法非常类似. hmac输出的长度和原始哈希算法的长度一致.
需要注意传入的key和message都是bytes类型, str类型需要首先编码为bytes.

## 练习-hmac

将上一节的`salt`改为标准的`hmac`算法, 验证用户口令:

```python
# -*- coding: utf-8 -*-
import hmac, random

def hmac_md5(key, s):
    return hmac.new(key.encode('utf-8'), s.encode('utf-8'), 'MD5').hexdigest()

class User(object):
    def __init__(self, username, password):
        self.username = username
        self.key = ''.join([chr(random.randint(48, 122)) for i in range(20)])
        self.password = hmac_md5(self.key, password)

db = {
    'michael': User('michael', '123456'),
    'bob': User('bob', 'abc999'),
    'alice': User('alice', 'alice2008')
}

def login(username, password):
    user = db[username]
    return user.password == hmac_md5(user.key, password)

# 测试:
assert login('michael', '123456')
assert login('bob', 'abc999')
assert login('alice', 'alice2008')
assert not login('michael', '1234567')
assert not login('bob', '123456')
assert not login('alice', 'Alice2008')
print('ok')
```

## 小结-hmac

Python内置的`hmac`模块实现了标准的`Hmac`算法,
它利用一个`key`对message计算"杂凑"后的`hash`,
使用`hmac`算法比标准`hash`算法更安全, 因为针对相同的message, 不同的key会产生不同的`hash`.

>注:

ASCII 码表, 十进制编码  `48` 到 `122`, 包括了数字, 大小写字母, 以及其它符号.
从数字 `0` 开始, 到小写字母 `z` 结束.
还包括了

```cpp
:   ;   <   =   >   ?   @   [      \     ]  ^   _   `
```
