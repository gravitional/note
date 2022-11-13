# 网易云音乐 ncm 格式解析

[网易云音乐ncm文件格式解析 ](http://www.bewindoweb.com/228.html)

## 前言

做了好久的心理建设鼓起勇气花了8块钱充了网易云音乐一个月会员,
准备下载一些歌到ipod上听, 下下来的却是:

![ncm](http://cdn.bewindoweb.com/uploadpic/e7f8b2c1484a01093bceee5a0bf7ddb5.png)

喵喵喵?充钱下了个加密文件, 是心梗的感觉,
参考知乎[<如何评价网易云音乐的ncm格式?>](https://www.zhihu.com/question/265966692).
开始搜转码吧, 可是总会好奇到底是怎么转的, 所以一步一步debug,
下载各种工具去查看, 大概弄懂了一些.
而且和实习的时候研究的网易云音乐前端JS加密一样,
猪厂真的很喜欢用 `AES` 和 `RSA` 加密方式,
而且很喜欢对数据加密之后再把它的密钥给加密……
首先贴上代码和软件, 如果不感兴趣可以直接去下载使用就好了:

1. [anonymous5l/ncmdump](https://github.com/anonymous5l/ncmdump) (C++, MIT协议)
    基于openssl库编写, 所以速度非常快, 而且又好.
1. [nondanee/ncmdump](https://github.com/nondanee/ncmdump)(python, MIT协议)
    依赖pycryptodome库, mutagen库, 比较完善了.
1. [lianglixin/ncmdump](https://github.com/lianglixin/ncmdump)(python, MIT协议)
    ![ncmdump](http://cdn.bewindoweb.com/uploadpic/b486b94805b6247db20ec176a7356baf.png)
    fork的nondanee作者的源码(开始没有注意以为是独立提交的),
    修改了依赖库依赖pycrypto库, 会有一些安装和使用问题:
1. windows GUI 直接运行的EXE文件:
项目名和源码下载地址  百度云下载地址   说明

+ [yoki123 / ncmdump](https://github.com/yoki123/ncmdump)   [点此下载](https://pan.baidu.com/s/17-qGDe1Pvs-1jH8DqOJEUw)(提取码: 92ib)  批量的
+ [anonymous5l / ncmdump-gui](https://github.com/anonymous5l/ncmdump-gui)  [点此下载](https://pan.baidu.com/s/1zaS-hKwV-ywBGSn2BkdgWg)(提取码:  fffn)  大佬原生程序
+ 未知  [点此下载](https://pan.baidu.com/s/1e-CH-OImgizifrwNZLHTAw)(提取码:  xmd9)  只需要把ncm拖进main.exe就可以了

写到这里, 默默大喊一句, 开源大法好!

## NCM文件格式和解密代码分析

由于第一个搜索到的项目是 [lianglixin/ncmdump](https://github.com/lianglixin/ncmdump),
所以我们以lianglinxin作者的python代码为基础来进行分析,
参考nondanee作者的代码注释来理清思路. 测试的ncm文件是[郭顶 - 水星记.ncm](https://pan.baidu.com/s/1ii_1IqIYFzgu2i43S7KCDA)(提取码: 5ua0),
字节查看器为UltraEdit.

整个文件就一个函数 `dump(file_path)`, 下面进行分段分析.

+ 1

```python
core_key = binascii.a2b_hex("687A4852416D736F356B496E62617857")
meta_key = binascii.a2b_hex("2331346C6A6B5F215C5D2630553C2728")
unpad = lambda s : s[0:-(s[-1] if type(s[-1]) == int else ord(s[-1]))]
f = open(file_path,'rb')
header = f.read(8)
assert binascii.b2a_hex(header) == b'4354454e4644414d'
```

定义了 `core_key` 和 `meta_key`,
`binascii.a2b_hex` 的意思就是把这个 `字符串` 按照 `十六进制`, 反解析为 `二进制字节序列`(bytes类型),
可以用 `ascii` 字符来表示, `b2a` 则进行相反操作.
如果对 `ascii码` 不熟悉可以查表, 比如 `0x68=h`, `0x7A=z`, 所以:

+ 2

```python
core_key = b'hzHRAmso5kInbaxW'
meta_key = b"#14ljk_!\\]&0U<'("
```

然后定义了一个lamda表达式(内嵌函数)`unpad`.
打开了 `ncm` 文件并读取了 `8` 个 byte, 确认这8个字节是否是字节序列 `b'4354454e4644414d'`,
用 `UltraEdit` 查看 `ncm` 文件, 发现这些字节是 `'CTENFDAM'`,
`0x43=C`, 说明这些就是 `ncm` 独有的文件标记, 就是通俗所谓的 `magic`.

+ 3

```python
f.seek(2, 1)
```

然后从当前位置跳过了 `2个字节`, 这两个字节是 `0x01 0x70`,
而且打开几个 `ncm` 文件都是一样的值, 为什么它不能直接读 `10` 个字节的magic呢?
可能代表不同的含义吧, 暂时不管.

+ 4

```python
key_length = f.read(4)
key_length = struct.unpack('<I', bytes(key_length))[0]
key_data = f.read(key_length)
key_data_array = bytearray(key_data)
for i in range (0,len(key_data_array)): key_data_array[i] ^= 0x64
key_data = bytes(key_data_array)
cryptor = AES.new(core_key, AES.MODE_ECB)
key_data = unpad(cryptor.decrypt(key_data))[17:]
key_length = len(key_data)
```

获取了 `4字节` 的 `key长度`, 并且按照 `小端`(<)的方式转为 `整型`(I).
(`高字节` 保存在内存的 `高地址` 中, 而数据的 `低字节` 保存在内存的 `低地址` 中, 也就是 `反序`)
这4个字节是: `0x80 0x00 0x00 0x00`, 所以反序再转换就是 `0x00000080 = 128`.
读取`128` 个字节的 `key` 数据, 并转为字符数组, 每个字节和 `0x64` 进行 `异或`,
还不清楚这个异或是为了什么目的.

然后用之前的 `core_key` 创建了 `AES_ECB`(Electronic Codebook Book, 电码本模式)的解密器.
`ECB` 模式是将整个明文分成若干段相同的小段, 然后对每一小段进行加密, 如果不足则会进行补足.

`cryptor.decrypt(key_data)` 解析出来的是:

```python
b'neteasecloudmusic10073261712832E7fT49x7dof9OKCgg9cdvhEuezy3iZCL1nFvBFd1T4uSktAJKmwZXsijPbijliionVUXXg9plTbXEclAE9Lb\r\r\r\r\r\r\r\r\r\r\r\r\r'
```

而应用 `unpad` 的 `lamda` 表达式之后, 末尾的 `\r` 就去掉了,
并且去掉了开头的标记符号, `NCM` 的大名 `Netease Cloud Music`:

```python
key_data = b'10073261712832E7fT49x7dof9OKCgg9cdvhEuezy3iZCL1nFvBFd1T4uSktAJKmwZXsijPbijliionVUXXg9plTbXEclAE9Lb'
```

在最后更新了一下 `key` 的长度, 更新为了 `128-13-17=98`.

+ 5

```python
key_data = bytearray(key_data)
key_box = bytearray(range(256))
c = 0
last_byte = 0
key_offset = 0
for i in range(256):
    swap = key_box[i]
    c = (swap + last_byte + key_data[key_offset]) & 0xff
    key_offset += 1
    if key_offset >= key_length: key_offset = 0
    key_box[i] = key_box[c]
    key_box[c] = swap
    last_byte = c
```

上面这部分是标准RC4-KSA算法(Key-scheduling algorithm)去计算 `S-box`.

+ 6

```python
meta_length = f.read(4)
meta_length = struct.unpack('<I', bytes(meta_length))[0]
meta_data = f.read(meta_length)
meta_data_array = bytearray(meta_data)
for i in range(0,len(meta_data_array)): meta_data_array[i] ^= 0x63
meta_data = bytes(meta_data_array)
meta_data = base64.b64decode(meta_data[22:])
cryptor = AES.new(meta_key, AES.MODE_ECB)
meta_data = unpad(cryptor.decrypt(meta_data)).decode('utf-8')[6:]
meta_data = json.loads(meta_data)
```

这部分和前面的 `key` 很相似, 读取 `4字节` 长度, 然后把数据进行 `异或`,
注意这里 `异或` 的是 `0x63`, 这个值怎么来的也不清楚.
接着发现 `meta_data` 的值是这样的:

```python
b"163 key(Don't modify):L64FU3W4YxX3ZFTmbZ+8/fOGFX4ZDFzRxiE6WTSCw8Wbw8yYSVQFmAmCHw9A96ZnO0UOuMsVWYFWvoqD0/YcH3r7VAGU8B3l+FBJm4JL6is23S2yXChnSbfLIksnEUcTC7JtrA1JAoR0GVnz+OT3hGTJRsjGIVQXg2yide/YKBACffE+oYBApqZ5Isq0n7h/MlBnjn6ihuSlIl5V2rXEjSISQr031eSBdEVJ/JcwttzLafIPBh2FQfaVd/U0inWY5jxCXZCw/jxcIdGmGH/0Oft3UlNPt2kDBrsivoVuD03tMWL6A5Flg/jCbofSOblHFC79oU3WF9doUjD24BXuu6K7wyoWkgyG7SJu8tk72hkGw3rLK1nbTHsSEIPjocC6Ba9mzF48SB087MFTSn+9PXPZIboMXFXGI3TpMj4rR6cD+6CEWS7EoZrUC1cipi/A0jT/rFtAirM4hmkbrvslJumMHDJz1q9o6t3XRWydyoIaC3ktXuesyV8sbuoQ+Y/EMWNZRN3KhGR/jnnQPBtseQ=="
```

前面有 `22` 位的 `163 key(Don't modify):`, 去掉之后用 `base64` 解码,
并同样地通过 `AES_ECB` 和 `meta_key` 进行解密:

```python
b'music:{"musicId":441491828,"musicName":"\xe6\xb0\xb4\xe6\x98\x9f\xe8\xae\xb0","artist":[["\xe9\x83\xad\xe9\xa1\xb6",2843]],"albumId":35005583,"album":"\xe9\xa3\x9e\xe8\xa1\x8c\xe5\x99\xa8\xe7\x9a\x84\xe6\x89\xa7\xe8\xa1\x8c\xe5\x91\xa8\xe6\x9c\x9f","albumPicDocId":2946691248081599,"albumPic":"https://p4.music.126.net/wSMfGvFzOAYRU_yVIfquAA==/2946691248081599.jpg","bitrate":320000,"mp3DocId":"668809cf9ba99c3b7cc51ae17a66027f","duration":325266,"mvId":5404031,"alias":[],"transNames":[],"format":"mp3"}\r\r\r\r\r\r\r\r\r\r\r\r\r'
```

于是这个作者去掉了前面的 `music:`, 然后转为了 `json` 字典:

+ 7

```python
crc32 = f.read(4)
crc32 = struct.unpack('<I', bytes(crc32))[0]
f.seek(5, 1)
```

这是 `CRC32` 校验码, 以及 `5` 个不知道为什么跳过的字符.

+ 8

```python
image_size = f.read(4)
image_size = struct.unpack('<I', bytes(image_size))[0]
image_data = f.read(image_size)
```

这是封面的图像数据.

+ 9

```python
file_name = meta_data['musicName'] + '.' + meta_data['format']
    m = open(os.path.join(os.path.split(file_path)[0],file_name),'wb')
    chunk = bytearray()
    while True:
        chunk = bytearray(f.read(0x8000))
        chunk_length = len(chunk)
        if not chunk:
            break
        for i in range(1,chunk_length+1):
            j = i & 0xff;
            chunk[i-1] ^= key_box[(key_box[j] + key_box[(key_box[j] + j) & 0xff]) & 0xff]
        m.write(chunk)
    m.close()
    f.close()
```

这部分是用修改后的 `RC4-PRGA` 算法(Pseudo-random generation algorithm)进行还原并输出成文件,
这是MP3的原本数据.

原本故事到这里就结束了, 然而发现输出的文件和另一个用GUI程序输出的文件不一样呢:

![cover](http://cdn.bewindoweb.com/uploadpic/e01c339bd1627f6374177d8d9941450f.png)

竟然木有封面……用MP3tag比较一下, 其他信息都全, 就是图片没有啊:

![cover2](http://cdn.bewindoweb.com/uploadpic/a2630113b3b3d9f143d19a6cc7ce39b7.png)

于是用 `eyed3` 库添加 `image_data` 进去, 查了半天源码, 终于找到合适的方法:

```python
audiofile = eyed3.load(u"E:\\CloudMusic\\3.mp3")
audiofile.tag.images.set(0x06, image_data, 'image/jpeg')
audiofile.tag.save()
```

这样就有封面啦. [nondanee/ncmdump][] 作者也发现了这个问题, 并且也是手动添加的 `image_data` 的 `tag` 数据:

![image_data](http://cdn.bewindoweb.com/uploadpic/e2168a3fde644ce514aaaf12cc5ba2e8.png)

不过python还是很慢的, 以后还是用C++那个程序比较好.

[nondanee/ncmdump]: https://github.com/nondanee/ncmdump
