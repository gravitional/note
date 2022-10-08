# urllib-URL操作

`urllib`提供了一系列用于操作URL的功能.

## Get

`urllib`的`request`模块可以非常方便地抓取`URL`内容,
也就是发送一个`GET`请求到指定的页面, 然后返回`HTTP`的响应:
例如, 对[豆瓣的一个URL][] 进行抓取, 并返回响应:

[豆瓣的一个URL]: https://api.douban.com/v2/book/2129650

```python
from urllib import request

with request.urlopen('https://api.douban.com/v2/book/2129650') as f:
    data = f.read()
    print('Status:', f.status, f.reason)
    for k, v in f.getheaders():
        print('%s: %s' % (k, v))
    print('Data:', data.decode('utf-8'))
```

可以看到`HTTP`响应的头和`JSON`数据:

```python
Status: 200 OK
Server: nginx
Date: Tue, 26 May 2015 10:02:27 GMT
Content-Type: application/json; charset=utf-8
Content-Length: 2049
Connection: close
Expires: Sun, 1 Jan 2006 01:00:00 GMT
Pragma: no-cache
Cache-Control: must-revalidate, no-cache, private
X-DAE-Node: pidl1
Data: {"rating":{"max":10,"numRaters":16,"average":"7.4","min":0},"subtitle":"","author":["廖雪峰编著"],"pubdate":"2007-6",...}
```

如果我们要想模拟浏览器发送`GET`请求, 就需要使用`Request`对象, 通过往`Request`对象添加`HTTP`头, 我们就可以把请求伪装成浏览器.
例如, 模拟`iPhone 6`去请求豆瓣首页:

```python
from urllib import request

req = request.Request('http://www.douban.com/')
req.add_header('User-Agent', 'Mozilla/6.0 (iPhone; CPU iPhone OS 8_0 like Mac OS X) AppleWebKit/536.26 (KHTML, like Gecko) Version/8.0 Mobile/10A5376e Safari/8536.25')
with request.urlopen(req) as f:
    print('Status:', f.status, f.reason)
    for k, v in f.getheaders():
        print('%s: %s' % (k, v))
    print('Data:', f.read().decode('utf-8'))
```

这样豆瓣会返回适合`iPhone`的移动版网页:

```python
    <meta name="viewport" content="width=device-width, user-scalable=no, initial-scale=1.0, minimum-scale=1.0, maximum-scale=1.0">
    <meta name="format-detection" content="telephone=no">
    <link rel="apple-touch-icon" sizes="57x57" href="http://img4.douban.com/pics/cardkit/launcher/57.png" />
```

## Post

如果要以`POST`发送一个请求, 只需要把参数`data`以`bytes`形式传入.

我们模拟一个微博登录, 先读取登录的邮箱和口令,
然后按照`weibo.cn`的登录页的格式以`username=xxx&password=xxx`的编码传入:

```python
from urllib import request, parse

print('Login to weibo.cn...')
email = input('Email: ')
passwd = input('Password: ')
login_data = parse.urlencode([
    ('username', email),
    ('password', passwd),
    ('entry', 'mweibo'),
    ('client_id', ''),
    ('savestate', '1'),
    ('ec', ''),
    ('pagerefer', 'https://passport.weibo.cn/signin/welcome?entry=mweibo&r=http%3A%2F%2Fm.weibo.cn%2F')
])

req = request.Request('https://passport.weibo.cn/sso/login')
req.add_header('Origin', 'https://passport.weibo.cn')
req.add_header('User-Agent', 'Mozilla/6.0 (iPhone; CPU iPhone OS 8_0 like Mac OS X) AppleWebKit/536.26 (KHTML, like Gecko) Version/8.0 Mobile/10A5376e Safari/8536.25')
req.add_header('Referer', 'https://passport.weibo.cn/signin/login?entry=mweibo&res=wel&wm=3349&r=http%3A%2F%2Fm.weibo.cn%2F')

with request.urlopen(req, data=login_data.encode('utf-8')) as f:
    print('Status:', f.status, f.reason)
    for k, v in f.getheaders():
        print('%s: %s' % (k, v))
    print('Data:', f.read().decode('utf-8'))
```

如果登录成功, 我们获得的响应如下:

```python
Status: 200 OK
Server: nginx/1.2.0
...
Set-Cookie: SSOLoginState=1432620126; path=/; domain=weibo.cn
...
Data: {"retcode":20000000,"msg":"","data":{...,"uid":"1658384301"}}
```

如果登录失败, 我们获得的响应如下:

```python
Data: {"retcode":50011015,"msg":"\u7528\u6237\u540d\u6216\u5bc6\u7801\u9519\u8bef","data":{"username":"example@python.org","errline":536}}
```

## Handler

如果还需要更复杂的控制, 比如通过一个Proxy去访问网站, 我们需要利用`ProxyHandler`来处理, 示例代码如下:

```python
proxy_handler = urllib.request.ProxyHandler({'http': 'http://www.example.com:3128/'})
proxy_auth_handler = urllib.request.ProxyBasicAuthHandler()
proxy_auth_handler.add_password('realm', 'host', 'username', 'password')
opener = urllib.request.build_opener(proxy_handler, proxy_auth_handler)
with opener.open('http://www.example.com/login.html') as f:
    pass
```

## 小结-urllib

`urllib`提供的功能就是利用程序去执行各种`HTTP`请求.
如果要模拟浏览器完成特定功能, 需要把请求伪装成浏览器.
伪装的方法是先监控浏览器发出的请求, 再根据浏览器的请求头来伪装,
`User-Agent`头就是用来标识浏览器的.

## 练习-urllib

利用`urllib`读取`JSON`, 然后将`JSON`解析为`Python`对象:

```python
# -*- coding: utf-8 -*-
from urllib import request
import json

def fetch_data(url):
    with request.urlopen(str(url)) as f:
        data = f.read().decode('utf-8')
        return json.loads(data)

# 测试
URL = 'https://yesno.wtf/api'
data = fetch_data(URL)
print(data)
assert data['answer']== 'yes' and (data['forced']== False)
print('ok')
```

`dumps()`方法返回一个`str`, 内容就是标准的`JSON`. 类似的, `dump()`方法可以直接把`JSON`写入一个file-like Object.

要把`JSON`反序列化为Python对象, 用`loads()`或者对应的`load()`方法, 前者把`JSON`的字符串反序列化, 后者从`file-like Object`中读取字符串并反序列化:
