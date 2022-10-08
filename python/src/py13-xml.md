# XML

`XML`虽然比`JSON`复杂, 在`Web`中应用也不如以前多了, 不过仍有很多地方在用, 所以, 有必要了解如何操作`XML`.

## DOM vs SAX

操作`XML`有两种方法: `DOM`和`SAX`. `DOM`会把整个XML读入内存, 解析为树, 因此占用内存大, 解析慢, 优点是可以任意遍历树的节点. `SAX`是流模式, 边读边解析, 占用内存小, 解析快, 缺点是我们需要自己处理事件.

正常情况下, 优先考虑`SAX`, 因为`DOM`实在太占内存.

在Python中使用`SAX`解析`XML`非常简洁, 通常我们关心的事件是`start_element`, `end_element`和`char_data`, 准备好这`3`个函数, 然后就可以解析`xml`了.

举个例子, 当`SAX`解析器读到一个节点时:

```python
<a href="/">python</a>
```

会产生3个事件:

+ `start_element`事件, 在读取`<a href="/">`时;
+ `char_data`事件, 在读取`python`时;
+ `end_element`事件, 在读取`</a>`时.

用代码实验一下:

```python
from xml.parsers.expat import ParserCreate

class DefaultSaxHandler(object):
    def start_element(self, name, attrs):
        print('sax:start_element: %s, attrs: %s' % (name, str(attrs)))

    def end_element(self, name):
        print('sax:end_element: %s' % name)

    def char_data(self, text):
        print('sax:char_data: %s' % text)

xml = r'''<?xml version="1.0"?>
<ol>
    <li><a href="/python">Python</a></li>
    <li><a href="/ruby">Ruby</a></li>
</ol>
'''

handler = DefaultSaxHandler()
parser = ParserCreate()
parser.StartElementHandler = handler.start_element
parser.EndElementHandler = handler.end_element
parser.CharacterDataHandler = handler.char_data
parser.Parse(xml)
```

需要注意的是读取一大段字符串时, `CharacterDataHandler`可能被多次调用, 所以需要自己保存起来, 在`EndElementHandler`里面再合并.

除了解析`XML`外, 如何生成`XML`呢? 99%的情况下需要生成的`XML`结构都是非常简单的, 因此, 最简单也是最有效的生成`XML`的方法是拼接字符串:

```python
L = []
L.append(r'<?xml version="1.0"?>')
L.append(r'<root>')
L.append(encode('some & data'))
L.append(r'</root>')
return ''.join(L)
```

如果要生成复杂的`XML`呢? 建议你不要用`XML`, 改成`JSON`.

## 小结-xml

解析`XML`时, 注意找出自己感兴趣的节点, 响应事件时, 把节点数据保存起来. 解析完毕后, 就可以处理数据.

## 练习-xml

请利用`SAX`编写程序解析`Yahoo`的`XML`格式的天气预报, 获取天气预报:

`https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20weather.forecast%20where%20woeid%20%3D%202151330&format=xml`

参数`woeid`是城市代码, 要查询某个城市代码, 可以在`weather.yahoo.com`搜索城市, 浏览器地址栏的`URL`就包含城市代码.

```python
# -*- coding:utf-8 -*-

from xml.parsers.expat import ParserCreate
from urllib import request

def parseXml(xml_str):
    print(xml_str)
    return {
        'city': '?',
        'forecast': [
            {
                'date': '2017-11-17',
                'high': 43,
                'low' : 26
            },
            {
                'date': '2017-11-18',
                'high': 41,
                'low' : 20
            },
            {
                'date': '2017-11-19',
                'high': 43,
                'low' : 19
            }
        ]
    }

# 测试:
URL = 'https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20weather.forecast%20where%20woeid%20%3D%202151330&format=xml'

with request.urlopen(URL, timeout=4) as f:
    data = f.read()

result = parseXml(data.decode('utf-8'))
assert result['city'] == 'Beijing'
```

## 总结-2

[看了三遍, 刚开始学xml, 大概有了点想法. ][]

[看了三遍, 刚开始学xml, 大概有了点想法. ]: https://www.liaoxuefeng.com/discuss/969955749132672/1303613376823330

首先, 如果看不明白他给的这个例子且不明白什么是`xml`的话, 可以先百度一下`xml`的格式和语法规则.

可以看看这个教程: `https://blog.csdn.net/sinat_29830917/article/details/70241786`

我理解的是`ParserCreat()`返回一个`ParserCreat`对象, 他有`StartElementHandler`, `EndElementHandler`, `CharacterDataHandler`三个成员函数,
但需要特别注意的是: 这三个函数均为`CallBack`类型的,
`ParserCreat`对象在创建时只是会提供三个空指针, 需要我们自己去绑定相应的具体函数.

>函数调用一般分为两种, 一种是主调, 即编写代码者, 自己调用的函数, 还一种为`Callback`函数, 编码者写好, 但他自己却不主动调用, 而是在某些条件下(编写者并不清楚具体时间和流程), 由其他函数调用.
>简单的比如设备驱动, 操作系统提供了某组设备的函数指针, 比如LCD屏驱动, 由一组画点, 画线, 画块函数组成, 当更换LCD时, 只需要把操作系统开放的函数指针, 指向新的接口即可, 操作系统再需要时, 会自动调用新的驱动函数

我们通过`ParserCreat`对象中的`Parse()`函数传入一个`xml`, 然后就是上文中写到的:

当`SAX`解析器读到一个节点时:

```xml
<a href="/">python</a>
```

会产生3个事件:

+ start_element事件, 在读取`<a href="/">`时;
+ char_data事件, 在读取`python`时;
+ end_element事件, 在读取`</a>`时.

SAX解析器会帮我们解析好这个`xml`语句, 并且准备好我们写的三个函数, 即`StartElementHandler`, `EndElementHandler`, `CharacterDataHandler`所需要的全部参数, 然后进行调用.

其中, `start_element`事件中的参数`attrs`指的是标签的属性.

把代码改一下或许可以舒服些:

```python
from xml.parsers.expat import ParserCreate

def start_element( name, attrs):
    print('sax:start_element: %s, attrs: %s' % (name, str(attrs)))

def end_element( name):
    print('sax:end_element: %s' % name)

def char_data( text):
    print('sax:char_data: %s' % text.replace(' ', '.').replace('\n', r'\n'))

xml = r'''<?xml version="1.0" encoding="utf-8"?>
<!--声明内部dTD-->
<!DOCTYPE students[
    <!ELEMENT students (student*) >
    <!ELEMENT students (name,school) >
    <!ATTLIST student id CDATA #REQUIRED >
    <!ELEMENT name (#PCDATA) >
    <!ELEMENT school (#PCDATA) >
]>
<students>
    <student id = "01">
        <name>CSL</name>
        <school>NUPT</school>
    </student>
    <student id = "02">
        <name>CSLB</name>
        <school>NUPT</school>
    </student>
</students>
'''
parser = ParserCreate()
parser.StartElementHandler = start_element
parser.EndElementHandler = end_element
parser.CharacterDataHandler = char_data
parser.Parse(xml)
```

输出:

```python
sax:start_element: students, attrs: {}
sax:char_data: \n
...
sax:end_element: students
[Finished in 0.2s]
```

# HTMLParser

如果我们要编写一个搜索引擎, 第一步是用爬虫把目标网站的页面抓下来, 第二步就是解析该`HTML`页面, 看看里面的内容到底是新闻, 图片还是视频.

假设第一步已经完成了, 第二步应该如何解析`HTML`呢?

`HTML`本质上是XML的子集, 但是`HTML`的语法没有XML那么严格, 所以不能用标准的DOM或SAX来解析`HTML`.

好在Python提供了`HTMLParser`来非常方便地解析`HTML`, 只需简单几行代码:

```python
from html.parser import HTMLParser
from html.entities import name2codepoint

class MyHTMLParser(HTMLParser):

    def handle_starttag(self, tag, attrs):
        print('<%s>' % tag)

    def handle_endtag(self, tag):
        print('</%s>' % tag)

    def handle_startendtag(self, tag, attrs):
        print('<%s/>' % tag)

    def handle_data(self, data):
        print(data)

    def handle_comment(self, data):
        print('<!--', data, '-->')

    def handle_entityref(self, name):
        print('&%s;' % name)

    def handle_charref(self, name):
        print('&#%s;' % name)

parser = MyHTMLParser()
parser.feed('''<html>
<head></head>
<body>
<!-- test html parser -->
    <p>Some <a href=\"#\">html</a> HTML&nbsp;tutorial...<br>END</p>
</body></html>''')
```

`feed()`方法可以多次调用, 也就是不一定一次把整个`HTML`字符串都塞进去, 可以一部分一部分塞进去.

特殊字符有两种, 一种是英文表示的`&nbsp`(&nbsp);, 一种是数字表示的`&#1234`(&#1234);, 这两种字符都可以通过Parser解析出来.

## 小结-htmlparser

利用`HTMLParser`, 可以把网页中的文本, 图像等解析出来.

## 练习-htmlparser

找一个网页, 例如`https://www.python.org/events/python-events/`,
用浏览器查看源码并复制, 然后尝试解析一下HTML,
输出Python官网发布的会议时间, 名称和地点.
