# js_3

## jQuery

你可能听说过`jQuery`,它名字起得很土,但却是`JavaScript`世界中使用最广泛的一个库.
江湖传言,全世界大约有`80~90%`的网站直接或间接地使用了`jQuery`.
鉴于它如此流行,又如此好用,所以每一个入门`JavaScript`的前端工程师都应该了解和学习它.

`jQuery`这么流行,肯定是因为它解决了一些很重要的问题.实际上,`jQuery`能帮我们干这些事情: 

+ 消除浏览器差异: 你不需要自己写冗长的代码来针对不同的浏览器来绑定事件,编写`AJAX`等代码;
+ 简洁的操作`DOM`的方法: 写`$('#test')`肯定比`document.getElementById('test')`来得简洁;
+ 轻松实现动画,修改`CSS`等各种操作.

`jQuery`的理念`Write Less, Do More`,让你写更少的代码,完成更多的工作!

### jQuery版本

目前`jQuery`有`1.x`和`2.x`两个主要版本,区别在于`2.x`移除了对古老的`IE 6,7,8`的支持,因此`2.x`的代码更精简.选择哪个版本主要取决于你是否想支持IE 6~8.

从`jQuery`官网可以下载最新版本.`jQuery`只是一个`jquery-xxx.js`文件, 但你会看到有`compressed`(已压缩)和`uncompressed`(未压缩)两种版本,
使用时完全一样,但如果你想深入研究`jQuery`源码,那就用`uncompressed`版本.

#### 使用jQuery

使用`jQuery`只需要在页面的`<head>`引入`jQuery`文件即可: 

```js
<html>
<head>
    <script src="//code.jquery.com/jquery-1.11.3.min.js"></script>
    ...
</head>
<body>
    ...
</body>
</html>
```

好消息是,当你在学习这个教程时,由于网站本身已经引用了`jQuery`,所以你可以直接使用: 

```js
'use strict';
console.log('jQuery版本: ' + $.fn.jquery);
```

`$`是著名的`jQuery`符号.实际上,`jQuery`把所有功能全部封装在一个全局变量`jQuery`中, 而`$`也是一个合法的变量名,它是变量`jQuery`的别名: 

```js
window.jQuery; // jQuery(selector, context)
window.$; // jQuery(selector, context)
$ === jQuery; // true
typeof($); // 'function'
```

`$`本质上就是一个函数,但是函数也是对象,于是`$`除了可以直接调用外, 也可以有很多其他属性.
**注意**,你看到的`$`函数名可能不是`jQuery(selector, context)`, 
因为很多`JavaScript`压缩工具可以对函数名和参数改名,所以压缩过的`jQuery`源码`$`函数可能变成`a(b, c)`.

绝大多数时候,我们都直接用`$`(因为写起来更简单嘛). 但是,如果`$`这个变量不幸地被占用了,而且还不能改,那我们就只能让`jQuery`把`$`变量交出来,然后就只能使用`jQuery`这个变量: 

```js
$; // jQuery(selector, context)
jQuery.noConflict();
$; // undefined
jQuery; // jQuery(selector, context)
```

这种黑魔法的原理是`jQuery`在占用`$`之前,先在内部保存了原来的`$`,调用`jQuery.noConflict()`时会把原来保存的变量还原.

### 选择器

选择器是`jQuery`的核心.一个选择器写出来类似`$('#dom-id')`.
为什么`jQuery`要发明选择器?回顾一下`DOM`操作中我们经常使用的代码: 

```js
// 按ID查找: 
var a = document.getElementById('dom-id');
// 按tag查找: 
var divs = document.getElementsByTagName('div');
// 查找<p class="red">: 
var ps = document.getElementsByTagName('p');
// 过滤出class="red":
// TODO:
// 查找<table class="green">里面的所有<tr>: 
var table = ...
for (var i=0; i<table.children; i++) {
    // TODO: 过滤出<tr>
}
```

这些代码实在太繁琐了, 并且在层级关系中,例如查找`<table class="green">`里面的所有`<tr>`,一层循环实际上是错的,因为`<table>`的标准写法是: 

```js
<table>
    <tbody>
        <tr>...</tr>
        <tr>...</tr>
    </tbody>
</table>
```

很多时候,需要递归查找所有子节点.

`jQuery`的选择器就是帮助我们快速定位到一个或多个`DOM`节点.

#### 按ID查找

如果某个`DOM`节点有`id`属性,利用`jQuery`查找如下: 

```js
// 查找<div id="abc">:
var div = $('#abc');
```

注意,`#abc`以`#`开头.返回的对象是`jQuery`对象.
什么是`jQuery`对象?`jQuery`对象类似数组,它的每个元素都是一个引用了`DOM`节点的对象.
以上面的查找为例,如果`id`为`abc`的`<div>`存在,返回的`jQuery`对象如下: 

    [<div id="abc">...</div>]

如果`id`为`abc`的`<div>`不存在,返回的`jQuery`对象如下: 

    []

总之`jQuery`的选择器不会返回`undefined`或者`null`,这样的好处是你不必在下一行判断`if (div === undefined)`.

`jQuery`对象和`DOM`对象之间可以互相转化: 

```js
var div = $('#abc'); // jQuery对象
var divDom = div.get(0); // 假设存在div,获取第1个DOM元素
var another = $(divDom); // 重新把DOM包装为jQuery对象
```

通常情况下你不需要获取`DOM`对象,直接使用`jQuery`对象更加方便.
如果你拿到了一个`DOM`对象,那可以简单地调用`$(aDomObject)`把它变成`jQuery`对象,这样就可以方便地使用`jQuery`的`API`了.

#### 按tag查找

按`tag`查找只需要写上`tag`名称就可以了: 

```js
var ps = $('p'); // 返回所有<p>节点
ps.length; // 数一数页面有多少个<p>节点
```

#### 按class查找

按`class`查找注意在`class`名称前加一个`.`: 

```js
var a = $('.red'); // 所有节点包含`class="red"`都将返回
// 例如:
// <div class="red">...</div>
// <p class="green red">...</p>
```

通常很多节点有多个`class`,我们可以查找同时包含`red`和`green`的节点: 

```js
var a = $('.red.green'); // 注意没有空格!
// 符合条件的节点: 
// <div class="red green">...</div>
// <div class="blue green red">...</div>
```

#### 按属性查找

一个`DOM`节点除了`id`和`class`外还可以有很多`属性`, 很多时候按`属性`查找会非常方便,比如在一个表单中按`属性`来查找: 

```js
var email = $('[name=email]'); // 找出<??? name="email">
var passwordInput = $('[type=password]'); // 找出<??? type="password">
var a = $('[items="A B"]'); // 找出<??? items="A B">
```

当属性的值包含空格等特殊字符时,需要用`双引号`括起来.
按属性查找还可以使用`前缀查找`或者`后缀查找`: 

```js
var icons = $('[name^=icon]'); // 找出所有name属性值以icon开头的DOM
// 例如: name="icon-1", name="icon-2"
var names = $('[name$=with]'); // 找出所有name属性值以with结尾的DOM
// 例如: name="startswith", name="endswith"
```

这个方法尤其适合通过`class`属性查找,且不受`class`包含多个名称的影响: 

```js
var icons = $('[class^="icon-"]'); // 找出所有class包含至少一个以`icon-`开头的DOM
// 例如: class="icon-clock", class="abc icon-home"
```

#### 组合查找

组合查找就是把上述简单选择器组合起来使用.
如果我们查找`$('[name=email]')`, 很可能把表单外的`<div name="email">`也找出来,但我们只希望查找`<input>`,就可以这么写: 

```js
var emailInput = $('input[name=email]'); // 不会找出<div name="email">
```

同样的,根据`tag`和`class`来组合查找也很常见: 

```js
var tr = $('tr.red'); // 找出<tr class="red ...">...</tr>
```

#### 多项选择器

多项选择器就是把多个选择器用,组合起来一块选: 

```js
$('p,div'); // 把<p>和<div>都选出来
$('p.red,p.green'); // 把<p class="red">和<p class="green">都选出来
```

要注意的是,选出来的元素是按照它们在`HTML`中出现的顺序排列的,而且不会有重复元素.
例如,`<p class="red green">`不会被上面的`$('p.red,p.green')`选择两次.

#### 练习

使用`jQuery`选择器分别选出指定元素: 

+ 仅选择JavaScript
+ 仅选择Erlang
+ 选择JavaScript和Erlang
+ 选择所有编程语言
+ 选择名字input
+ 选择邮件和名字input

```html
<!-- HTML结构 -->
<div id="test-jquery">
    <p id="para-1" class="color-red">JavaScript</p>
    <p id="para-2" class="color-green">Haskell</p>
    <p class="color-red color-green">Erlang</p>
    <p name="name" class="color-black">Python</p>
    <form class="test-form" target="_blank" action="#0" onsubmit="return false;">
        <legend>注册新用户</legend>
        <fieldset>
            <p><label>名字: <input name="name"></label></p>
            <p><label>邮件: <input name="email"></label></p>
            <p><label>口令: <input name="password" type="password"></label></p>
            <p><button type="submit">注册</button></p>
        </fieldset>
    </form>
</div>
```

运行查看结果: 

```js
'use strict';
var selected = null;
// 仅选择JavaScript
selected = $('#para-1');
selected = $('[class=color-red]');  // 完全匹配的方式
//仅选择Erlang
selected = $('[class="color-red color-green"]')
selected = $('.color-red.color-green')
//选择JavaScript和Erlang
selected = $('.color-red');
//选择所有编程语言
selected = $('[class^="color-"]');
//选择名字input
selected = $('input[name=name]');
//选择邮件和名字input
selected = $('input[name=name],input[name=email]');
// 高亮结果:
if (!(selected instanceof jQuery)) {
    return console.log('不是有效的jQuery对象!');
}
$('#test-jquery').find('*').css('background-color', '');
selected.css('background-color', '#ffd351');
```

### 层级选择器

除了基本的选择器外,`jQuery`的层级选择器更加灵活,也更强大.
因为`DOM`的结构就是层级结构,所以我们经常要根据层级关系进行选择.

#### 层级选择器(Descendant Selector)

如果两个`DOM`元素具有层级关系,就可以用`$('ancestor descendant')`来选择,层级之间用`空格`隔开.例如: 

```html
<!-- HTML结构 -->
<div class="testing">
    <ul class="lang">
        <li class="lang-javascript">JavaScript</li>
        <li class="lang-python">Python</li>
        <li class="lang-lua">Lua</li>
    </ul>
</div>
```

要选出`JavaScript`,可以用层级选择器: 

```js
$('ul.lang li.lang-javascript'); // [<li class="lang-javascript">JavaScript</li>]
$('div.testing li.lang-javascript'); // [<li class="lang-javascript">JavaScript</li>]
```

因为`<div>`和`<ul>`都是`<li>`的祖先节点,所以上面两种方式都可以选出相应的`<li>`节点.

要选择所有的`<li>`节点,用: 

```js
$('ul.lang li');
```

这种层级选择器相比单个的选择器好处在于,它缩小了选择范围,因为首先要定位父节点,才能选择相应的子节点, 这样避免了页面其他不相关的元素.
例如: 

```js
$('form[name=upload] input');
```

就把选择范围限定在`name`属性为`upload`的表单里.如果页面有很多表单, 其他表单的`<input>`不会被选择.

多层选择也是允许的: 

```js
$('form.test p input'); // 在form表单选择被<p>包含的<input>
```

#### 子选择器(Child Selector)

子选择器`$('parent>child')`类似层级选择器,但是限定了层级关系必须是父子关系,就是`<child>`节点必须是`<parent>`节点的直属子节点.还是以上面的例子: 

```js
$('ul.lang>li.lang-javascript'); // 可以选出[<li class="lang-javascript">JavaScript</li>]
$('div.testing>li.lang-javascript'); // [], 无法选出,因为<div>和<li>不构成父子关系
```

#### 过滤器(Filter)

过滤器一般不单独使用,它通常附加在选择器上,帮助我们更精确地定位元素. 观察过滤器的效果: 

```js
$('ul.lang li'); // 选出JavaScript,Python和Lua 3个节点

$('ul.lang li:first-child'); // 仅选出JavaScript
$('ul.lang li:last-child'); // 仅选出Lua
$('ul.lang li:nth-child(2)'); // 选出第N个元素,N从1开始
$('ul.lang li:nth-child(even)'); // 选出序号为偶数的元素
$('ul.lang li:nth-child(odd)'); // 选出序号为奇数的元素
```

#### 表单相关

针对表单元素, `jQuery` 还有一组特殊的选择器: 

+ `:input` ; 可以选择`<input>`,`<textarea>`, `<select>` 和 `<button>`;
+ `:file` ; 可以选择`<input type="file">`, 和`input[type=file]`一样;
+ `:checkbox` ; 可以选择`复选框`, 和`input[type=checkbox]`一样;
+ `:radio` ; 可以选择`单选框`, 和`input[type=radio]`一样;
+ `:focus` ; 可以选择当前输入焦点的元素,例如把光标放到一个`<input>`上,用`$('input:focus')`就可以选出;
+ `:checked` ; 选择当前勾上的`单选框`和`复选框`, 用这个选择器可以立刻获得用户选择的项目,如`$('input[type=radio]:checked')`;
+ `:enabled` ; 可以选择可以正常输入的`<input>`, `<select> `等,也就是没有灰掉的输入;
+ `:disabled` ; 和`:enabled`正好相反,选择那些不能输入的.

此外, `jQuery`还有很多有用的选择器, 例如选出可见的或隐藏的元素: 

```js
$('div:visible'); // 所有可见的div
$('div:hidden'); // 所有隐藏的div
```

#### 练习

针对如下HTML结构: 

```html
<!-- HTML结构 -->
<div class="test-selector">
    <ul class="test-lang">
        <li class="lang-javascript">JavaScript</li>
        <li class="lang-python">Python</li>
        <li class="lang-lua">Lua</li>
    </ul>
    <ol class="test-lang">
        <li class="lang-swift">Swift</li>
        <li class="lang-java">Java</li>
        <li class="lang-c">C</li>
    </ol>
</div>
```

选出相应的内容并观察效果: 

```js
'use strict';
var selected = null;
// 分别选择所有语言,所有动态语言,所有静态语言,JavaScript,Lua,C等:
// 分别选择所有语言,所有动态语言,所有静态语言,JavaScript,Lua,C等:
selected = $('[class^=lang]');
selected = $('ul.test-lang>li');
selected = $('ol>li');
selected = $('ul>li.lang-javascript');
selected = $('ul>li.lang-lua');
selected = $('ol>li.lang-c');
// 高亮结果:
if (!(selected instanceof jQuery)) {
    return console.log('不是有效的jQuery对象!');
}
$('#test-jquery').find('*').css('background-color', '');
selected.css('background-color', '#ffd351');
```
