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

针对如下`HTML`结构:

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

### 查找和过滤

通常情况下选择器可以直接定位到我们想要的元素,但是,当我们拿到一个`jQuery`对象后, 还能以这个对象为基准,进行查找和过滤.

最常见的查找是在某个节点的所有子节点中查找,使用`find()`方法,它本身又接收一个任意的选择器. 例如下面的`HTML`结构:

```html
<!-- HTML结构 -->
<ul class="lang">
    <li class="js dy">JavaScript</li>
    <li class="dy">Python</li>
    <li id="swift">Swift</li>
    <li class="dy">Scheme</li>
    <li name="haskell">Haskell</li>
</ul>
```

用`find()`查找:

```js
var ul = $('ul.lang'); // 获得<ul>
var dy = ul.find('.dy'); // 获得JavaScript, Python, Scheme
var swf = ul.find('#swift'); // 获得Swift
var hsk = ul.find('[name=haskell]'); // 获得Haskell
```

如果要从当前节点开始向上查找,使用`parent()`方法:

```js
var swf = $('#swift'); // 获得Swift
var parent = swf.parent(); // 获得Swift的上层节点<ul>
var a = swf.parent('.red'); // 获得Swift的上层节点<ul>,同时传入过滤条件.如果ul不符合条件,返回空jQuery对象
```

对于位于同一层级的节点,可以通过`next()`和`prev()`方法,例如:
当我们已经拿到`Swift`节点后:

```js
var swift = $('#swift');
swift.next(); // Scheme
swift.next('[name=haskell]'); // 空的jQuery对象,因为Swift的下一个元素Scheme不符合条件[name=haskell]
swift.prev(); // Python
swift.prev('.dy'); // Python,因为Python同时符合过滤器条件.dy
```

#### 过滤

和函数式编程的 `map`,`filter` 类似,`jQuery` 对象也有类似的方法.

`filter()` 方法可以过滤掉不符合选择器条件的节点:

```js
var langs = $('ul.lang li'); // 拿到JavaScript, Python, Swift, Scheme和Haskell
var a = langs.filter('.dy'); // 拿到JavaScript, Python, Scheme
```

或者传入一个函数,要特别注意函数内部的`this`被绑定为`DOM`对象,不是`jQuery`对象:

```js
var langs = $('ul.lang li'); // 拿到JavaScript, Python, Swift, Scheme和Haskell
langs.filter(function () {
    return this.innerHTML.indexOf('S') === 0; // 返回S开头的节点
}); // 拿到Swift, Scheme
```

`map()`方法把一个`jQuery`对象包含的若干`DOM`节点转化为其他对象:

```js
var langs = $('ul.lang li'); // 拿到JavaScript, Python, Swift, Scheme和Haskell
var arr = langs.map(function () {
    return this.innerHTML;
}).get(); // 用get()拿到包含string的Array: ['JavaScript', 'Python', 'Swift', 'Scheme', 'Haskell']
```

此外, `jQuery`对象如果包含了不止一个`DOM`节点,`first()`,`last()` 和 `slice()` 方法可以返回一个新的`jQuery`对象,把不需要的`DOM`节点去掉:

```js
var langs = $('ul.lang li'); // 拿到JavaScript, Python, Swift, Scheme和Haskell
var js = langs.first(); // JavaScript,相当于$('ul.lang li:first-child')
var haskell = langs.last(); // Haskell, 相当于$('ul.lang li:last-child')
var sub = langs.slice(2, 4); // Swift, Scheme, 参数和数组的slice()方法一致
```

#### 练习

对于下面的表单:

```html
<form id="test-form" action="#0" onsubmit="return false;">
    <p><label>Name: <input name="name"></label></p>
    <p><label>Email: <input name="email"></label></p>
    <p><label>Password: <input name="password" type="password"></label></p>
    <p>Gender: <label><input name="gender" type="radio" value="m" checked> Male</label> <label><input name="gender" type="radio" value="f"> Female</label></p>
    <p><label>City: <select name="city">
       <option value="BJ" selected>Beijing</option>
       <option value="SH">Shanghai</option>
       <option value="CD">Chengdu</option>
       <option value="XM">Xiamen</option>
    </select></label></p>
    <p><button type="submit">Submit</button></p>
</form>
```

输入值后,用`jQuery`获取表单的`JSON`字符串,`key`和`value`分别对应每个输入的`name`和相应的 `value`,例如: `{"name":"Michael","email":...}`

```js
'use strict';
var json = null;
let formData = {};
$('#test-form label [name]').map(function (x) {
    formData[this.name] = this.value;
});
formData["gender"]=$('#test-form input[name="gender"][value=f]').parent()[0].innerText
json = JSON.stringify(formData);
// 显示结果:
if (typeof(json) === 'string') {
    console.log(json);
}
else {
    console.log('json变量不是string!');
}
```

### 操作DOM

`jQuery`的`选择器`很强大, 拿到了`jQuery`对象之后, 可以用来操作对应的`DOM`节点.

回顾一下修改`DOM`的 `CSS`,`文本`,设置`HTML`有多么麻烦,
而且有的浏览器只有`innerHTML`,有的浏览器支持`innerText`,有了`jQuery`对象,不需要考虑浏览器差异了,全部统一操作!

#### 修改Text和HTML

`jQuery`对象的`text()`和`html()`方法分别获取节点的文本和原始`HTML`文本,例如,如下的`HTML`结构:

```html
<!-- HTML结构 -->
<ul id="test-ul">
    <li class="js">JavaScript</li>
    <li name="book">Java &amp; JavaScript</li>
</ul>
```

分别获取`文本`和`HTML`:

```js
$('#test-ul li[name=book]').text(); // 'Java & JavaScript'
$('#test-ul li[name=book]').html(); // 'Java &amp; JavaScript'
```

如何设置`文本`或`HTML`?
`jQuery`的`API`设计非常巧妙: 无参数调用`text()`是获取文本,传入参数就变成设置文本,`HTML`也是类似操作,自己动手试试:

```js
'use strict';
var j1 = $('#test-ul li.js');
var j2 = $('#test-ul li[name=book]');
j1.html('<span style="color: red">JavaScript</span>');
j2.text('JavaScript & ECMAScript');
```

`jQuery`对象可以包含`0`个或`任意`个`DOM`对象,它的方法实际上会作用在对应的每个`DOM`节点上.在上面的例子中试试:

```js
$('#test-ul li').text('JS'); // 是不是两个节点都变成了JS?
```

所以`jQuery`对象的另一个好处是我们可以执行一个操作,作用在对应的一组`DOM`节点上.
即使选择器没有返回任何`DOM`节点,调用`jQuery`对象的方法仍然不会报错:

```js
// 如果不存在id为not-exist的节点:
$('#not-exist').text('Hello'); // 代码不报错,没有节点被设置为'Hello'
```

这意味着`jQuery`帮你免去了许多`if`语句.

#### 修改CSS,链式调用

`jQuery`对象有`批量操作`的特点, 这用于修改`CSS`实在是太方便了. 考虑下面的`HTML`结构:

```html
<!-- HTML结构 -->
<ul id="test-css">
    <li class="lang dy"><span>JavaScript</span></li>
    <li class="lang"><span>Java</span></li>
    <li class="lang dy"><span>Python</span></li>
    <li class="lang"><span>Swift</span></li>
    <li class="lang dy"><span>Scheme</span></li>
</ul>
```

要`高亮显示`动态语言,调用`jQuery`对象的`css('name', 'value')`方法, 我们用一行语句实现:

```js
'use strict';
$('#test-css li.dy>span').css('background-color', '#ffd351').css('color', 'red');
```

注意, `jQuery`对象的所有方法都返回`jQuery`对象(可能是新的也可能是自身),这样我们可以进行`链式调用`,非常方便.
这里修改的是`HTML`的`CSS`属性. `jQuery`对象的`css()`方法可以用来:

+ 修改`class`的值,

    ```js
    var div = $('#test-div');
    div.css('color'); // '#000033', 获取CSS属性
    div.css('color', '#336699'); // 设置CSS属性
    div.css('color', ''); // 清除CSS属性
    ```

    为了和`JavaScript`保持一致,`CSS` 属性可以用 `'background-color'` 和 `'backgroundColor'` 两种格式.
    `css()` 方法将作用于`DOM`节点的`style`属性,具有最高优先级.

+ 添加或删除`class`,可以用`jQuery`的下列方法:

    ```js
    var div = $('#test-div');
    div.hasClass('highlight'); // false, class是否包含highlight
    div.addClass('highlight'); // 添加highlight这个class
    div.removeClass('highlight'); // 删除highlight这个class
    ```

练习:  使用`css()`方法和`addClass()`方法高亮显示`JavaScript`:

```html
<!-- HTML结构 -->
<style>
.highlight {
    color: #dd1144;
    background-color: #ffd351;
}
</style>
<div id="test-highlight-css">
    <ul>
        <li class="py"><span>Python</span></li>
        <li class="js"><span>JavaScript</span></li>
        <li class="sw"><span>Swift</span></li>
        <li class="hk"><span>Haskell</span></li>
    </ul>
</div>
```

```js
'use strict';
var div = $('#test-highlight-css');
// TODO:
div.find('li.js>span').addClass('color').addClass('background-color').css('background-color', '#ffd351').css('color', 'red');
```

#### 显示和隐藏DOM

要隐藏`DOM`,我们可以设置`CSS`的`display`属性 = `none`,利用`css()`方法就可以实现.
不过,要显示这个`DOM`就需要恢复原有的`display`属性,这就得先记下来原有的`display`属性到底是`block`还是`inline`还是别的值.

考虑到显示和隐藏`DOM`元素使用非常普遍, `jQuery` 直接提供`show()`和`hide()`方法, 我们不用关心它是如何修改`display`属性的, 总之它能正常工作:

```js
var a = $('a[target=_blank]');
a.hide(); // 隐藏
a.show(); // 显示
```

注意,隐藏`DOM`节点并未改变`DOM`树的结构,它只影响`DOM`节点的显示. 这和删除`DOM`节点是不同的.

#### 获取DOM信息

利用`jQuery`对象的若干方法,我们直接可以获取`DOM`的高宽等信息, 而无需针对不同浏览器编写特定代码:

```js
// 浏览器可视窗口大小:
$(window).width(); // 800
$(window).height(); // 600
// HTML文档大小:
$(document).width(); // 800
$(document).height(); // 3500
// 某个div的大小:
var div = $('#test-div');
div.width(); // 600
div.height(); // 300
div.width(400); // 设置CSS属性 width: 400px,是否生效要看CSS是否有效
div.height('200px'); // 设置CSS属性 height: 200px,是否生效要看CSS是否有效
```

`attr()`和`removeAttr()`方法用于操作`DOM`节点的属性:

```js
// <div id="test-div" name="Test" start="1">...</div>
var div = $('#test-div');
div.attr('data'); // undefined, 属性不存在
div.attr('name'); // 'Test'
div.attr('name', 'Hello'); // div的name属性变为'Hello'
div.removeAttr('name'); // 删除name属性
div.attr('name'); // undefined
```

`prop()`(property)方法和`attr()`类似, 但是`HTML5`规定有些属性在`DOM`节点中可以没有值, 只有出现与不出现两种,例如:

```html
<input id="test-radio" type="radio" name="test" checked value="1">
```

等价于:

```html
<input id="test-radio" type="radio" name="test" checked="checked" value="1">
```

`attr()`和`prop()`对于属性`checked`处理有所不同:

```js
var radio = $('#test-radio');
radio.attr('checked'); // 'checked'
radio.prop('checked'); // true
```

`prop()`返回值更合理一些. 不过,用`is()`方法判断更好:

```js
var radio = $('#test-radio');
radio.is(':checked'); // true
```

类似的属性还有`selected`,处理时最好用`is(':selected')`.

#### 操作表单

对于表单元素, `jQuery` 对象统一提供`val()`方法获取和设置对应的`value`属性:

```html
    <input id="test-input" name="email" value="test">
    <select id="test-select" name="city">
        <option value="BJ" selected>Beijing</option>
        <option value="SH">Shanghai</option>
        <option value="SZ">Shenzhen</option>
    </select>
    <textarea id="test-textarea">Hello</textarea>
```

```js
var
    input = $('#test-input'),
    select = $('#test-select'),
    textarea = $('#test-textarea');

input.val(); // 'test'
input.val('abc@example.com'); // 文本框的内容已变为abc@example.com

select.val(); // 'BJ'
select.val('SH'); // 选择框已变为Shanghai

textarea.val(); // 'Hello'
textarea.val('Hi'); // 文本区域已更新为'Hi'
```

可见,一个`val()`就统一了各种输入框的取值和赋值的问题.

### 修改DOM结构

直接使用浏览器提供的API对`DOM`结构进行修改,不但代码复杂,而且要针对浏览器写不同的代码.
有了`jQuery`,我们就专注于操作`jQuery`对象本身,底层的`DOM`操作由`jQuery`完成就可以了,这样一来,修改`DOM`也大大简化了.

#### 添加DOM

要添加新的`DOM`节点,除了通过`jQuery`的`html()`这种暴力方法外,还可以用`append()`方法,例如:

```html
<div id="test-div">
    <ul>
        <li><span>JavaScript</span></li>
        <li><span>Python</span></li>
        <li><span>Swift</span></li>
    </ul>
</div>
```

如何向列表新增一个语言?首先要拿到`<ul>`节点:

```js
var ul = $('#test-div>ul');
```

然后,调用`append()`传入`HTML`片段:

```js
ul.append('<li><span>Haskell</span></li>');
```

除了接受字符串, `append()` 还可以传入原始的`DOM`对象,`jQuery`对象和函数对象:

```js
// 创建DOM对象:
var ps = document.createElement('li');
ps.innerHTML = '<span>Pascal</span>';
// 添加DOM对象:
ul.append(ps);
// 添加jQuery对象:
ul.append($('#scheme'));
// 添加函数对象:
ul.append(function (index, html) { // 索引和html代码
    return '<li><span>Language - ' + index + '</span></li>';
});
```

传入函数时,要求返回一个字符串,`DOM`对象或者`jQuery`对象.
因为`jQuery`的`append()`可能作用于一组`DOM`节点,只有传入函数才能针对每个`DOM`生成不同的子节点.

`append()`把`DOM`添加到最后,`prepend()`则把`DOM`添加到最前.
另外注意,如果要添加的`DOM`节点已经存在于`HTML`文档中,它会首先从文档移除,然后再添加,也就是说,用`append()`,你可以移动一个`DOM`节点.

如果要把新节点插入到指定位置,例如,`JavaScript`和`Python`之间,那么,可以先定位到`JavaScript`,然后用`after()`方法:

```js
var js = $('#test-div>ul>li:first-child'); // $('#test-div>ul>li:nth-child(2)') 等等
js.after('<li><span>Lua</span></li>');
```

也就是说,同级节点可以用`after()`或者`before()`方法.

#### 删除节点

要删除`DOM`节点,拿到`jQuery`对象后直接调用`remove()`方法就可以了.
如果`jQuery`对象包含若干`DOM`节点,实际上可以一次删除多个`DOM`节点:

```js
var li = $('#test-div>ul>li');
li.remove(); // 所有<li>全被删除
```

练习

除了列出的3种语言外,请再添加`Pascal`,`Lua` 和 `Ruby`,然后按字母顺序排序节点:

```html
<!-- HTML结构 -->
<div id="test-div">
    <ul>
        <li><span>JavaScript</span></li>
        <li><span>Python</span></li>
        <li><span>Swift</span></li>
    </ul>
</div>
```

例:

```js
'use strict';
var ul = $('#test-div>ul');

if (ul.children().length<4){
ul.append('<li><span>Pascal</span></li>')
ul.append('<li><span>Lua</span></li>')
ul.append('<li><span>Ruby</span></li>')
}
var li = $('#test-div>ul>li');
li.sort((x,y)=>x.innerText.toUpperCase()>y.innerText.toUpperCase()?1:-1);
ul.append(li);
// 测试:
;(function () {
    var s = $('#test-div>ul>li').map(function () {
        return $(this).text();
    }).get().join(',');
    if (s === 'JavaScript,Lua,Pascal,Python,Ruby,Swift') {
        console.log('测试通过!');
    } else {
        console.log('测试失败: ' + s);
    }
})();
```

or; [梦回不醒雨未停](https://www.liaoxuefeng.com/discuss/1023622307115840/1395792495509537)

```js
li=Array.from($("#test-div>ul>li>span"))
val=li.map(x=>x.innerHTML)
val.push("Pascal")
val.push("Lua")
val.push("Ruby")
val.sort()
$("#test-div>ul>li").remove()
val.map(x=>$("#test-div>ul").append(` <li><span>${x}</span></li>`))
```

### 事件

[jQuery API 速查表,Shifone](https://jquery.cuishifeng.cn/checkbox.html)
[:checked Selector](https://api.jquery.com/checked-selector/#entry-examples)
[jQuery; prop](https://jquery.cuishifeng.cn/prop.html)

`$(":checkbox")` ; 匹配所有复选框

因为`JavaScript`在浏览器中以单线程模式运行, 
页面加载后,一旦页面上所有的`JavaScript`代码被执行完后,就只能依赖触发事件来执行`JavaScript`代码.

浏览器在接收到用户的鼠标或键盘输入后,会自动在对应的`DOM`节点上触发相应的事件.
如果该节点已经绑定了对应的`JavaScript`处理函数,该函数就会自动调用.
由于不同的浏览器绑定事件的代码都不太一样,所以用`jQuery`来写代码,就屏蔽了不同浏览器的差异,我们总是编写相同的代码.

举个例子,假设要在用户点击了超链接时弹出提示框,我们用`jQuery`这样绑定一个`click`事件: 

```js
/* HTML:
 *
 * <a id="test-link" href="#0">点我试试</a>
 *
 */

// 获取超链接的jQuery对象:
var a = $('#test-link');
a.on('click', function () {
    alert('Hello!');
});
```

`on`方法用来绑定一个事件, 我们需要传入`事件名称`和对应的`处理函数`.

另一种更简化的写法是直接调用`click()`方法: 

```js
a.click(function () {
    alert('Hello!');
});
```

两者完全等价. 我们通常用后面的写法.

`jQuery`能够绑定的事件主要包括: 

+ 鼠标事件
  + `click`: 鼠标单击时触发;
  + `dblclick`: 鼠标双击时触发;
  + `mouseenter`: 鼠标进入时触发;
  + `mouseleave`: 鼠标移出时触发;
  + `mousemove`: 鼠标在DOM内部移动时触发;
  + `hover`: 鼠标进入和退出时触发两个函数,相当于mouseenter加上mouseleave.
+ 键盘事件; 键盘事件仅作用在当前焦点的`DOM`上,通常是`<input>`和`<textarea>`.
  + `keydown`: 键盘按下时触发;
  + `keyup`: 键盘松开时触发;
  + `keypress`: 按一次键后触发.
+ 其他事件
  + `focus`: 当 `DOM` 获得焦点时触发;
  + `blur`: 当`DOM`失去焦点时触发;
  + `change`: 当`<input>`,`<select`>或`<textarea>`的内容改变时触发;
  + `submit`: 当`<form>`提交时触发;
  + `ready`: 当页面被载入并且`DOM`树完成初始化后触发.

其中,`ready`仅作用于`document`对象.
由于`ready`事件在`DOM`完成初始化后触发,且只触发一次,所以非常适合用来写其他的初始化代码.
假设我们想给一个`<form>`表单绑定`submit`事件,下面的代码没有预期的效果: 

```js
<html>
<head>
    <script>
        // 代码有误:
        $('#testForm').on('submit', function () {
            alert('submit!');
        });
    </script>
</head>
<body>
    <form id="testForm">
        ...
    </form>
</body>
```

因为`JavaScript`在此执行的时候, `<form>` 尚未载入浏览器,所以`$('#testForm)`返回`[]`, 并没有绑定事件到任何`DOM`上.
所以我们自己的初始化代码必须放到`document`对象的`ready`事件中,保证`DOM`已完成初始化: 

```html
<html>
<head>
    <script>
        $(document).on('ready', function () {
            $('#testForm).on('submit', function () {
                alert('submit!');
            });
        });
    </script>
</head>
<body>
    <form id="testForm">
        ...
    </form>
</body>
```

这样写就没有问题了.因为相关代码会在`DOM`树初始化后再执行.
由于`ready`事件使用非常普遍,所以可以这样简化: 

```js
$(document).ready(function () {
    // on('submit', function)也可以简化:
    $('#testForm).submit(function () {
        alert('submit!');
    });
});
```

甚至还可以再简化为: 

```js
$(function () {
    // init...
});
```

上面的这种写法最为常见.如果你遇到`$(function () {...})`的形式,牢记这是`document`对象的`ready`事件处理函数.
完全可以反复绑定事件处理函数,它们会依次执行: 

```js
$(function () {
    console.log('init A...');
});
$(function () {
    console.log('init B...');
});
$(function () {
    console.log('init C...');
});
```

#### 事件参数

有些事件, 如`mousemove`和`keypress`,我们需要获取鼠标位置和按键的值,否则监听这些事件就没什么意义了.
所有事件都会传入`Event`对象作为参数,可以从`Event`对象上获取到更多的信息: 

```js
$(function () {
    $('#testMouseMoveDiv').mousemove(function (e) {
        $('#testMouseMoveSpan').text('pageX = ' + e.pageX + ', pageY = ' + e.pageY);
    });
});
```

#### 取消绑定

已被绑定的事件可以`解除绑定`,通过`off('click', function)`实现: 

```js
function hello() {
    alert('hello!');
}
a.click(hello); // 绑定事件
// 10秒钟后解除绑定:
setTimeout(function () {
    a.off('click', hello);
}, 10000);
```

需要特别注意的是,下面这种写法是无效的: 

```js
// 绑定事件:
a.click(function () {
    alert('hello!');
});
// 解除绑定:
a.off('click', function () {
    alert('hello!');
});
```

这是因为两个匿名函数虽然长得一模一样,但是它们是两个不同的函数对象, `off('click', function () {...})`无法移除已绑定的第一个匿名函数.
为了实现移除效果,可以使用`off('click')`一次性移除已绑定的`click`事件的所有处理函数.

同理,无参数调用`off()`一次性移除已绑定的所有`类型`的事件处理函数.

#### 事件触发条件

一个需要注意的问题是, 事件的触发总是由用户操作引发的. 例如,我们监控文本框的内容改动: 

```js
var input = $('#test-input');
input.change(function () {
    console.log('changed...');
});
```

当用户在文本框中输入时,就会触发`change`事件.但是,如果用`JavaScript`代码去改动文本框的值,将不会触发`change`事件: 

```js
var input = $('#test-input');
input.val('change it!'); // 无法触发change事件
```

有些时候,我们希望用代码触发`change`事件,可以直接调用无参数的`change()`方法来触发该事件: 

```js
var input = $('#test-input');
input.val('change it!');
input.change(); // 触发change事件
```

`input.change()`相当于`input.trigger('change')`, 它是`trigger()`方法的简写.

为什么我们希望手动触发一个事件呢?如果不这么做,很多时候,我们就得写两份一模一样的代码.

#### 浏览器安全限制

在浏览器中,有些`JavaScript`代码只有在用户触发下才能执行,例如,`window.open()`函数: 

```js
// 无法弹出新窗口,将被浏览器屏蔽:
$(function () {
    window.open('/');
});
```

这些 `敏感代码` 只能由用户操作来触发: 

```js
var button1 = $('#testPopupButton1');
var button2 = $('#testPopupButton2');
function popupTestWindow() {
    window.open('/');
}
button1.click(function () {
    popupTestWindow();
});
button2.click(function () {
    // 不立刻执行popupTestWindow(),3秒后执行:
    setTimeout(popupTestWindow, 3000);
});
```

当用户点击`button1`时, `click`事件被触发, 由于`popupTestWindow()`在`click`事件处理函数内执行,这是浏览器允许的,
而`button2`的`click`事件并未立刻执行`popupTestWindow()`,延迟执行的`popupTestWindow()`将被浏览器拦截.

#### 练习

对如下的`Form`表单: 

```html
<!-- HTML结构 -->
<form id="test-form" action="test">
    <legend>请选择想要学习的编程语言: </legend>
    <fieldset>
        <p><label class="selectAll"><input type="checkbox"> <span class="selectAll">全选</span><span class="deselectAll">全不选</span></label> <a href="#0" class="invertSelect">反选</a></p>
        <p><label><input type="checkbox" name="lang" value="javascript"> JavaScript</label></p>
        <p><label><input type="checkbox" name="lang" value="python"> Python</label></p>
        <p><label><input type="checkbox" name="lang" value="ruby"> Ruby</label></p>
        <p><label><input type="checkbox" name="lang" value="haskell"> Haskell</label></p>
        <p><label><input type="checkbox" name="lang" value="scheme"> Scheme</label></p>
        <p><button type="submit">Submit</button></p>
    </fieldset>
</form>
```

绑定合适的事件处理函数,实现以下逻辑: 

当用户勾上"全选"时,自动选中所有语言,并把"全选"变成"全不选";
当用户去掉"全不选"时,自动不选中所有语言;
当用户点击"反选"时,自动把所有语言状态反转(选中的变为未选,未选的变为选中);
当用户把所有语言都手动勾上时,"全选"被自动勾上,并变为"全不选";
当用户手动去掉选中至少一种语言时,"全不选"自动被去掉选中,并变为"全选".

```js
'use strict';
var
    form = $('#test-form'),
    langs = form.find('[name=lang]'),
    selectAll = form.find('label.selectAll :checkbox'),
    selectAllLabel = form.find('label.selectAll span.selectAll'),
    deselectAllLabel = form.find('label.selectAll span.deselectAll'),
    invertSelect = form.find('a.invertSelect');
// 重置初始化状态:
form.find('*').show().off();
form.find(':checkbox').prop('checked', false).off();
deselectAllLabel.hide();
// 拦截form提交事件:
form.off().submit(function (e) {
    e.preventDefault();
    alert(form.serialize());
});
selectAll.click(function(){
    langs.prop('checked', $(this).prop('checked'))
});
// TODO:绑定事件
selectAll.change(function () {
    if ($(this).prop('checked')) {
        deselectAllLabel.show();
        selectAllLabel.hide();
    } else {
        deselectAllLabel.hide();
        selectAllLabel.show();
    }
});
langs.change(function(){
    if (langs.get().every(e => e.checked)) {
        selectAll.prop('checked', true)
    } else {
        selectAll.prop('checked', false)
    };
    selectAll.change();
});
invertSelect.click(function() {
    langs.each(function() {
        $(this).prop('checked', (x, y) => !y);
    });
    langs.change();
})
```

or

```js
selectAll.click(() => {
    if (selectAll.is(":checked")) {
        langs.prop("checked", true);
        selectAllLabel.hide();
        deselectAllLabel.show();
    } else {
        langs.prop("checked", false);
        selectAllLabel.show();
        deselectAllLabel.hide();
    }
}
);
invertSelect.click(() => {
    langs.prop("checked", (i, val) => {
        return !val;
    })
})


selectAll.click( ()=>  {
    langs.prop('checked',
        $(this).prop('checked'))
});
```

#### 统计复选框数目

```js
var countChecked = function() {
  var n = $( "input:checked" ).length;
  $( "div" ).text( n + (n === 1 ? " is" : " are") + " checked!" );
};
countChecked();
$( "input[type=checkbox]" ).on( "click", countChecked );
```

### AJAX
