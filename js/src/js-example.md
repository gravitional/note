# javascript examples

[一行代码可以做什么](https://www.zhihu.com/question/285586045/answer/1164579350)

自由修改网页内容

```js
document.body.contentEditable='true';
```

## 复制粘贴限制

[功能实现-解除页面禁止复制功能](https://segmentfault.com/a/1190000039087909)

前端实现禁止复制功能的方法, 目前就两大类

+ 通过`js`阻止复制功能实现禁止复制.
+ 通过`css`样式`user-select:none`阻止选中实现禁止复制.

目前一般选择其中一种, 比如简书, 掘金, 知乎等资讯问答类, 也有混合使用的, 比如起点网这类阅读为主的.

破解禁止复制功能的方法
禁止当前页面的 `javascript`, 毕竟我们对复制拦截就是通过`js`实现的.

删除或者覆盖`oncopy`和`onselect`属性事件.

有些网站直接让你无法选中, 给文本增加了css样式`user-select:none`.

非开发实现: 将文章转发到手机, 使用手机的长按选择复制文本
仍旧是非开发实现: 截图, 哈哈哈, 皮一下就很开心.

最牛皮的方法: 手敲, 解决一切妖魔鬼怪.

js实现防复制功能
js实现防复制一般就是使用`copy`这个api:

```js
document.body.oncopy = function(e) {
    // 全局监听
};
$('#articl_content').oncopy = function(e) {
    // 局部监听
};
window.addEventListener('copy', e => {
    // 全局监听
})
```

以及 `document.getSelection()`, 这个方法是用来操作选中的内容的 api, 其中:
`document.getSelection(0).toString()`可以 获得选中内容的文字部分,
`document.getSelection(0).empty()`可以取消选中,
在 `getSelection()` 中传参可以获取不同的选中 内容段, 更多有趣的方法在它原型里.

另外顺便提一下和复制相关的还有剪切-`cut`, 粘贴-`paste`, 用法和copy一致.

一些网站虽然不禁止用户复制内容, 但会在复制的内容后面加版权以及来源信息,
这个就需要用到剪切板这个对象了, 即: clipboa rdData.
它常用的方法有3个,
`setData('text', 插入的内容)`插入数据,
`getData()`获取剪切板的数据,
`clearData('text')` 清除数据.

### 知乎的转载加版权实现

一般常用的`js`复制功能有2种, 一种是`document.execCommand('copy')`,
一般在富文本编辑器框架中经常使用到,
目前虽然在`js` 手册中显示已废弃, 但大部分浏览器还是支持该功能的.

另外一种就是`oncopy`监听, 复制的数据通过`window.getSelection`这个`api`获取,
如果需要加版权信息, 在数据放入剪切板前处理下 , 加上版权信息接口, 知乎就是用的这种, 大部分网站都是这样的.

一般网站都是在部分元素上做监听的, 不会进行全局监听, `document.body.oncopy`实现的是全局监听,
`document.getElementById( 'xx').oncopy` 实现的某个元素内监听.

一般实现:

```js
document.body.oncopy = (event) => {
    // 是否登录, 没登陆就禁止转载
    if (!isLogin()) return alert('禁止转载');
    event.preventDefault(); // 取消浏览器原本默认的copy事件
    let authorizationInfo, // 授权信息
        copyText = window.getSelection(0).toString(); // 获取被复制的文本内容, 没有dom结构
    if (copyText.length > 20) {
        // 一般少量复制网站是允许的, 字数长度超过20个, 则加入授权信息
        copyText = `${copyText}

作者: xxx,
连接: xxx
来源: 知乎
...
`
    }
    // 兼容一下ie浏览器, ie中是window, 其他浏览器是event
    let clipboardData = event.clipboardData || window.clipboardData;
    clipboardData.setData('text', copyText);
}
```

### 起点网防复制功能实现

禁止复制+剪切
禁止右键, 右键某些选项: 复制粘贴剪切
禁用文字选择: `user-select:none`
具体实现:

```js
// 禁止右键
document.body.oncontextmenu = e => {
    return false;
    // e.preventDefault();
};
// 禁止文字选择
document.body.onselectstart = e => {
    return false;
    // e.preventDefault();
};
// 禁止复制
document.body.oncopy = e => {
    return false;
    // e.preventDefault();
}
// css禁止用户选择
.content{
   user-select: none;
   -moz-user-select: none;
   -webkit-user-select: none;
   -ms-user-select: none;
}
```

### 破解转载代码-简单版(适用开发者)

打开浏览器, 按下`f12`, 最上面一排找到 `console`,复制下面代码,
然后选择你要复制的文本内容, 将下面代码粘贴到 `console` 中并回车,
即可看到你选中的内容的纯文本形式, 这个方式只能复制纯文本.

```js
window.getSelection(0).toString();
```

### 禁用js, 适用所有人

Chrome浏览器的话: 打开浏览器控制台, 按F1进入Setting, 勾选Disable JavaScript(禁止js), 放图:

![disable js](https://segmentfault.com/img/remote/1460000039087913)
