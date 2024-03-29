# final cut pro 教程

[B站最良心!包你学会的FCPX教程建议收藏](https://www.bilibili.com/video/BV15T4y127n4?p=13&vd_source=a7a8fe02bc1634566f069ff312b17c99)

## 剪辑准备

新建资源库

资源库: 储存素材
导入素材时, 可以勾选让 `文件保存在原位`,
如果需要拷贝工程文件, 可以再勾选 `拷贝到资源库`.

在素材库中, `cmd +/-` 可以调节预览图的大小,
勾选波形, 可以显示声音信息.

如果在导入之前就将素材分好类, 导入的时候可以继承分类.
也可以导入之后, 使用新建关键词精选 `shift+cmd+k`, 建立分类.

资源库建好之后, 可以 `新建项目`,
调节分辨率,  帧率
帧率默认有 30, 29.27, 25fps

## 浏览, 选择素材, 基础剪辑, 输出

+ 在资源库窗口, 选中素材, 按 `space` 就可以播放.
+ 通过按键 `i,o` 可以选中范围, 鼠标可以将选中范围拖到 `时间轴`.
+ 快捷键 `q`, 将选中范围append后面.
+ `cmd+a`全选, 按`q`可以全部Q到 `时间轴`上.
+ 不需要的素材可以按住 `delete` 删除掉.

`cmd+4`可以打开`检查器`, 由多个`子检查器`组成.
视频检查器可以调节 混合模式, 不透明度, 变换, 裁剪. 等等

拖动视频时, 按住 `option` 可以自动拷贝多份.

导出视频, 在 时间轴上i,o 选定范围, 点击右上角共享按钮, 或者 `cmd+e` 导出.

## 磁性时间线

final cut 与其他软件最大区别就是 磁性时间线.
中间的灰色轨道称为 主故事线
两边的称为支线

上面支线: 视频, 图片, 文字等素材
下面支线: 音频.

磁性特点:

+ 删除中间素材时, 后面的会自动吸附上来.
+ 上下两层(主线与分支)也会连接起来, 挪动主线时支线会跟着走.
+ `ctrl+cmd+2`打开 `时间线浏览器`, 胶片图标中可以调整 片段在时间线中的外观

## 素材操纵快捷键

+ `Q`;  将素材选取到时间线上方,
+ `W`;  将素材插入到指针前面(prepend)
+ `E`;  将素材附着到视频末尾 (append)
+ `V`;  恢复到鼠标指针选取状态

### 独立移动视频

+ 在片段上右键, 从故事情节中提取 `opt+cmd+up`, 就可以单独挪动片段
+ 英文状态, 按住 `tilde` 键, 独立移动素材
+ `shift+delete`; 单独删除主线上的素材
+ `cmd+z`; 撤销操作

## 复合片段

类似于premier 里面的嵌套.
选中多个素材, 右键 新建复合片段, `opt+G`.
`shift+cmd+G`; 还原成多个片段

## 插入空白

`opt+w`; 在片段中间插入黑屏, 可以插入黑幕, 用来放字幕等

## fcpx 资源库管理

当项目已经完成或者交付时,
可以整理资源库, 即对 `*.fcpbundle` 项目文件进行瘦身.
在项目文件上, 右键>显示包内容,
点开项目目录, 其中的
Original Media, Render Files, Shared Items
可以直接删除, 其中是缓存文件.

如果需要恢复, 打开工程, `cmd+a`全选
`文件`>`重新链接文件`>`查找全部`
定位到素材目录, 即可链接成功.

如果在制作过程中, 项目体积过大, 可以点击
`文件` > `删除生成的资源库文件`
