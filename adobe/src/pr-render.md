# 避免PR渲染错误, 失败

[避免PR渲染错误, 失败](https://zhuanlan.zhihu.com/p/83595879)

## 删除临时文件

`编译影片时出错` 也可能是上次编辑, 渲染的PR工程有残留与这次的工程有冲突,
因此需要删除历史遗留的临时文件:
打开 `我的电脑-系统盘-用户-USER--Administrator-我的文档-Adobe文件夹`,
直接删除`Premiere Pro`整个文件夹就可以了.

## 内存设置

### 内存分配

PR渲染的时候出现影片编译错误,除了硬盘问题, 有时也是内存不够的原因,
渲染需要内存比较大, 所以要最大限度地增加可用内存.

选择--编辑--首选项--内存, 将为其他程序保留的内存改为`2`--`4G`,
不可以太小, 会造成系统不稳定;也不可以太大, 使PR渲染没有内存保障.

### 优化渲染

其次, 内存选项下有个"优化渲染为"选项, 如何选择呢?

当内存不够时, 选择性能, 即优先利用`CPU`的性能.
当你的内存足够大时, 比如你有`64G`内存, 那么可以选择内存.

### 虚拟内存

内存大小不够可以设置虚拟内存缓解一下内存压力.
有时即使内存足够大了, 比如你有`64G`大内存, 依然要设置虚拟缓存.

怎么设置虚拟内存:

+ 右键点击桌面 `我的电脑`, 选择 `属性--高级`,
+ 弹出 `控制面板`--点击 `高级系统设置`,
+ 弹出 `系统属性` 对话框, 点击 --`性能`下的设置按钮,
+ 弹出 `性能选项` 对话框, 点击 `高级选项卡` --选择 `虚拟内存` 下的更改按钮,
+ 弹出 `虚拟内存` 对话框, 在上方点击选中 `C/D/E/F` 盘中的任意一个分区,
+ 然后勾选 `自定义大小`, 输入一个 `内存数值`, 一般设置为 `实际内存` 的2倍到3倍,
+ 最后点击--`设置`--`确定` 就可以了.

渲染需要的内存比较大, 建议把虚拟内存设到c盘以外的盘符,
可以在你的缓存盘, 和导出盘下设置虚拟内存,
各个硬盘不要都存满东西尽量留够虚拟内存的空间, 保证运行稳定性.

### 时间码和帧率

`时码` 不是标准的 `时码`, (如起始时码不是标准的`00:00:00`),
`帧率` 不是标准的帧率, (帧率不是常规的`24fps`, `25fps`, `30fps`等).
这样的 `时码` 和 `帧率` 也是非常容易导致渲染出错的地方,

时码问题是由于摄像机正在录制, 突断电或者关机, 没时间保存造成的.
`帧率` 问题是由于摄像机 `升格拍摄` 或 `降格拍摄` 造成的非标准帧率,
比如`99FPS`这样的帧率, `PR`是支持不了这么高的帧率的. 如何解决呢?

#### 时码

+ 首先, 修改默认设置, `编辑`→`首选项`→`媒体`→`找到时间码`→`更改为从00:00:00:00开始`.
+ 其次, 单独修改, 切换到项目面板, 查看所有素材的起始时码,
对于媒体开始不是`00:00:00:00`的非标准时码素材, 需要修改.
在素材上右键--`修改`--`时间码`--将时间码改为"`0`"就可以了.

#### 帧率

在项目面板检查帧率, 找到非标准帧率的素材, 比如这里的`50FPS`的素材.

在素材上`右键`--`修改`--`解释素材`--勾选`采用此帧率`--输入`25帧`--确定.
对所有非标准时码和帧率都做这样的修改.

有时直接解释素材可以起作用, 有时还是会出错,
那就只能用`AME`转码了, 将有非标准时码和非标准帧率的素材转码为标准的视频, 再导入到PR中.

## 检查时间线

渲染时弹出 `找不到文件` 警告, 就是因为有 `脱机媒体`,
素材在硬盘上被删掉或者移动位置就会显示脱机, 为了减少渲染隐患, 应该首先排查"脱机媒体":

拖动时间线查看, 蓝色的片段表示正常, 红色片段表示脱机, 需要重新找到.

如果要批量找回所有丢失素材, 在序列的右上角扩展选项那里点击,
选择--链接媒体, 同样弹出离线媒体对话框, 找回其中一个丢失素材,
同文件夹下的所有丢失素材就一次性同步找回了, 很方便.

还有一个更快捷的办法, 直接选择--`文件`--`链接媒体`,
这样PR工程中所有丢失的素材就一次性显示出来了, 按之前同样的方法查找就可以了.

使用链接媒体和查找文件对话框找到并重新链接脱机媒体,
确保渲染时要调用的文件均已链接, 避免渲染中断, 报错, 或渲染后出现黑场.

## 整理时间线

1. 先整理素材: `菜单栏`--`编辑`--`移除未使用项目`.
(注意: 这一步会删除没有导入时间线的素材, 如果想保留所有素材, 就略过这一步. )
1. `菜单栏`--`文件`--`项目管理`, 弹出项目管理器对话框.
1. 在左上方勾选所有序列, 这一步对保证工程的完整性很重要.
1. 勾选`收集文件并复制到新位置`,
1. 点击--`计算`, 可以算出预计文件大小, 一般4K文件都比较大, 可以根据大小选择合适的保存硬盘.
1. 点击目标路径下的`浏览按钮`, 选择一个`保存路径`, 作为暂时的`保存路径`.
1. 新建一个文件夹, 保存.
1. 在右边的选项中, 取消勾选排除未使用项目, (这一步和步骤1重复, 作用也和步骤1相同, 须注意, 会删除部分未使用素材)其他默认保持勾选, 点击确定.

这样就把PR工程和之前分散在各个硬盘上的图片, 音乐, 视频, PSD等等素材, 收集到了一个文件夹下了, 方便渲染时的调用, 加快渲染, 减少渲染卡顿和出错.

## 导出前预渲染

最终输出前要用预渲染检查, 这是基本常识,
如果渲染是问题的, 那么预览肯定也是有问题的, 所以先预渲染时间线找出错误:

1. 选择菜单栏--`标记`--`入点/出点`, 快捷键 `i` 和 `o`.
可以将出点设置为开始, 入点设置为结束, 即包括住整个时间线.
2. 选择菜单栏--`序列`--`渲染入点到出点`.
3. 弹出渲染进度, 结束后可以看到 `绿线`, `红线` 以及 `黄线`.

`绿线` 就是没问题的, `红线` 就是有问题的. `黄线` 代表中性, 不用管它.

时间线上的 `绿色条状` 代表预览的进度,
到哪里停止, 就证明停止点那里的素材, 效果就是有问题的, 删除或替换就可以了.
渲染报错多半都是时间轴上的 `素材`, `特效`, `插件`, `轨道`, `转场` 问题, 导致软件无法进行解码渲染.

`预渲染` 可以在时间轴上找到产生问题的准确位置.
`预渲染` 还可以起到加快渲染速度的作用, 预渲染过的视频导出速度要快很多.
注意: `预渲染` 是把视频和特效合成缓存到硬盘里, 因此缓存位置的空间要够大.

(所以要保留缓存, 才能加速, `渲染` 前不可清理, `渲染` 后再清理).

## 分段导出

有时, 预渲染也不能完全找出错误, 剩下的办法就是分段输出, 一段一段地输成MP4, 最后再合成一次.
需要使用`Adobe Media Encoder`

1. 将时间线上的整个序列平分成`3`--`10`个片段, 根据你的视频时长分割, 时长越长, 片段分割的就越多.
1. 先导出第一段: 用入点和出点作为标记, 快捷键`i`和`O`.
1. 然后选择文件--导出--媒体(快捷键: `Ctrl+M`)
1. 弹出导出设置对话框, 选择一下格式, 比特率, 保存位置, 最后点击队列按钮,
1. 弹出AME窗口, 这样入点和出点之间的片段就添加到AME渲染队里了.
1. 依次类推, 接着标记下一个时间段, 并添加到队列, 直到添加完所有时间段后,
1. 点击AME右上角的启动渲染队列按钮(绿色的)就可以了.

注意: AME的版本要和PR一致, 提前链接脱机素材和整理打包才能防止渲染出错.

如果渲染出错就会有提示, 哪一段出错了就证明哪一段有问题,
比如在某一片段输出到50%那里出现错误提示, 那么出错点大概就在那一时间段的时间线中间位置,
找到后删除或者替换掉问题素材或特效, 然后重新单独导出这一小段就可以了.

注意: 注意段与段之间要一帧都不能少, 或者多余几帧, 后续再剪切掉亦可以.

导出以后, 再新建一个工程文件, 将你分割的段落导进来拼接在一起,
组合成一个序列, 再输出视频就可以了, 这就是最终成片了.

既找出了问题, 又同时完成了出片任务, 可谓一举两得.
对问题进行分解, 即分段导出, 才可以在时间轴上准确找到产生问题的位置.
