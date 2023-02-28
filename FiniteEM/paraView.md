# paraView 软件使用

+ 调整背景颜色

在菜单栏上右键, 调出 `Main Controls`工具栏, 上面有一个类似 `windows画图`(调色板) 的图标,
点击此图标可以设置背景颜色.

+ 调整云图colorMap, 右键调出 `Color Map Editor`,
或者在 `properties`面板的 `coloring 段落` 调节,
其中有一个 `文件夹爱心` 的图标用于选择预设颜色.

## 调节动画播放速度

[2.11. Time](https://docs.paraview.org/en/latest/Tutorials/SelfDirectedTutorial/basicUsage.html?highlight=speed#time)

ParaView has many powerful options for controlling time and animation.
The majority of these are accessed through the animation view.
From the menu, click on `View ->Animation View`.

[AnimationView.png](https://docs.paraview.org/en/latest/_images/AnimationView.png)

For now we will examine the controls at the top of the animation view.
(We will examine the use of the tracks in the rest of the animation view later in Section 2.15.)
The animation mode parameter determines how ParaView will step through time during playback.
There are three modes available.

+ Sequence
Given a start and end time, break the animation into a specified number of frames spaced equally apart.

+ Real Time
ParaView will play back the animation such that it lasts the specified number of seconds.
The actual number of frames created depends on the update time between frames.

+ Snap To TimeSteps
ParaView will play back exactly those time steps that are defined by your data.

选择 Real Time 模式

### 时间插值器

通常情况下, 显示原始数据的 原始时间步(jerky time steps)是所希望的行为;
它向你展示了数据中确切存在的东西.
然而, 如果你想为演示文稿制作一个动画, 你可能想要一个更平滑的动画.

在ParaView中有一个 滤镜(filter ) 就是为这个目的设计的.
它被称为 `时间插值器`(temporal interpolator).
这个过滤器将在原始数据集定义的时间步长之间对位置和场数据进行插值.

练习2.22 (时间插值)

这个练习是练习2.21的继续. 在开始这个练习之前, 你需要先完成那个练习.

+ 请确保数据对象在`管道浏览器`(pipeline browser)中被高亮显示.
+ 选择过滤器→时序→时序插值器, 或者使用快速启动(ctrl+space Win/Linux, alt+space Mac)应用时序插值器过滤器.
+ 分割视图 pqSplitHorizontal,
+ 在一个视图中显示 TemporalInterpolator1, 在另一个视图中显示 can.ex2, 并连接摄像机.

你应该注意到, 时序插值器的输出比原始数据的动画效果要流畅得多.

值得注意的是, 时间插值器可以(而且经常)在数据中引入伪影(artifacts ).
正是因为如此, ParaView绝不会自动应用这种类型的插值, 你必须明确地添加时序插值器.
一般来说, 网格变形往往能很好地插值, 但通过静态网格的 `运动场` 则不能.
还要注意的是, 时序插值器只有在拓扑结构保持一致的情况下才能工作.
如果你有一个自适应的网格, 从一个时间步长到下一个时间步长的变化, 时序插值器会出现错误.
