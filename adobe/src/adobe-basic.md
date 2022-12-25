# 安装维护

## Premiere Pro CC 2019 闪退

[Premiere Pro CC 2019 闪退 崩溃 的解决办法](https://zhuanlan.zhihu.com/p/545703790)

最近需要学着剪片子, 发现之前安装的Premiere Pro CC 2019 在新建项目时就会出现闪退, 崩溃的情况.
上网查了一下, 发现是系统过于新……造成的(想到刚好最近笔记本坏了, 买了一个台式机).
赶紧按照网上高手提供的解决办法试了一下, 但后来发现新建项目不闪退了, 但后续还会出现崩溃的情况, 所以总结一下解决办法:

所幸的是, Premiere Pro CC 2019提供了启动记录(log), 详细到具体是哪个文件造成的崩溃,
具体位置在:

```powershell
%homepath%\AppData\Roaming\Adobe\Premiere Pro\13.0\logs"
```

经查证, 造成新建项目闪退的原因在于, `ZXPSignLib-minimal.dll`,
下载之后, 复制到Premiere Pro CC 2019的安装目录,
注意安装的x64还是x32版本, program files路径不太一样的.
事实上, 搞定新建项目闪退之后, 还发现会有崩溃无法使用的情况,
同理可证, 找到一个名为 `plugplug.dll` 的文件报错, 下载之后, 同样复制到上面的目录中, 就不会崩溃了.
关于dll文件下载的网址, 真心可靠的还真的是网友推荐的:
[dll下载专区, 帮您解决文件丢失导致"d找不到"的系统错误问题--金山毒霸 ](https://www.ijinshan.com/filerepair/dll/index.shtml)

After Effect 同理
