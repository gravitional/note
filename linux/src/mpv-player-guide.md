# [mpv播放器的使用引导](https://hooke007.github.io/unofficial/mpv_start.html)

## [mpv拓展脚本](https://github.com/Eisa01/mpv-scripts)

3.脚本增强(LUA)

JS脚本也受支持, 但本文暂时不记录, 用法大体相似

mpv中的很多功能都是依赖LUA脚本的, 比如 i 键显示的统计信息使用的内置脚本 stats.lua , 你使用的简易控制界面是 osc.lua .
脚本的部分功能可由主设置文件控制, 比如 --no-osc . 但是绝大多数功能依旧是由各自的设置文件控制.
因此在 mpv.conf 的所在位置新建 script-opts 文件夹, 这里面将放置所有脚本的设置文件, mpv会自动读取.
脚本的对应设置文件名为 同脚本名.conf

🔺 此处开始往后, 如果你在使用参数 --save-position-on-quit=yes , 应注意:
===目前为止, 这个参数除了保存了播放进度外, 还储存了许多 额外的状态 , 个人倾向不单独使用它;
===如果你希望保留退出时记录播放进度的功能, 可以不禁用该功能,
同时用另一个参数 watch-later-options=start,vid,aid,sid 限制其记录的属性(示例对应的是: 播放位置, 视频轨, 音轨, 字幕轨序号)
3.1内置脚本

内置脚本由于已集成, 直接创建/编辑对应的设置文件即可, 你可以在 此处 查看内置脚本的更新历史.

以我的推荐用法为例, 在 `X:/xxxxx/你的MPV文件夹/portable_config/script-opts/` 里放置 xxxx.conf 的脚本设置文件
你依然可以在 我的仓库 里参考:
其中 console.conf osc.conf stats.conf ytdl_hook.conf 都是内置脚本的设置文件

🔺 此后为了避免识别问题, 严格文本规范:
===应该直接写 xxx=yyy 不要加多余的空格, 如需注释务必单独另起一行.
===文本编码依旧 UTF-8 , 额外的须要换行为 Unix(LR)

![encoding](https://hooke007.github.io/unofficial/_assets/mpv_start-05.webp)
