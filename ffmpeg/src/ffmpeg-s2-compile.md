
## 编译和裁剪

FFMpeg与大部分GNU软件的编译方式类似, 是通过configure脚本来实现编译前定制的.
这种途径允许用户在编译前对软件进行裁剪,
同时通过对宿主系统和目标系统的自动检测来筛选参与编译的模块并为其设定合适的配置.
但是, FFMpeg的configure脚本并非如通常的GNU软件一样通过配置工具自动生成, 而是完全由人工编写的.
configure脚本生成的config.mak和config.h分别在Makefile和源代码的层次上实现编译的控制.

通过运行 `./configure –help` 可以了解到脚本支持的参数, 这些参数大体分为下面几类:

+ 标准选项——GNU软件例行配置项目如安装路径等. 例: –prefix=…,……
+ 列出当前源代码支持的能力集, 如编解码器, 解复用器, 输入输出设备, 文件系统等.
例: –list-decoders, –list-encoders, ……

+ 授权选项: –enable-version3, –enable-gpl, –enable-nofree. 代码的缺省授权是LGPL v2,
如果要使用以LGPL v3, GPL授权的模块或者某些不遵循自有软件授权协议的模块, 必须在运行configure时显式使能相应的选项.

+ 编译, 链接选项. 例: –disable-static, –enable-shared, ...
缺省配置是生成静态库而不生成动态库, 如果希望禁止静态库, 生成动态库都需要显式指定.

+ 可执行程序控制选项, 决定是否生成ffmpeg, ffplay, ffprobe和ffserver.
+ 模块控制选项, 筛选参与编译的模块, 包括整个库的筛选,
例如: –disable-avdevice;一组模块的筛选,
例如: –disable-decoders, 单个模块的筛选, 如: –disable-decoder=… 等.

+ 专家级选项, 允许开发者进行深度定制,
如交叉编译环境的配置, 自定义编译器参数的设定, 指令级优化, debug控制等.

对于–disable, –enable类的控制选项,
如果以–disable为前缀, 则缺省是enable的, 反之亦然.

总之, 无论从商业角度还是技术角度出发,
使用configure脚本对FFMpeg进行裁剪是最安全的方式,
只有针对于某些configure无法满足的定制要求,
才需要考虑修改configure脚本——甚至修改configure生成的配置文件.

以下是一个配置实例, 实现运行与Android系统中的ffmpeg库的编译:

```bash
./configure --prefix=. --cross-prefix=$NDK_TOOLCHAIN_PREFIX --enable-cross-compile --arch=arm --target-os=linux --cpu=cortex-a8 \
                       --disable-static --enable-shared --enable-pic --disable-ffmpeg --disable-ffplay --disable-ffserver --disable-ffprobe \
                       --extra-cflags="-I$NDK_PLATFORM/usr/include" \
                       --extra-ldflags="-nostdlib -Wl,-T,$NDK_PREBUILT/arm-linux-androideabi/lib/ldscripts/armelf_linux_eabi.x \
                                        -L$NDK_PLATFORM/usr/lib \
                                        $NDK_PREBUILT/lib/gcc/arm-linux-androideabi/4.4.3/crtbegin.o $NDK_PREBUILT/lib/gcc/arm-linux-androideabi/4.4.3/crtend.o \
                                        -lc -lm -ldl"
```

缺省的编译会生成4个可执行文件和9个库:
可执行文件包括用于转码的ffmpeg, 用于获取媒体信息的ffprobe,
用于播放媒体的ffplay和用于推送媒体流的ffserver;
库包括avutil, avformat, avcodec, avfilter, avdevice, swresample, swscale, postproc及avresample,
其中, 核心库有5个, 分别为基础库avutil, 文件格式及协议库avformat,
编解码库avcodec, 输入输出设备库avdevice和过滤器avfilter.

这些功能足以支持一个功能强大的多媒体播放器,
因为最复杂的解复用, 解码, 数据分析过程已经在FFMpeg内部实现了, 需要关注的仅剩同步问题.
