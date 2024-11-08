# pdf 工具

## pdf 合并

[Stirling-Tools/Stirling-PDF](https://github.com/Stirling-Tools/Stirling-PDF?tab=readme-ov-file)
[linux poppler 库](https://poppler.freedesktop.org/)

## pdf 添加目录

https://sspai.com/post/69601
https://krasjet.com/voice/pdf.tocgen/
https://github.com/Krasjet/pdf.tocgen

## [zathura](https://pwmt.org/projects/zathura/installation/)

### [intltool](https://www.freedesktop.org/wiki/Software/intltool/)

+ 直接使用 pacman 安装

```bash
pacman -S intltool perl-XML-Parser
```

+ 源码安装, failed

[intltool 源码](https://launchpad.net/intltool/+download)

```bash
tar zxvf intltool-0.40.6.tar.gz # 解压
cd intltool-0.40.6 # 进入目录
./configure # 配置
make && make install
```

+ 需要安装 perl 包 `XML::Parser`, 安装过程中缺少 `langinfo.h` 等 glibc 头文件

[langinfo.h](https://www.gnu.org/software/gnulib/manual/html_node/langinfo_002eh.html)

```bash
cpanm XML::Parser
# features.h
# langinfo.h
# nl_types.h
```

## [girara](https://pwmt.org/projects/girara/)

## [Setting up GTK for Windows](https://www.gtk.org/docs/installations/windows/)

msys2 ucrt64 上的 gtk+ 包

```bash
ucrt64/mingw-w64-ucrt-x86_64-gtkmm3 3.24.8-1
    C++ bindings for gtk3 (mingw-w64)
ucrt64/mingw-w64-ucrt-x86_64-gtk2 2.24.33-6
    GTK+ is a multi-platform toolkit (v2) (mingw-w64)
ucrt64/mingw-w64-ucrt-x86_64-gtk3 3.24.41-3 [installed]
    GObject-based multi-platform GUI toolkit (v3) (mingw-w64)
```
