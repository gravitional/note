# latexmk 需要自己进行简单的配置后才能使用起来比较顺手。 建议在个人根目录的 $HOME/.latexmkrc 文件中配置个人的使用习惯， 然后在项目的工作目录的 latexmkrc 文件配置项目相关的编译方法， 如使用 xelatex 或开启 -shell-escape 等。

# 如果使用 xelatex 可以设为 5（需要 TeX Live 2017 以上）。 我个人由于需要使用不同的引擎，所以默认设为 1， 然后用命令行参数切换不同的引擎，比如 latexmk -xelatex。
$pdf_mode = 5;
 
#给编译命令添加额外的参数：-file-line-error 使报错输出文件和行号； -halt-on-error 和 -interaction=nonstopmode 使编译遇到错误时立即停止； -synctex=1 则开启 synctex 的功能。
$pdflatex = "pdflatex -file-line-error -halt-on-error -interaction=nonstopmode -synctex=1 %O %S";
$xelatex = "xelatex -file-line-error -halt-on-error -interaction=nonstopmode -no-pdf -synctex=1 %O %S";
$xdvipdfmx = "xdvipdfmx -E -o %D %O %S";
# xelatex 的 -no-pdf 参数使编译时只生成 xdv 文件，最后才用 xdvipdfmx 生成 pdf， 这样可以节省嵌入图片耗费的时间（需要 TeX Live 2017 以上）。 注意 -E 参数可以避免一些字体嵌入问题（见 https://github.com/CTeX-org/ctex-kit/issues/352 ）


#通常 bbl 文件是由 BibTeX 编译 bib 文件生成的，在清理辅助文件时可以删掉； 但是有时并没由提供 bib 文件，只有 bbl（比如 arxiv 上的 TeX 源码）， $bibtex_use = 1.5 可以自动检测根据条件清理 bbl 文件（需要 TeX Live 2018 以上）
$bibtex_use = 1.5;
 

#编译时将所有生成文件（包括辅助文件）写到 latex.out 目录下。注意这个配置虽然可以使目录更加整洁，但是可能导致一些软件的功能受影响，建议谨慎选择。
$out_dir = "temp";
 

#编译完成后自动打开 pdf；还可以用 $pdf_previewer 配置 pdf 阅读器， 以及 $preview_continuous_mode 连续自动编译
$preview_mode = 1;
#$pdf_previewer="C:\Program Files\SumatraPDF\SumatraPDF.exe"; 

#额外需要清理的辅助文件；如配置过 $out_dir 就不再需要了。
#$clean_ext = "hd nav snm synctex.gz xdv";
 

#一些宏包需要额外的编译命令， 比如 nomencl 宏包需要调用 makeindex 命令将 nlo 文件编译成 nls 文件， 所以要手动配置这个编译过程。
add_cus_dep('glo', 'gls', 0, 'glo2gls');
sub glo2gls {
    system("makeindex -s gglo.ist -o \"$_[0].gls\" \"$_[0].glo\"");
}
push @generated_exts, "glo", "gls";
 
add_cus_dep('nlo', 'nls', 0, 'nlo2nls');
sub nlo2nls {
    system("makeindex -s nomencl.ist -o \"$_[0].nls\" \"$_[0].nlo\"");
}
push @generated_exts, "nlo", "nls";
