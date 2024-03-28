# asymptote 命令行

Asymptote近线 2.85 版
[(C) 2004 Andy Hammerlindl, John C. Bowman, Tom Prince].
https://asymptote.sourceforge.io/

## 用法

```bash
asy [options] [file ...]
```

## 选项

将 `-` 替换为 `-no` 来 逆转布尔选项

### 选项组1

输出到 `build/` 文件夹下, 注意最后要有 `/`

```bash
asy -V my-fig.asy -o build/
asy -noView my-fig.asy -o build/ # 不自动调用pdf阅读器
```

sss

```bash
-cd directory          Set current directory; command-line only
-V,-View               View output; command-line only
-absolute              Use absolute WebGL dimensions [false]
-a,-align C|B|T|Z      Center, Bottom, Top, or Zero page alignment [C]
-aligndir pair         Directional page alignment (overrides align) [(0,0)]
-animating             [false]
-antialias n           Antialiasing width for rasterized output [2]
-auto3D                Automatically activate 3D scene [true]
-autobillboard         3D labels always face viewer by default [true]
-autoimport str        Module to automatically import
-autoplay              Autoplay 3D animations [false]
-autorotate            Enable automatic PDF page rotation [false]
-axes3                 Show 3D axes in PDF output [true]
-bw                    Convert all colors to black and white [false]
-cmyk                  Convert rgb colors to cmyk [false]
-d,-debug              Enable debugging messages [false]
-digits n              Default output file precision [7]
-o,-outname name       Alternative output directory/filename
-f,-outformat format   Convert each output file to specified format
```

### 选项组2

```bash
-GPUblockSize n        Compute shader block size [8]
-GPUcompress           Compress GPU transparent fragment counts [false]
-GPUindexing           Compute indexing partial sums on GPU [true]
-GPUinterlock          Use fragment shader interlock [true]
-GPUlocalSize n        Compute shader local size [256]
-autoplain             Enable automatic importing of plain [true]

-batchMask             Mask fpu exceptions in batch mode [false]
-batchView             View output in batch mode [true]

-c,-command str        Command to autoexecute
-compact               Conserve memory at the expense of speed false
-compress              Compress images in PDF output [true]
-convertOptions str    []

-devicepixelratio n    Ratio of physical to logical pixels [1]

-divisor n             Garbage collect using purge(divisor=n) [2]
-dvipsOptions str      []
-dvisvgmMultipleFiles  dvisvgm supports multiple files [true]
-dvisvgmOptions str    []
-embed                 Embed rendered preview image [true]
-e,-environment        Show summary of environment settings; command-line only
-exitonEOF             Exit interactive mode on EOF [true]
-fitscreen             Fit rendered image to screen [true]
-framerate frames/s    Animation speed [30]
-glOptions str         []
-globalread            Allow read from other directory true
-globalwrite           Allow write to other directory false
-gray                  Convert all colors to grayscale false
-gsOptions str         []
-h,-help               Show summary of options; command-line only
-historylines n        Retain n lines of history [1000]
-htmlviewerOptions str
                       []
-hyperrefOptions str   [setpagesize=false,unicode,pdfborder=0 0 0]
-ibl                   Enable environment map image-based lighting [false]
-iconify               Iconify rendering window [false]
-image str             Environment image name [snowyField]
-imageDir str          Environment image library directory [ibl]
-imageURL str          Environment image library URL [https://vectorgraphics.gitlab.io/asymptote/ibl]
-inlineimage           Generate inline embedded image [false]
-inlinetex             Generate inline TeX code [false]
-inpipe n              Input pipe [-1]
-interactiveMask       Mask fpu exceptions in interactive mode [true]
-interactiveView       View output in interactive mode [true]
-interactiveWrite      Write expressions entered at the prompt to stdout [true]
-interrupt             [false]
-k,-keep               Keep intermediate files [false]
-keepaux               Keep intermediate LaTeX .aux files [false]
-level n               Postscript level [3]
-l,-listvariables      List available global functions and variables [false]
-localhistory          Use a local interactive history file [false]
-loop                  Loop 3D animations [false]
-lossy                 Use single precision for V3D reals [false]
-lsp                   Interactive mode for the Language Server Protocol [false]
-m,-mask               Mask fpu exceptions; command-line only
-maxtile pair          Maximum rendering tile size [(1024,768)]
-maxviewport pair      Maximum viewport size [(0,0)]
-multiline             Input code over multiple lines at the prompt [false]
-multipleView          View output from multiple batch-mode files [false]
-multisample n         Multisampling width for screen images [4]
-offline               Produce offline html files [false]
-O,-offset pair        PostScript offset [(0,0)]

-outpipe n             Output pipe [-1]
-paperheight bp        Default page height [0]
-paperwidth bp         Default page width [0]
-p,-parseonly          Parse file [false]
-pdfreload             Automatically reload document in pdfviewer [false]
-pdfreloadOptions str  []
-pdfreloaddelay usec   Delay before attempting initial pdf reload [750000]
-pdfviewerOptions str  []
-position pair         Initial 3D rendering screen position [(0,0)]
-prc                   Embed 3D PRC graphics in PDF output [false]
-prerender resolution  Prerender V3D objects (0 implies vector output) [0]
-prompt str            Prompt [> ]
-prompt2 str           Continuation prompt for multiline input  [..]
-psviewerOptions str   []
-q,-quiet              Suppress welcome text and noninteractive stdout [false]
-render n              Render 3D graphics using n pixels per bp (-1=auto) [-1]
-resizestep step       Resize step [1.2]
-reverse               reverse 3D animations [false]
-rgb                   Convert cmyk colors to rgb [false]
-safe                  Disable system call [true]
-scroll n              Scroll standard output n lines at a time [0]
-shiftHoldDistance n   WebGL touch screen distance limit for shift mode [20]
-shiftWaitTime ms      WebGL touch screen shift mode delay [200]
-spinstep deg/s        Spin speed [60]
-svgemulation          Emulate unimplemented SVG shading [true]
-tabcompletion         Interactive prompt auto-completion [true]
-tex engine            latex|pdflatex|xelatex|lualatex|tex|pdftex|luatex|context|none [latex]
-thick                 Render thick 3D lines [true]
-thin                  Render thin 3D lines [true]
-threads               Use POSIX threads for 3D rendering [true]
-toolbar               Show 3D toolbar in PDF output [true]
-s,-translate          Show translated virtual machine code [false]
-twice                 Run LaTeX twice (to resolve references) [false]
-twosided              Use two-sided 3D lighting model for rendering [true]
-u,-user str           General purpose user string
-v,-verbose            Increase verbosity level (can specify multiple times) 0
-version               Show version; command-line only
-vibrateTime ms        WebGL shift mode vibrate duration [25]
-viewportmargin pair   Horizontal and vertical 3D viewport margin [(0.5,0.5)]
-wait                  Wait for child processes to finish before exiting [false]
-warn str              Enable warning; command-line only
-webgl2                Use webgl2 if available [false]
-where                 Show where listed variables are declared [false]
-wsl                   Run asy under the Windows Subsystem for Linux. [false]
-xasy                  Interactive mode for xasy [false]
-zoomPinchCap limit    WebGL maximum zoom pinch [100]
-zoomPinchFactor n     WebGL zoom pinch sensitivity [10]
-zoomfactor factor     Zoom step factor [1.05]
-zoomstep step         Mouse motion zoom step [0.1]
```
