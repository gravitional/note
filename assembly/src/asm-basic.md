# nasm 安装使用

## [安装](https://labs.bilimedtech.com/nasm/windows-install/3.html)

[nasm + vscode 搭建ASM汇编开发环境](https://www.cnblogs.com/mq0036/p/16975944.html)

windows 安装了 strawberry-perl 环境之后, 会自带 `nasm` 和 `gcc`

```bash
which perl
C:\MyProgram\strawberry-perl\c\bin\nasm.exe
which gcc
C:\MyProgram\strawberry-perl\c\bin\gcc.exe
```

## nasm 使用

[NASM Tutorial](https://cs.lmu.edu/~ray/notes/nasmtutorial/)
[MSVC x64 calling convention](https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170&viewFallbackFrom=vs-2019)

直接套用通常教程中的的命令编译执行会报错

```bash
xxx/ld.exe: i386 architecture of input file `helloworld.obj' is incompatible with i386:x86-64 output

xxx/ld.exe: helloworld.obj:helloworld.asm:(.text+0x6): undefined reference to `_printf'

xxx/ld.exe: xxx/libmingw32.a(lib64_libmingw32_a-crtexewin.o):crtexewin.c:(.text+0x130): undefined reference to `WinMain'
```

+ 一是因为现在的电脑大多是 `x86-64`指令集架构
+ 二是因为 MSVC x64 的 calling convention
+ 三是因为默认的 `exe` 是窗口程序, 需要连接到 `WinMain`

见 [Topic: Need help in dealing with "undefined reference to `_printf'" error](https://forum.nasm.us/index.php?topic=2669.0).

您试图用 64 位 assembler/linker 选项编译 32 位示例.
如果你想要 32 位版本.
只要使用正确的 汇编 和 链接 命令, 应该可以正常工作.
注意是 `gcc -m32 -mconsole hello32.obj -o hello32.exe`

```asm
global  _main
;;assemble with > nasm -fwin32 hello32.asm
;;Link with > gcc -m32 -mconsole hello32.obj -o hello32.exe

    extern  _printf
    section .text
_main:
    push    message
    call    _printf
    add     esp, 4

    mov eax,0  ;;return code of our program
    ret
message:
    db  'Hello, World', 10, 0
```

如果您打算在 64 位 `.NET` 环境下运行此程序,
您可能需要查看以下链接以获取更多信息:

1. [notes/nasmtutorial/](https://cs.lmu.edu/~ray/notes/nasmtutorial/)
尤其是 "在 Windows 中使用 Nasm "部分. (不错的教程, 作者可能就在本论坛中)

2. [Windows 的 x64 调用约定](https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention?view=vs-2019).

+ 我注意到, 在 64 位与 C 库链接时, 需要去掉下划线
(`main` 而不是 `_main`, `printf` 而不是 `_printf`,
我对此没有明确的解释).
+ 对 `WinMain` 的未定义引用:
您需要使用 console subsystem 来告诉链接器这是一个控制台应用程序.
链接方法: `gcc -m64 -mconsole hello.obj -o hello.exe`

```asm
;;64 bit version.
;;assemble with >  nasm -fwin64 hello.asm
;; link with >  gcc -m64 -mconsole hello.obj -o hello.exe
global  main
    extern  printf
    section .text
main:
;---------
    push rbp
    mov rbp, rsp
    ;; reserve for local variables
    ;; 2 qwords for argc and argv
    ;; and 1 qword for return value
    ;; 1 more qword to align with 16 bits
    sub rsp, 8*4

    mov rcx,   message
    call    printf

    mov rax,0
    add rsp,8*4    ;; deallocate local variables
    pop rbp
    ret
;---------
message:
    db  'Hello, World', 13, 10, 0
```
