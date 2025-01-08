# openHarmony 的 LLDB工具介绍

https://gitee.com/openharmony/third_party_llvm-project/blob/master/lldb/README_zh.md#

LLDB（Low Level Debugger）是新一代高性能调试器。当前OpenHarmony主干分支的LLDB工具是在[llvm15.0.4](https://github.com/llvm/llvm-project/releases/tag/llvmorg-15.0.4)基础上适配演进出来的工具。支持在桌面和Openharmony设备或模拟器上调试。

> #### 说明
>
> 在应用调试中，配合[hdc工具](https://gitee.com/openharmony/docs/blob/master/zh-cn/device-dev/subsystems/subsys-toolchain-hdc-guide.md)，实现远程调试。

## 2. LLDB工具获取

### LLDB工具获取方式

通过OpenHarmony的SDK获取，获取路径：[每日构建](http://ci.openharmony.cn/workbench/cicd/dailybuild) 。

LLDB工具在SDK中的路径：**\ohos-sdk\\\[system]\native\llvm。

### 使用举例：

下面以windows平台使用方式举例

  解压sdk，直接在LLDB工具所在路径\**\ohos-sdk\windows\native\llvm\bin运行lldb.exe即可。

远程调试时需要lldb-server和lldb配合使用。

**表1** lldb-server架构说明

| lldb-server所在路径                                          | 说明                                        |
| ------------------------------------------------------------ | ------------------------------------------- |
| **\ohos-sdk\\[system]\native\llvm\lib\clang\\\[version]\bin\aarch64-linux-ohos\lldb-server | 当调试的设备为ohos平台，aarch64架构时使用。 |
| **\ohos-sdk\\\[system]\native\llvm\lib\clang\\\[version]\bin\arm-linux-ohos\lldb-server | 当调试的设备为ohos平台，arm架构时使用。     |
| **\ohos-sdk\\\[system]\native\llvm\lib\clang\\\[version]\bin\x86_64-linux-ohos\lldb-server | 当调试的设备为ohos平台，x86_64架构时使用。  |

> **说明**
>
> 表2 system参数取值说明
>
> | system参数取值 | 说明                    |
> | -------------- | ----------------------- |
> | windows        | 适用于windows平台的工具 |
> | linux          | 适用于linux平台的工具   |
> | darwin         | 适用于Mac平台的工具     |
> | ohos           | 适用于ohos平台的工具|
>
> Mac平台的工具有arm64架构和x86_64架构区分。可根据”组件形态“进行区分，其中mac-sdk-full和mac-sdk-public的适用于x86_64架构。mac-sdk-m1-full和mac-sdk-m1-public的适用于arm64架构。
>
> lldb-server所在路径会根据llvm版本变更有所变化。比如12.0.1版本的llvm，lldb-server所在目录则在\***\native\llvm\lib\clang\12.0.1\bin；15.0.4版本的llvm，lldb-server所在目录则在*\**\native\llvm\lib\clang\15.0.4\bin。

> **注意**
>
> - 尽可能使用同一SDK中配套的LLDB和hdc工具。
> - 尽可能保证待调试的ELF文件或应用编译时使用的编译器和获取的LLDB调试器工具来源同一版本的工具链。

# 3. LLDB命令
>
> **说明**
>
> 更多原始命令可参考[GDB to LLDB command map](https://lldb.llvm.org/use/map.html)
>
> 获取更多命令，可以在LLDB命令行窗口执行`help`
>
> ```
> (lldb) help
> Debugger commands:
>   apropos           -- List debugger commands related to a word or subject.
>   breakpoint        -- Commands for operating on breakpoints (see 'help b' for shorthand.)
>   command           -- Commands for managing custom LLDB commands.
>   disassemble       -- Disassemble specified instructions in the current target.  Defaults to the current function for the current thread and stack frame.
>   expression        -- Evaluate an expression on the current thread.  Displays any returned value with LLDB's default formatting.
>   frame             -- Commands for selecting and examing the current thread's stack frames.
> ...
> ```
>

## 3.1 option相关的命令

- -v/--version用于显示LLDB相关的版本信息。

  **表3** 命令说明

  |    返回值    |               返回值说明               |
  | :----------: | :------------------------------------: |
  | 返回对应信息 | 版本信息，其中revision后跟的是commitID |

  使用方法：

  ```
  lldb -v / lldb --version
  ```

- -h/--help用于显示LLDB相关的帮助信息。

  **表4** 命令说明

  |    返回值    | 返回值说明 |
  | :----------: | :--------: |
  | 返回对应信息 |  帮助信息  |

  使用方法：

  ```
  E:\ohos-sdk\windows\native\llvm\bin>lldb.exe --help
  OVERVIEW: LLDB

  USAGE: lldb.exe [options]

  ATTACHING:
    --attach-name <name> Tells the debugger to attach to a process with the given name.
    --attach-pid <pid>   Tells the debugger to attach to a process with the given pid.
    -n <value>           Alias for --attach-name
    -p <value>           Alias for --attach-pid
    --wait-for           Tells the debugger to wait for a process with the given pid or name to launch before attaching.
    -w                   Alias for --wait-for
  ...
  ```

- --arch <architecture>/-arch <architecture>/-a <value> 告诉调试器在启动和运行程序时使用指定的体系结构。

  使用方法：

  ```
  E:\ohos-sdk\windows\native\llvm\bin>lldb.exe --arch arm
  ```

- --debug/-d 告诉调试器打印出更多信息以进行调试。

  使用方法：

  ```
  E:\ohos-sdk\windows\native\llvm\bin>lldb.exe --debug
  ```

- --no-use-colors/-X 不使用颜色。

  使用方法：

  ```
  E:\ohos-sdk\windows\native\llvm\bin>lldb.exe --no-use-colors
  ```

## 3.2 日志

为了方便定位问题，可以使用`log <subcommand> [<command-options>]`命令记录LLDB日志。

- help log 查看log命令帮助

  ```
  (lldb) help log
  Commands controlling LLDB internal logging.

  Syntax: log <subcommand> [<command-options>]

  The following subcommands are supported:

        disable -- Disable one or more log channel categories.
        enable  -- Enable logging for a single log channel.
        list    -- List the log categories for one or more log channels.  If none specified, lists them all.
        timers  -- Enable, disable, dump, and reset LLDB internal performance timers.

  For more help on any particular subcommand, type 'help <command> <subcommand>'.
  ```

- log list 查看支持的日志列表

  ```bash
  (lldb) log list
  ```

- 打印LLDB的所有日志到D:\lldb.log（-T 带时间戳，-S 将堆栈回溯附加到每个日志行）

  ```bash
  (lldb) log enable lldb all -T -S -f D:\lldb.log
  ```

- 记录平台事件和活动到D:\platform.log。

  ```bash
  (lldb) log enable lldb platform -f D:\platform.log

- 记录模块活动，例如创建、销毁、替换模块等到D:\module.log。

  ```bash
  (lldb) log enable lldb module -f D:\module.log
  ```

- 打印gdb-remote的所有日志到D:\gdb-remote.log。

  ```bash
  (lldb) log enable gdb-remote all -f D:\gdb-remote.log
  ```

  其他命令可结合help命令自行获取，例如`help log enable'` 查看log enable命令信息。

  ```bash
  (lldb) help log enable
  Enable logging for a single log channel.

  Syntax: log enable <cmd-options> <log-channel> <log-category> [<log-category> [...]]

  Command Options Usage:
    log enable [-FSTanpstv] [-f <filename>] <log-channel> <log-category> [<log-category> [...]]

         -F ( --file-function )
              Prepend the names of files and function that generate the logs.

         -S ( --stack )
              Append a stack backtrace to each log line.

         -T ( --timestamp )
              Prepend all log lines with a timestamp.

         -a ( --append )
              Append to the log file instead of overwriting.

         -f <filename> ( --file <filename> )
              Set the destination file to log to.

         -n ( --thread-name )
              Prepend all log lines with the thread name for the thread that generates the log line.

         -p ( --pid-tid )
              Prepend all log lines with the process and thread ID that generates the log line.

         -s ( --sequence )
              Prepend all log lines with an increasing integer sequence id.

         -t ( --threadsafe )
              Enable thread safe logging to avoid interweaved log lines.

         -v ( --verbose )
              Enable verbose logging.

       This command takes options and free-form arguments.  If your arguments resemble option specifiers (i.e., they start with a - or --), you must use ' -- '
       between the end of the command options and the beginning of the arguments.
  ```

## 3.3 平台

LLDB中用于管理和创建平台的命令有`platform [connect|disconnect|info|list|status|select] ...`

### 3.3.1 platform相关信息

使用LLDB时，如果想获取有关platform的帮助信息和相关的命令，可以使用`help platform` 和`apropos platform`命令。

- windows平台查看platform帮助的信息。

  ```bash
  (lldb) help platform
  Commands to manage and create platforms.

  Syntax: platform [connect|disconnect|info|list|status|select] ...

  The following subcommands are supported:

        connect        -- Select the current platform by providing a connection URL.
        disconnect     -- Disconnect from the current platform.
        file           -- Commands to access files on the current platform.
        get-file       -- Transfer a file from the remote end to the local host.
        get-size       -- Get the file size from the remote end.
        list           -- List all platforms that are available.
        mkdir          -- Make a new directory on the remote end.
        process        -- Commands to query, launch and attach to processes on the current platform.
        put-file       -- Transfer a file from this system to the remote end.
        select         -- Create a platform if needed and select it as the current platform.
        settings       -- Set settings for the current target's platform, or for a platform by name.
        shell          -- Run a shell command on the current platform.  Expects 'raw' input (see 'help raw-input'.)
        status         -- Display status for the current platform.
        target-install -- Install a target (bundle or executable file) to the remote end.

  For more help on any particular subcommand, type 'help <command> <subcommand>'.
  (lldb)
  ```

- windows平台查看与platform相关的命令。

  ```
  (lldb) apropos platform
  The following commands may relate to 'platform':
    platform                -- Commands to manage and create platforms.
    platform connect        -- Select the current platform by providing a connection URL.
    platform disconnect     -- Disconnect from the current platform.
    platform file           -- Commands to access files on the current platform.
    platform list           -- List all platforms that are available.
    platform process        -- Commands to query, launch and attach to processes on the current platform.
    platform process launch -- Launch a new process on a remote platform.
    platform process list   -- List processes on a remote platform by name, pid, or many other matching attributes.
    platform select         -- Create a platform if needed and select it as the current platform.
    platform settings       -- Set settings for the current target's platform, or for a platform by name.
    platform shell          -- Run a shell command on the current platform.
    platform status         -- Display status for the current platform.
    process                 -- Commands for interacting with processes on the current platform.

  The following settings variables may relate to 'platform':
    target.auto-install-main-executable -- Always install the main executable when connected to a remote platform.
    target.breakpoints-use-platform-avoid-list -- Consult the platform module avoid list when setting non-module specific breakpoints.
    symbols.enable-external-lookup -- Control the use of external tools and repositories to locate symbol files. Directories listed in
                                      target.debug-file-search-paths and directory of the executable are always checked first for separate debug info files.
                                      Then depending on this setting: On macOS, Spotlight would be also used to locate a matching .dSYM bundle based on the
                                      UUID of the executable. On NetBSD, directory /usr/libdata/debug would be also searched. On platforms other than NetBSD
                                      directory /usr/lib/debug would be also searched.
    plugin.jit-loader.gdb.enable -- Enable GDB's JIT compilation interface (default: enabled on all platforms except macOS)
  ```

### 3.3.2 查看可用的platform信息

LLDB可以使用`platform list`命令查看可用的platform信息。

- windows平台查看可用的platform。

  ```
  (lldb) platform list
  Available platforms:
  host: Local Windows user platform plug-in.
  remote-linux: Remote Linux user platform plug-in.
  remote-android: Remote Android user platform plug-in.
  remote-freebsd: Remote FreeBSD user platform plug-in.
  remote-gdb-server: A platform that uses the GDB remote protocol as the communication transport.
  darwin: Darwin platform plug-in.
  remote-ios: Remote iOS platform plug-in.
  remote-macosx: Remote Mac OS X user platform plug-in.
  host: Local Mac OS X user platform plug-in.
  remote-netbsd: Remote NetBSD user platform plug-in.
  remote-openbsd: Remote OpenBSD user platform plug-in.
  qemu-user: Platform for debugging binaries under user mode qemu
  remote-windows: Remote Windows user platform plug-in.
  remote-hos: Remote HarmonyOS user platform plug-in.
  remote-ohos: Remote Open HarmonyOS user platform plug-in.
  (lldb)
  ```

- linux平台查看可用的platform。

  ```
  (lldb) platform list
  Available platforms:
  host: Local Linux user platform plug-in.
  remote-linux: Remote Linux user platform plug-in.
  remote-android: Remote Android user platform plug-in.
  remote-freebsd: Remote FreeBSD user platform plug-in.
  remote-gdb-server: A platform that uses the GDB remote protocol as the communication transport.
  darwin: Darwin platform plug-in.
  remote-ios: Remote iOS platform plug-in.
  remote-macosx: Remote Mac OS X user platform plug-in.
  host: Local Mac OS X user platform plug-in.
  remote-netbsd: Remote NetBSD user platform plug-in.
  remote-openbsd: Remote OpenBSD user platform plug-in.
  qemu-user: Platform for debugging binaries under user mode qemu
  remote-windows: Remote Windows user platform plug-in.
  remote-hos: Remote HarmonyOS user platform plug-in.
  remote-ohos: Remote Open HarmonyOS user platform plug-in.
  ```

### 3.3.3 platform选择

LLDB可以使用` platform select <platform-name> `命令选择即将连接的远端的platform信息。

- platform select 选择平台。
  **示例** 调试应用在ohos平台设备，如RK3568、一些phone等。

  ```
  platform select remote-ohos
  ```

  > 注意：如果使用10.0.1版本的LLDB，ohos平台选择的是`remote-hos`。

- 选择远程连接的平台的同时，指定包含所有远程系统文件根目录的SDK跟目录。

  ```
  platform select --sysroot <filename> <platform-name>
  ```

### 3.3.4 platform状态信息

LLDB可以使用`platform status`命令查看远端连接的平台状态信息。

- 远端平台未连接时的状态信息。

  ```
  (lldb) platform select remote-ohos
    Platform: remote-ohos
   Connected: no
   Container: no
  (lldb) platform status
    Platform: remote-ohos
   Connected: no
   Container: no
  ```

- 远端平台连接成功时的状态信息。

  ```
  ./lldb-server platform --server --listen "*:8080"

  (lldb) platform select remote-ohos
    Platform: remote-ohos
   Connected: no
   Container: no
  (lldb) platform connect connect://localhost:8080
    Platform: remote-ohos
      Triple: arm-unknown-linux-unknown
    Hostname: localhost
   Connected: yes
   Container: no
  WorkingDir: /data/local/tmp
      Kernel: #1 SMP Fri Mar 31 20:52:12 CST 2023
  (lldb) platform status
    Platform: remote-ohos
      Triple: arm-unknown-linux-unknown
  OS Version: 4294967295 (5.10.97)
    Hostname: localhost
   Connected: yes
   Container: no
  WorkingDir: /data/local/tmp
      Kernel: #1 SMP Fri Mar 31 20:52:12 CST 2023
  (lldb)
  ```

### 3.3.5 连接platform

LLDB可以使用`platform connect <connect-url>`命令指定要连接的平台。

- 通过端口号建立连接。

  **示例**

  步骤一： lldb-server端创建连接。

  ```
  # ./lldb-server platform --server --listen "*:8080"
  ```

  步骤二：lldb客户端建立连接。

  ```
  (lldb) platform select remote-ohos
    Platform: remote-ohos
   Connected: no
   Container: no
  (lldb) platform connect connect://localhost:8080
    Platform: remote-ohos
      Triple: arm-unknown-linux-unknown
    Hostname: localhost
   Connected: yes
   Container: no
  WorkingDir: /data/local/tmp
      Kernel: #1 SMP Fri Mar 31 20:52:12 CST 2023
  ```

  lldb-server端连接成功响应。

  ```
  # ./lldb-server platform --server --listen "*:8080"
  Connection established.
  ```

- 使用unix socket方式建立连接。

  **示例**

  步骤一：lldb-server端创建连接

  ```
  ./lldb-server platform --server --listen unix-abstract:///com.example.myapplication/platform-1667463465318.sock
  ```

  步骤二：LLDB客户端建立连接

  ```
  (lldb) platform select remote-ohos
    Platform: remote-ohos
   Connected: no
   Container: no
  (lldb) platform connect unix-abstract-connect:///com.example.myapplication/platform-1667463465318.sock
    Platform: remote-ohos
      Triple: arm-unknown-linux-unknown
    Hostname: localhost
   Connected: yes
   Container: no
  WorkingDir: /data/local/tmp
      Kernel: #1 SMP Fri Mar 31 20:52:12 CST 2023
  ```

  lldb-server端连接成功响应。

  ```
  Connection established.
  ```

### 3.3.6 断开连接

LLDB可以使用`platform disconnect`命令断开与当前平台的连接。

- 断开与当前平台的连接。

  **示例**

  LLDB客户端断开连接。

  ```
  (lldb) platform disconnect
  Disconnected from "localhost"
  ```

  lldb-server端断开连接响应。

  ```
  error: lost connection
  lldb-server exiting..
  ```

### 3.3.7 查看platform连接端进程

LLDB可以使用`platform process list`查看远程连接的平台上的进程信息。

- platform process list 列举platform连接端的进程。

  ```
  (lldb) platform process list
  ```

- 列举远程连接的平台上特定PID的进程信息。

  ```
  (lldb) platform process list --pid <pid>
  (lldb) platform process list -p <pid>
  ```

## 3.4 函数

### 3.4.1 是否进入带调试信息的函数

使用LLDB调试遇到函数时，用`thread step-in <cmd-options> [<thread-id>]`（thread step-in可简写为step或s）命令可以进入函数（函数必须有调试信息）。

- 进入带调试信息的函数内。

  ```
  (lldb) thread step-in
  (lldb) step
  (lldb) s
  ```

可以使用`thread step-over <cmd-options> [<thread-id>]`（thread step-over可简写为next或n）不进入函数，会直接执行下一行代码。

- 不进入函数内，进行源代码级别的单步执行。

  ```
  (lldb) thread step-over
  (lldb) next
  (lldb) n
  ```

### 3.4.2 进入不带调试信息的函数

默认情况下，LLDB执行`thread step-in`命令（“thread step-in”和“step”的缩写均是“s”）不会进入不带调试信息的函数。

- 跳过不带调试信息的函数，此时的功能类似与`thread step-over`。

  ```
  (lldb) thread step-in
  (lldb) step
  (lldb) s
  ```

### 3.4.3 退出正在调试的函数

LLDB中使用`finish <cmd-options> [<thread-id>]`完成当前堆栈帧的执行。即当单步调试一个函数时，如果不想继续跟踪下去了，就可以使用“finish”命令退出。也可使用`thread step-out <cmd-options> [<thread-id>]`。

- 退出当前调试的函数。

  ```
  (lldb) finish
  (lldb) thread step-out
  ```

### 3.4.4 直接执行函数

使用LLDB调试程序时，可以使用“call”或“expr”或“print”命令直接调用函数执行。

- 使用expr命令调用函数。

  ```
  (lldb) expr (int) printf ("Print nine: %d.", 4 + 5)
  ```

- 使用call命令调用函数。

  ```
  (lldb) call (int) printf ("Print nine: %d.", 4 + 5)
  ```

- 使用print命令调用函数。

  ```
  (lldb) print (int) printf ("Print nine: %d.", 4 + 5)
  ```

## 3.5 断点

LLDB中可以使用`breakpoint <subcommand> [<command-options>]`或`_regexp-break`命令设置断点。

### 3.5.1 设置行断点

**示例**：在文件中的某一行打断点。

- 方法一  (此方法仅适用于源文件是单独文件时使用)

  ```
  (lldb) b 8
  ```

- 方法二

  ```
  (lldb) breakpoint set --file hello.cpp --line 9
  (lldb) br s -f hello.cpp -l 9
  ```

### 3.5.2 设置函数断点

- 给函数打断点。

  ```
  (lldb) b main
  (lldb) breakpoint set --method main
  (lldb) br s -M main
  ```

- 使用`_regexp-break <module>`<name>`给特定库中的函数打断点

  ```
  (lldb) _regexp-break libc.so`malloc
  ```

### 3.5.3 设置条件断点

LLDB可以设置条件断点，也就是只有在条件满足时，断点才会被触发。使用-c参数设置。

- 设置当hello.cpp中的第10行满足i ==5 时的条件断点。

  ```
  (lldb) breakpoint set --file hello.cpp --line 10 --condition 'i==5'
  (lldb) breakpoint set -f hello.cpp -l 10 -c 'i==5'
  ```

### 3.5.4 设置临时断点

在使用LLDB时，如果想让断点只生效一次，可以使用_regexp-tbreak命令（简写tb）。

- 临时函数断点。

  ```
  (lldb) _regexp-tbreak test
  ```

- 临时行断点。

  ```
  (lldb) _regexp-tbreak hello.cpp:10
  ```

### 3.5.5 列出所有断点

使用LLDB工具，可以利用`breakpoint list <cmd-options> [<breakpt-id>]`（breakpoint list简写为br list）查看所有断点。

- 查看所有断点。

  ```
  (lldb) breakpoint list
  (lldb) br list
  ```

- 查看内部断点。

  ```
  (lldb) breakpoint list --internal
  (lldb) br list -i
  ```

- 给出断点及其位置的完整描述。

  ```
  (lldb) breakpoint list --full
  (lldb) br list -f
  ```

- 解析关于断点的信息。

  ```
  (lldb) breakpoint list --verbose
  (lldb) br list -v
  ```

### 3.5.6 删除断点

可以使用`breakpoint delete <cmd-options> [<breakpt-id | breakpt-id-list>]`命令（breakpoint delete 简写br delete）删除断点。

- 删除某个断点。

  ```
  (lldb) breakpoint delete 2
  ```

- 删除所有断点

  ```
  (lldb) breakpoint delete
  ```

- 删除某个文件的某行断点

  ```
  (lldb) breakpoint clear -l 20 -f hello.cpp
  (lldb) breakpoint clear --line 20 --file hello.cpp
  ```

### 3.5.7 禁用断点

在使用LLDB时，如果想禁用设置的断点而不是删除断点，可以使用`breakpoint disable [<breakpt-id | breakpt-id-list>]`（breakpoint disable简写br dis）。以上面程序为例：

- 禁用某个断点。

  ```
  (lldb) breakpoint disable 1
  1 breakpoints disabled.
  ```

- 禁用所有断点。

  ```
  (lldb) breakpoint disable
  ```

### 3.5.8 启用断点

在使用LLDB时，如果想启用指定的禁用断点，可以使用`breakpoint enable [<breakpt-id | breakpt-id-list>]`（breakpoint enable简写br en）。

- 启用某个禁用的断点。

  ```
  (lldb) breakpoint enable 3
  1 breakpoints enabled.
  ```

- 启用所有禁用的断点。

  ```
  (lldb) breakpoint enable
  ```

## 3.6 观察点

LLDB中使用`watchpoint <subcommand> [<command-options>]`设置观察点。与breakpoint相比，watchpoint 是当某个变量/表达式发生变化时，停止。

### 3.6.1设置观察点

LLDB可以使用`watchpoint set <subcommand> [<subcommand-options>]`（watchpoint set简写为wa s）命令设置观察点。

- 给变量设置观察点。

  ```
  (lldb) watchpoint set variable global_var
  (lldb) wa s v global_var
  ```

- 设置表达式观察点。

  ```
  (lldb) watchpoint set expression -- my_ptr
  (lldb) wa s e -- my_ptr
  ```

### 3.6.2 列出所有观察点

LLDB可以使用`watchpoint list <cmd-options> [<watchpt-id | watchpt-id-list>]`（watchpoint list简写为watch l）列出所有观察点。

- 列出所有观察点。

  ```
  (lldb) watchpoint list
  (lldb) watch l
  ```

- 解析所有观察点信息。

  ```
  (lldb) watchpoint list -v
  (lldb) watch l -v
  ```

- 列出所有观察点有关位置描述的全部信息

  ```
  (lldb) watchpoint list -f
  (lldb) watch l -f
  ```

### 3.6.3 删除观察点

LLDB可以使用`watchpoint delete <cmd-options> [<watchpt-id | watchpt-id-list>]`（watchpoint delete简写为watch del）删除观察点。

- 删除某个观察点

  ```
  (lldb) watchpoint delete 2
  1 watchpoints deleted.
  ```

- 删除所有的观察点

  ```
  (lldb) watchpoint delete
  About to delete all watchpoints, do you want to do that?: [Y/n] Y
  All watchpoints removed. (2 watchpoints)
  ```

### 3.6.4 禁用观察点

LLDB可以使用watchpoint disable（简写为watch dis）禁用观察点。

- 禁用某个观察点

  ```
  (lldb) watchpoint disable 2
  1 watchpoints disabled.
  ```

- 禁用所有的观察点

  ```
  (lldb) watchpoint disable
  (lldb) watch dis
  ```

### 3.6.5 启用观察点

LLDB可以使用watchpoint enable（简写为watch en）启用被禁用的观察点。

- 启用某个观察点

  ```
  (lldb) watchpoint enable 2
  1 watchpoints enabled.
  ```

- 启用所有被禁用的观察点

  ```
  (lldb) watchpoint enable
  (lldb) watch en
  ```

## 3.7 表达式

LLDB中使用`expression <cmd-options> -- <expr>`（expression 简写为expr）可以实现表达式求值。

- 创建一个变量

  ```
  (lldb) print int $value1 = 7
  (lldb) expression int $value2 = 7
  ```

- 打印变量值

  ```
  (lldb) print $value1
  (lldb) expression $value2
  ```

- 打印全局变量global_var

  ```
  (lldb) expression global_var
  ```

- 变量运算

  ```
  (lldb) expression global_var * 3
  ```

- 打印变量（默认十进制）

  ```
  (lldb) print value
  (lldb) expression value
  (lldb) p value
  ```

- 打印变量（十六进制）

  ```
  (lldb) print/x value
  (lldb) expression/x value
  (lldb) p/x value
  ```

- 打印变量（八进制）

  ```
  (lldb) print/o value
  (lldb) expression/o value
  (lldb) p/o value
  ```

- 打印变量（二进制）

  ```
  (lldb) print/t value
  (lldb) expression/t value
  (lldb) p/t value
  ```

## 3.8 查看变量

在LLDB中可以使用`frame variable <cmd-options> [<variable-name> [<variable-name> [...]]]`和`target variable <cmd-options> <variable-name> [<variable-name> [...]]`命令显示变量信息。

- 显示当前帧的参数和局部变量。

  ```
  (lldb) frame variable
  (lldb) fr v
  ```

- 显示当前帧源文件全局变量和静态变量。

  ```
  (lldb) frame variable -g
  (lldb) fr v -g
  ```

- 在运行进程之前或期间读取当前目标的全局变量。

  ```
  (lldb) target variable
  (lldb) ta v
  ```

- 获取指定文件的全局变量。

  ```
  (lldb) target variable --file hello.cpp
  (lldb) ta v --file hello.cpp
  ```

## 3.9 进程/线程

### 3.9.1 堆栈回溯

使用LLDB调试程序时，可以使用`thread backtrace <cmd-options>`（thread backtrace简写bt）显示当前线程的堆栈回溯。

- 显示当前线程的堆栈回溯。

  ```
  (lldb) thread backtrace
  (lldb) bt
  ```

- 回溯当前线程的前两帧。

  ```
  (lldb) thread backtrace -c 2
  ```

- 显示所有线程的堆栈回溯

  ```
  (lldb) thread backtrace all
  (lldb) bt all
  ```

- 退出当前堆栈帧。

  ```
  (lldb) thread return
  ```

### 3.9.2 显示当前目标进程中的线程

使用LLDB调试程序时，可以用`thread list`显示当前目标进程中的线程。

- 显示当前目标进程中的线程。

  ```
  (lldb) thread list
  ```

- 显示线程信息。

  ```
  (lldb) thread info
  ```

### 3.9.3 线程内栈帧

使用LLDB调试时，可以使用`frame select <cmd-options> [<frame-index>]`在当前线程中切换栈帧。

- 选择当前线程中索引值为2的栈帧。

  ```
  (lldb) frame select 2
  ```

- 选择调用当前堆栈帧的堆栈帧。

  ```
  (lldb) up
  (lldb) frame select --relative=1
  ```

- 选择当前堆栈帧调用的堆栈帧。

  ```
  (lldb) down
  (lldb) frame select --relative=-1
  (lldb) fr s -r-1
  ```

- 列出有关当前线程中当前选定帧的信息。

  ```
  (lldb) frame info
  ```

- 显示当前帧和源码。

  ```
  (lldb) frame select
  (lldb) f
  (lldb) process status
  ```

### 3.9.4 寄存器

LLDB中用于访问当前线程和堆栈帧的寄存器的命令为`register [read|write] ...`。

- 显示当前线程的通用寄存器。

  ```
   (lldb) register read
  ```

- 修改寄存器的值

  ```
   (lldb) register write rax 0x0000000000401470
  ```

## 3.10 可执行文件和共享库

在使用LLDB调试时，如果可执行文件的搜索路径与本地文件系统不一致时，可以使用`settings append target.exec-search-paths`设置搜索路径。

- 将编译的hap的libentry.so和libc++_shared.so所在路径’E:\DevEcoStudioProjects\MyApplication\entry\build\default\intermediates\cmake\default\obj\armeabi-v7a‘设置到调试搜索路径中。

  ```
  (lldb) settings append target.exec-search-paths "E:\DevEcoStudioProjects\MyApplication\entry\build\default\intermediates\cmake\default\obj\armeabi-v7a"
  ```

- target symbols add 加载符号文件

  ```
  (lldb) target symbols add a.out
  ```

如果想要访问一个或多个目标模块的信息，可以使用`target modules <sub-command>`或`target`命令。

- 列出当前可执行文件和相关共享库信息。

  ```
  (lldb) target modules list
  (lldb) image list
  ```

- 在可执行文件和相关共享库中查找函数或符号。

  ```
  (lldb) target modules lookup --name main
  (lldb) image lookup -n main
  ```

- 仅在a.out中查找地址信息。

  ```
  (lldb) target modules lookup --address 0x4012df a.out
  (lldb) image lookup -a 0x4012df a.out
  ```

## 3.11 源文件

使用LLDB调试时，如果想要获取关于源代码相关的信息，可以使用`source <subcommand> [<subcommand-options>]`命令。

- 显示当前目标进程的所在行信息。

  ```
  (lldb) source info
  ```

- 显示某个文件的信息

  ```
  (lldb) source info -f hello.cpp
  ```

- 查看当前目标进程的源码

  ```
  (lldb) source list
  ```

- 查看源码

  ```
  (lldb) _regexp-list
  (lldb) list
  ```

如果源文件不再位于与生成程序时相同的位置（例如，如果程序是在其他计算机上生成的），则需要告诉调试器如何在其本地文件路径而不是生成系统的文件路径中查找源。此时就需要重新映射调试会话的源文件路径名。lldb中可以使用`settings set target.source-map /buildbot/path /my/path`映射源文件路径。

- 可执行文件源码路径在D:\demo目录，但是将源码目录移动到了E:\demo。此时要重新映射调试的源文件路径。

  ```
  (lldb) settings set target.source-map D:\demo E:\demo
  ```

## 3.12 汇编

LLDB中使用`disassemble [<cmd-options>]`（disassemble 简写为dis或di）命令进行汇编处理。

- 显示当前堆栈帧的汇编指令。

  ```
  (lldb) disassemble --frame
  (lldb) dis -f
  (lldb) di -f
  ```

- 显示main函数的汇编指令

  ```
  (lldb) disassemble --name main
  (lldb) dis -n main
  (lldb) di -n main
  ```

- 显示特定数目的汇编指令

  ```
  (lldb) disassemble --count 10
  (lldb) dis -c 10
  (lldb) di -c 10
  ```

- 显示当前帧的源码行的汇编指令

  ```
  (lldb) disassemble --line
  (lldb) dis -l
  (lldb) di -l
  ```

## 3.13 改变程序的执行

### 3.13.1 跳转到指定位置执行

当使用LLDB调试时，可能走过了要调试的地方时，可以使用`jump`命令跳转到对应位置。

- 跳转到当前函数的27行

  ```
  (lldb) jump 27
  ```

  > **注意**
  >
  > 要谨慎使用jump命令，改变程序执行可能会出现不同的结果。
  >
  > jump命令不能跨函数。
  >

### 3.13.2 修改寄存器的值

PC寄存器会存储程序下一条要执行的指令，通过修改这个寄存器的值，可以达到改变程序执行流程的目的。

- 修改寄存器值。

  ```
  (lldb) register write pc `$pc+8`
  ```

## 3.14 信号

使用LLDB调试程序时，可以使用`process handle <cmd-options> [<unix-signal> [<unix-signal> [...]]]`命令管理LLDB对于当前目录进程的操作系统信号的处理策略。

- 显示当前目标进程的操作系统信号的LLDB处理策略。

  ```
  (lldb) process handle
  ```

- 给当前目标进程设置对于操作系统信号SIGRTMIN+1的处理策略。

  ```
  (lldb) process handle -p false -s false -n false SIGRTMIN+1
  ```

- 给当前目标进程设置对于操作系统信号SIGRTMIN+1的处理策略。

  ```
  (lldb) process handle -p true -s true -n false SIGRTMIN+1
  ```

  > **说明**
  >
  > -n <boolean> ( --notify <boolean> ): 如果收到信号，调试器是否应通知用户。true表示通知，false表示不通知。
  >
  > -p <boolean> ( --pass <boolean> ):是否应将信号传递给进程。true表示传递给进 程，false表示不传递。
  >
  > -s <boolean> ( --stop <boolean> ):如果接收到信号，是否应停止处理。true表示停止处理，false表示不停止处理。

## 3.15 core dump

在用LLDB调试程序时，可以用`process save-core [-s corefile-style -p plugin-name] FILE`命令来产生core dump文件。记录现在进程的状态，以供以后分析。

- 为当前进程生成core.dump文件。

  ```
  (lldb) process save-core --plugin-name=minidump --style=stack core.dump
  ```

- 利用core.dump文件，确认程序crash的位置。

  ```
  lldb --core /path/to/core
  lldb <project-name> -c /path/to/core
  ```

## 3.16 其他

### 3.16.1 关键字搜索

习惯了使用gdb命令，或许在使用LLDB时不清楚究竟应该使用哪个命令。可以使用`apropos <search-word>`命令查找与关键字相关的调试器命令。

- 列出与platform相关的调试器命令。

  ```
  (lldb) apropos platform
  ```

- 列出与info相关的调试器命令

  ```
  (lldb) apropos info
  ```

### 3.16.2 命令别名

LLDB中的命令别名机制可以为常用命令构造别名。使用`command alias <cmd-options> -- <alias-name> <cmd-name> [<options-for-aliased-command>]`构造命令别名。

- 构造断点命令别名

  原始命令

  ```
  (lldb) breakpoint set --file hello.cpp --line 12
  ```

  别名

  ```
  (lldb) command alias bfl breakpoint set --file hello.cpp --line %2
  ```

### 3.16.3 配置相关信息

- 显示调试器配置信息。

  ```
  (lldb) settings show
  ```

- 查看进程启动时环境信息。

  ```
  (lldb) target show-launch-environment
  ```

- 设置用户需要传递给可执行文件的环境变量及其值。

  ```
  (lldb) settings set target.env-vars MYPATH=~/.:/usr/bin SOME_ENV_VAR=12345
  ```

- 查看用户传递给可执行文件的环境变量及其值信息。

  ```
  (lldb) settings show target.env-vars
  (lldb) _regexp-env
  ```

- 设置是否从正在运行LLDB的进程继承环境信息。“true”表示继承；“false”表示不继承。

  ```
  (lldb) settings set target.inherit-env true
  ```

- 查看环境信息继承策略。

  ```
  (lldb) settings show target.inherit-env
  ```

- 更多“environment”相关调试器命令获取。

  ```
  (lldb) apropos environment
  ```

- 设置平台module cache路径。

  ```
  (lldb) setting set platform.module-cache-directory "D:\.lldb\module_cache"
  ```

- 查看平台module cache路径。

  一般windows平台默认的缓存路径为 "C:\Users\UserName\\.lldb\module_cache"。

  ```
  (lldb) setting show platform.module-cache-directory
  ```

- 设置平台module cache使用策略。（“true”表示开启；“false”表示关闭）

  ```
  setting set platform.use-module-cache true
  ```

> **说明**
>
> 平台module cache使用策略在开启的情况下，使用` setting set platform.module-cache-directory <value>`有效。
>
> 建议如果调试的设备版本有变化时，建议可以清空下历史缓存的module cache。

### 3.16.4 历史命令

LLDB支持使用`history <cmd-options>`命令处理会话中命令的历史记录。可以使用 "!<INDEX>"再次运行历史记录里面的命令。

- 查看历史历史记录。

  ```
  (lldb) session history
  (lldb) history
  ```

- 清空历史命令记录。

  ```
  (lldb) session history --clear
  (lldb) history --clear
  (lldb) history -C
  ```

- 列出历史记录中开始索引为2，结束索引为3的命令。

  ```
  (lldb) session history --start-index 2 --end-index 3
  (lldb) history -s 2 -e 3
  ```

### 3.16.5 命令自动联想补齐

LLDB支持输入命令起始字符，按`Tab`键实现命令自动联想补齐。

> 说明
>
> - 需要通过编译LLVM工程时添加`--build-libedit`参数使能此功能。建议和`--build-ncurses`参数一起使用。
> - 命令自动联想补齐功能适配平台有：Linux, Mac x86-64 和 M1。

- 输入pl，按`Tab`键实现命令自动联想。(LLDB命令中包含多个以"pl"开头的命令)

  ```
  (lldb) pl
  Available completions:
          platform -- Commands to manage and create platforms.
          plugin   -- Commands for managing LLDB plugins.
  (lldb) pl
  ```

- 输入his，按`Tab`键实现命令自动补全。(LLDB命令中仅包含一个以"his"开头的命令)

  ```
  (lldb) history
     0: history
  ```

### 3.16.6 图形化界面

LLDB支持使用`gui`命令进入图形化调试界面。

> 说明
>
> - 需要通过编译LLVM工程时添加`--build-ncurses`参数使能此功能。
>
> - 图形化调试界面适配平台有：Linux, Mac x86-64 和 M1。

- 进入图形化调试界面。

  ```
  (lldb) gui
  ```

# 4. windows平台远程调试

## 4.1 可执行文件调试

**场景一** 自定义源码，使用`target create <cmd-options> <filename>`或`file <cmd-options> <filename>`方式指定调试的可执行文件。其中可执行文件是使用SDK编译的。

**示例**

设备侧

运行lldb-server。

```
./lldb-server platform --server --listen "*:8080"
```

LLDB客户端

1）运行lldb.exe。

2）在lldb命令行窗口进行连接调试。

```
(lldb) platform select remote-ohos
  Platform: remote-ohos
 Connected: no
 Container: no
(lldb) platform connect connect://localhost:8080
  Platform: remote-ohos
    Triple: arm-unknown-linux-unknown
  Hostname: localhost
 Connected: yes
 Container: no
...
(lldb) target create <cmd-options> <filename>
...
(lldb) breakpoint set --name main
...
(lldb) platform disconnect
  Disconnected from "localhost"
(lldb) quit
```

**场景二** 自定义源码，使用attach方式调试SDK编译出的可执行文件

**示例**

设备侧

1）运行可执行文件，并使用ps命令查看进程pid。

```
./process-name
```

2）运行lldb-server

```
./lldb-server p --server --listen unix-abstract:///com.example.myapplication/platform-1648104534646.sock
```

LLDB客户端

1）运行lldb.exe。

2）在lldb命令行窗口进行连接调试。

```
(lldb) platform select remote-ohos
  Platform: remote-ohos
 Connected: no
 Container: no
(lldb) platform connect unix-abstract-connect:///com.example.myapplication/platform-1648104534646.sock
  Platform: remote-ohos
    Triple: arm-unknown-linux-unknown
  Hostname: localhost
 Connected: yes
 Container: no
WorkingDir: /data/local/tmp
    Kernel: #1 SMP Fri Mar 31 20:52:12 CST 2023
(lldb) breakpoint set --file <filename> --line <linenum>
...
(lldb) process attach --name process-name         // 或'attach <pid> | <process-name>'
...
(lldb) process detach                             // 或'detach'
Process ... detached
(lldb) platform disconnect
  Disconnected from "localhost"
(lldb) quit                                        // 简写'q'
```

> **说明：**
>
> 使用SDK编译源码参考[HOW TO USE NDK(windows)](https://gitee.com/openharmony/third_party_musl/wikis/HOW%20TO%20USE%20NDK%20(windows))或[HOW TO USE NDK(linux)](https://gitee.com/openharmony/third_party_musl/wikis/HOW%20TO%20USE%20NDK%20(linux))，也可以直接使用sdk中的clang或clang++编译器编译。
>
> 如，编译ohos平台，arm架构的可执行文件：<clang distribution>`/bin/clang++ --target=arm-linux-ohos--gcc-toolchain=<toolchain distribution>  --sysroot=<sysroot distribution> -O0 -g <file> -o <outfile>`
>
> 如果源文件不再位于与生成程序时相同的位置（可能程序是在其他计算机上生成的），则需要告诉调试器如何在其本地文件路径而不是生成系统的文件路径中查找源，则需要使用`settings set target.source-map /buildbot/path /my/path`重新映射调试会话的源文件路径名。
>
> 其中“/buildbot/path”为原始代码编译时的路径，“/my/path”为源代码当前所在路径。
>
> 例如：当要在另一台windows机器上调试此程序时，如果构建时源码路径为“D:\demo\hello_world.cpp”，当前源码路径为“F:\Test\hello_world.cpp”，则命令为`settings set target.source-map "D:\demo" "F:\Test"`
>
> 如果使用的编译参数OHOS_STL=c++_shared，调试的设备是arm32位，则还需要上传libc++_shared.so到/system/lib目录下；如果调试的设备是aarch64位，则上传libc++_shared.so到/system/lib64目录；如果使用的编译参数OHOS_STL=c++_static，则无需上传libc++_shared.so。
>
> libc++_shared.so 来自sdk中的native目录。
>
> 如果上传libc++_shared.so提示“[Fail]Error opening file: read-only file system...”，则先执行`hdc.exe shell mount -o rw,remount /`

## 4.2 hap包调试

**场景一** 基于DevEco编译的debug版本的hap包调试

**示例**

设备侧

1）安装hap包并运行，进入使用ps查看应用pid。

```
hdc shell bm install -p <file-path>
hdc shell aa start  -a <ability-name> -b <bundle-name> && hdc shell pidof <bundle-name>
```

2）运行lldb-server。

```
./lldb-server platform --server --listen "*:8080"
```

LLDB客户端

1）运行lldb.exe。

2）在lldb命令行窗口进行连接调试。

```
(lldb) platform select remote-ohos
  Platform: remote-ohos
 Connected: no
 Container: no
(lldb) platform connect connect://localhost:8080
  Platform: remote-ohos
    Triple: arm-unknown-linux-unknown
  Hostname: localhost
 Connected: yes
 Container: no
WorkingDir: /data/local/tmp
    Kernel: #1 SMP Fri Mar 31 20:52:12 CST 2023
(lldb) settings append target.exec-search-paths <value>
(lldb) breakpoint set --file <filename> --line <linenum>
...
(lldb) process attach --pid <pid>                // 或'attach <pid> | <process-name>'
...
(lldb) continue
...
(lldb) process detach                           // 或'detach'
(lldb) platform disconnect
  Disconnected from "localhost"
(lldb) quit                                     // 简写'q'
```

**场景二** 应用启动调试

**示例**

设备侧

1）安装debug版本的hap包并运行，并查看应用pid。

```
hdc shell bm install -p <file-path>
hdc shell aa start -a <ability-name> -b <bundle-name> -N && hdc shell pidof <bundle-name>
```

> **说明**
>
> aa工具中的-N参数表示使能应用启动调试，应用会停在appspawn阶段等待调试器连接。
>
> 更多aa功能使用说明，参考[aa工具](https://gitee.com/openharmony/docs/blob/master/zh-cn/application-dev/tools/aa-tool.md)。

2）运行lldb-server。

```
./lldb-server platform --server --listen "*:8080"
```

LLDB客户端

1）运行lldb.exe。

2）在lldb命令行窗口进行连接调试。

```
(lldb) platform select remote-ohos
  Platform: remote-ohos
 Connected: no
 Container: no
(lldb) platform connect connect://localhost:8080
  Platform: remote-ohos
    Triple: arm-unknown-linux-unknown
  Hostname: localhost
 Connected: yes
 Container: no
WorkingDir: /data/local/tmp
    Kernel: #1 SMP Fri Mar 31 20:52:12 CST 2023
(lldb) breakpoint set --file <filename> --line <linenum>
...
(lldb) process attach --pid <pid>                // 或'attach <pid> | <process-name>'
...
(lldb) continue
...
(lldb) process detach                           // 或'detach'
(lldb) platform disconnect
  Disconnected from "localhost"
(lldb) quit                                     // 简写'q'
```

> **提示**
>
> debug版本so的调试参考hap包调试即可。

## 4.3 OpenHarmony服务启动调试

> **注意**
>
> 此类调试只能在root版本上使用，user版本上不支持。

> **说明**
>
> 使用ps -elf 查询出来的PPID为1的进程为system ablility服务。

**场景一** 单个system ablility调试

**示例** 以调试netmanager为例

设备侧

1）设置需要调试的服务，并获取其pid。

- 进入交互命令环境。

  ```
  hdc shell
  ```

- 指定要调试的服务名称，如netmanager。

  ```
  # param set llvm.debug.service.name netmanager
  Set parameter llvm.debug.service.name netmanager success
  ```

- 停止服务。

  ````
  service_control stop netmanager
  ````

- 启动服务，使得服务进程停在init阶段等待调试器连接：

  ```
  service_control start netmanager
  ```

- 获取需要调试服务的pid。

  ```
  # ps -elf | grep init
  root             1     0 0 17:02:55 ?     00:00:03 init --second-stage
  root          1720     1 0 17:19:32 ?     00:00:00 init --second-stage
  root          1723  1624 5 17:19:39 pts/0 00:00:00 grep init
  ```

  其中1号进程为init进程，1720进程是我们需要调试的。

2）运行lldb-server。

```
./lldb-server platform --server --listen "*:8080"
```

LLDB客户端

1）运行lldb.exe。

2）在lldb命令行窗口进行连接调试。

```
(lldb) platform select remote-ohos
  Platform: remote-ohos
 Connected: no
 Container: no
(lldb) platform connect connect://localhost:8080
  Platform: remote-ohos
    Triple: arm-unknown-linux-unknown
  Hostname: localhost
 Connected: yes
 Container: no
WorkingDir: /data/local/tmp
    Kernel: #1 SMP Fri Mar 31 20:52:12 CST 2023
(lldb) breakpoint set --file <filename> --line <linenum>
...
(lldb) process attach --pid <pid>                // 或'attach <pid>'
...
(lldb) continue
...
(lldb) process detach                           // 或'detach'
(lldb) platform disconnect
  Disconnected from "localhost"
(lldb) quit                                     // 简写'q'
```

**场景二** 多个system ability调试

**示例**

设备侧

1）设置需要调试的服务，并获取其pid。

- 进入交互命令环境。

  ```
  hdc shell
  ```

- 指定要调试的服务。

  ```
  # param set llvm.debug.service.all 1
  Set parameter llvm.debug.service.all 1 success
  ```

- 停止需要调试的服务1。

  ````
  service_control stop <servicename1>
  ````

- 启动服务1，使得服务进程停在init阶段等待调试器连接。

  ```
  service_control start <servicename1>
  ```

- 获取需要调试服务1的pid。

  ```
  ps -elf | grep init

2）运行lldb-server

```
./lldb-server platform --server --listen "*:8080"
```

LLDB客户端1

1）运行lldb.exe。

2）在lldb命令行窗口进行连接调试。

```
(lldb) platform select remote-ohos
  Platform: remote-ohos
 Connected: no
 Container: no
(lldb) platform connect connect://localhost:8080
  Platform: remote-ohos
    Triple: arm-unknown-linux-unknown
  Hostname: localhost
 Connected: yes
 Container: no
WorkingDir: /data/local/tmp
    Kernel: #1 SMP Fri Mar 31 20:52:12 CST 2023
(lldb) breakpoint set --file <filename> --line <linenum>
...
(lldb) process attach --pid <pid1>                // 或'attach <pid1>'
...
(lldb) continue
...
(lldb) process detach                           // 或'detach'
(lldb) platform disconnect
  Disconnected from "localhost"
(lldb) quit                                     // 简写'q'
```

设备侧

停止需要调试的服务2。

```
service_control stop <servicename2>
```

启动服务2，使得服务进程停在init阶段等待调试器连接。

```
service_control start <servicename2>
```

获取需要调试服务2的pid。

```
ps -elf | grep init
```

LLDB客户端2

1）运行lldb.exe。

2）在lldb命令行窗口进行连接调试。

```
(lldb) platform select remote-ohos
  Platform: remote-ohos
 Connected: no
 Container: no
(lldb) platform connect connect://localhost:8080
  Platform: remote-ohos
    Triple: arm-unknown-linux-unknown
  Hostname: localhost
 Connected: yes
 Container: no
WorkingDir: /data/local/tmp
    Kernel: #1 SMP Fri Mar 31 20:52:12 CST 2023
(lldb) breakpoint set --file <filename> --line <linenum>
...
(lldb) process attach --pid <pid2>                // 或'attach <pid2>'
...
(lldb) continue
...
(lldb) process detach                           // 或'detach'
(lldb) platform disconnect
  Disconnected from "localhost"
(lldb) quit                                     // 简写'q'
```

多个服务调试同两个服务调试步骤，只需要开启多个LLDB命令行窗口即可。

# 5 LLDB 命令行调试辅助脚本

## 5.1 运行 Python 代码

LLDB 支持运行 Python 代码。目前，该功能已在 OpenHarmony 发布的 Linux、Windows 和 macOS 版本 NDK 支持。

1. 执行单行 Python 代码

```
script print("Hello OpenHarmony!")
```

2. 执行 `script` 命令进入 Python shell，然后执行 Python 代码

```python
import os
env_path_value = os.environ.get("PATH", "")
print(f"PATH='{env_path_value}'")
```

## 5.2 使用辅助脚本连接和启动 `lldb-server`

使用 OpenHarmony NDK 的 LLDB 命令行远程调试时，通常需要以下步骤：

1. 将 `lldb-server` 推送到 OpenHarmony 设备上。

2. 在 OpenHarmony 设备上启动 `lldb-server`。

3. 开发者在电脑上运行 `lldb` 和连接 `lldb-server`。

4. attach 正在运行的进程或者启动命令行程序，然后执行调试命令。

OpenHarmony NDK 提供了辅助脚本，简化了调试流程，输入以下指令即可完成 `lldb-server` 的启动和连接：

```
script import lldb_python
script lldb_python.main()
```

### 5.2.1 查看参数

查看脚本使用的 `lldb-server` 启动和连接参数：

```
script lldb_python.show_server_configs()
```

### 5.2.2 修改参数

修改启动和连接参数：

```
script lldb_python.set_server_config("tcp-listen-port", 8080)
```

目前支持下列参数：

| 参数名 | 默认值 | 说明 |
| --- | --- | --- |
| `arch` | `unknown` | 可选值：`arm` 表示 ARM 32位架构；`aarch64` 表示 ARM 64位架构；`x86_64` 表示 x86 64位架构。根据硬件架构，选择响应的 `lldb-server`。参数值为 `unknown` 时，会尝试判断设备的架构；如果自动判断架构出错，用户可以手动设置成其他值，关闭架构发现功能（使用用户指定的架构）。 |
| `install-path` | `/data/local/tmp/lldb/lldb-server` | `lldb-server` 在 OpenHarmony 设备上的安装路径。 |
| `tcp-listen-port` | `1234` | `lldb-server` 监听的 tcp 端口号 |
| `platform` | `remote-ohos` | `lldb-server` 所在设备平台。 |

# 6. LLDB Standalone调试

Standalone调试区别于远程调试，无需配置远程连接，直接在设备上进行调试。

## 6.1 LLDB Standalone工具获取

该工具需要通过编译LLVM工程时增加`--build-lldb-static`参数获取。

LLVM工程编译参考：[llvm-build](https://gitee.com/openharmony/third_party_llvm-project/blob/master/llvm-build/README.md)

编译完成后，可通过如下路径获取到Standalone版本的工具：

1）生成路径： llvm-project/out/lib/lldb-server-[platform]-linux-ohos/bin

2）Install路径：llvm-project/out/llvm-install/lib/clang/15.0.4/bin/[platform]-linux-ohos

3）package压缩包：llvm-project/out/clang-dev-linux-x86_64.tar.bz2内，clang-dev/lib/clang/15.0.4/bin/[platform]-linux-ohos

​ [platform]值根据调试设备架构，支持arm和aarch64

## 6.2 LLDB Standalone调试准备

1）使用HDC工具将[6.1 LLDB Standalone工具获取](#61-lldb-standalone工具获取)中获取到的lldb和lldb-server传输到设备工作路径，以/data/local/tmp为例

```
hdc.exe file send lldb /data/local/tmp
hdc.exe file send lldb-server /data/local/tmp
```

2）进入命令行交互模式

```
hdc.exe shell
```

3）赋予lldb工具执行权限

```
# chmod +x /data/local/tmp/lldb
# chmod +x /data/local/tmp/lldb-server
```

## 6.3 LLDB Standalone调试

通常情况下，通过attach到运行中的进程或者直接通过LLDB启动目标程序来开始调试。当成功attach到目标进程或者通过LLDB启动目标程序后，就可以结合调试目的，使用[3. LLDB命令](#3-lldb命令)中相应的LLDB调试命令来调试目标程序。

> **提示：**
>
> - 命令print、call、expr尚不支持调用函数
> - 如遇删除键（Backspace）功能无法正常删除时，使用Ctrl+Backspace删除

### 6.3.1 attach 到运行的进程

#### 6.3.1.1 启动LLDB

```
# /data/local/tmp/lldb
(lldb)
```

通过ps或者top等命令获取目标程序进程PID或进程名。

#### 6.3.1.2 通过进程ID<pid> attach到进程

```
(lldb) attach -p <pid>
```

#### 6.3.1.3 通过进程名<name> attach到进程

```
(lldb) process attach --name <name>
```

### 6.3.2 通过LLDB启动目标程序

#### 6.3.2.1 启动LLDB的同时指定目标程序hello_world

```
# /data/local/tmp/lldb /data/local/tmp/hello_world
```

#### 6.3.2.2 使用target create命令指定调试目标程序hello_world

```
# /data/local/tmp/lldb
(lldb) target create /data/local/tmp/hello_world
```

#### 6.3.2.3 设置断点并启动目标程序

```
(lldb) b main
(lldb) r
```

# 7. OpenHarmony LLDB调试器

该调试器是可以直接在OpenHarmony设备运行的LLDB，无需进行远程连接。

## 7.1 工具获取

该工具需要通过编译LLVM工程获取。

工具编译参考：[ohos-toolchain-build](https://gitee.com/openharmony/third_party_llvm-project/blob/master/llvm-build/README.md#build-process-of-aarch64-toolchain)。

编译完成后，可通过如下路径获取到OpenHarmony调试器工具压缩包：llvm-project/packages/clang-dev-ohos-aarch64tar.bz2。

## 7.2 调试准备

1）受设备环境限制，你可能需要将压缩包重新打包为：clang-dev-ohos-aarch64.tar

```
bunzip2 -c clang-dev-ohos-aarch64.tar.bz2 > clang-dev-ohos-aarch64.tar
```

2）使用hdc工具将压缩包推送到工作路径，以/data/local/tmp为例

```
hdc file send clang-dev-ohos-aarch64.tar /data/local/tmp
```

3）进入命令行交互模式

```
hdc shell
```

4）解压工具包

```
# cd /data/local/tmp/
# tar -xf clang-dev-ohos-aarch64.tar
```

## 7.3 调试

通常情况下，通过attach到运行中的进程或者直接通过LLDB启动目标程序来开始调试。当成功attach到目标进程或者通过LLDB启动目标程序后，就可以结合调试目的，使用[3. LLDB命令](#3-lldb命令)中相应的LLDB调试命令来调试目标程序。

> **提示：**
>
> - 如果编译时未启用libedit扩展库功能，可能删除键（Backspace）功能无法正常删除，使用Ctrl+Backspace删除

LLDB执行程序的位置：`/data/local/tmp/ohos-aarch64-install/bin/`

具体的调试方法参考[6.3 LLDB Standalone调试](#63-LLDB-Standalone调试)
