# lldb 使用

## lldb --help

USAGE:

```bash
lldb.exe [options]
```

ATTACHING:
  --attach-name <name> Tells the debugger to attach to a process with the given name.
  --attach-pid <pid>   Tells the debugger to attach to a process with the given pid.
  -n <value>           Alias for --attach-name
  -p <value>           Alias for --attach-pid
  --wait-for           Tells the debugger to wait for a process with the given pid or name to launch before attaching.
  -w                   Alias for --wait-for

COMMANDS:
  --batch              Tells the debugger to run the commands from -s, -S, -o & -O, and then quit.
  -b                   Alias for --batch
  -K <value>           Alias for --source-on-crash
  -k <value>           Alias for --one-line-on-crash
  --local-lldbinit     Allow the debugger to parse the .lldbinit files in the current working directory, unless --no-lldbinit is passed.
  --no-lldbinit        Do not automatically parse any '.lldbinit' files.
  --one-line-before-file <command>
                       Tells the debugger to execute this one-line lldb command before any file provided on the command line has been loaded.
  --one-line-on-crash <command>
                       When in batch mode, tells the debugger to run this one-line lldb command if the target crashes.
  --one-line <command> Tells the debugger to execute this one-line lldb command after any file provided on the command line has been loaded.
  -O <value>           Alias for --one-line-before-file
  -o <value>           Alias for --one-line
  -Q                   Alias for --source-quietly
  --source-before-file <file>
                       Tells the debugger to read in and execute the lldb commands in the given file, before any file has been loaded.
  --source-on-crash <file>
                       When in batch mode, tells the debugger to source this file of lldb commands if the target crashes.
  --source-quietly     Tells the debugger not to echo commands while sourcing files or one-line commands provided on the command line.
  --source <file>      Tells the debugger to read in and execute the lldb commands in the given file, after any file has been loaded.
  -S <value>           Alias for --source-before-file
  -s <value>           Alias for --source
  -x                   Alias for --no-lldbinit

OPTIONS:
  --arch <architecture> Tells the debugger to use the specified architecture when starting and running the program.
  -a <value>            Alias for --arch
  --core <filename>     Tells the debugger to use the full path to <filename> as the core file.
  -c <value>            Alias for --core
  --debug               Tells the debugger to print out extra information for debugging itself.
  -d                    Alias for --debug
  --editor              Tells the debugger to open source files using the host's "external editor" mechanism.
  -e                    Alias for --editor
  --file <filename>     Tells the debugger to use the file <filename> as the program to be debugged.
  -f <value>            Alias for --file
  --help                Prints out the usage information for the LLDB debugger.
  -h                    Alias for --help
  --no-use-colors       Do not use colors.
  --version             Prints out the current version number of the LLDB debugger.
  -v                    Alias for --version
  -X                    Alias for --no-use-color

REPL:
  -r=<flags>     Alias for --repl=<flags>
  --repl-language <language>
                 Chooses the language for the REPL.
  --repl=<flags> Runs lldb in REPL mode with a stub process with the given flags.
  --repl         Runs lldb in REPL mode with a stub process.
  -R <value>     Alias for --repl-language
  -r             Alias for --repl

SCRIPTING:
  -l <value>    Alias for --script-language
  --print-script-interpreter-info
                Prints out a json dictionary with information about the scripting language interpreter.
  --python-path Prints out the path to the lldb.py file for this version of lldb.
  -P            Alias for --python-path
  --script-language <language>
                Tells the debugger to use the specified scripting language for user-defined scripts.

## 示例:

调试器可以在 几种模式 下启动.

将可执行文件作为 位置参数 传递, 可使 `lldb` 调试给定的可执行文件.
为了区分 **传给 lldb 的参数** 和 **传给被调试可执行文件的参数**, 以 `-` 开头的参数必须放在 `--` 之后.
例如

```bash
lldb --arch x86_64 /path/to/program program argument -- --arch armv7
```

For convenience, passing the executable after -- is also supported.

lldb --arch x86_64 -- /path/to/program program argument --arch armv7

Passing one of the attach options causes lldb to immediately attach to the given process.

```bash
lldb -p <pid>
lldb -n <process-name>
```

Passing --repl starts lldb in REPL mode.

```bash
lldb -r
```

Passing --core causes lldb to debug the core file.

```bash
lldb -c /path/to/core
```

Command options can be combined with these modes and cause lldb to run the
specified commands before or after events, like loading the file or crashing,
in the order provided on the command line.

```bash
lldb -O 'settings set stop-disassembly-count 20' -o 'run' -o 'bt'
lldb -S /source/before/file -s /source/after/file
lldb -K /source/before/crash -k /source/after/crash
```

Note: In REPL mode no file is loaded, so commands specified to run after
loading the file (via -o or -s) will be ignored.
