# [get_filename_component][def]

Get a specific component of a full filename.

Changed in version 3.20: This command has been superseded by the cmake_path() command, except for REALPATH, which is now offered by file(REAL_PATH), and PROGRAM, now available in separate_arguments(PROGRAM).

Changed in version 3.24: The undocumented feature offering the capability to query the Windows registry is superseded by cmake_host_system_information(QUERY WINDOWS_REGISTRY) command.

```cmake
get_filename_component(<var> <FileName> <mode> [CACHE])
```

Sets <var> to a component of <FileName>, where <mode> is one of:

```bash
DIRECTORY = Directory without file name
NAME      = File name without directory
EXT       = File name longest extension (.b.c from d/a.b.c)
NAME_WE   = File name with neither the directory nor the longest extension
LAST_EXT  = File name last extension (.c from d/a.b.c)
NAME_WLE  = File name with neither the directory nor the last extension
PATH      = Legacy alias for DIRECTORY (use for CMake <= 2.8.11)
Added in version 3.14: Added the LAST_EXT and NAME_WLE modes.
```

Paths are returned with forward slashes and have no trailing slashes. If the optional CACHE argument is specified, the result variable is added to the cache.

```cmake
get_filename_component(<var> <FileName> <mode> [BASE_DIR <dir>] [CACHE])
```

Added in version 3.4.

Sets <var> to the absolute path of <FileName>, where <mode> is one of:

ABSOLUTE  = Full path to file
REALPATH  = Full path to existing file with symlinks resolved

## ref

[def]: https://cmake.org/cmake/help/latest/command/get_filename_component.html