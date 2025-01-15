# Path Conversion

```cmake
file(REAL_PATH <path> <out-var> [BASE_DIRECTORY <dir>] [EXPAND_TILDE] )
```

*3.19版的新功能.*
计算现有文件或目录的 `绝对路径`, 并解析 `符号链接`.

+ `BASE_DIRECTORY <dir>`
如果提供的 `<path>` 是相对路径, 则相对于给定的基础目录 `<dir>`进行计算.
如果没有提供基础目录, 默认的基础目录将是 [CMAKE_CURRENT_SOURCE_DIR][].

+ `EXPAND_TILDE`
*在3.21版本中新增.*
如果 `<path>` 是 `~` 或者以 `~/` 开头, 那么 `~` 将被用户的主目录取代.
主目录的路径从 `环境变量` 中获得.
在 `Windows` 上, 使用 `USERPROFILE` 环境变量,
如果 `USERPROFILE` 没有定义, 则fallback 到 `HOME` 环境变量.
在所有其他平台上, 只使用 `HOME`.

+ 计算从 `<directory>` 到 `<file>` 的相对路径, 并将其存储在 `<变variable量>` 中.

```cmake
file(RELATIVE_PATH <variable> <directory> <file>)
```

+ `TO_CMAKE_PATH` 模式将原生的 `<path>` 转换为
带有 `正斜杠`(`/`)的 `cmake-style` 路径, 存入 `<variable>`.
输入可以是单独的路径, 也可以是 `系统搜索路径`, 比如 `$ENV{PATH}`.
搜索路径将被转换为 cmake-风格的列表, 它以 `;` 字符分隔.

```camke
file(TO_CMAKE_PATH "<path>" <variable>)
file(TO_NATIVE_PATH "<path>" <variable>)
```

`TO_NATIVE_PATH` 模式将 cmake-style `<path>` 转换为带有平台特定斜线的 `本地路径`.
(在 Windows 主机上为 `\`, 其他平台为 `/`).

记得总在 `<path>` 周围使用双引号, 以确保它被当作这个命令的 `单个参数`.

[CMAKE_CURRENT_SOURCE_DIR]: https://cmake.org/cmake/help/latest/variable/CMAKE_CURRENT_SOURCE_DIR.html#variable:CMAKE_CURRENT_SOURCE_DIR

## Transfer

```cmake
file(DOWNLOAD <url> [<file>] [<options>...])
file(UPLOAD   <file> <url> [<options>...])
```

The DOWNLOAD subcommand downloads the given <url> to a local <file>. The UPLOAD mode uploads a local <file> to a given <url>.

New in version 3.19: If <file> is not specified for file(DOWNLOAD), the file is not saved. This can be useful if you want to know if a file can be downloaded (for example, to check that it exists) without actually saving it anywhere.

Options to both DOWNLOAD and UPLOAD are:

INACTIVITY_TIMEOUT <seconds>
Terminate the operation after a period of inactivity.

LOG <variable>
Store a human-readable log of the operation in a variable.

SHOW_PROGRESS
Print progress information as status messages until the operation is complete.

STATUS <variable>
Store the resulting status of the operation in a variable. The status is a ; separated list of length 2. The first element is the numeric return value for the operation, and the second element is a string value for the error. A 0 numeric error means no error in the operation.

TIMEOUT <seconds>
Terminate the operation after a given total time has elapsed.

USERPWD <username>:<password>
New in version 3.7.

Set username and password for operation.

HTTPHEADER <HTTP-header>
New in version 3.7.

HTTP header for operation. Suboption can be repeated several times.

NETRC <level>
New in version 3.11.

Specify whether the .netrc file is to be used for operation. If this option is not specified, the value of the CMAKE_NETRC variable will be used instead. Valid levels are:

IGNORED
The .netrc file is ignored. This is the default.

OPTIONAL
The .netrc file is optional, and information in the URL is preferred. The file will be scanned to find which ever information is not specified in the URL.

REQUIRED
The .netrc file is required, and information in the URL is ignored.

NETRC_FILE <file>
New in version 3.11.

Specify an alternative .netrc file to the one in your home directory, if the NETRC level is OPTIONAL or REQUIRED. If this option is not specified, the value of the CMAKE_NETRC_FILE variable will be used instead.

TLS_VERIFY <ON|OFF>
Specify whether to verify the server certificate for https:// URLs. The default is to not verify. If this option is not specified, the value of the CMAKE_TLS_VERIFY variable will be used instead.

New in version 3.18: Added support to file(UPLOAD).

TLS_CAINFO <file>
Specify a custom Certificate Authority file for https:// URLs. If this option is not specified, the value of the CMAKE_TLS_CAINFO variable will be used instead.

New in version 3.18: Added support to file(UPLOAD).

For https:// URLs CMake must be built with OpenSSL support. TLS/SSL certificates are not checked by default. Set TLS_VERIFY to ON to check certificates.

Additional options to DOWNLOAD are:

EXPECTED_HASH ALGO=<value>

Verify that the downloaded content hash matches the expected value,
where ALGO is one of the algorithms supported by file(<HASH>).
If the file already exists and matches the hash, the download is skipped.
If the file already exists and does not match the hash, the file is downloaded again.
If after download the file does not match the hash, the operation fails with an error.
It is an error to specify this option if DOWNLOAD is not given a <file>.

EXPECTED_MD5 <value>
Historical short-hand for EXPECTED_HASH MD5=<value>. It is an error to specify this if DOWNLOAD is not given a <file>.

RANGE_START <value>
New in version 3.24.

Offset of the start of the range in file in bytes. Could be omitted to download up to the specified RANGE_END.

RANGE_END <value>
New in version 3.24.

Offset of the end of the range in file in bytes.
Could be omitted to download everything from the specified RANGE_START to the end of file.

## Locking

```camke
file(LOCK <path> [DIRECTORY] [RELEASE]
     [GUARD <FUNCTION|FILE|PROCESS>]
     [RESULT_VARIABLE <variable>]
     [TIMEOUT <seconds>])
```

New in version 3.2.

Lock a file specified by <path> if no DIRECTORY option present and file <path>/cmake.lock otherwise.
File will be locked for scope defined by GUARD option (default value is PROCESS).
RELEASE option can be used to unlock file explicitly.
If option TIMEOUT is not specified CMake will wait until lock succeed or until fatal error occurs.
If TIMEOUT is set to 0 lock will be tried once and result will be reported immediately.
If TIMEOUT is not 0 CMake will try to lock file for the period specified by <seconds> value.

Any errors will be interpreted as fatal if there is no RESULT_VARIABLE option.
Otherwise result will be stored in <variable> and will be 0 on success or error message on failure.

Note that lock is advisory - there is no guarantee that other processes will respect this lock,
i.e. lock synchronize two or more CMake instances sharing some modifiable resources.
Similar logic applied to DIRECTORY option - locking parent directory doesn't prevent other LOCK commands to lock any child directory or file.

Trying to lock file twice is not allowed.
Any intermediate directories and file itself will be created if they not exist.
GUARD and TIMEOUT options ignored on RELEASE operation.

## Archiving

```camke
file(ARCHIVE_CREATE OUTPUT <archive>
  PATHS <paths>...
  [FORMAT <format>]
  [COMPRESSION <compression> [COMPRESSION_LEVEL <compression-level>]]
  [MTIME <mtime>]
  [VERBOSE])
```

New in version 3.18.

Creates the specified <archive> file with the files and directories listed in <paths>. Note that <paths> must list actual files or directories, wildcards are not supported.

Use the FORMAT option to specify the archive format. Supported values for <format> are 7zip, gnutar, pax, paxr, raw and zip. If FORMAT is not given, the default format is paxr.

Some archive formats allow the type of compression to be specified. The 7zip and zip archive formats already imply a specific type of compression. The other formats use no compression by default, but can be directed to do so with the COMPRESSION option. Valid values for <compression> are None, BZip2, GZip, XZ, and Zstd.

New in version 3.19: The compression level can be specified with the COMPRESSION_LEVEL option. The <compression-level> should be between 0-9, with the default being 0. The COMPRESSION option must be present when COMPRESSION_LEVEL is given.

Note With FORMAT set to raw only one file will be compressed with the compression type specified by COMPRESSION.
The VERBOSE option enables verbose output for the archive operation.

To specify the modification time recorded in tarball entries, use the MTIME option.

```cmake
file(ARCHIVE_EXTRACT INPUT <archive>
  [DESTINATION <dir>]
  [PATTERNS <patterns>...]
  [LIST_ONLY]
  [VERBOSE]
  [TOUCH])
```

New in version 3.18.

Extracts or lists the content of the specified <archive>.

The directory where the content of the archive will be extracted to can be specified using the DESTINATION option. If the directory does not exist, it will be created. If DESTINATION is not given, the current binary directory will be used.

If required, you may select which files and directories to list or extract from the archive using the specified <patterns>. Wildcards are supported. If the PATTERNS option is not given, the entire archive will be listed or extracted.

LIST_ONLY will list the files in the archive rather than extract them.

New in version 3.24: The TOUCH option gives extracted files a current local timestamp instead of extracting file timestamps from the archive.

With VERBOSE, the command will produce verbose output.
