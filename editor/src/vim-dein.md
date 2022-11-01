# vim 插件管理器 dein

[Vim 插件管理器 – dein.vim](https://www.codenong.com/js0874e6be9d74/)
[Vim 插件管理器 - dein.vim](https://www.jianshu.com/p/0874e6be9d74)

## 前言

[vim-plug][] 是一个非常优秀的 Vim 插件管理器,
但是随着安装的插件越来越多, 逐渐发现即使使用 [vim-plug][],
首次启动速度仍然很慢.

究其原因, 虽然 [vim-plug][] 本身提供了优秀的延迟加载机制,
但是可用于延迟加载的选项相对较少,
另一方面, [vim-plug][] 对插件的延迟加载与配置无法进行统一,
很多时候, 我们想对插件进行延迟加载, 但是插件配置项如果调用了插件功能,
则加载的时候就会报错(因为插件此时还未加载)...

针对上述问题, [dein.vim][] 都给出了更优秀的解决方案.

>注: [vim-plug][] 是一款非常优秀的插件管理器,
>具备优秀的插件管理性能, 同时操作及其简单,
>且具备优秀的 UI 显示, 通常情况下, 建议使用 [vim-plug][].
>但是如果当使用 [vim-plug][] 后, 启动速度仍然很慢, 那就可以考虑下 [dein.vim][]
>
>注: dein 本身只提供函数接口进行操作,
>没有提供命令与 UI 显示, 对于用户相对不友好.
>不过 Github 上面已经有人对其进行了再一次封装:

+ [dein-command.vim][]: 为 `dein` 封装了一些命令
+ [dein-ui.vim]:为 dein 提供了类似 [vim-plug][] 的 UI 界面

可以结合以上两个插件, 简化 dein 使用.

[dein-command.vim]: https://github.com/haya14busa/dein-command.vim
[dein-ui.vim]: https://github.com/wsdjeg/dein-ui.vim
[vim-plug]: https://github.com/junegunn/vim-plug
[dein.vim]: https://github.com/Shougo/dein.vim

## 优点

+ 启动快
+ 支持异步安装
+ 支持本地插件
+ 支持多种 VCS(包括 git)
+ 支持缓存
+ 支持merge特性, 有效减少runtimepath层级
+ ...

## 安装

+ 对于 Unix/Linux/Mac:

```bash
curl https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh > installer.sh
# 安装路径我们选择: ~/.vim/dein
sh ./installer.sh ~/.vim/dein
```

## 对于 Windows:

```powershell
Invoke-WebRequest https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.ps1 -OutFile installer.ps1
# Allow to run third-party script
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
# 安装路径我们选择: ~/.vim/dein
./installer.ps1 ~/.vim/dein
```

## dein 基本使用

### 最简配置: 基本结构如下:

```viml
if &compatible
    set nocompatible
endif

let s:dein_path = '~/.vim/dein'

" Add the dein installation directory into runtimepath
let &runtimepath = &runtimepath.','.s:dein_path.'/repos/github.com/Shougo/dein.vim'

if dein#load_state(s:dein_path)
  call dein#begin(s:dein_path)

  call dein#add(s:dein_path.'/repos/github.com/Shougo/dein.vim')

  " install third-party plugins
  " call dein#add('tpope/vim-surround')

  call dein#end()
  call dein#save_state()
endif

filetype plugin indent on
syntax enable
```

### 插件安装

插件基本安装方法如下:

1. 在 `dein#begin()` 和 `dein#end()` 之间使用 `dein#add({plugin})` 声明要安装的插件:

```viml
call dein#begin(s:dein_path)

" install third-party plugins
call dein#add('tpope/vim-surround')

call dein#end()
```

2. 重新打开 Vim 或者手动重新加载配置 `:so %` 后, 使用如下命令安装插件:

```viml
:call dein#install()
```

注: 由于 `dein` 没有 UI 显示下载进度, 需等待一段时间, 等下载完成后, 底部提示栏会有显示通知.
也可以直接到 `<dein_path>/repos/` 查看插件是否已安装完成.

3. 关闭 Vim, 再重新打开, 就可以使用已安装的插件功能了.

### 插件卸载

`dein.vim` 并未直接提供插件卸载功能, 因此其插件卸载相对麻烦, 步骤如下:

1. 取消插件添加配置: 将要卸载的插件进行注释:

```viml
call dein#begin(s:dein_path)

" uninstall vim-surround
" call dein#add('tpope/vim-surround')

call dein#end()
```

2. 清除缓存: 关闭再重新打开 Vim, 执行 `:call dein#recache_runtimepath()`.

>注: 由于 `dein` `采用了merge` 功能, 其会将所有插件的 `plugin/` 目录下的内容,
都缓存到同一个目录, 具体为 `<dein_path>/.cache/init.vim/.dein/plugin` 中,
因此虽然我们上一步配置文件取消了添加插件, 但由于缓存未删除,
导致本次启动仍然会加载相应插件, 故还需手动进行缓存清除操作.

3. 关闭再重新打开 Vim, 就可以发现插件功能已禁止(可选)

4. 删除插件: 以上操作只是停用了插件, 插件仍然存在于本地电脑,
如果想进行删除, 还需调用如下命令:

```viml
:call map(dein#check_clean(), "delete(v:val, 'rf')")
```

此时就可以看到 `dein` 目录下的 `repo` 下的相应插件被删除了,
缓存文件夹 `.cache下` 的 `.dein/plugin` 相关的插件缓存也被删除了.

### 禁用插件

不删除插件, 只是不进行加载, 步骤如下:

1. 将配置文件中的 `dein#add` 改为 `dein#disable`, 如下所示:

```viml
call dein#begin(s:dein_path)

call dein#add('tpope/vim-surround')
" disable 添加到 add 后面
call dein#disable('vim-surround')

call dein#end()
```

2. 关闭 Vim, 再重新打开, 执行 `:call dein#recache_runtimepath()`

3. 关闭 Vim, 再重新打开, 就可以看到插件功能禁止了.

注: 以上任何操作如果失败, 一律进行如下操作, 确保不受 `dein` 缓存机制影响:

+ 重新打开 Vim, 执行 `:call dein#clear_state()`, 清除状态文件, 强制 `dein` 重新加载配置.
+ 退出后重新打开 Vim, 执行 `:call dein#recache_runtimepath()`, 清除旧缓存
+ 退出 Vim, 再重新打开, 执行剩余操作

## 一些有用操作/设置

在对 dein 进行详细介绍前, 先了解下其提供的一些比较有用的操作/设置, 方便理解与使用.

### 手动安装插件: 依次输入以下命令:

```viml
" 开始块, 安装路径可以自由更改
:call dein#begin('~/.cache/dein')
" 添加插件安装声明
:call dein#add('~/.vim/plugB')
" 配置块结束, 安装自动开始
:call dein#end()
```

### 手动加载插件: dein#source([{plugins}])

```viml
:call dein#source('vim-surround')
```

注: `dein#source` 会直接加载插件, 无论插件是否配置了懒加载.

+ 检测插件是否已安装: dein#check_install({plugins})
    + 返回 `0`: 表示插件已安装, 可以正常使用
    + 返回 `-1`: 表示是无效插件
    + 返回 `其他`: 表示插件未安装

    ```viml
    :call dein#check_install('vim-surround')
    ```

+ 检测插件是否已加载: `dein#is_sourced({plugin-name})`
    + 返回 `0`: 表示插件未加载
    + 返回 `其他`: 表示插件存在且已加载

    ```viml
    :call dein#is_sourced('vim-surround')
    ```

> 注: 由于 `dein` 存在懒加载, 因此存在插件已安装> (`dein#check_install`)
> 但未加载(`dein#is_sourced`)的情形.

+ 检测插件是否被禁用: `dein#tap({plugin-name})`

+ 返回 `0`: 表示插件不存在, 或者被禁用
+ 返回 `其他`: 表示插件存在, 且可以使用(懒加载插件在未加载前也是属于可以使用状态)

    ```viml
    :call dein#tap('vim-surround')
    ```

### 插件懒加载

```viml
call dein#add('junegunn/fzf', {
    \'lzay': 1,                                               " 非0 表示启用懒加载
    \ 'if': s:has_exec('fzf'),                                " 条件满足时才加载(判断类型`String`/`Number`)
    \ 'on_if': 'winnr("$") > 1',                              " 条件满足时才加载(判断类型`String`)
    \ 'on_cmd': 'FZF',                                        " 存在 FZF 命令时才加载
    \ 'on_func': 'fzf#run',                                   " 调用了函数 fzf#run 时才进行加载
    \ 'on_event': ['VimEnter', 'BufRead']                     " 事件发生时才加载
    \ 'on_ft': 'python'                                       " 文件类型匹配时才加载
    \ 'on_map': { 'n' : ['<C-n>', '<C-p>'], 'x' : '<C-n>'}} " 匹配特定模式下的按键映射时才加载
    \ 'on_path': '.editorconfig',                             " 路径匹配时才加载
    \ 'on_source':  ['vim-surround']                          " 插件 vim-surround 加载时才加载
    \ })
```

>注: 懒加载时 `lazy` 可忽略, `dein` 会自动根据其他选项自动判断是否启用懒加载.

+ 检测未使用的插件目录(可以清除): `dein#check_clean()`
+ 获取插件配置: `dein#get([{plugin-name}])`

    ```viml
    :echo dein#get('vim-surround')
    ```

## 函数

以下是 `dein.vim` 内置的函数简介:

+ `dein#add({repo}[, {options}])`: 初始化/添加插件.
    + `{repo}`: 表示插件 URI 或者插件本地路径.
    + `{options}`: 对插件进行额外选项配置, 具体选项请参考下文: 选项(OPTIONS)
    >注: `dein#add` 必须在 `dein#begin()` 块中使用.

+ `dein#begin({base-path}, [{vimrcs}])`: 初始化 `dein.vim`, 开启插件配置块.
    + `{base-path}`: 表示插件下载安装路径
    + `{vimrcs}`: 额外配置选项或者 `TOML` 配置文件. 默认值为 `$MYVIMRC`
    >注: 不能在 `has('vim_starting')` 块中调用 `dein#begin()`
    >注: `dein#begin()` 会自动设置: `filetype off`

## tabular 插件

[godlygeek/tabular](https://github.com/godlygeek/tabular)
[Vim 文本对齐插件 tabular 简介](https://blog.csdn.net/techfield/article/details/84186402)

Tabular 解决对齐问题, 可以指定各种对齐符号, 对齐方式.
光是这一个插件就给了使用 Vim 的充分理由.

+ 安装

```viml
Plug 'godlygeek/tabular'
```

Vim 插件安装方法参见 [Vim-plug 插件管理器简介](https://blog.csdn.net/techfield/article/details/84183045)

+ 文档; `:help tabular`
+ 格式符: [对齐方式[此方式后添加的空格数量]],
    + 对齐方式; `左l`, `右r`, `中c`. 需要与前面用 `/` 隔开.

### 常用命令

`:Tabularize` 可简化为 `:Tab`, 以下都省略了选中区域后自动生成的 `'<,'>`.

+ 冒号对齐; `:Tab /:`
+ 逗号对齐; `:Tabularize /,`
+ 运行上个对齐命令 `:Tab`
+ // 对齐(需要 escape); `:Tab /\/\/`

+ `:Tabularize /,/r1c1l0`;  含义是: 对齐指定区域的文本, 以逗号分割.
将第一个逗号前的所有文本右对齐, 然后添加一个空格;
将逗号居中对齐, 然后添加一个空格;
然后将逗号后所有文本左对齐, 不添加空格(添加 `0` 个空格).

### 示例

测试文本:

```cpp
m_varName1;   // Comment 1
m_varName1AndABit;       // Command 1 and a bit
m_varName2;     // Comment 2
m_varName3ButReally2AndABit;   // Comment 3 (but really 2 and a bit)
```

用 visual 模式选中后按 `//` 对齐, `:Tab /\/\/`;

![图片描述](https://img-blog.csdnimg.cn/20181117141223729.png)

按 `//` 对齐, 左边左对齐, 右边右对齐 `:Tab /\/\//l1c1r0`;

![img](https://img-blog.csdnimg.cn/20181117141749292.png)

### 同类插件

[vim-easy-align](http://vimawesome.com/plugin/vim-easy-align)
[align-all-too-well](http://vimawesome.com/plugin/align-all-too-well)
