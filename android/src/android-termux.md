# Termux

[国光:Termux 安装与配置](https://www.sqlsec.com/2018/05/termux.html)

## Nginx 解析 PHP

Termux 下的 `Nginx` 解析 `PHP` 这里单独拿出一级标题来叙述,
成功解析的话,下面安装 `wordpress` 等 `PHP` 网站就会轻松很多.

## 安装 php-fpm

`Nginx` 本身不能处理 `PHP`, 它只是个 `Web` 服务器,
当接收到 `PHP` 请求后发给 `PHP` 解释器处理.
`Nginx` 一般是把请求转发给 `fastcgi` 管理进程处理,
`PHP-FPM` 是一个 `PHP FastCGI` 管理器, 所以这里得先安装它:

```bash
pkg install php-fpm
```

安装完成顺便检查一下版本信息吧:

### 配置 php-fpm

编辑 `php-fpm` 的配置文件 `www.conf`:

```bash
vim $PREFIX/etc/php-fpm.d/www.conf
```

定位搜索 `listen =` 找到

```ini
listen = /data/data/com.termux/files/usr/var/run/php-fpm.sock
```

将其改为:

```ini
listen = 127.0.0.1:9000
```

### 配置 Nginx

编辑 `Nginx` 的配置文件 `nginx.conf`:

```bash
vim $PREFIX/etc/nginx/nginx.conf
```

下面国光贴出配置好的完整配置文件, 大家可以参考下面这些图, 只需要两大步骤:

+ 添加 `index.php` 到默认首页的规则里面:
+ 取消 `location ~ \.php$` 这些注释, 按照图片上的 提示修改:

`Termux` 里面的 `Nginx` 默认网站的根目为:

```verilog
/data/data/com.termux/files/usr/share/nginx/html
```

如果想要修改默认路径的话 只需要在配置文件中 替换 `2处` 出现的这个路径即可
下面贴一份完整的配置文件:

```nginx
worker_processes  1;
events {
    worker_connections  1024;
}

http {
    include       mime.types;
    default_type  application/octet-stream;
    sendfile        on;
    keepalive_timeout  65;

    server {
        listen       8080;
        server_name  localhost;
        location / {
            root   /data/data/com.termux/files/usr/share/nginx/html;
            index  index.html index.htm index.php;
        }

        error_page   500 502 503 504  /50x.html;
        location = /50x.html {
            root   /data/data/com.termux/files/usr/share/nginx/html;
        }

        location ~ \.php$ {
            root           html;
            fastcgi_pass   127.0.0.1:9000;
            fastcgi_index  index.php;
            fastcgi_param  SCRIPT_FILENAME  /data/data/com.termux/files/usr/share/nginx/html$fastcgi_script_name;
            include        fastcgi_params;
        }
    }
}
```

### 测试 PHP 解析

Nginx 默认网站的根目录为:

```verilog
/data/data/com.termux/files/usr/share/nginx/html
```

在这个网站根目录下新建 `info.php` 内容为: `<?php phpinfo(); ?>`

```bash
echo '<?php phpinfo(); ?>' > $PREFIX/share/nginx/html/info.php
```

+ 启动服务

先启动 `php-fpm` 服务:

```bash
php-fpm
```

然后再启动 `Nginx` 服务:

```bash
nginx
```

如果你的 `Nginx` 已经启动了的话, 使用 `nginx -s reload` 重启 `Nginx`

+ 访问测试

浏览器访问 `http://127.0.0.1:8080/info.php` 来看看刚刚新建的测试文件是否解析了:

哇哦~ awesome

## termux 备份

[Backing up Termux](https://wiki.termux.com/wiki/Backing_up_Termux)

### 备份

在这个例子中, 将显示 `home` 和 `sysroot` 的备份.
产生的 `备份存档` 将存储在你的共享存储器(`/sdcard`)上, 并用 `gzip` 进行压缩.

1. 确保授予存储权限.

    ```bash
    termux-setup-storage
    ```

2. 备份文件.

    ```bash
    tar -zcf /sdcard/termux-backup.tar.gz -C /data/data/com.termux/files ./home ./usr
    ```

`备份` 应该在没有任何报错的情况下完成.
应该不会有任何 `权限拒绝`, 除非用户滥用了root权限.
如果你收到一些关于 `socket` 文件的警告, 请忽略它们.

警告: 不要把备份存放在 Termux 的私有目录中. 它们的路径可能看起来像.

`/data/data/com.termux` - 内部存储中的Termux私有目录
`/sdcard/Android/data/com.termux `- 共享存储器上的私有Termux目录
`/storage/XXXX-XXXX/Android/data/com.termux` - 外部存储器上的私有Termux目录, XXXX-XXXX是你的微型SD卡的UUID.
`${HOME}/storage/external-1` - micro-sd上Termux私有目录的别名.

一旦你从设置中清除 `Termux` 数据, 这些目录也会被清除.

### 复原

这里假设你已把 `home` 和 `usr` 目录备份到同一个存档中.
请注意, 在这个过程中, 所有文件都会被覆盖.

1. 确保存储权限已被授予.

    ```bash
    termux-setup-storage
    ```

2. 提取 `home` 和 `usr`, 覆盖所有文件.
通过 `--recursive-unlink` 来删除任何垃圾和无主的文件.
通过 `--preserve-permissions` 来设置文件权限,
如同存档时一样, 忽略 `umask`值.
通过结合这些额外的选项, 你将得到与归档文件中完全一样的安装状态.

```bash
tar -zxf /sdcard/termux-backup.tar.gz -C /data/data/com.termux/files --recursive-unlink --preserve-permissions
```

现在用通知中的 `exit` 按钮关闭 `Termux`, 然后再次打开它.

## 使用附带的脚本

### Using supplied scripts

The latest version of package "termux-tools" contains basic scripts for backup and restore of Termux installation. 
They are working in a way similar to tar commands mentioned above.

These scripts backup and restore scripts will not backup, restore or in any other way touch your home directory. 
Check out the notice if have questions why so. Termux developers are not responsible with what you are doing with your files. 
If you managed to lost data, that would be your own problem.
Using termux-backup

Simple backup with auto compression:

```bash
termux-backup /sdcard/backup.tar.xz
```

Compression format is determined by file extension, which is typically .tar.gz (gzip), .tar.xz (xz) or .tar (no compression).

It is possible to stream backup contents to standard output, for example to encrypt it with GnuPG utility or send to remote storage. 
Set file name as "-" to enable streaming to stdout:

```bash
termux-backup - | gpg --symmetric --output /sdcard/backup.tar.gpg
```

Content written to stdout is not compressed.

### Using termux-restore

Warning: restore procedure will destroy any previous data stored in $PREFIX. 
Script will perform a complete rollback to state state exactly as in backup archive.

Restoring backup is also simple:

```bash
termux-restore /sdcard/backup.tar.xz
```

Once finished, restart Termux application.

The utility termux-restore is able to read backup data from standard input. 
You can use this for reading content supplied by other tool. Backup contents supplied to stdin must not be compressed. 
See below example for restoring from encrypted, compressed backup:

```bash
export GPG_TTY=$(tty)
gpg --decrypt /sdcard/backup.tar.gz.gpg | gunzip | termux-restore -
```

Please note that if restore procedure will be terminated before finish, your environment will be corrupted.
