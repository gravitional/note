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

如果想要修改默认路径的话 只需要在配置文件中 替换2处出现的这个路径即可
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
