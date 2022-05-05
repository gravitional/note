# WebDAV

[网络存储文件共享之WebDAV](https://zhuanlan.zhihu.com/p/352216119)

常用的文件共享有三种: `FTP`, `Samba`, `WebDAV`, 它们各有优缺点, 了解后才能更好地根据自己的需求选择方案.

+ `FTP` 属于古老的文件共享方式了, 因为安全性, 现代浏览器最新已默认不能打开FTP协议.
`SFTP` 在FTP基础上增加了加密, 在 Linux 上安装 `OpenSSH` 后可以直接用SFTP协议传输.
使用 `SFTP` 临时传送文件还可以, 但做文件共享, 性能不高, 速度较慢.

+ `Samba` 是Linux下 `CIFS` 协议的实现, 优势在于对于小白使用简章,
和 `Windows` 系统文件共享访问一样, 不需要安装第三方软件, 而且移动端也有大量APP支持.
苹果手机 `文件APP` 中添加网络存储用的就是这种方式.
`Windows` 下文件共享使用 `445端口`, 且不能更改.
`445` 端口常常受黑客关照, 在广域网上大多运营封掉了访端口, 所以这种文件共享只适合在内网使用.

+ `WebDAV` 基于 `HTTP` 协议的通信协议,
在 GET, POST, HEAD等几个HTTP标准方法以外添加了一些新的方法,
使应用程序可对Web Server直接读写, 并支持写文件锁定(Locking)及解锁(Unlock), 还可以支持文件的版本控制.
因为基于 `HTTP`, 在广域网上共享文件有天然的优势, 移动端文件管理APP也大多支持 `WebDAV` 协议.
使用 `HTTPS` 还能保安全性.
`Apache` 和 `Nginx` 支持 `WebDAV`, 可作为 `WebDAV` 文件共享 `服务器`软件.
也可以使用专门的WebDAV软件部署.

## WebDAV Server (推荐)

`WebDAV` 是 `GitHub` 上开源的项目, 基于 `Go` 语言实现,
不仅跨平台, 还支持 ARM 架构, 可在㠌入式设备中部署 WebDAV 服务器.
项目地址: [hacdias/webdav](https://github.com/hacdias/webdav)

在 `GitHub` 下载对应的架构 `WebDAV`,
如: `windows-amd64-webdav.zip` . 解压后获得 `webdav.exe` .

用文本编辑器新建 `config.yaml` 文件, 内容如下:

```yaml
# 监听任意网卡, 多网卡可指定对应ip
address: 0.0.0.0
port: 8081
# 如果无需验证填 false
auth: true
# 如果不需要 https 则填 false
tls: true
# https证书和密钥, 如果 tls 为 false, cert 和 key 不需要
cert: /data/www/cert/szhome.xf1024.com_nginx/cert.pem
key: /data/www/cert/szhome.xf1024.com_nginx/cert.key
# 访问前缀, 建议默认
prefix: /

# 如果 auth 为 false 生效, 文件共享的路径
scope: /data/users/public
# 是否允许修改
modify: true
rules: []

# 跨域设置
cors:
  enabled: true
  credentials: true
  allowed_headers:
    - Depth
  allowed_hosts:
    - http://localhost:8081
  allowed_methods:
    - GET
  exposed_headers:
    - Content-Length
    - Content-Range

# 用户信息, 如果 auth 为 true 生效
users:
  - username: user1
    password: 123456
    scope: /data/users/2021
  - username: user2
    password: 654321
    scope: /data/users/2022
```

注意 `yaml` 文件格式的书写规则, `users` 下是需认证的 `用户名`, `密码`, 及 `用户共享文件`.
使用以下命令运行服务器:

```bash
webdav -c ./config.yaml
```

## Apache 开启 WebDAV

`Apache` 开启 `WebDAV` 需要打开以下模块:

```config
LoadModule dav_module modules/mod_dav.so
LoadModule dav_fs_module modules/mod_dav_fs.so
LoadModule dav_lock_module modules/mod_dav_lock.so
```

配置如下:

```xml
<VirtualHost *:80>
    ServerName dav.engr-z.com
    DocumentRoot /data/webdav
    <Directory "/data/webdav">
        Options Indexes FollowSymLinks
        AllowOverride None
        Dav on
        AuthType Basic
        AuthName "WebDAV Upload"
        AuthUserFile conf/.htpasswd
        AuthBasicProvider file
        Require user webdav
    </Directory>
</VirtualHost>

<VirtualHost *:443>
    ServerName dav.engr-z.com
    DocumentRoot /data/webdav
    <Directory "/data/webdav">
        Options Indexes FollowSymLinks
        AllowOverride None
        Dav on
        AuthType Basic
        AuthName "WebDAV Upload"
        AuthUserFile conf/.htpasswd
        AuthBasicProvider file
        Require user webdav
    </Directory>
#    Header always set Strict-Transport-Security "max-age=63072000; includeSubdomains; preload"
    # 添加 SSL 协议支持协议, 去掉不安全的协议
    SSLProtocol all -SSLv2 -SSLv3
    # 修改加密套件如下
    SSLCipherSuite HIGH:!RC4:!MD5:!aNULL:!eNULL:!NULL:!DH:!EDH:!EXP:+MEDIUM
    SSLHonorCipherOrder on
    # 证书公钥配置
    SSLCertificateFile cert/dav.engr-z.com_apache/public.crt
    # 证书私钥配置
    SSLCertificateKeyFile cert/dav.wangzhengzhen.com_apache/cert.key
    # 证书链配置, 如果该属性开头有 '#'字符, 请删除掉
    SSLCertificateChainFile cert/dav.engr-z.com_apache/chain.crt
</VirtualHost>
```

`.htpasswd` 文件是保存用户名密码的文件, 使用 `apache` 工具 `htpasswd` 创建:

```bash
htpasswd -c /etc/webdav/.htpasswd user1
```

如果需要创建多个用户, 在第二次执行时注意去掉 `-c` 参数, 防止生成文件覆盖.

## Nginx 开启 WebDAV

在 `Nginx` 中实现 `WebDAV` 需要安装 `libnginx-mod-http-dav-ext` 模块, 以下是 `Nginx` 的配置:

```conf
server {
        listen 80;
        listen [::]:80;

        server_name dav.engr-z.com;
        auth_basic "Authorized Users Only";
        auth_basic_user_file /etc/.htpasswd;

        location / {
                root /data/webdav;
                client_body_temp_path /var/temp;
                dav_methods PUT DELETE MKCOL COPY MOVE;
                dav_ext_methods PROPFIND OPTIONS;
                create_full_put_path on;
                client_max_body_size 10G;
        }
}

server {
        listen 443;
        listen [::]:443;
        server_name dav.engr-z.com;

        ssl on;
        ssl_certificate /data/www/cert/dav.engr-z.com_nginx/cert.pem;
        ssl_certificate_key /data/www/cert/dav.engr-z.com_nginx/cert.key;
        ssl_session_timeout 5m;
        ssl_protocols SSLv2 SSLv3 TLSv1;
        ssl_ciphers ALL:!ADH:!EXPORT56:RC4+RSA:+HIGH:+MEDIUM:+LOW:+SSLv2:+EXP;
        ssl_prefer_server_ciphers on;

        location / {
                root /data/webdav;
                client_body_temp_path /var/temp;
                dav_methods PUT DELETE MKCOL COPY MOVE;
                dav_ext_methods PROPFIND OPTIONS;
                create_full_put_path on;
                client_max_body_size 10G;
        }

}
```

`.htpasswd` 用户密码文件的创建方式和 `Apache` 一样, `htpasswd` 是 `apache` 的工具,
如果使用 `nginx`, 可以单独安装该工具而不安装整个 `apache`.
在 `Ubuntu` 中使用 `sudo apt install apache2-utils` 安装.

`Nginx` 对 `WebDAV` 支持不是太好, 建议使用 `Apache` 或专用于 `WebDAV` 服务软件架设.

## WebDAV挂载/映射

### Windows

打开 `计算机` , 点右键添加一个网络位置, 按向导填入地址, 用户名, 密码.

![网络位置](https://pic3.zhimg.com/80/v2-574df1552698ddb7250065412448de9a_720w.jpg)
![挂载](https://pic2.zhimg.com/80/v2-0d67494bf9b1dde761c1ad6d005fc875_720w.jpg)

+ 挂载指定盘符:

```powershell
net use Y: https://dav.engr-z.com/ /qizheng:engrz /persistent:YES   password
```

+ 其中 `qizheng` 是我的用户名
+ 把 `password` 换成你的密码.
+ `/persistent` 表示保存映射, 下次开机还在.
+ 执行完, 打开资源管理器, 可以看到磁盘映射了.
+ 如果下次开机, 发现不能打开磁盘, 访问失败, 可以检查 `WebClient` 服务是否开启.

![服务](https://pic3.zhimg.com/80/v2-9db337b3bcfdebee7b8f4cb9f83a89be_720w.jpg)

+ 从 Windows Vista 起, 微软就禁用了 `http` 形式的基本 `WebDAV` 验证形式(KB841215), 必须使用 `https` 连接.
我们可以修改注册表

    HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services\WebClient\Parameters

找到 `BasicAuthLevel` 把这个值从 `1` 改为 `2`,
然后进 `控制面板->服务`, 把 `WebClient` 服务重启(没有启动的就启动它).

在某些版本的 Windows 操作系统中, WebDAV 驱动器的最大文件大小被限制为 50MB.
如果你试图复制超过 50MB 大小的文件, Windows 就会弹出错误提示框.
当然, 这个限制是可以通过修改注册表来消除的.
将注册表中位于

    HKLM\SYSTEM\CurrentControlSet\Services\WebClient\Parameters\FileSizeLimitInBytes

处的键值由 `50000000` (50MB) 修改为更大的数值.
最大修改为: `4294967295`(`0xffffffff`)字节, 即 `4G`.

### RaiDrive

这里推荐使用免费软件 [RaiDrive](https://www.raidrive.com) ,
通过 `RaiDrive` 映射的磁盘, 没有 `http` 和 上传文件大小限制, 无需修改注册表.

`RaiDrive` 是一款能够将一些网盘映射为本地网络磁盘的工具,
支持 Google Drive, Google Photos, Dropbox, OneDrive, FTP, SFTP, WebDAV.

![RaiDrive](https://pic1.zhimg.com/80/v2-6f89effbcce2cbb86293816b886d7bac_720w.jpg)

### Linux

Linux 的文件管理工具大多都支持 `WebDAV` , 以 `Ubuntu` 为例,
在文件管理器左侧栏的 `Other Location`中:

![Other Location](https://pic2.zhimg.com/80/v2-653a93b8d6d190b09a62a5aa34fda7ed_720w.jpg)

+ 还可以使用命令挂载, 需要安装 `davfs2` :

```bash
sudo apt install davfs2
```

执行命令后系统会自动安装, 出现以下提示, 提醒允许非 `root`用户挂载 `WebDAV` 资源.
如果选择错误, 可以通过 `dpkg-reconfigure davfs2` 重新选择:
这里我们选 `<是>`.

![non-root](https://pic2.zhimg.com/80/v2-1eda57424692a8c8b4861ddfe9ce6f0d_720w.jpg)

挂载:

```bash
sudo mount -t davfs http://dav.engr-z/ ./webdav/
```
