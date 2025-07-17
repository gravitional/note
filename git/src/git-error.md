# git 报错

## git http 拉取代码报错

```bash
Cloning into 'tps_service'...
fatal: unable to access 'http://xxxxxx.git/': Failed to connect to 192.168.0.41 port 8082 after 21052 ms: Could not connect to server
```

原因是设置了 git 的全局代理, 默认配置文件在 `~/.gitconfig`,
把类似这样的行注释掉,

```yaml
[http]
    proxy = http://192.168.0.41:8082
```

## git 拉取 子模块(github repo) 报错

```bash
Cloning into 'C:/Users/tom/cppTest/git_repos/argparse'...
Host key verification failed.
fatal: Could not read from remote repository.
```

原因是 github 的 key 没有通过验证,
随便在其他目录 `git clone` 一个 github 上的仓库到本地,会提示

```bash
Cloning into 'fmt'...
The authenticity of host 'github.com (20.205.243.166)' can't be established.
ED25519 key fingerprint is SHA256:+DiY3wvvV6TuJJhbpZisF/zLDA0zPMSvHdkr4UvCOqU.
This key is not known by any other names.
Are you sure you want to continue connecting (yes/no/[fingerprint])?
```

输入 `yes` 回车; 这时候再回到原先的项目运行,

```bash
git submodule update --init --recursive
```

即可成功克隆 github 子模块.