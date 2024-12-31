# npm basic.md

## 首先下载 node.js

[https://nodejs.org/zh-cn](https://nodejs.org/zh-cn)

后面使用 npm 安装包, 卡住不动, 重新安装 node.js 解决问题.

## [npmmirror 镜像站](https://npmmirror.com/)

换源

### 功能简介

这是一个完整 npmjs.com 镜像, 你可以用此代替官方版本(只读), 我们将尽量与官方服务实时同步.

### 使用说明

你可以使用我们定制的cnpm命令行工具代替默认的 npm.
cnpm 支持除了写相关操作外的所有命令, 例如 install, info, view 等.

```bash
npm install -g cnpm --registry=https://registry.npmmirror.com
```

或者你直接通过添加 npm 参数 alias 一个新命令:

```bash
alias cnpm="npm --registry=https://registry.npmmirror.com --cache=$HOME/.npm/.cache/cnpm --disturl=https://npmmirror.com/mirrors/node --userconfig=$HOME/.cnpmrc"
```

当然, 你也可以使用任意你心仪的命令行工具, 只要配置 `registry` 即可

```bash
npm config set registry https://registry.npmmirror.com
```

安装模块

```bash
cnpm install [name]
```

同步模块

```bash
cnpm sync cnpmcore
```

当然, 你可以直接通过 web 方式来同步, 界面打开时会自动比对版本信息

```bash
open https://npmmirror.com/sync/cnpmcore
```

### 使用 Forge 的 make 命令来创建可分发的应用程序:

```bash
npm run make
```

这一步会失败, 换用 `cnpm` 即可

```bash
cnpm run make
```