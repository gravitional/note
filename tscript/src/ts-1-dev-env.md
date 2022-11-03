# npm typescript introduction

[TypeScript开发(一) 环境搭建](https://blog.csdn.net/yexudengzhidao/article/details/103106797)
[TypeScript 开发环境搭建](https://juejin.cn/post/7026582077114073125)
[5分钟上手TypeScript](https://www.tslang.cn/docs/handbook/typescript-in-5-minutes.html)
[Webpack 中文文档](https://www.webpackjs.com/concepts/)

## 使用webpack配置开发环境

+ 直接配置 `webpack`, `让webpack` 对我们编写的代码进行一个编译, 并且自动引入编译后的js文件;
+ 而且 `webpack` 可以在代码修改后重新帮助我们进行编译, 并且自动刷新浏览器, 不需要手动操作;

### 安装 node packages

+ 使用 `npm install -g` 全局安装 `typescript`编译器, `eslint` 语法规范工具

```bash
npm install -g typescript eslint
```

+ `eslint`初始化

```bash
eslint --init
```

### webpack 模块打包

新建一个空目录, 例如`webapp`, 然后安装本地 `TypeScript` 依赖

```bash
mkdir webapp && cd webapp
npm install typescript
```

在项目中初始化 `eslint` 的配置文件: `.eslintrc.json`

+ `ts-loader`, html插件, `webpack` 模块

```bash
npm install cross-env ts-loader html-webpack-plugin -D
npm install webpack webpack-cli webpack-dev-server -D
```

这些插件的作用分别是:

+ `cross-env`; 这个插件的作用是可以在 `webpack.config.js` 中,
通过 `process.env.NODE_ENV` 来获取当前是开发还是生产环境
+ `ts-loader`; 因为我们需要解析 `.ts` 文件, 所以需要依赖对应的 loader.
+ `html-webpack-plugin`; 编译后的代码需要对应的 `html模块` 作为它的运行环境, 
所以我们需要使用 `html-webpack-plugin` 来将它插入到对应的模板中: 
+ 使用 `webpack` 开发和打包, 需要依赖 `webpack`, `webpack-cli`, `webpack-dev-server`.

## 项目环境的基础配置

### 新建目录

为了我们之后的学习和使用方便, 我们来配置一个 `webpack` 的环境: 
在环境中我们编写对应的 `TypeScript` 代码, 让 `webpack` 自动帮助我们编译, 
并且在浏览器中查看结果.

创建项目的目录结构

```bash
│ index.html
├─build
│   webpack.config.js
└─src
    main.ts
```

目录和文件夹结构分析:

+ `index.html` 是跑在浏览器上的模块文件
+ `build` 文件夹中用于存放 `webpack` 的配置信息
+ `src` 用于存放我们之后编写的所有 `TypeScript` 代码

### npm管理项目依赖

`webpack` 本身需要有很多的依赖, 并且之后我们也需要启动 `node` 服务,
来快速浏览 `index.html` 模板, 以及编译后的 `JavaScript` 代码. 
使用 `npm` 来初始化 `package.json` 文件, 在项目根目录下运行:

```bash
npm init -y
```

+ 为了方便启动 `webpack`, 我们在刚刚生成的 `package.json` 中添加如下启动命令

```json
"scripts": {
"test": "echo \"Error: no test specified\" && exit 1",
"serve": "cross-env NODE_ENV=development webpack-dev-server --mode=development --config build/webpack.config.js"
},
```

### TypeScript 配置文件, tsconfig.json

在进行 `TypeScript` 开发时, 我们会针对 `TypeScript` 进行相关的配置, 
而这些 `配置信息` 存放在 `tsconfig.json` 文件中.
我们可以通过命令行生成一个模板文件: 

```bash
tsc --init
```

### 配置webpack.config.js文件

按如下所示配置 `webpack.config.js` 文件:

```json
const HtmlWebpackPlugin = require("html-webpack-plugin");

module.exports = {
    entry: "./src/main.ts",
    output: {
        filename: "build.js"
    },
    resolve: {
        extensions: [".tsx", ".ts", ".js"]
    },
    module: {
        rules: [
            {
                test: /\.tsx?$/,
                use: "ts-loader",
                exclude: /node_modules/
            }
        ]
    },
    devtool: process.env.NODE_ENV === "production" ? false : "inline-source-map",
    devServer: {
        static: "./dist",
        // stats: "errors-only",
        compress: false,
        host: "localhost",
        port: 8080
    },
    plugins: [
        new HtmlWebpackPlugin({
            template: "./index.html"
        })
    ]
};
```

## 项目环境下代码测试

下面我们就可以愉快的在main.ts中编写代码, 之后只需要启动服务即可: 

在终端中启动服务: 

```bash
npm run serve
```

在浏览器中打开: `http://localhost:8080/`
修改代码, 直接可以看到修改后的效果: 不需要手动进行任何刷新
