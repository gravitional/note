# 在持续集成中运行mdbook

虽然下面的例子使用了 `Travis CI`, 但主要`原则`应该可以直接推广到到其他`持续集成`(continuous integration)供应商.

## 确保你的书的构建和测试通过

下面是一个 `Travis CI .travis.yml`的配置样本, 它可以确保 `mdbook build`和 `mdbook test` 成功运行.
快速的`CI周转时间`(turnaround times) 的关键是, 缓存 `mdbook` 的安装, 这样你就不需要在每次运行 `CI`时编译 `mdbook`.

```yaml
language: rust
sudo: false

cache:
  - cargo

rust:
  - stable

before_script:
  - (test -x $HOME/.cargo/bin/cargo-install-update || cargo install cargo-update)
  - (test -x $HOME/.cargo/bin/mdbook || cargo install --vers "^0.3" mdbook)
  - cargo install-update -a

script:
  - mdbook build && mdbook test # 如果是自定义图书路径, 使用: mdbook build 路径/mybook && mdbook test 路径/mybook
```

## 将你的书部署到GitHub页面

按照这些说明, 在仓库的`master`分支上成功运行`CI`后, 你的书就会发布到 `GitHub页面`上.

首先, 创建一个具有 `public_repo` 权限的 GitHub `Personal Access Token`(个人访问令牌, 如果是私有仓库, 则是 `repo`权限).
进入仓库的 `Travis CI` 设置页面, 添加一个名为 `GITHUB_TOKEN` 的环境变量, 该变量被标记为`安全`, 并且`不在`日志中显示.

在仓库的设置页面中, 导航到`Options`, 将 `GitHub` 页面上的 `源代码` 改为 `gh-pages`.

然后, 把这段代码附加到你的 `.travis.yml` 中, 并更新`book `目录所在的`路径`.

```yaml
deploy:
  provider: pages
  skip-cleanup: true
  github-token: $GITHUB_TOKEN
  local-dir: book # 如果是自定义路径, 使用 : 路径/book
  keep-history: false
  on:
    branch: main
```

完事儿!

注意: `Travis` 有一个新的 [dplv2](https://blog.travis-ci.com/2019-08-27-deployment-tooling-dpl-v2-preview-release)配置, 目前正在测试中.
要使用这种新格式, 请将你的 `.travis.yml` 文件更新为.

```yaml
language: rust
os: linux
dist: xenial

cache:
  - cargo

rust:
  - stable

before_script:
  - (test -x $HOME/.cargo/bin/cargo-install-update || cargo install cargo-update)
  - (test -x $HOME/.cargo/bin/mdbook || cargo install --vers "^0.3" mdbook)
  - cargo install-update -a

script:
  - mdbook build && mdbook test # 如果是自定义路径, 使用 : mdbook build path/to/mybook && mdbook test path/to/mybook

deploy:
  provider: pages
  strategy: git
  edge: true
  cleanup: false
  github-token: $GITHUB_TOKEN
  local-dir: book # 如果是自定义路径, 使用 : path/to/mybook/book
  keep-history: false
  on:
    branch: main
  target_branch: gh-pages
```

### 手动部署到GitHub页面

如果你的 `CI` 不支持 `GitHub页面`, 或者你在其他地方部署 `Github页面` 等集成: 
注意: 你可能要使用不同的`临时文件夹`(tmp dirs).

```bash
$> git worktree add /tmp/book gh-pages
$> mdbook build
$> rm -rf /tmp/book/* # 这部不会删除  .git 目录
$> cp -rp book/* /tmp/book/
$> cd /tmp/book
$> git add -A
$> git commit 'new book message'
$> git push origin gh-pages
$> cd -
```

或者把这个放到 `Makefile` 规则中.

```makefile
.PHONY: deploy
deploy: book
    @echo "====> deploying to github"
    git worktree add /tmp/book gh-pages
    rm -rf /tmp/book/*
    cp -rp book/* /tmp/book/
    cd /tmp/book && \
        git add -A && \
        git commit -m "deployed on $(shell date) by ${USER}" && \
        git push origin gh-pages
```

### 将你的书部署到GitLab页面

在你的`仓库`的`书本项目`的`根目录`下, 创建一个名为 `.gitlab-ci.yml` 的文件, 内容如下.

```yaml
stages:
    - deploy

pages:
  stage: deploy
  image: rust
  variables:
    CARGO_HOME: $CI_PROJECT_DIR/cargo
  before_script:
    - export PATH="$PATH:$CARGO_HOME/bin"
    - mdbook --version || cargo install mdbook
  script:
    - mdbook build -d public
  rules:
    - if: '$CI_COMMIT_REF_NAME == "master"'
  artifacts:
    paths:
      - public
  cache:
    paths:
      - $CARGO_HOME/bin
```

在你`提交`并`推送`这个新文件后, `GitLab CI` 就会运行, 你的书就能看啦 ~ ~
