# MySQL

[liaoxuefeng.com/SQL教程](https://www.liaoxuefeng.com/wiki/1177760294764384/1218728424164736)

简单地说, `SQL`就是访问和处理关系数据库的计算机标准语言.
你可能还听说过`NoSQL`数据库, 也就是非`SQL`的数据库, 包括`MongoDB`, `Cassandra`, `Dynamo`等等, 它们都不是关系数据库.
有很多人鼓吹现代Web程序已经无需关系数据库了, 只需要使用`NoSQL` 就可以. 但事实上, `SQL`数据库从始至终从未被取代过

为什么需要数据库?
如果每个应用程序都各自写自己的读写数据的代码, 一方面效率低, 容易出错, 另一方面, 每个应用程序访问数据的接口都不相同, 数据难以复用.
所以, 数据库作为一种专门管理数据的软件就出现了. 应用程序不需要自己管理数据, 而是通过数据库软件提供的接口来读写数据. 至于数据本身如何存储到文件, 那是数据库软件的事情, 应用程序自己并不关心.

## 安装 SQL

[如何在 Ubuntu 20.04 上安装 MySQL](https://zhuanlan.zhihu.com/p/137339787)
[Mysql添加用户](https://blog.csdn.net/qq_39331713/article/details/81747188)

```bash
sudo apt update
sudo apt install mysql-server
```

在`MySQL 8.0`上, `root` 用户默认通过`auth_socket`插件授权.
`auth_socket`插件通过 `Unix socket` 文件来验证所有连接到`localhost`的用户. 这意味着你不能通过提供密码, 验证为`root`.
以 `root` 用户身份登录 `MySQL` 服务器, 输入;

```bash
sudo mysql
```

如果你想以 `root` 身份登录 `MySQL` 服务器, 使用其他的程序, 例如 `phpMyAdmin`, 你有两个选择.

第一个就是将验证方法从`auth_socket`修改成`mysql_native_password`. 你可以通过运行下面的命令实现:

```sql
ALTER USER 'root'@'localhost' IDENTIFIED WITH mysql_native_password BY '密码';
FLUSH PRIVILEGES;
```

第二个选项, 推荐的选项, 就是创建一个新的独立管理用户, 拥有所有数据库的访问权限:

```sql
GRANT ALL ON *.* TO 'administrator'@'localhost' IDENTIFIED BY '密码';
```

### 创建用户

```sql
CREATE USER 'username'@'host' IDENTIFIED BY 'password';
```

`username` -你将创建的用户名说明:
`host` – 指定该用户在哪个主机上可以登陆,如果是本地用户可用`localhost`,  如 果想让该用户可以从任意远程主机登陆,可以使用通配符`%`
`password` –  该用户的登陆密码,密码可以为空,如果为空则该用户可以不需要密码登 陆服务器

例子:

```sql
CREATE USER 'javacui'@'localhost' IDENTIFIED BY '123456';
CREATE USER 'javacui'@'172.20.0.0/255.255.0.0' IDENDIFIED BY '123456';
CREATE USER 'javacui'@'%' IDENTIFIED BY '123456';
```

### 授权

```sql
GRANT privileges ON databasename.tablename TO 'username'@'host';
```

+ `privileges` – 用户的操作权限,如 `SELECT` , `INSERT` , `UPDATE`  等(详细列表见该文最后面).如果要授予所有的权限则使用`ALL`说明:
+ `databasename` –  数据库名
+ `tablename`-表名,如果要授予该用户对所有数据库和表的相应操作权限则可用`*` 表示, 如`*.*`

例子:

```sql
GRANT SELECT, INSERT ON test.user TO 'javacui'@'%';
GRANT ALL ON *.* TO 'javacui'@'%';
```

```sql
GRANT All ON *.* TO 'tom'@'%' IDENTIFIED BY '9512';
```

注意:用以上命令授权的用户不能给其它用户授权,如果想让该用户可以授权,用以下命令

```sql
GRANT privileges ON databasename.tablename TO 'username'@'host' WITH GRANT OPTION;
```

### 设置与更改用户密码

```sql
SET PASSWORD FOR 'username'@'host' = PASSWORD('newpassword');
/* 如果是当前登陆用户用  */
SET PASSWORD = PASSWORD("newpassword");
```

### 撤销用户权限

```sql
REVOKE privilege ON databasename.tablename FROM 'username'@'host';
```

`privilege`, `databasename`, `tablename` – 同授权部分

假如你在给用户`'javacui'@'%'`授权的时候是这样的:

```sql
GRANT SELECT ON test.user TO 'javacui'@'%';
```

则在使用`REVOKE SELECT ON *.* FROM 'javacui'@'%';`命令并不能撤销该用户对`test`数据库中`user`表的`SELECT`操作.

相反,如果授权使用的是

```sql
GRANT SELECT ON  *.* TO 'javacui'@'%';
```

则`REVOKE SELECT ON test.user FROM  'javacui'@'%';`命令也不能撤销该用户对`test`数据库中`user`表的 `Select` 权限
具体信息可以用命令`SHOW GRANTS FOR 'javacui'@'%';`查看

### 删除用户

```sql
DROP USER 'username'@'host';
```

操作后切记刷新数据库

```sql
flush privileges;
```

## MySQL Server

命令行程序 `mysql` 实际上是MySQL客户端,
真正的MySQL服务器程序是 `mysqld`, 在后台运行.
打开命令提示符, 输入命令 `mysql -u root -p`, 提示输入口令.
填入MySQL的root口令, 如果正确, 就连上了MySQL Server, 同时提示符变为 `mysql>`:

输入 `exit` 断开与MySQL Server的连接并返回到命令提示符.

在MySQL Client中输入的SQL语句通过TCP连接发送到MySQL Server.
默认端口号是 `3306`, 即如果发送到本机MySQL Server, 地址就是 `127.0.0.1:3306`.

也可以只安装 MySQL Client, 然后连接到远程MySQL Server.
假设远程MySQL Server的IP地址是 `10.0.1.99`, 那么就使用 `-h` 指定 `IP` 或 `域名`:

```bash
mysql -h 10.0.1.99 -u root -p
```

## 管理MySQL

要管理MySQL, 可以使用可视化图形界面 [MySQL Workbench](https://dev.mysql.com/downloads/workbench/).

MySQL Workbench可以用可视化的方式查询, 创建和修改数据库表,
但是, 归根到底, MySQL Workbench是一个图形客户端, 它对MySQL的操作仍然是发送SQL语句并执行.
因此, 本质上, MySQL Workbench和MySQL Client命令行都是客户端, 和MySQL交互, 唯一的接口就是SQL.

因此, MySQL提供了大量的SQL语句用于管理.
虽然可以使用MySQL Workbench图形界面来直接管理MySQL,
但是很多时候通过SSH远程连接时, 只能使用SQL命令,
所以了解并掌握常用的SQL管理操作是必须的.

## 数据库

在一个运行MySQL的服务器上, 实际上可以创建多个 `数据库`(Database).
要列出所有数据库, 使用命令:

```sql
mysql> SHOW DATABASES;
```

其中, `information_schema`, `mysql`, `performance_schema` 和 `sys` 是系统库, 不要去改动它们.
其他的是 `用户创建` 的数据库.

要创建一个新数据库, 使用命令:

```sql
mysql> CREATE DATABASE test;
Query OK, 1 row affected (0.01 sec)
```

要删除一个数据库, 使用命令:

```sql
mysql> DROP DATABASE test;
Query OK, 0 rows affected (0.01 sec)
```

注意: 删除 `数据库` 将导致该数据库的所有表全部被删除.

对一个数据库进行操作时, 要首先将其切换为当前数据库:

```sql
mysql> USE test;
Database changed
```

## 表

列出当前数据库的所有表, 使用命令:

```sql
mysql> SHOW TABLES;
```

要查看一个表的结构, 使用命令:

```sql
mysql> DESC students;
```

还可以使用以下命令查看创建表的SQL语句:

```sql
mysql> SHOW CREATE TABLE students;
```

创建表使用 `CREATE TABLE` 语句, 而删除表使用 `DROP TABLE` 语句:

```sql
mysql> DROP TABLE students;
Query OK, 0 rows affected (0.01 sec)
```

修改表就比较复杂. 如果要给 `students` 表新增一列 `birth`, 使用:

```sql
ALTER TABLE students ADD COLUMN birth VARCHAR(10) NOT NULL;
```

要修改 `birth` 列, 例如把列名改为 `birthday`, 类型改为 `VARCHAR(20)`:

```sql
ALTER TABLE students CHANGE COLUMN birth birthday VARCHAR(20) NOT NULL;
```

要删除 column, 使用:

```sql
ALTER TABLE students DROP COLUMN birthday;
```

## 退出MySQL

使用EXIT命令退出MySQL:

```sql
mysql> EXIT
Bye
```

注意EXIT仅仅断开了客户端和服务器的连接, MySQL服务器仍然继续运行.
