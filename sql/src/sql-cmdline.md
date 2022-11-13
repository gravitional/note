# MYSQL 命令行

[MYSQL 命令行大全 ](https://blog.csdn.net/qq_35409127/article/details/79760797)

## 启动与退出

+ 进入MySQL:
启动MySQL Command Line Client(MySQL的DOS界面), 直接输入安装时的密码即可.
此时的提示符是: `mysql>`
+ 退出MySQL: `quit` 或 `exit`

## MYSQL常用命令

+ 导出整个数据库

```sql
mysqldump -u 用户名 -p –default-character-set=latin1 数据库名 > 导出的文件名(数据库默认编码是latin1)
mysqldump -u wcnc -p smgp_apps_wcnc > wcnc.sql
```

+ 导出一个表

```sql
mysqldump -u 用户名 -p 数据库名 表名> 导出的文件名
mysqldump -u wcnc -p smgp_apps_wcnc users> wcnc_users.sql
```

+ 导出一个数据库结构

```sql
mysqldump -u wcnc -p -d –add-drop-table smgp_apps_wcnc >d:wcnc_db.sql
```

-d 没有数据 –add-drop-table 在每个create语句之前增加一个drop table

### 导入数据库

+ source 命令, 常用
进入mysql数据库控制台, 如 `mysql -u root -p`

```sql
mysql>use 数据库
```

然后使用source命令, 后面参数为脚本文件(如这里用到的.sql)

```sql
mysql>source wcnc_db.sql
```

+ mysqldump命令

```sql
mysqldump -u username -p dbname < filename.sql
```

+ mysql命令

```sql
mysql -u username -p -D dbname < filename.sql
```

## 库操作

+ 创建数据库命令:

```sql
create database <数据库名>
```

例如: 建立一个名为xhkdb的数据库

```sql
mysql> create database xhkdb;
```

+ 显示所有的数据库:

```sql
mysql> show databases; --注意: 最后有个s
```

+ 删除数据库

```sql
drop database <数据库名>
```

例如: 删除名为 xhkdb的数据库

```sql
mysql> drop database xhkdb;
```

+ 连接数据库

```sql
use <数据库名>
```

例如: 如果xhkdb数据库存在, 尝试存取它:

```sql
mysql> use xhkdb;
```

屏幕提示: Database changed
5, 查看当前使用的数据库
mysql> select database();
6, 当前数据库包含的表信息:
mysql> show tables; (注意: 最后有个s)

## 表操作, 操作之前应连接某个数据库

+ 建表

```sql
create table <表名> ( <字段名> <类型> [,..<字段名n> <类型n>]);
mysql> create table MyClass(
> id int(4) not null primary key auto_increment,
> name char(20) not null,
> sex int(4) not null default '′,
> degree double(16,2));
```

+ 获取表结构

```sql
-- desc 表名, 或者show columns from 表名
mysql>DESCRIBE MyClass
mysql> desc MyClass;
mysql> show columns from MyClass;
```

+ 删除表

```sql
drop table <表名>
-- 例如: 删除表名为 MyClass 的表
mysql> drop table MyClass;
```

+ 插入数据

```sql
insert into <表名> [( <字段名>[,..<字段名n > ])] values ( 值 )[, ( 值n )]
```

例如, 往表 MyClass中插入二条记录, 这二条记录表示:
编号为的名为Tom的成绩为 96.45,
编号为 的名为Joan 的成绩为 82.99,
编号为 的名为Wang 的成绩为 96.59.

```sql
mysql> insert into MyClass values(1,'Tom',96.45),(2,'Joan',82.99), (2,'Wang', 96.59);
```

## 查询表中的数据

+ 查询所有行

```sql
select <字段, 字段, …> from < 表名 > where < 表达式 >
-- 查看表 MyClass 中所有数据
mysql> select  from MyClass;
```

+ 查询前几行数据

```sql
--查看表 MyClass 中前行数据
mysql> select  from MyClass order by id limit 0,2;
-- 或者:
mysql> select  from MyClass limit 0,2;
```

+ 删除表中数据

```sql
delete from 表名 where 表达式
--例如: 删除表 MyClass中编号为 的记录
mysql> delete from MyClass where id=1;
```

+ 修改表中数据:

```sql
update 表名 set 字段=新值,…where 条件
mysql> update MyClass set name='Mary'where id=1;
```

+ 在表中增加字段:

```sql
alter table 表名 add字段 类型 其他;
-- 例如: 在表MyClass中添加了一个字段passtest, 类型为int(4), 默认值为 ''
mysql> alter table MyClass add passtest int(4) default '′
```

+ 更改表名:

```sql
rename table 原表名 to 新表名;
-- 例如: 在表MyClass名字更改为YouClass
mysql> rename table MyClass to YouClass;
```

+ 更新字段内容

```sql
update 表名 set 字段名 = 新内容
update 表名 set 字段名 = replace(字段名,'旧内容','新内容')
```
