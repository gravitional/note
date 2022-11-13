# 查询数据

在关系数据库中, 最常用的操作就是查询.
为了便于讲解和练习, 我们先准备好了一个`students`表和一个`classes`表, 它们的结构和数据如下:
[在线练习](https://www.liaoxuefeng.com/wiki/1177760294764384/1179610544539040)

如果你想用 `MySQL` 练习, 可以[下载这个SQL脚本](https://github.com/michaelliao/learn-sql/blob/master/mysql/init-test-data.sql), 然后在命令行运行:

```sql
set character set utf8; --用于读取中文
-- 如果test数据库不存在, 就创建test数据库:
CREATE DATABASE IF NOT EXISTS test;

-- 切换到test数据库
USE test;

-- 删除classes表和students表(如果存在):
DROP TABLE IF EXISTS classes;
DROP TABLE IF EXISTS students;

-- 创建classes表:
CREATE TABLE classes (
    id BIGINT NOT NULL AUTO_INCREMENT,
    name VARCHAR(100) NOT NULL,
    PRIMARY KEY (id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- 创建students表:
CREATE TABLE students (
    id BIGINT NOT NULL AUTO_INCREMENT,
    class_id BIGINT NOT NULL,
    name VARCHAR(100) NOT NULL,
    gender VARCHAR(1) NOT NULL,
    score INT NOT NULL,
    PRIMARY KEY (id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- 插入classes记录:
INSERT INTO classes(id, name) VALUES (1, '一班');
INSERT INTO classes(id, name) VALUES (2, '二班');
INSERT INTO classes(id, name) VALUES (3, '三班');
INSERT INTO classes(id, name) VALUES (4, '四班');

-- 插入students记录:
INSERT INTO students (id, class_id, name, gender, score) VALUES (1, 1, '小明', 'M', 90);
INSERT INTO students (id, class_id, name, gender, score) VALUES (2, 1, '小红', 'F', 95);
INSERT INTO students (id, class_id, name, gender, score) VALUES (3, 1, '小军', 'M', 88);
INSERT INTO students (id, class_id, name, gender, score) VALUES (4, 1, '小米', 'F', 73);
INSERT INTO students (id, class_id, name, gender, score) VALUES (5, 2, '小白', 'F', 81);
INSERT INTO students (id, class_id, name, gender, score) VALUES (6, 2, '小兵', 'M', 55);
INSERT INTO students (id, class_id, name, gender, score) VALUES (7, 2, '小林', 'M', 85);
INSERT INTO students (id, class_id, name, gender, score) VALUES (8, 3, '小新', 'F', 91);
INSERT INTO students (id, class_id, name, gender, score) VALUES (9, 3, '小王', 'M', 89);
INSERT INTO students (id, class_id, name, gender, score) VALUES (10, 3, '小丽', 'F', 85);

-- OK:
SELECT 'ok' as 'result:';
```

例如将此表保存为 `init-test-data.sql`, 在 linux or macos 上,
使用下列命令初始化:

```cmd
mysql -u root -p < init-test-data.sql
```

在 windows 系统中, 在 `powershell` 中使用下列命令:

```powershell
mysql -u root -p #按提示输入密码, 命令符样式转为 mysql>
```

在 mysql 环境下输入下列命令以初始化数据库

```sql
source init-test-data.sql
```

就可以自动创建`test`数据库, 并且在`test`数据库下创建`students`表和`classes`表, 以及必要的初始化数据.
和内存数据库不同的是, 对`MySQL`数据库做的所有修改, 都会保存下来.
如果你希望恢复到初始状态, 可以再次运行该脚本.

### 命令行选项

+ `--user=user_name`, or `-u user_name`; 用于连接到服务器的`MySQL`账户的用户名.
+ `--password[=password]`, or `-p[password]`; 用于连接服务器的MySQL账户的密码. 密码值是可选的.
如果不给, `mysql`会提示输入. 如果给定, 在`--password=`或`-p`和后面的密码之间不能有空格. 如果没有指定密码选项, 则默认不发送密码.

### mysql>环境

使用 `\! cmd` 的形式, 可以在 `mysql>` 环境中执行 shell 命令, 或者 windows cmd 命令

```powershell
\! dir # 列出目录下的文件
\! cls # 清除屏幕内容
```

### 基本查询

[MySQL 选择数据库](https://www.runoob.com/mysql/mysql-select-database.html)

选择数据库; `USE test;`, `test`是数据库的名称.

要查询数据库表的数据, 我们使用如下的`SQL`语句:

```sql
SELECT * FROM <表名>
```

假设表名是`students`, 要查询`students`表的所有`行`, 我们用如下`SQL`语句:

```sql
SELECT * FROM students;
```

使用`SELECT * FROM students`时, `SELECT`是关键字, 表示将要执行一个查询, `*`表示`所有列`, `FROM`表示将要从哪个表查询, 本例中是`students`表.
该`SQL`将查询出`students`表的所有数据. 注意: 查询结果也是一个二维表, 它包含列名和每一行的数据.

要查询`classes`表的所有行, 我们用如下SQL语句:

```sql
SELECT * FROM classes;
```

运行上述`SQL`语句, 观察查询结果.

SELECT语句其实并不要求一定要有`FROM`子句. 我们来试试下面的`SELECT`语句:

```sql
SELECT 100+200;
```

上述查询会直接计算出表达式的结果. 虽然`SELECT`可以用作计算, 但它并不是`SQL`的强项.
但是, 不带FROM子句的`SELECT`语句有一个有用的用途, 就是用来判断当前到数据库的连接是否有效. 许多检测工具会执行一条`SELECT 1;`来测试数据库连接.

### 小结

+ 使用`SELECT`查询的基本语句`SELECT * FROM <表名>`, 可以查询一个表的所有行和所有列的数据.
+ `SELECT`查询的结果是一个二维表.

### 条件查询

使用`SELECT * FROM <表名>`可以查询到一张表的所有记录. 但是, 很多时候, 我们并不希望获得所有记录, 而是根据条件选择性地获取指定条件的记录, 例如, 查询分数在80分以上的学生记录.
在一张表有数百万记录的情况下, 获取所有记录不仅费时, 还费内存和网络带宽.

`SELECT`语句可以通过`WHERE`条件来设定查询条件, 查询结果是满足查询条件的记录. 例如, 要指定条件`分数在80分或以上的学生`, 写成`WHERE`条件就是
`SELECT * FROM students WHERE score >= 80`.

其中, `WHERE`关键字后面的`score >= 80`就是条件. `score`是列名, 该列存储了学生的成绩, 因此, `score >= 80`就筛选出了指定条件的记录:

```sql
SELECT * FROM students WHERE score >= 80;
```

因此, 条件查询的语法就是:

```sql
SELECT * FROM <表名> WHERE <条件表达式>
```

条件表达式可以用`<条件1> AND <条件2>`表达满足`条件1`并且满足`条件2`. 例如, 符合条件`分数在80分或以上`, 并且还符合条件`男生`, 把这两个条件写出来:

+ 条件1: 根据`score`列的数据判断: `score >= 80`;
+ 条件2: 根据`gender`列的数据判断: `gender = 'M'`, 注意`gender`列存储的是字符串, 需要用单引号括起来.

就可以写出`WHERE`条件: `score >= 80 AND gender = 'M'`:

```sql
SELECT * FROM students WHERE score >= 80 AND gender = 'M';
```

第二种条件是`<条件1> OR <条件2>`, 表示满足`条件1`或者满足`条件2`. 例如, 把上述`AND`查询的两个条件改为`OR`, 查询结果就是`分数在80分或以上`或者`男生`, 满足任意之一的条件即选出该记录:

```sql
SELECT * FROM students WHERE score >= 80 OR gender = 'M';
```

很显然`OR`条件要比`AND`条件宽松, 返回的符合条件的记录也更多.

第三种条件是`NOT <条件>`, 表示`不符合该条件`的记录. 例如, 写一个`不是2班的学生`这个条件, 可以先写出`是2班的学生`: `class_id = 2`, 再加上`NOT: NOT class_id = 2`:

```sql
SELECT * FROM students WHERE NOT class_id = 2;
```

上述`NOT`条件`NOT class_id = 2`其实等价于`class_id <> 2`, 因此, `NOT`查询不是很常用.

要组合三个或者更多的条件, 就需要用小括号`()`表示如何进行条件运算. 例如, 编写一个复杂的条件: 分数在`80`以下或者`90`以上, 并且是男生:

```sql
SELECT * FROM students WHERE (score < 80 OR score > 90) AND gender = 'M';
```

如果不加括号, 条件运算按照`NOT`, `AND`, `OR`的优先级进行, 即`NOT`优先级最高, 其次是`AND`, 最后是`OR`. 加上括号可以改变优先级.

### 常用的条件表达式

***
条件    表达式举例1    表达式举例2    说明

+ 使用`=`判断相等  ;  `score = 80`  , `name = 'abc'` ;   字符串需要用单引号括起来
+ 使用`>`判断大于  ;  `score > 80`  ,  `name > 'abc'`    字符串比较根据`ASCII`码, 中文字符比较根据数据库设置
+ 使用`>=`判断大于或相等 ;   `score >= 80` ,   `name >= 'abc'`
+ 使用`<`判断小于 ;  `score < 80` ,  `name <= 'abc'`
+ 使用`<=`判断小于或相等  ;  `score <= 80`  ;  `name <= 'abc'`
+ 使用`<>`判断不相等  ;  `score <> 80`  , `name <> 'abc'`
+ 使用`LIKE`判断相似  ; `name LIKE 'ab%'`, `name LIKE '%bc%'` ;  `%`表示任意字符, 例如`'ab%'`将匹配`'ab'`, `'abc'`, `'abcd'`

表示区间的等价方法:

+ `SELECT * FROM students WHERE score >= 60 AND score <= 90`
+ `SELECT * FROM students WHERE score BETWEEN 60 AND 90;`
