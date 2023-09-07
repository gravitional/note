# fortran

## Fortran入门

|版|注意|发布|
|---|---|---|
| FORTRAN 66 |    ASA首次标准化(现为AN​​SI) | 1966年3月7日 |
| FORTRAN 77 |     固定形式, 历史性 |    1978年4月15日|
| Fortran 90 |     自由形式, ISO标准, 阵列操作 |    1991年6月15日|
| Fortran 95 |     纯粹的元素程序 |    1997年6月15日|
| Fortran 2003 |     面向对象编程 |    2004年4月4日|
| Fortran 2008 |     联合数组 |    2010-09-10 |

**fortran 不区分大小写**

任何Fortran程序都必须包括 `end` 作为最后一个语句.
因此, 最简单的Fortran程序如下所示：

```fortran
end
```

以下是“hello, world”程序的一些示例：

```fortran
print *, "Hello, world"
end
```

用 `write` 语句：

```fortran
write(*,*) "Hello, world"
end
```

为清楚起见, 现在通常使用 `program` 语句来启动程序并为其命名.
然后,  `end` 语句可以引用此名称以明确它所引用的内容, 并让编译器检查代码的正确性.

此外, 所有 `Fortran` 程序都应包含一个 `implicit none` 语句. 因此, 最小的Fortran程序实际应该如下所示：

```fortran
program hello
  implicit none
  write(*,*) 'Hello world!'
end program hello
```

下一个步骤是如何查看`hello world`程序的结果.
本节介绍如何在类似`Linux`的环境中实现这一点.

然后转到命令行并导航到保存源文件的目录, 然后键入以下命令：

```fortran
gfortran -o hello hello.f90
```

您刚刚创建了`hello world`可执行程序. 在技​​术方面, 您刚编译了您的程序. 要运行它, 请键入以下命令：

```fortran
./hello
```

您应该在shell终端上看到以下行.

```fortran
Hello world!
```

### 二次方程

今天Fortran主要用于数值计算.
这个非常简单的例子说明了求解二次方程的基本程序结构：

```fortran
program quadratic
  !a comment

  !should be present in every separate program unit
  implicit none

  real :: a, b, c
  real :: discriminant
  real :: x1, x2

  print *, "Enter the quadratic equation coefficients a, b and c:"
  read *, a, b, c

  discriminant = b**2 - 4*a*c

  if ( discriminant>0 ) then

    x1 = ( -b + sqrt(discriminant)) / (2 * a)
    x2 = ( -b - sqrt(discriminant)) / (2 * a)
    print *, "Real roots:"
    print *, x1, x2

    ! Comparison of floating point numbers for equality is often not recommended.
    ! Here, it serves the purpose of illustrating the "else if" construct.
  else if ( discriminant==0 ) then

    x1 = - b / (2 * a)
    print *, "Real root:"
    print *, x1
  else

    print *, "No real roots."
  end if
end program quadratic
```

## IO

### 句法

+ `WRITE(unit num, format num)` 在新行中输出括号后面的数据.
+ `READ(unit num, format num)` 输入括号后面的变量.
+ `OPEN(unit num, FILE=file)` 打开一个文件.  (打开文件有更多选项, 但它们对`I/O`并不重要.
+ `CLOSE(unit num)` 关闭文件

### 传递命令行参数

在支持命令行参数的地方, 可以通过`get_command_argument`内在函数读入它们(在`Fortran 2003`标准中引入).

`command_argument_count`内在提供了一种了解命令行提供的参数数量的方法.

所有命令行参数都以字符串形式读入, 因此必须对数字数据进行内部类型转换.
例如, 这个简单的代码汇总了命令行提供的两个数字：

```fortran
PROGRAM cmdlnsum
IMPLICIT NONE
CHARACTER(100) :: num1char
CHARACTER(100) :: num2char
REAL :: num1
REAL :: num2
REAL :: numsum

! 首先, 确保输入提供了正确的参数个数
IF(COMMAND_ARGUMENT_COUNT().NE.2)THEN
  WRITE(*,*)'错误, 需要两个命令行参数, 停止'
  STOP
ENDIF

CALL GET_COMMAND_ARGUMENT(1,num1char)   !first, read in the two values
CALL GET_COMMAND_ARGUMENT(2,num2char)

READ(num1char,*)num1    !then, convert them to REALs
READ(num2char,*)num2

numsum=num1+num2      !sum numbers
WRITE(*,*)numsum              !write out value

END PROGRAM
```

`get_command_argument`的`number`参数有用地介于`0`和`command_argument_count`的结果之间. 如果值为`0`则提供命令名称(如果支持).

许多编译器还提供非标准内在函数(例如 `getarg` )来访问命令行参数. 由于这些是非标准的, 因此应参考相应的编译器文档.

`get_command_argument` 使用可以使用`length`和`status`参数扩展到上面的示例之外. 例如, 用

```fortran
character(5) arg
integer stat
call get_command_argument(number=1, value=arg, status=stat)
```

如果第一个参数存在且长度大于`5`, 则`stat`的值将为`-1` . 如果检索参数存在其他困难, 则`stat`的值将为某个正数(并且`arg`将完全由空白组成). 否则其值将为`0` .

`length`参数可以与延迟长度字符变量组合, 例如在以下示例中.

```fortran
character(:), allocatable :: arg
integer arglen, stat
call get_command_argument(number=1, length=arglen)  ! Assume for simplicity success
allocate (character(arglen) :: arg)
call get_command_argument(number=1, value=arg, status=stat)
```

## Fortran基本语法

让我们来写一个程序, 相加了两个数字, 并打印出结果：

```fortran
program addNumbers
! This simple program adds two numbers
   implicit none

! Type declarations
   real :: a, b, result

! Executable statements
   a = 12.0
   b = 15.0
   result = a + b
   print *, 'The total is ', result

end program addNumbers
```

### 令牌(Tokens)

令牌是基本字符集中的字符. 令牌可以是一个关键字, 标识符, 常量, 字符串文字或符号.
它对程序语句作出标记.

## 选择结构

The following example shows the use of some of the inbuilt functions.

```fortran
program trig
implicit none
real :: a,pi
print *,'Enter an angle between 0 and 90'
read *, a
pi=4.0*atan(1.0)
print *,'the sine of ',a,' is ',sin(a*pi/180)
end program trig
```

分支结构示例：

```fortran
program test
implicit none
!use of a simple menu
real :: x,y,answer
integer :: choice
!set up the menu – the user may enter 1, 2 or 3
print *,'Choose an option'
print *,'1 Multiply'
print *,'2 Divide'
print *,'3 Add'
read *,choice
x=3.4
y=2.9
!the following line has 2 consecutive
!equals signs – (no spaces in between)
if (choice == 1) then
answer=x*y
print *,'result = ',answer
end if
if (choice == 2) then
answer=x/y
print *,'result = ',answer
end if
if (choice == 3) then
answer=x+y
print *,'result = ',answer
end if
end program test
```

`if ... end if` statements work like this:

```fortran
if (choice = = 1) then
do something
else if (choice = =2) then
do something else
else
do this if nothing else satisfies the conditions
end if
```

`.and.` `.or.` `.not.`

### 简单的if结构

```fortran
print *, 'enter a positive number'
read *, num
if (num <0) stop
if (num < 10) print *, 'less than 10'
if (num > 10) print *, 'greater than 10'
print *,'It is a positive number'
```

注意其中的 `stop`

### 检测是否为0

整数变量是精确的, 但浮点数是不精确的. 通过一个小量来比较.

```fortran
if (abs(x) < .000001) then
print *,’No zero values! Please enter another number’
read *
```

## 循环

```fortran
do x = 1,2
        do y = 1,4,0.5
            z = x/y
            print *, x,y,z
        end do
    end do
```

### 不同类型变量混合

执行下面这个程序

```fortran
program divide
implicit none
integer :: x
real :: y
x = 1
y = x/3
print *, y
end program divide
```

结果是

`0.00000`

把`x = 1`替换成`x = 10`

Your output should now be:

`3.00000`

FORTRAN会把小数部分丢掉. 为了告诉fortran我们使用浮点数算法, 我们需要改成

```fortran
y=x/3.0
```

real number 的存在, 告诉fortran使用实型算法计算右边的式子.

例如以下程序：

```fortran
program check
!Integer and real arithmetic
implicit none
real :: x,y
integer i
x=2.0
i=2
y=x*((2**i)/3)
print *,y
y=x*((2.0**i)/3)
print *,y
end program check
```

### DO 循环

除非我们能重复执行程序, 不然也许计算器更好用.

```fortran
program loop
implicit none
integer :: i
do i=0,20,1
print *,i
end do
end program loop
```

do 循环的标签形式

```fortran
outter: DO i=1,3
  inner: do j=1,3
    write (*,"('(',i2,',',i2')')") i,j
    END DO inner
END DO outter
```

+ `i` 被称为循环计数器, 在本例中, 开始值为1
+ `do` and `end do`之间所有statement 被执行
+ 每次执行过后, 循环变量增加`1`
+ 等到执行完毕, 即`i`增加到`20`, 继续后面的程序
+ 步长缺省为`1`, 也可以为负数, 如`do i = 5,-5,-2`

**Exercise 3.4**

Using a do loop to generate integer values of `x` between `-10` and `10` in steps of `1`,
write a program that constructs a table of values of

```fortran
y=1.0/x
```

What happened when `x` had the value zero?
Use an if, end if to test for the condition that gives the
incorrect value,
and print out an appropriate message.
Compare your result with divbyzero.f95.

Division by zero is one of the commonest reasons for a program to fail.

### 嵌套DO循环

DO 循环可以嵌套, 例如：

**Exercise 3.5**

```fortran
program xytab
    implicit none
    !constructs a table of z=x/y for values of x from 1 to 2 and
    !y from 1 to 4 in steps of .5
    real:: x, y, z
    print *, '        x                y               z'
    do x = 1,2
        do y = 1,4,0.5
            z = x/y
            print *, x,y,z
        end do
    end do
end program xytab
```

观察输出, 这里用`print`产生了一个表头

### 使用loop求和

查看以下程序的输出：

```fortran
program increment
    implicit none
    integer :: i
    real :: x
    x=1.0
    do i=1,10
    x=x+1.0
    print *, i,x
    end do
    end program increment
```

+ 注意我们在循环外设置了`x`的初始值.
+ 需要给`x`设置初始值, 否则`x`的值可能为随机数

## 文献和精度

### 读取文件

使用 read 函数

```fortran
program readdata
implicit none
!reads data from a file called mydata.txt
real :: x,y,z
open(10,file='mydata.txt')
read(10,*) x,y,z
print *,x,y,z
end program readdata
```

`open`语句把文件'mydata.txt'和输入设备号`10`连接起来. (也可以是任意正整数)
在`read`中把`10`作为第一个参数.

在文件中, 格式为每行一个数字输入

### 输出到文件

使用 `write` 进行输出

```fortran
program io2
!illustrates writing arrays to files
implicit none
real :: num
integer :: i
open(12,file='myoutput')
do i = 1,100
num = i/3.0
write(12,*) nums
end do
print *, 'finished'
end program io2
```

### 拓展精确度

若想提高计算精度, 可以仿照如下指定数据类型

```fortran
program extended
implicit none
integer, parameter :: ikind=selected_real_kind(p=15)
real (kind=ikind) :: sum,x
integer :: i
sum=0.0
do i=1,100
x=i
sum = sum + 1.0/(x**6)
end do
print *, sum
end program extended
```

产生以下输出:
`1.01734306196`

我们常常用类似的方法制定精度.

```fortran
integer, parameter :: ikind=selected_real_kind(p=15)
```

15位数字精度, 这种指定与机器的架构无关, 然后用这个参数去指定变量.

```fortran
real (kind=ikind) :: sum,x
```

用`PRINT`输出时有一个问题是, 无法控制数字的位数, 这可以用输出格式化部分的知识解决.

`parameters` 声明之后一般不再改变.

下面示例了如何使用拓展精度的常数：

```fortran
program extendedconstants
!demonstrates use of extended precision
implicit none
integer, parameter :: ikind=selected_real_kind(p=18)
real (kind=ikind) :: val,x,y
val=10/3
print*,val !10/3 calculated as integer - wrong!
x=10.0
y=3.0
val=x/y !x/y assigned to extended precision - right!
print*,val
val=10.0_ikind/3 !extend precision constant - right!
print*,val
val=10.0/3.0 !real constants - wrong!
print*,val
val = .12345678901234567890 ! real constants - wrong!
print *, val
val = .12345678901234567890_ikind  !ext precision consts - right!
print *, val
end program extendedconstants
```

上面的例子, 展示了程序的数据类型很容易出错.
一个系统的方法避免这种错误是, 设计检测程序, 用已知类型的数据去检测.

### 幅度限制

对数字取值的上限也有限制.

|Value of p |Decimal places | Range |
|--- |--- |--- |
|6|6(default)|$\pm 10^{38}$|
|15|15|$\pm 10^{307}$|
|18|18|$\pm 10^{4931}$|

### 收敛--判定退出的条件

如级数展开：
$$
  \sum_{x=1}^{x=10} \frac{1}{x^6}
$$
可以通过级数求和过程中, 贡献的增长情况来判断. 若本次贡献已经很小, 就可以退出了, 例如：

```fortran
program whileloop
implicit none
integer, parameter :: ikind=selected_real_kind(p=15)
real (kind=ikind) :: sum,previoussum,x,smallnumber,error
integer :: i
sum=0.0
previoussum=0.0
smallnumber = 10.0**(-15.0)
do i=1,1000
x=i
sum = sum + 1.0 /(x**6)
error=abs(sum-previoussum)
if (error<smallnumber) then
print *,'sum ',sum,' number of loops ',i
exit
end if
previoussum = sum
end do
end program whileloop
```

现实世界中, 我们要根据实际测量精度来选择数据精度.
比如, 如果测量精度在正负$1% $的话, 就没必要取 $15$个数字精度了.

在`do`循环中使用`counter`并不是必须的. 如下所示：

```fortran
smallnumber = .0000001_ikind
do
print *, 'enter a positive number '
read *, number
if (number <= smallnumber) exit
end do
```

但是这会面临死循环的风险.

## 数组和格式化I/O

### 数组

处理大量数据的时候, 可以使用数组.
为了告诉 FORTRAN 我们要使用数组, 我们需要给出它的大小.

```fortran
real, dimension(100) ::x
.
.
x(1) = 3
x(66) = 4
```

这个代码片段给数组`x`分配了`100`个内存位置.
如上分别访问了数组的第`1`和`66`个元素.

数组示例程序：

```fortran
program av2
implicit none
real ,dimension(10) :: x
real :: average,sum
integer :: i
print *, 'enter 10 numbers'
sum=0.0
do i=1,10
read *, x(i)
sum=sum+x(i)
end do
average=sum/10
print *, 'the average is ',average
print *, 'the numbers are'
print *,x
end program av2
```

`print*, x` 将会打印出数组的所有内容.

下面展示了, 如何让程序具有良好的拓展性：

```fortran
program av3
!just change the value of the parameter to change the size of the
!array
implicit none
integer, parameter :: imax = 10
real,dimension(imax) :: x
real :: average,sum
integer :: i
print *, 'enter’ ,imax, ’ numbers'
sum=0.0
do i=1,imax
read *, x(i)
sum=sum+x(i)
end do
average=sum/imax
print *, 'the average is ',average
print *, 'the numbers are'
print *,x
end program av3
```

这样的代码易于维护, 可以每次动态地分配内存.

下面的程序展示了, 如何使用未知长度的数组.

```fortran
program alloc
implicit none
!++++++++++++
integer, allocatable,dimension(:):: vector
!++++++++++++
!note syntax - dimension(:)
integer :: elements,i
print *,'enter the number of elements in the vector'
read *,elements
!++++++++++++
allocate(vector(elements))
!++++++++++++
!allocates the correct amount of memory
print *,' your vector is of size ',elements,'. Now enter each
element'
do i=1,elements
read *,vector(i)
end do
print *,'This is your vector'
do i=1,elements
print *,vector(i)
end do
!++++++++++++
deallocate(vector)
!++++++++++++
!tidies up the memory
end program alloc
```

其中加标注的行, 展示了如何告诉编译器, 数组矢量--allocatable, 可以在运行的时候再决定 size.

### 矢量化运算

数组允许矢量化运算.

```fortran
program ramagic
implicit none
real ,dimension(100) :: a,b,c,d
open(10,file='data.txt')
read(10,*) a
b=a*10
c=b-a
d=1
print *, 'a= ',a
print *, 'b= ',b
print *, 'c= ',c
print *, 'd= ',d
end program ramagic
```

### 多维数组

还可以使用多维数组. 比如2维数组, 相当于矩阵, 比如：

```fortran
Integer, dimension(5,5) :: a
```

建立一个具有`25`个整型位置的空间.

```fortran
program twodra
implicit none
integer,dimension(2,3) :: a
integer ::row,col,count
count = 0
!creates an array with 3 cols and 2 rows
!sets col 1 to 1, col2 to 2 and so on
do row=1,2
count=0
do col =1,3
count=count+1
a(row,col)=count
end do
end do
do row=1,2
do col =1,3
print *,a(row,col)
end do
end do
end program twodra
```

FORTRAN 最多支持7维数组, 也可以同时指定数组精度, 如：
`real (kind=ikind),dimension(10,20,30) :: b`

### 格式化输出

下面介绍的是格式化输出. 迄今为止, 我们都在用output的默认选项, 就是
write and print 参数中的`*`

```fortran
write(10,*) x,y,z
print *, 'program finished'
```

```fortran
program format
implicit none
! 展示格式化输出
integer, parameter :: ikind=selected_real_kind(p=15)
real , dimension(4) :: x
integer, dimension(4) :: nums
integer :: i
real(kind=ikind),dimension(4) :: computed
!fill up the arrays with something
do i = 1,4
nums(i) = i * 10
computed(i) = cos(0.1*i)
x(i)= computed(i)
end do
print *,'nums - integer'
write(*,1) nums
1 format(2i10)
print *, 'x - real'
write(*,2) x
2 format(f6.2)
print *, 'computed - double precision'
write(*,3) computed
3 format(f20.7)
end program format
```

可见, `write` and `format` 是成对使用的.

```fortran
write(output device,label) variable(s)
label format(specification)
```

例子中我们使用的是`*`作为输出设备--也就是屏幕.

格式声明可以在程序的任意地方, 但是一般放在相关`write`的下面.

格式指定的细节如下：

#### 整数指定

一般形式：`nim`

+ 右对齐
+ `m`是预留的字符空位(包括符号), 如果实际宽度小于`m`, 则用空格补充
+ `n`是每行输出的整数个数. 如果忽略, 每行一个

#### 浮点数指定

一般形式：`nfm.d`

+ 右对齐
+ `m`是整数部分的空间, 和小数点
+ 如果实际所需小于`m`, 用空格填充
+ `n`是每行数字的个数. 如果忽略, 每行一个.
+ `d`是小数部分预留空间, 用`0`补充
+ 如果`m`设置的太小, fortran 会输出`***`.

规则：$m>=$整数部分+小书部分+1个小数点+一个符号(若是负数)

总之, 得把`m`设置的宽一些.

#### 指数指定

一般形式：`nEm.d`

+ `real` 型输出的可选指定
+ `d`是小数的位数
+ `m`是总宽度, 包括符号, 字符`E`, 和它的符号, 小数点, 和小数的位置.
`m`应该设置的宽一些
+ `n`是每行数字的个数. 如果忽略, 每行一个.

例子：

```fortran
real :: a,b
a = sqrt(5.0)
b = -sqrt(a)
write(*,10) a,b
10 format(2E14.5)
```

produces:

```fortran
0.22361E+01 -0.14953E+01
```

#### Character Specification

一般形式：`nAm`

+ `n` 是要打印的字符串的数目
+ `m` 是打印的总字符数目

例如：

```fortran
program chars
implicit none
character ::a*10,b*10
a='hello'
b='goodbye'
write(*,10) a,b
10 format(2a10)
end program chars
```

### 隐式DO循环输出数组

迄今为止, 我们使用的输入和输出形式如下：

```fortran
integer :: col,row
real :: ra(10,10)
!using do loop
do row = 1,10
do col = 1,10
read *, ra(row,col)
write(*,*) ra(row,col)
end do
end do
```

也可以使用隐式`DO`循环输出, 这样能保持行列结构

```fortran
real :: ra(10,10)
integer :: row,col
!use implied do
do row = 1,10
do col = 1,10
read *,ra(row,col)
end do
end do
do row=1,10
write(*,10) (ra(row,col),col=1,10)
end do
10 format(10f5.1)
```

隐DO表可以嵌套, 如：

```Fortran
PRINT *, ((A(I,J),I=1,3),J=1,3)
```

## 子套路和子函数

### 重用代码--子套路

查看以下代码：

```fortran
program output
implicit none
real,dimension(3) :: a,b,c
character :: answer*1
!initialise arrays
a = 1.5
b = 2.5
c = 3.5
write(*,1) 'a',a
print *, 'type y to continue or any other key to finish'
read *, answer
if (answer /= 'y') stop
write(*,1) 'b',b
print *, 'type y to continue or any other key to finish'
read *, answer
if (answer /= 'y') stop
write(*,1) 'c',c
print *, 'type y to continue or any other key to finish'
read *, answer
if (answer /= 'y') stop
write(*,1) 'a*b*c',a * b * c
1 format(a,3f8.3)
end program output
```

这个程序建立一些数组, 并输出它们, 其中有很多重复的部分, 如果可以重用代码, 将会很棒.
答案就是使用 `subroutines`

```fortran
program output1
implicit none
real,dimension(3) :: a,b,c
!initialise arrays
a = 1.5
b = 2.5
c = 3.5
write(*,1) 'a',a
call prompt()
write(*,1) 'b',b
call prompt()
write(*,1) 'c',c
call prompt()
write(*,1) 'a*b*c',a * b * c
1
format(a,3f8.3)
end program output1
!++++++++++++++++++++++++++++++++++++++++++++++
subroutine prompt()
!prompts for a keypress
implicit none
character answer*1
print *, 'type y to continue or any other key to finish'
read *, answer
if (answer /= 'y') stop
end subroutine prompt
```

查看代码, 每次我们type `call prompt()`, 程序就跳转到 `subroutine prompt()`, 然后执行找到的每一行程序, 直到遇到`end subroutine prompt`,
然后返回主程序, 继续从刚刚离开的地方开始执行.

执行 prompting 的代码都在一个地方. 如果我们想要修改的话, 只需要修改这一处.
这使得程序更容易维护.

### 子套路的参数

我们看到 subroutines 在执行重复代码的时候很有用.

subroutine 可以看成是一个单独的程序, 需要时就调用, 它跟主程序是独立的--它不知道主程序的变量. 主程序也不知道 `subroutines` 中的变量, 所以变量名可以重复.

可以通过参数的方法在主程序和 `subroutines` 之间传递变量.

例：计算两个球体体积差的程序

```fortran
program vols
  !Calculates difference in volume of 2 spheres
  implicit none
  real :: rad1,rad2,vol1,vol2
  character :: response
  do
  print *, 'Please enter the two radii'
  read *, rad1,rad2
  !!++++++++++++
  call volume(rad1,vol1)
  call volume(rad2,vol2)
  !!++++++++++++
  write(*,10) 'The difference in volumes is, ',abs(vol1-vol2)
  10  format(a,2f10.3)
  print *, 'Any more? - hit Y for yes, otherwise hit any key'
  read *, response
  if (response /= 'Y' .and. response /= 'y') stop
  end do
  end program vols
  !---------------------------------
  subroutine volume(rad,vol)
  implicit none
  real :: rad,vol,pi
  !calculates the volume of a sphere
  pi=4.0*atan(1.0)
  vol=4./3.*pi*rad*rad*rad
  !It's a little quicker in processing to do r*r*r than r**3!
end subroutine volume
```

当程序到达`call volume(rad1,vol1)`
它跳转到`subroutine volume(rad,vol)`, `rad1 and vol1`的值被传递给子套路.

子套路将会计算体积, 到达`end subroutine volume`这一行时, `volume`的值被返回给主程序.

注意：

+ 一个程序中可以有好几个子套路, 每个从事一项专门的任务
+ 除了被用作参数的变量, 主程序和子套路中的变量名可以重复
+ 在子套路中容易忘记声明变量, 请总是使用`implicit none`来避免忘记
+ 子套路中的所有变量必须提前声明.
+ 参数的顺序很重要. 子套路不知道变量在主程序中的名字, 所以变量的位置和类型都很重要. 主从程序中的变量类型应该一致.
+ 如果子套路的参数是一个数组, 在子套路中也必须声明为一个数组

### 用户自定义函数

我们已经接触过FORTRAN 的内置函数, 比如： `abs`,`cos`,`sqrt`.
我们也可以自定义函数, 就像子程序那样

```fortran
print *,'Enter a number'
read *, a
pi=4.0*atan(1.0)
print *,'the sine of ',a,' is ',sin(a*pi/180)
```

这个代码片段把角度转换成弧度, 我们也可以利用函数实现这一功能：

```fortran
program func
! demonstrates use of user defined functions
implicit none
integer, parameter :: ikind=selected_real_kind(p=15)
!++需要给出自定义函数的数据类型
real (kind=ikind):: deg,rads
!+++++++++++++++
print *, 'Enter an angle in degrees'
read *, deg
write(*,10) 'sin = ',sin(rads(deg))
write(*,10) 'tan = ',tan(rads(deg))
write(*,10) 'cos = ',cos(rads(deg))
10
format(a,f10.8)
end program func
!---------------------------------
function rads(degrees)
implicit none
integer, parameter :: ikind=selected_real_kind(p=15)
! returns radians
real (kind=ikind) :: pi,degrees,rads
pi=4.0_ikind*atan(1.0_ikind)
rads=(degrees*pi/180.0_ikind)
end function rads
```

自定义的函数`rads`, 可以像` sqrt`, `cos`, and `abs`等内置函数一样使用.

当到达`write(*,10) 'sin = ',sin(rads(deg))`这一行时,
程序跳转到`function rads(degrees)`, `degrees`的值被传递给自定义程序,

程序进行一些计算, 然后将计算的值返回给主程序, 体现在如下一行

```fortran
rads=(degrees*pi/180.0_ikind)
```

注意, 它返回的并不是参数列表中的变量(as does a subroutine), 而是把值赋予自有变量`rads`.

+ 函数`rads`把`degrees`转换成`radians`
+ 我们必须在主程序和 funtion 中, 都声明 function 的 数据类型.
+ 函数返回一个值. 这个值被赋予函数的名称, 好像函数名是个变量一样`rads=(degrees*pi/180.0_ikind)`

## 高级主题

### 数组函数

FORTRAN provides a number of intrinsic functions that are useful for working with arrays. Among
these are some which are specifically aimed at working with matrices and vectors.

FORTRAN 提供了一系列内置函数, 用来处理数组. 其中有一些专门用来处理矩阵和矢量.

| | | |
|---|---|---|
| `MATMUL` | `Matrix/vector` | 矩阵相乘或者矩阵和矢量相乘 |
| `DOT_PRODUCT` | `Vector` | 矢量的标量积 |
| `TRANSPOSE` | `Matrix` | 对矩阵进行转置 |
| `MAXVAL` | `Any array` | 数组的最大值, 或者数组某一纬度的最大值 |
| `MINVAL` | `Any array` | 数组的最小值, 或者数组某一纬度的最小值 |
| `SUM` | `Any array` | 数组的和, 或者数组某一纬度的和 |

程序`matrixmul.f95`, 演示了这些函数的用法. 此外, 它还包括两个有用的子套路, 用来处理矩阵/数组操作：

`fill_array`用来填充数组元素；
`outputra` 把数组的元素打印到屏幕.

这个程序也是一个动态分配内存的例子.

```fortran
program matrixmul
    !demonstrates use of matmul array function and dynamic
    !allocation of array
    real, allocatable, dimension(:,:) :: ra1,ra2,ra3
    integer :: size
    !initialize the arrays
    print*, 'Shows array manipulation using SQUARE arrays.'
    print*, 'Allocate the space for the array at run time.'
    print*, 'Enter the size of your array'
    read *, size
    allocate(ra1(size,size),ra2(size,size),ra3(size,size))
    print*, 'enter matrix elements for ra1 row by row'
    call fill_array(size,ra1)
    print*, 'enter matrix elements for ra2 row by row'
    call fill_array(size,ra2)
    !echo the arrays
    print *,'ra1'
    call outputra(size,ra1)
    print *,'ra2'
    call outputra(size,ra2)
    !demonstrate the use of matmul and transpose intrinsic
    !functions
    ra3=matmul(ra1,ra2)
    print *,'matmul of ra1 and ra2'
    call outputra(size,ra3)
    ra3=transpose(ra1)
    print *,'transpose of ra1'
    call outputra(size,ra3)
    deallocate(ra1,ra2,ra3)
    end program matrixmul
    !---------------------------------------------------------
    subroutine outputra(size,ra)
    implicit none
    !will output a real square array nicely
    integer :: size,row,col
    real,dimension(size,size) :: ra
    character :: reply*1
    do row =1,size
    write(*,10) (ra(row,col),col=1,size)
    10 format(100f10.2)
    !as we don't know how many numbers are to be output, specify
    !more than we need - the rest are ignored
    end do
    print*,'__________________________________________________'
    print*,'Hit a key and press enter to continue'
    read *,reply
    end subroutine outputra
    !---------------------------------------------------------
    subroutine fill_array(size,ra)
    implicit none
    !fills the array by prompting from keyboard
    integer :: row,col,size
    real  :: num
    real, dimension(size,size) :: ra
    do row=1,size
        do col=1,size
        print *, row,col
        read *,num
        ra(row,col)=num
        end do
    end do
    end subroutine fill_array
```

### 流程图

现实中的程序一般会比较庞大, 所以 underlying 的逻辑一般会比较复杂.

所以, 不管是设计程序, 还是维护程序, 使用流程图都很有帮助.

## De‐bugging 建议

***
是否有舍入误差？

不要用整数进行浮点数运算. 确保你的精度是一致且足够的. 确保赋值左右的精度和类型一样.

***

你的计算完全错误了吗？

+ 初始化变量--不要忘了数组！
+ 确保你的数组足够大, 可以装下所有数据
+ 确保传递给子套路参数 大小, 类型和位置 完全匹配
+ 程序的背后逻辑合理吗

### 明智的预防措施和节约时间的建议

不要检验浮点数是否相等. 例如`if (x == 1) then...`是不合理的.

你应该使用绝对值比较, Example:

```Fortran
if
(abs(x-y)<.00001) then
```

不要过度组合逻辑测试. 如：

```Fortran
if (((x.AND.y).OR.z > 10).OR.(.NOT. xx < 0)) then ...
```

你写的时候可能是理解的, 但是如果出问题, 调试将会很困难.

+ 不要试图一次完成一个很复杂的程序. 每次完成一小部分, 然后让每一部分正常工作.
+ 在所有程序和子程序开头使用`implicit none`
+ 如果程序需要用户输入, 可以先用文件输入模拟, 这样可以节约开发时间
+ 在做除法的时候, 总是进行除以`0`的检验
+ 使代码保持整齐, 方便阅读

## 如何寻找bugs

使用一个打印陈述, 在程序内打印出数值, 比如：

```Fortran
x = x + 1
    z = x * y
        print *, 'debug statement 1 value of x,y,z ', x,y,z
    do ii =1,10
    z = x * ii
    if (ii == 5) then
        print *, 'debug do loop value of z when ii = 5 ',z
    end if
    end do
    if (z>2000) then
        print *, 'debug statement – z>2000 value of z ',z
        stop
    end if
```

注意我们如何打印特定变量的值, 如果有必要, `stop`程序.
