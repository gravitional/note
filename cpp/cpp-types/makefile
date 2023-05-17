tar1=exa1        # 目标文件
tar2=exa2
tar3=integer
tar4=fixed-integer
tar5=integer-prefix
tar6=float
tar7=char

src=$(wildcard *.cpp)   # 用wildcard函数找到所有.c文件, server.c、client.c、wrap.c
deps=$(wildcard *.h)   # 用wildcard函数找到所有的.h文件, wrap.h
obj=$(patsubst %.cpp, %.o, $(src))    #用 patsubst 函数将所有 .cpp 文件替换成 .o 文件

# 目标文件server和client, 多个目标文件一定形成此依赖关系
all:$(tar1) $(tar2) $(tar3)  $(tar4) $(tar5) $(tar6)  $(tar7)  

# exa1 的依赖为 types-exa1.o, 可以有多个, 空格分开
# 利用gcc编译器生成可执行文件 exa1
$(tar1):types-exa1.o  
	g++ $^ -o $@ -Wall   

$(tar2):types-exa2.o
	g++ $^ -o $@ -Wall 

$(tar3):types-integer.o
	g++ $^ -o $@ -Wall 

$(tar4):types-fixed-integer.o
	g++ $^ -o $@ -Wall 

$(tar5):types-integer-prefix.o
	g++ $^ -o $@ -Wall 

$(tar6):types-float.o
	g++ $^ -o $@ -Wall     

$(tar7):types-char.o
	g++ $^ -o $@ -Wall     

#任意一个.o中间目标文件的依赖是其对应的.c文件, 如: client.o 的依赖为 client.c
# 根据目标文件编译的需求依次将依赖编译成对应的中间目标文件
%.o: %.c $(deps)  
	g++ -c $< -o $@ -Wall   

.PHONY:clean  #伪文件, 需要在终端输入make clean才会调用

# 删除所有的目标文件以及中间目标文件, 用于重新编译。
clean:
	-rm -rf $(tar1) $(tar2) $(obj)   
