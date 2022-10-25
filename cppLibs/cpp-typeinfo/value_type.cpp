#include <iostream>
#include "my_type_info.h"

struct Person
{
	int age;
	static int weight;
};

struct Person2
{
	int age;
	static int weight;
	std::string& name;
};

int& f()
{
	int* a = new int{ 10 };
	return *a;
}

void g()
{
}

int&& h()
{
	return 10;
}

int main()
{
	std::cout << "\n************************************** lrvalue" << std::endl;
	std::cout << std::boolalpha;
	// 1. 普通变量
	std::cout << "======================" << std::endl;
	int a = 10;
	std::cout << "a is lvalue : " << is_lvalue<decltype((a))> << std::endl;
	// a is lvalue : true
	std::cout << "======================" << std::endl;

	// 2. 函数
	std::cout << "main() is lvalue : " << is_lvalue<decltype((main))> << std::endl;
	// main() is lvalue : true
	std::cout << "======================" << std::endl;

	// 3. 成员变量
	Person p;
	std::cout << "p.age is lvalue : " << is_lvalue<decltype((p.age))> << std::endl;
	std::cout << "Person::weight is lvalue : " << is_lvalue<decltype((Person::weight))> << std::endl;
	// p.age is lvalue : true
	// Person::weight is lvalue : true
	std::cout << "======================" << std::endl;

	// 4. 字符类型的字面量
	std::cout << R"("hello world" is lvalue : )" << is_lvalue<decltype(("hello world"))> << std::endl;
	std::cout << R"(The expr type of "hello world" : )" << type_to_string<decltype("hello world")>() << std::endl;
	// "hello world" is lvalue : true
	// The expr type of "hello world" : const char (&)[12]
	std::cout << "======================" << std::endl;

	// 5. 左值引用类型的函数返回值
	std::cout << "f()->int& is lvalue : " << is_lvalue<decltype((f()))> << std::endl;
	// f()->int& is lvalue : true
	std::cout << "======================" << std::endl;

	// 6. 函数不管怎么变都是左值
	std::cout << R"(The expr type of g : )" << type_to_string<decltype(g)>() << std::endl;
	std::cout << "====================== void(&g1)() = g" << std::endl;
	void(&g1)() = g;
	std::cout << R"(The expr type of g1 : )" << type_to_string<decltype(g1)>() << std::endl;
	std::cout << R"(&g is lvalue : )" << is_lvalue<decltype((g1))> << std::endl;
	// The expr type of g1 : void (&)()
	// &g is lvalue : true

	std::cout << "====================== void(&&g2)() = g" << std::endl;
	void(&& g2)() = g;
	std::cout << R"(The expr type of g2 : )" << type_to_string<decltype(g2)>() << std::endl;
	std::cout << R"(&&g is lvalue : )" << is_lvalue<decltype((g2))> << std::endl;
	// The expr type of g2 : void (&&)()
	// &&g is lvalue : true

	// 函数 std::move 后变成右值
	std::cout << "============================ std::move(g)" << std::endl;
	std::cout << R"(The expr type of std::move(g) : )" << type_to_string<decltype(std::move(g))>() << std::endl;
	std::cout << R"(std::move(g) is rvalue : )" << is_rvalue<decltype((std::move(g)))> << std::endl;
	// The expr type of std::move(g) : void(__cdecl&&)(void)
	// std::move(g) is rvalue : true

	std::cout << "\n************************************** prvalue" << std::endl;
	std::cout << std::boolalpha;
	// 1. 除字符串字面量以外的其它字面量
	std::cout << "10 is prvalue : " << is_prvalue<decltype((10))> << std::endl;
	std::cout << "true is prvalue : " << is_prvalue<decltype((true))> << std::endl;
	std::cout << "nullptr is prvalue : " << is_prvalue<decltype((nullptr))> << std::endl;
	// 10 is prvalue : true
	// true is prvalue : true
	// nullptr is prvalue : true

	// 2. 函数返回值
	std::cout << "main()->int is prvalue : " << is_prvalue<decltype((main()))> << std::endl;
	// main()->int, return type is prvalue : true

	// 3. 临时对象
	std::cout << "Person{} is prvalue : " << is_prvalue<decltype((Person{})) > << std::endl;

	// 4. 取地址符
	std::cout << "&main is prvalue : " << is_prvalue<decltype((&main))> << std::endl;

	// 5. lambda表达式
	std::cout << "lambda, [](){} is prvalue : " << is_prvalue<decltype(([]() {})) > << std::endl;

	std::cout << "\n************************************** xrvalue" << std::endl;
	std::cout << std::boolalpha;
	// 1. std::move
	// int a = 10;
	std::cout << "std::move(10) is xvalue : " << is_xvalue<decltype((std::move(10)))> << std::endl;
	std::cout << "std::move(a) is xvalue : " << is_xvalue<decltype((std::move(a)))> << std::endl;
	// std::move(10) is xvalue : true
	// std::move(a) is xvalue : true
	// 2. static_cast<T&&>
	std::cout << "static_cast<T&&>(10) is xvalue : " << is_xvalue<decltype((static_cast<int&&>(10)))> << std::endl;
	std::cout << "static_cast<T&&>(a) is xvalue : " << is_xvalue<decltype((static_cast<int&&>(a)))> << std::endl;

	// 3. 函数返回值为T&&类型
	std::cout << "h()->int&& is xvalue : " << is_xvalue<decltype((h()))> << std::endl;
	// h()->int&& is xvalue : true

	// 4. rvalue 对象的非静态成员
	// Person p;
	std::cout << "Person{}.age is xvalue : " << is_xvalue<decltype((Person{}.age)) > << std::endl;
	std::cout << "std::move(p).age is xvalue : " << is_xvalue<decltype((std::move(p).age))> << std::endl;
	// Person{}.age is xvalue : true
	// std::move(p).age is xvalue : true

	std::cout << "\n************************************** move special case" << std::endl;
	// weight和name都不是Person2 类的一部分, Person2 类对于这两个成员变量没有所有权
	std::cout << std::boolalpha;
	std::string name = "xiaoming";
	Person2 p2{ 10, name };
	std::cout << "===============" << std::endl;
	value_type(std::move(p2).age); // 结果是 lvalue
	std::cout << "===============" << std::endl;
	value_type(std::move(p2).weight); // 结果是 lvalue
	std::cout << "===============" << std::endl;
	value_type(std::move(p2).name);
}