// #include "stdafx.h"
#include <iostream>
using namespace std;

class Base
{
public:
	Base() {};
	virtual void Show() { cout << "This is Base calss"; }
};
class Derived : public Base
{
public:
	Derived() {};
	void Show() { cout << "This is Derived class" << endl; }
};
int main()
{
	cout << endl << "up casting of pointer ==================" << endl;
	Base* base;
	Derived* der = new Derived;
	// base = dynamic_cast<Base*>(der); //正确, 但不必要.
	base = der; // 向上转换总是安全的
	base->Show();

	// 引用的情形
	cout << endl << "up casting of reference ==================" << endl;
	Derived c;
	Derived& der2 = c;
	Base& base2 = dynamic_cast<Base&>(der2);//向上转换, 安全
	base2.Show();

	system("pause");
}

