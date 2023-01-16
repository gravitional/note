#include <iostream>
// #include "stdafx.h"
using namespace std;

class Base
{
public:
	Base() {};
	// 如果去掉virtual, 运行将会失败
	virtual void Show() { cout << "This is Base calss"; }
};
class Derived : public Base
{
public:
	Derived() {};
	void Show() { cout << "This is Derived class"; }
};

int main()
{
	cout << endl << "up casting of pointer ==================" << endl;
	// 这是第一种情况
	Base* base = new Derived;
	if (Derived* der = dynamic_cast<Derived*>(base))
	{
		cout << "第一种情况转换成功" << endl;
		der->Show();
		cout << endl;
	}
	// 这是第二种情况
	Base* base1 = new Base;
	if (Derived* der1 = dynamic_cast<Derived*>(base1))
	{
		cout << "第二种情况转换成功" << endl;
		der1->Show();
	}
	else
	{
		cout << "第二种情况转换失败" << endl;
	}

	delete (base);
	delete (base1);

	cout << endl << "up casting of reference ==================" << endl;
	//第一种情况, 转换成功
	Derived b;
	Base& base2 = b;
	Derived& der2 = dynamic_cast<Derived&>(base2);
	cout << "第一种情况: ";
	der2.Show();
	cout << endl;

	//第二种情况
	Base a;
	Base& base3 = a;
	cout << "第二种情况: ";
	try {
		Derived& der3 = dynamic_cast<Derived&>(base3);
	}
	catch (bad_cast)
	{
		cout << "转化失败,抛出bad_cast异常" << endl;
	}

	system("pause");
}