# override与final

C++11: override 与 final

override

    多态行为的基础: 基类声明虚函数, 继承类声明一个函数覆盖该虚函数

    覆盖要求:  函数签名(signatture)完全一致

    函数签名包括: 函数名 参数列表 const

下列程序就仅仅因为疏忽漏写了const, 导致多态行为没有如期进行

8-2.png
显式函数覆盖

-　C++11 引入显式函数覆盖, 在编译期而非运行期捕获此类错误.  - 在虚函数显式重载中运用, 编译器会检查基类是否存在一虚拟函数, 与派生类中带有声明override的虚拟函数, 有相同的函数签名(signature);若不存在, 则会回报错误.
　final

    C++11提供的final, 用来避免类被继承, 或是基类的函数被改写 例:  struct Base1 final { };

    struct Derived1 : Base1 { }; // 编译错误: Base1为final, 不允许被继承

    struct Base2 { virtual void f() final; };

    struct Derived2 : Base2 { void f(); // 编译错误: Base2::f 为final, 不允许被覆盖 };