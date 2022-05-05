# 含有可变参数的函数

    C++标准中提供了两种主要的方法

        如果所有的实参类型相同, 可以传递一个名为initializer_list的标准库类型;

        如果实参的类型不同, 我们可以编写可变参数的模板(第9章).

    initializer_list

        initializer_list是一种标准库类型, 用于表示某种特定类型的值的数组, 该类型定义在同名的头文件中

initializer_list提供的操作

![initializer_list]()

initializer_list的使用方法

    initializer_list是一个类模板(第9章详细介绍模板)

    使用模板时, 我们需要在模板名字后面跟一对尖括号, 括号内给出类型参数. 例如:

        initializer_list<string>  ls;  // initializer_list的元素类型是string

        initializer_list<int> li;      // initializer_list的元素类型是int

    initializer_list比较特殊的一点是, 其对象中的元素永远是常量值, 我们无法改变initializer_list对象中元素的值.

    含有initializer_list形参的函数也可以同时拥有其他形参

initializer_list使用举例

    在编写代码输出程序产生的错误信息时, 最好统一用一个函数实现该功能, 使得对所有错误的处理能够整齐划一.
    然而错误信息的种类不同, 调用错误信息输出函数时传递的参数也会各不相同.

    使用initializer_list编写一个错误信息输出函数, 使其可以作用于可变数量的形参.
