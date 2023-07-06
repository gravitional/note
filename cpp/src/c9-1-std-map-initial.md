# map 初始化

[C++(17):map的初始化](https://blog.csdn.net/Leo_csdn_/article/details/86589904)

关于C++中关联容器map的初始化/赋值有好几种方式

## 直接赋值法

```cpp
map<string, int> m1;
m1["def"] = 2;
```

## 用insert添加

```cpp
map<string, int> m2;
m2.insert({ "abc", 1 });    //使用这种就可以了

//其他形式和方式
m2.insert(make_pair(string("def"), 2));
m2.insert(pair<string, int>(string("ghi"), 3));
```

## 列表初始化

```cpp
map<string,int> m3 = {
    {"string",1}, {"sec",2}, {"trd",3}
};

map<string,string> m4 = {
    {"first","second"}, {"third","fourth"},
    {"fifth","sixth"}, {"begin","end"}
};
```

注: 列表初始化适用于c++11和以上的版本, 低于11的版本则无法使用

如果需要插入一个key并且不指定value, 可以直接用下面这种写法,
看起来不是一个表达式, 不过map对"[]"进行了重载, 本质上仍然是表达式,
其含义是如果存在该变量则直接返回对应value, 如果不存在则增加该key值并自动初始化为0

```cpp
map<string,int> a;    //a是string到int的空map
a["new"];        //"new"是新增的key
```

不过需要注意的是, 如果value类型为数型,
即使像上面一样没有进行赋值操作, 也一般会默认赋值为——0,

如果 `value` 是 `string` 或 `char` 类型, 则默认为空, 没有默认值,
因此 `string` 或 `char` 可以用列表初始化进行不给 `value` 赋值的添加 `key`

```cpp
string<string,string> m5 = {
    {"first",""}, {"second",""}
    {"third",""}, {"fourth",""}
};
```

## 多类型嵌套的map赋值

map的定义支持多层不同类型的嵌套, 形如:

```cpp
map<int, map<string, vector<double>>>  testmap;
这种多层嵌套的map, 也是可以直接进行赋值操作的, 如:

map<int, map<string, vector<double>>>  testmap;
testmap[3]["three"].push_back(3.75);
```
