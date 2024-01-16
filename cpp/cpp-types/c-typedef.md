# c language typedef

[Why should we typedef a struct so often in C?](https://stackoverflow.com/questions/252780/why-should-we-typedef-a-struct-so-often-in-c)

类型定义意味着你不必再到处写 `struct`.
这不仅可以节省击键次数, 还可以使代码更简洁, 因为它提供了更多的抽象.
比如

```c
typedef struct {
  int x, y;
} Point;

Point point_new(int x, int y)
{
  Point a;
  a.x = x;
  a.y = y;
  return a;
}
```

当你不需要在所有地方都看到 `struct` 关键字时,
代码就会变得更简洁, 看起来更像是在你的语言中真的有一个叫做 `Point` 的类型.
我想, 在类型定义之后, 情况就是这样.

还要注意的是, 虽然你的示例(和我的示例)省略了对结构体本身的命名,
但实际上命名结构体对于提供不透明类型也是很有用的.
例如, 你可以在头文件中使用这样的代码:

```c
typedef struct Point Point;

Point * point_new(int x, int y);
```

然后在实现文件中提供结构体定义:

```c
struct Point
{
  int x, y;
};

Point * point_new(int x, int y)
{
  Point *p;
  if((p = malloc(sizeof *p)) != NULL)
  {
    p->x = x;
    p->y = y;
  }
  return p;
}
```

在后一种情况下, 你不能返回 Point by value,
因为它的定义对头文件的用户是隐藏的.
例如, [GTK+](http://www.gtk.org/) 中就广泛使用了这种技术.

>更新 请注意, 在一些备受推崇的 C 项目中,
使用 typedef 隐藏 struct 被认为是一个坏主意,
Linux 内核可能是最著名的此类项目.
有关 Linus 的愤怒言论, 请参阅 [Linux内核编码风格文档第5章](https://www.kernel.org/doc/html/latest/process/coding-style.html#typedefs).
我想说的是, 问题中的 `should` 也许并不是一成不变的.
