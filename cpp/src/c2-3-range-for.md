# c++ range for

[Is there a compact equivalent to Python range() in C++/STL](https://stackoverflow.com/questions/13152252/is-there-a-compact-equivalent-to-python-range-in-c-stl)

In C++11, there's std::iota:

```cpp
#include <vector>
#include <numeric> //std::iota

int main() {
    std::vector<int> x(10);
    std::iota(std::begin(x), std::end(x), 0); //0 is the starting number
}
```

C++20 introduced a lazy version (just like Python) as part of the ranges library:

```cpp
#include <iostream>
#include <ranges>

namespace views = std::views;

int main() {
    for (int x : views::iota(0, 10)) {
        std::cout << x << ' '; // 0 1 2 3 4 5 6 7 8 9
    }
}
```

## range lib

[What does iota of std::iota stand for?](https://stackoverflow.com/questions/9244879/what-does-iota-of-stdiota-stand-for)

I've been using this library for this exact purpose for years:
[cpp11-range](https://github.com/klmr/cpp11-range)

Works very well and the proxies are optimized out.

```cpp
for (auto i : range(1, 5))
    cout << i << "\n";

for (auto u : range(0u))
    if (u == 3u)
        break;
    else
        cout << u << "\n";

for (auto c : range('a', 'd'))
    cout << c << "\n";

for (auto i : range(100).step(-3))
    if (i < 90)
        break;
    else
        cout << i << "\n";

for (auto i : indices({"foo", "bar"}))
    cout << i << '\n';
```
