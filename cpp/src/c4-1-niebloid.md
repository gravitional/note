# c++ niebloid

The term niebloid comes from Eric Niebler's name.
In simple words, they are `function objects` that disable `ADL` (argument-dependent lookup) from happening so that
the overloads in `std::` aren't picked up
when an algorithm from `std::ranges` is called.`

Here's a tweet (from 2018) and answer from Eric himself suggesting the name.
Eric wrote an article in 2014 explaining this concept.
It can best be seen in action in the standard document itself:

25.2.2
The entities defined in the `std​::​ranges` namespace in this Clause are not found by `argument-dependent name lookup`(basic.lookup.argdep).
When found by `unqualified name lookup`(basic.lookup.unqual)  for the postfix-expression in a function call,
they inhibit argument-dependent name lookup.

```cpp
void foo() {
  using namespace std::ranges;
  std::vector<int> vec{1,2,3};
  find(begin(vec), end(vec), 2);        // #1
}
```

The function call expression at #1 invokes std​::​ranges​::​find,
not std​::​find, despite that
(a) the iterator type returned from begin(vec) and end(vec) may be associated with namespace std and
(b) `std​::​find` is more specialized ([temp.func.order]) than std​::​ranges​::​find since the former requires its first two parameters to have the same type.

The above example has ADL turned off,
so the call goes directly to `std::ranges::find`.

Let's create a small example to explore this further:

```cpp
namespace mystd
{
    class B{};
    class A{};
    template<typename T>
    void swap(T &a, T &b)
    {
        std::cout << "mystd::swap\n";
    }
}

namespace sx
{
    namespace impl {
       //our functor, the niebloid
        struct __swap {
            template<typename R, typename = std::enable_if_t< std::is_same<R, mystd::A>::value >  >
            void operator()(R &a, R &b) const
            {
                std::cout << "in sx::swap()\n";
                // swap(a, b);
            }
        };
    }
    inline constexpr impl::__swap swap{};
}

int main()
{
    mystd::B a, b;
    swap(a, b); // calls mystd::swap()

    using namespace sx;
    mystd::A c, d;
    swap(c, d); //No ADL!, calls sx::swap!

    return 0;
}
```

Description from cppreference:

The function-like entities described on this page are niebloids, that is:

Explicit template argument lists may not be specified when calling any of them.
None of them is visible to argument-dependent lookup.
When one of them is found by normal unqualified lookup for the name to the left of the function-call operator, it inhibits argument-dependent lookup.
Niebloids aren't visible to argument dependent lookup(ADL)
because they are function objects, and ADL is done only for free functions and not function objects.
The third point is what happened in the example from the standard:

```cpp
find(begin(vec), end(vec), 2); //unqualified call to find
```

The call to `find()` is unqualified,
so when lookup starts, it finds `std::ranges::find` function object, which in turn stops ADL from happening.

Searching some more, I found [this](https://quuxplusone.github.io/blog/2019/08/02/the-tough-guide-to-cpp-acronyms/#cpo) which,
in my opinion is the most understandable explanation of niebloids and CPOs (customization point objects):

... a CPO is an object (not a function); it's callable;
it's constexpr-constructible, [...] it's customizable
(that's what it means to "interact with program-defined types"); and it's concept-constrained.

[...]

If you remove the adjectives "customizable, concept-constrained"
from the above, then you have a function object that turns off ADL — but is not necessarily a customization point.
The C++2a Ranges algorithms, such as std::ranges::find, are like this.
Any callable, constexpr-constructible object is colloquially known as a "niebloid," in honor of Eric Niebler.
