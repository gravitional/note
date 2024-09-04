# haskell weak heand norm form

[What is Weak Head Normal Form?](https://stackoverflow.com/questions/6872898/what-is-weak-head-normal-form)

我试着用简单的语言解释一下.
正如其他人所指出的, `head normal form` 式并不适用于 Haskell, 所以我在这里就不考虑它了.

## Normal form

`normal form` 的表达式是完全求值的,
并且没有任何子表达式可以进一步求值(即不包含未求值的 thunks).

这些表达式都是 normal form:

```hs
42
(2, "hello")
\x -> (x + 1)
```

这些表达式不是 `nornmal form`:

```hs
1 + 2                 -- we could evaluate this to 3
(\x -> x + 1) 2       -- we could apply the function
"he" ++ "llo"         -- we could apply the (++)
(1 + 1, 2 + 2)        -- we could evaluate 1 + 1 and 2 + 2
```

## Weak head normal form

weak head normal form 的表达式已求值到最外层的 data constructor 或 lambda 抽象(the head).
子表达式可能已经求值, 也可能尚未求值.
因此, 每个 normal form 表达式也都是 weak head normal form, 但一般相反不成立.

要确定一个表达式是否为 weak head normal form, 我们只需查看表达式的最外层部分.
如果它是一个 data constructor 或 lambda, 那么它就是 weak head normal form.
如果是 函数应用(function application), 则不是.

这些表达式都是 weak head normal form:

```hs
(1 + 1, 2 + 2)       -- the outermost part is the data constructor (,)
\x -> 2 + 2          -- the outermost part is a lambda abstraction
'h' : ("e" ++ "llo") -- the outermost part is the data constructor (:)
```

如前所述, 上面列出的所有 normal form表达式 也都是 weak head normal form.

这些表达式不属于 weak head normal form:

```hs
1 + 2                -- the outermost part here is an application of (+)
(\x -> x + 1) 2      -- the outermost part is an application of (\x -> x + 1)
"he" ++ "llo"        -- the outermost part is an application of (++)
```

## Stack overflows

Evaluating an expression to weak head normal form may require that other expressions be evaluated to WHNF first. For example, to evaluate 1 + (2 + 3) to WHNF, we first have to evaluate 2 + 3. If evaluating a single expression leads to too many of these nested evaluations, the result is a stack overflow.

This happens when you build up a large expression that does not produce any data constructors or lambdas until a large part of it has been evaluated. These are often caused by this kind of usage of foldl:

```hs
foldl (+) 0 [1, 2, 3, 4, 5, 6]
 = foldl (+) (0 + 1) [2, 3, 4, 5, 6]
 = foldl (+) ((0 + 1) + 2) [3, 4, 5, 6]
 = foldl (+) (((0 + 1) + 2) + 3) [4, 5, 6]
 = foldl (+) ((((0 + 1) + 2) + 3) + 4) [5, 6]
 = foldl (+) (((((0 + 1) + 2) + 3) + 4) + 5) [6]
 = foldl (+) ((((((0 + 1) + 2) + 3) + 4) + 5) + 6) []
 = (((((0 + 1) + 2) + 3) + 4) + 5) + 6
 = ((((1 + 2) + 3) + 4) + 5) + 6
 = (((3 + 3) + 4) + 5) + 6
 = ((6 + 4) + 5) + 6
 = (10 + 5) + 6
 = 15 + 6
 = 21
```

Notice how it has to go quite deep before it can get the expression into weak head normal form.

You may wonder, why does not Haskell reduce the inner expressions ahead of time? That is because of Haskell's laziness. Since it cannot be assumed in general that every subexpression will be needed, expressions are evaluated from the outside in.

(GHC has a strictness analyzer that will detect some situations where a subexpression is always needed and it can then evaluate it ahead of time. This is only an optimization, however, and you should not rely on it to save you from overflows).

This kind of expression, on the other hand, is completely safe:

```hs
data List a = Cons a (List a) | Nil
foldr Cons Nil [1, 2, 3, 4, 5, 6]
 = Cons 1 (foldr Cons Nil [2, 3, 4, 5, 6])  -- Cons is a constructor, stop.
```

To avoid building these large expressions when we know all the subexpressions will have to be evaluated, we want to force the inner parts to be evaluated ahead of time.

seq
seq is a special function that is used to force expressions to be evaluated. Its semantics are that seq x y means that whenever y is evaluated to weak head normal form, x is also evaluated to weak head normal form.

It is among other places used in the definition of foldl', the strict variant of foldl.

```hs
foldl' f a []     = a
foldl' f a (x:xs) = let a' = f a x in a' `seq` foldl' f a' xs
```

Each iteration of foldl' forces the accumulator to WHNF. It therefore avoids building up a large expression, and it therefore avoids overflowing the stack.

```hs
foldl' (+) 0 [1, 2, 3, 4, 5, 6]
 = foldl' (+) 1 [2, 3, 4, 5, 6]
 = foldl' (+) 3 [3, 4, 5, 6]
 = foldl' (+) 6 [4, 5, 6]
 = foldl' (+) 10 [5, 6]
 = foldl' (+) 15 [6]
 = foldl' (+) 21 []
 = 21                           -- 21 is a data constructor, stop.
```

But as the example on HaskellWiki mentions, this does not save you in all cases, as the accumulator is only evaluated to WHNF. In the example below, the accumulator is a tuple, so it will only force evaluation of the tuple constructor, and not acc or len.

```hs
f (acc, len) x = (acc + x, len + 1)

foldl' f (0, 0) [1, 2, 3]
 = foldl' f (0 + 1, 0 + 1) [2, 3]
 = foldl' f ((0 + 1) + 2, (0 + 1) + 1) [3]
 = foldl' f (((0 + 1) + 2) + 3, ((0 + 1) + 1) + 1) []
 = (((0 + 1) + 2) + 3, ((0 + 1) + 1) + 1)  -- tuple constructor, stop.
```

To avoid this, we must make it so that evaluating the tuple constructor forces evaluation of acc and len. We do this by using seq.

```hs
f' (acc, len) x = let acc' = acc + x
                      len' = len + 1
                  in  acc' `seq` len' `seq` (acc', len')

foldl' f' (0, 0) [1, 2, 3]
 = foldl' f' (1, 1) [2, 3]
 = foldl' f' (3, 2) [3]
 = foldl' f' (6, 3) []
 = (6, 3)                    -- tuple constructor, stop.
```

## [Weak head normal form](https://wiki.haskell.org/Weak_head_normal_form)

如果一个表达式是以下两种情况之一, 那么它就是 `弱⾸范式`(WHNF):

+ `constructor`(最终应用于参数), 如 `True`, `Just (square 42)` 或 `(:) 1`.
+ 内置函数(应用于数量不足的参数), 如 `(+) 2` 或 `sqrt`.
+ 或 lambda 抽象 `x -> expression`.

需要注意的是, 处于 弱⾸范式 中 的 表达式, 本身并不一定要经过完全求值;
因此, 虽然 `square 42` 可以 化简成 `42 * 42`
(它本身也可以 化简为 `1764` 的 范式, normal form),
但 `Just (square 42)` 不需要进一步求值就是 `WHNF`.
同样, `(+) (2 * 3 * 4)` 也是 `WHNF`, 尽管 2 * 3 * 4 可以还原成 normal form 24.

一个例外情况是, a fully applied constructor for a data
with some fields declared as strict
那么这些字段的参数也需要是 `WHNF`.

上述定义似乎将内置函数与通过 lambda 抽象定义的函数区别对待.
然而, 这种区别在语义上并不重要.
如果 lambda 抽象被应用于 "参数太少 "的情况,
那么对应用进行计算只是意味着用参数来替代 lambda 抽象的某些变量,
其结果总是一个 now-unapplied lambda 抽象.
