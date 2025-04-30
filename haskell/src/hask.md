# hask.md

[Control-Monad.html](https://hackage.haskell.org/package/base-4.16.3.0/docs/Control-Monad.html#t:Monad)
[wiki.haskell.org/Monad](https://wiki.haskell.org/Monad)

`Monad` and `Applicative` 的联系如下:

```hs
pure = return
m1 <*> m2 = m1 >>= (x1 -> m2 >>= (x2 -> return (x1 x2)))

--- The above laws imply:
fmap f xs  =  xs >>= return . f
(>>) = (*>)
```

也可以表述成

```hs
mfab <*> ma  =  do {
    fab <- mfab
    a <- ma
    return (fab a)
}
--  mfab <*> ma = mfab >>= (\ fab -> ma >>= (return . fab))
--  mfab <*> ma = mfab `ap` ma

ap :: Monad m => m (a -> b) -> m a -> m b
ap mab ma = do
    f <- mab
    a <- ma
    return $ f a
```

## Monad 的定义, by `join`

有了前面的铺垫, 我们就可以定义 `Monad` 类型类了.
数学上定义的 Monad 并不是采用的 `return` 与 `>>=` 定义,
而是通过 `fmap`, `return` 与 `join`, 它们是等价的两组定义,
因为借助 `fmap` 与 `id` 函数 `join` 与 `>>=` 可以互相定义:

```hs
class Applicative m => Monad m where
    return :: a -> m a
    join :: m (m a) -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    return = pure

    join mma = mma >>= id -- 从 mma 中取出 ma, 应用 id::ma->mb 函数, 得到 mb == ma
    (>>=) ma mf = join $ fmap mf ma -- 将 m 中的函数 f 应用到 ma, 得到 mma, 再使用 join 脱去一层
    (>>) ma mb = ma >>= \_ -> mb
```

这样我们知道了 `Pointed`, 函子 `Functor`, `可应用函子` 与 `Monad` 的完整关系了:
`(Pointed f,Functor f) => Applicative f => Monad f`.

从 `Monad` 类型类的 `join` 类型签名我们可以看
到这个函数的类型是 `m (m a) -> m a`, 也就是将构造器脱去一层.
如果 Haskell 中的 Monad 类型类是这样定义的,
那么声明 Monad 类型类的实例时只需要定义 `join` 或者 `>>=` 其中之一就可以了.

数学上, join 函数满足下面的定律:

定律 10.7.1. `join ◦ return = id = join ◦ fmap return`
定律 10.7.2. `join ◦ join = join ◦ fmap join`

## haskell do notation

[Haskell/do_notation](https://en.wikibooks.org/wiki/Haskell/do_notation)

```hs
do { x1 <- action1
   ; x2 <- action2
   ; mk_action3 x1 x2 }

-- 等价于如下形式; parentheses 可以去掉
action1 >>= (\x1 -> action2 >>= (\x2 -> mk_action3 x1 x2 ))

-- 也可以写成 缩进形式;
action1 >>=
    \x1 -> action2 >>=
            \x2 -> mk_action3 x1 x2
```

haskell 支持返回函数 和 偏函数.
因此 `\x2 -> mk_action3 x1 x2` 中的 依赖 `x1` 被抛出到上层,
即 `\x1 -> action2 >>= \x2 -> mk_action3 x1 x2` 中的 `\x1`,
`\x1` 被抛出到 `action1 >>= ...`, 即从容器 `action1` 中取出.

也可以正过来看, bind operator `(>>=)` 的第二个参数 是 lambda 函数,
lambda 指明了如何使用 `action1` 的结果, 而 `action1` 是 `(>=)` 的第一个.
因此, chains of lambdas pass the results downstream.

简单地说, 等价形式如下

```hs
x1 <- action1 ...
-- 等价于
action1 >>= \x1 -> ...
```

## foldl, foldr,

```hs
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] = v
foldr f v (x:xs) = f x (foldr f v xs)

-- 例如
foldr (+) 0 [1,2,3]
= 1 + (foldr (+) 0 [2,3])
= 1 + (2 + (foldr (+) 0 [3]))
= 1 + (2 + (3 + (foldr (+) 0 [])))
= 1 + (2 + (3 + (0)))
= 6
```

有一种直观的写法理解 foldr, foldl 如下,

```hs
<1, <2, <3, <4, <5, <...>>>>>>
<1, <2, <3, <4, <5, 0>>>>>


<<<<<0, 1>, 2>, 3>, 4>, ...>
<<<<<0, 1>, 2>, 3>, 4>, 5>
```

这样理解的好处是, 

+ 括号在**右边堆叠**就是 `foldr`, 括号在**左边堆叠**就是 `foldl`,
+ 其中 `<x,y>` 表示某种塌缩函数 f, 即

```hs
<>: a->a->a
<x,y> = f x y
```

+ 对于给定的列表 `[1,2,3,4,5]`, `foldr` 从右边塌缩, `foldl` 从左边塌缩,
其中 `0` 是 **初始值** or **启动值**, 
对于 `foldr`, 放到最右边; 对于 `foldl`, 放到最左边

### 不动点函数 fix, Y 组合子

```hs
fix :: (a -> a) -> a 
fix rec = rec (fix rec)
-- 输入一个函数 rec, 计算 rec 的不动点;
-- 这里的 a 可以是例如 Int, 也可以是 Int->Int, 也可以是高阶函数

fix f
= f (fix f)
= f (f (fix f))
= f (f (f (fix f)))
```

因此 不动点函数 fix 是 foldr.

## id, const, slot 重抛出

```hs
-- 恒等函数
id :: a -> a
id x = x

-- 给定两个元素, 只返回第一个
const :: a -> b -> a
```

那么 `const id` 的类型为

```hs
const id

a -> b -> a @ c -> c
b -> (c -> c)
-- 因为 -> 是 右结合的, foldr
b -> c -> c
```
其中用 `@` 表示函数复合, 显然 `const id` 的效果是给出

## 应用和声明的结合性

函数应用是左结合, 即 `f g h` 等于 `(f g) h`;
而类型签名中的 `->` 为右结合, 即 `a -> b -> c` 等于 `a -> (b -> c)`,

+ 这是因为 haskell 中的函数是 curry 化的,
对于`f a b c`, `f` 先应用到 `a`, 因此 `f g h` 等于 `(f g) h`;
+ 但是 使用 和 定义 的顺序相反; 使用是从 f 开始, 从左到右的, `foldl`
声明的时候, 是从右到左的, foldr,
如下所示, 圆括号表示 Apply 的顺序, 方括号表示 定义的顺序,
相反的结合性保证了, 
`A g` 抵消, `B h` 抵消, `C i` 抵消 ...,
这和 Haskell 的约定有关, haskell 约定输入槽(slot) 是从左边开始的, 
或者说从左边入栈

```hs
(((f g) h) i) j -- eq1; Apply
A -> [B -> [C -> [D -> E]]] -- eq2; Declaration

-- eq2 代入 eq1, 得到
(((A -> [B -> [C -> [D -> E]]]  g)  h)  i)  j
((([B -> [C -> [D -> E]]])  h)  i)  j

((B -> [C -> [D -> E]]  h)  i)  j
(([C -> [D -> E]])  i)  j

(C -> [D -> E]  i)  j
([D -> E])  j

D -> E  j
```

