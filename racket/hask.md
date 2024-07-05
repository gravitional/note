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

    join mma = mma >>= id -- 从 mma 中取出 ma, 应用 id 函数, 得到 mb == ma
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
