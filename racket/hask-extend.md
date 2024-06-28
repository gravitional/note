# haskell 语言拓展

## MultiParamTypeClasses

```hs
-- 多参数的类型类(multi-parameter typeclass
{-# LANGUAGE MultiParamTypeClasses #-}
class GEq a b where
    equals :: a -> b -> Bool
```

## FunctionalDependencies, 类型依赖

```hs
{-# LANGUAGE MultiParamTypeClasses #-}
class Fun a b where
    fun :: a -> b

instance Fun Int Nat where
    fun a = Zero
instance Fun Int Int where
    fun _ = 0
--  fun (5::Int), 则会引起歧义,返回的类型可以是 Nat, 还可以是Int
-- 需要使用 (fun (5::Int))::Int 明确结果类型
```

``` hs
-- 可以使用语言扩展, 在定义类型类时声明类型参数间的依赖关系
-- 这种类型间的关系称为类型的函数依赖(functional dependency)
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
class Fun a b | a -> b where
    fun :: a -> b

{-# LANGUAGE MultiParamTypeClasses #-}
import GHC.Float
class (Num a, Num b, Num c) => GPlus a b c where
    plus :: a -> b -> c

instance GPlus Int Float Float where
    plus a b = fromIntegral a + b

instance GPlus Int Float Double where
    plus a b = fromIntegral a + float2Double b

instance GPlus Float Float Float where
    plus a b = a + b

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
class (Num a, Num b, Num c) => GPlus a b c | a b -> c where
    plus :: a -> b -> c
```

### FlexibleInstances, 类型类 灵活实例

```hs
-- 比如, 定义一个容器的 类型类, 一个容器类型类中定义了三个基本的操作,
-- 这个容器为空, 插入一个新元素, 判断一个元素是不是在这个容器中.
-- 这里, 需要使用 FlexibleInstances 编译器参数是因为
-- a 与 [a] 之间有依赖关系, 而默认情况下, GHC 不允许这种情况的发生.
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
class Collection e ce where
    empty :: ce
    insert :: e -> ce -> ce
    member :: e -> ce -> Bool

instance Eq a => Collection a [a] where
    empty = []
    insert x xs = (x : xs)
    member = elem
```

### FunctionalDependencies

```hs
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
class Collection e ce | ce -> e where
    empty :: ce
    insert :: e -> ce -> ce
    member :: e -> ce -> Bool

> :t insert 5 (insert 'c' empty)
<interactive>:1:8:
    No instance for (Num Char)
    arising from the literal '5'
    Possible fix: add an instance declaration for (Num Char)
    In the first argument of 'insert', namely '5'
    In the expression: insert 5 (insert 'c' empty)
```

## sdf

```hs
{-# LANGUAGE TypeSynonymInstances #-}

```
