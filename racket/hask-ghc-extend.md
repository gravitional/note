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

## InstanceSigs

在使用 `instance` 关键字声明类型类实例时, `Haskell` 默认是不允许我们写出类型签名的,
但是有时写出类型签名对我们有辅助的作用,
GHC 提供了 `InstanceSigs `语言扩展来让我们 给出类型类中函数的签名:

```hs
{-# LANGUAGE InstanceSigs #-}
instance Eq Shape where
    (==) :: Shape -> Shape -> Bool
    Circle r1 == Circle r2 = r1 == r
```

## FlexibleInstances, FlexibleContext

我们这里的 `Shape` 并不带有类型参数,
如果它带有一个类型参数的话我们就可以手工指定其中半径还有边长的类型了:

```hs
data Shape a = Circle a | Square a | Rectangle a a
```

而当我们试着把 `Shape Double` 类型实现为 `Eq` 类型类的实例时就会得到下面的错误:

```hs
instance Eq (Shape Double)
>
    Illegal instance declaration for 'Eq (Shape Double)'
        (All instance types must be of the form (T a1 ... an)
        where a1 ... an are *distinct type variables*,
        and each type variable appears at most once in the instance head.
        Use FlexibleInstances if you want to disable this.)
    In the instance declaration for 'Eq (Shape Double)'
```

这是由于 `Haskell` 标准中规定,
类型类中类型变量在实现时也必须使用类型变量去匹配, 即写成:

```hs
instance Eq (Shape a)
```

但是这样不是总能满足我们在编程实践中的需要,
所以 GHC 对这一点做了扩展, 即使用 `FlexibleInstances` 语言扩展,
这样我们可以对 有着任意类型的 `Shape` 实现 `Eq` 类型类.
更多关于这个语言扩展的内容可以参阅 [87] 的 9.8.3.2.

除了对于类型变量的限定外,
Haskell 标准中还对 类型类实例声明的 **类型上下文** 做出了限定,
例如我们给定 `Shape` 的类型参数为 `(a,a)`, 而我们希望在实现相等类型类实例时使用
`Eq (a,a)` 而非 `Eq a`, 虽然在这种情形上意义并不大, 但是在很多时候还是需要的,
此时就需要 `FlexibleContext` 语言扩展来打破这一限定:

```hs
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
data Shape a = Circle a | Square a | Rectangle a a

instance Eq (Shape Double) -- 只需要FlexibleInstances
instance Eq (a,a) => Eq (Shape (a,a)) -- 同时需要两个扩展
```

更多关于 `FlexibleContext` 的内容可以参阅 [87] 的 9.8.1.2.
