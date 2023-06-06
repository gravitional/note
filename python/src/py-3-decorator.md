# python 装饰器

[如何理解Python装饰器? - 刘志军的回答 - 知乎](https://www.zhihu.com/question/26930016/answer/99243411)

## 描述符

[Python进阶——什么是描述符?](https://zhuanlan.zhihu.com/p/336926012)

## @staticmethod, @classmethod

[正确理解Python中的 @staticmethod @classmethod方法](https://zhuanlan.zhihu.com/p/28010894)

## @property

[python @property 的介绍与使用](https://zhuanlan.zhihu.com/p/64487092)

类中 单下划线和双下划线的属性, 在类外部, 均可以使用实例化对象访问并且修改.
区别在于: 双下划线的属性名 `____FileName` 被解释器重写为 `_ClassName__FileName`,
所以访问或者修改它的时候是 `实例名._ClassName__FileName`,
而单下划线的属性 `_FileName`, 依然可以用 `实例名._FileName` 访问或修改

### 修饰方法, 是方法可以像属性一样访问.

```python
class DataSet(object):
  @property
  def method_with_property(self): ##含有@property
      return 15
  def method_without_property(self): ##不含@property
      return 15

l = DataSet()
print(l.method_with_property) # 加了@property后, 可以用调用属性的形式来调用方法,后面不需要加().

print(l.method_without_property())  #没有加@property , 必须使用正常的调用方法的形式, 即在后面加()
```

### 与所定义的属性配合使用, 这样可以防止属性被修改.

由于python进行属性的定义时, 没办法设置私有属性, 因此要通过 `@property` 的方法来进行设置.
这样可以隐藏属性名, 让用户进行使用的时候无法随意修改.

```python
class DataSet(object):
    def __init__(self):
        self._images = 1
        self._labels = 2 #定义属性的名称
    @property
    def images(self): #方法加入@property后, 这个方法相当于一个属性, 这个属性可以让用户进行使用, 而且用户有没办法随意修改.

        return self._images
    @property
    def labels(self):
        return self._labels
l = DataSet()
#用户进行属性调用的时候, 直接调用images即可, 而不用知道属性名_images, 因此用户无法更改属性, 从而保护了类的属性.

print(l.images) # 加了@property后, 可以用调用属性的形式来调用方法,后面不需要加().
```
