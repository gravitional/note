#set text(font: "Noto Sans CJK SC")

= typst 中的数组

数组的元素将按 原样显示,

- 数字: InputForm 和 OutPutForm 相同
- 字符串: InputForm 和 OutPutForm 相同
- 方程等其它对象: 显示 raw 格式

== Array of digits

`#let values = (1, 7, 4, 5, 9)`
#let values = (1, 7, 4, 5, 9)

- 直接显示数字\
  #values\
  但 array of digits 不能使用 `.join()`\
  因为数字若“合并”，会变成一个数字 (1,2,3,4,5) => 12345

- 也可以用 for loop 遍历\
  #for i in values {
    "("
    text[#i]
    ")  "
  }\

- 也可以用 map 遍历\
  #values.map(it => "<"+text[#it] + "> ").join()\

== Array of string

`#let values = ("1", "7", "4", "-5", "9")`
#let values = ("1", "7", "4", "-5", "9")

- 直接显示字符串\
  #values

- 使用 join 格式化\
  #values.join()\
  #values.join(",")\
  #values.join("__")\


== array of equations

`#let values = ($a$, $b_1$, $arrow.double$, $subset$, $integral$)`
#let values = ($a$, $b_1$, $arrow.double$, $subset$, $integral$)

- 直接显示\
  type: #repr(type(values))\
  #values\

- 使用 join 格式化\
  #values.join()\
  #values.join("--")
