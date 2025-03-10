= typst functions

#let fn(a, b, k1: none, k2: "qq", c, d) = {
  (a, b, c, d, k1, k2)
}

#fn("a", "b", k1: "k1", k2: "k2", "c", "d")
// #fn("a", "b", "k1", "k2", "c", "d") // !!wrong

key-value 参数可以任意交换位置:\
#fn("a", "b", "c", "d", k1: "k1", k2: "k2")\
#fn(k1: "k1", k2: "k2", "a", "b", "c", "d")\
#fn("a", k1: "k1", k2: "k2", "b", "c", "d")\
#fn("a", k1: "k1", "b", k2: "k2", "c", "d")\
#fn("a", k1: "k1", "b", "c", k2: "k2", "d")\

#fn("b", "a", "c", "d", k1: "k1", k2: "k2")\

== b

函数参数可以是 array, \
#let fn2(a) = {
  (a.at(0), a.at(1))
}
#fn2((1, 3, 5))

也可以是 array of array\
#let fn3(a) = {
  (a.at(0).at(0), a.at(1).at(0))
}
#fn3((
  (1, 2, 3),
  (4, 5, 6),
))


#{
  set table.cell(stroke: yellow, fill: green.lighten(50%))
  context table.cell.stroke
  linebreak()
  context table.cell.fill
  let a = table(
    columns: 3,
    [1], [2], [3],
    [4], [5], [6],
    [7], [8], [9],
  )
  linebreak()
  repr(a)
  linebreak()
  [#type(a)]
  linebreak()
  a
}

#{
  show table: set table.cell(stroke: red, fill: blue.lighten(50%))
  context table.cell.stroke
  linebreak()
  context table.cell.fill
  let a = table(
    columns: 3,
    [1], [2], [3],
    [4], [5], [6],
    [7], [8], [9],
  )
  linebreak()
  repr(a)
  linebreak()
  [#type(a)]
  linebreak()
  a
}
