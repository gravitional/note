#set text(font: "Noto Sans CJK SC")

= typst table

#[
  #set table(
    stroke: none,
    gutter: 0.2em,
    fill: (x, y) => if x == 0 or y == 0 { gray },
    inset: (right: 1.5em),
  )

  #show table.cell: it => {
    if it.x == 0 or it.y == 0 {
      set text(white)
      strong(it)
    } else if it.body == [] {
      // Replace empty cells with 'N/A'
      pad(..it.inset)[_N/A_]
    } else {
      "qq" + it
    }
  }


  #let mygre = table.cell(fill: green.lighten(60%))[green1]
  #let myaqu = table.cell(fill: aqua.lighten(60%))[aqua1]
  #let myred = table.cell(fill: red)[red]

  #table(
    columns: 4,
    [none], [col1], [col2], [col3],
    [row1], [1], [2], [3],
    [row2], "qq", "ww", "rr",
    [row3], myred, mygre, mygre,
    [row4], myred + "22", strong(myred), text(navy)[#myaqu],
  )

  #repr(myred + "22")\
  #myred + "22"

  #repr(text(navy)[#myaqu])\
  #text(navy)[#myaqu]
]

== show rule 会递归调用

#let te1 = table.cell[]
#let te2 = table.cell[test]

=== fun

#[
  // t: table.cell
  #let fn(t) = {
    if (t.body == []) {
      // 勉强能编译过, 但是颜色设置不生效
      table.cell(fill: red, align: center)[NA]
    } else {
      // 直接返回参数 t 可以收敛
      t
    }
  }

  //============================== 测试
  blue: #repr(blue)\
  red: #repr(red)

  te1: #repr(te1)\
  #let r = fn
  #repr(r(te1))\
  #repr(r(r(te1)))\
  #repr(r(r(r(te1))))\

  te2: #repr(te2)\
  #repr(r(te2))\
  #repr(r(r(te2)))\
  #repr(r(r(r(te2))))\

  === fun 1

  #let fn1(t) = {
    if (t.body == []) {
      // 会递归不收敛; []->[]
      table.cell(fill: red, align: center)[]
    } else {
      t
    }
  }

  blue: #repr(blue)\
  red: #repr(red)

  te1: #repr(te1)\
  #let r = fn1
  #repr(r(te1))\
  #repr(r(r(te1)))\
  #repr(r(r(r(te1))))\

  te2: #repr(te2)\
  #repr(r(te2))\
  #repr(r(r(te2)))\
  #repr(r(r(r(te2))))\

  === fun2

  #let fn2(t) = {
    if (t.body == []) {
      table.cell(fill: red, align: center)[NA]
    } else {
      // 会递归不收敛
      table.cell(fill: green, align: left)[#t.body]
    }
  }

  red: #repr(red)\
  green: #repr(green)

  te1: #repr(te1)\
  #let r = fn2
  #repr(r(te1))\
  #repr(r(r(te1)))\
  #repr(r(r(r(te1))))\

  te2: #repr(te2)\
  #repr(r(te2))\
  #repr(r(r(te2)))\
  #repr(r(r(r(te2))))\

  === func3

  // 会递归不收敛
  #let fn3 = it => table.cell[#(it.body + "qq")]

  te1: #repr(te1)\
  #let r = fn3
  #repr(r(te1))\
  #repr(r(r(te1)))\
  #repr(r(r(r(te1))))\

  te2: #repr(te2)\
  #repr(r(te2))\
  #repr(r(r(te2)))\
  #repr(r(r(r(te2))))\

  == func 4

  #let fn4(t) = {
    if (t.body == []) {
      table.cell[NA]
    } else {
      // 会递归不收敛
      table.cell(fill: green, align: center)[#t.body]
    }
  }

  green: #repr(green)

  te1: #repr(te1)\
  #let r = fn4
  #repr(r(te1))\
  #repr(r(r(te1)))\
  #repr(r(r(r(te1))))\

  te2: #repr(te2)\
  #repr(r(te2))\
  #repr(r(r(te2)))\
  #repr(r(r(r(te2))))\

  === table

  #show table.cell: fn

  #let a = table(
    columns: 4,
    [none], [col1], table.cell(fill: yellow)[], [],
    [row1], [1], [2], table.cell(fill: gray)[],
    [row2], [qq], [ww], [rr],
  )

  #repr(a)

  fn 中的颜色指定会被 table 中 cell 的颜色覆盖掉,
  没有指定就是 none
  #a

  设置默认颜色
  #set table.cell(fill: red)
  #a

  === 嵌套 cell

  #table(
    columns: 2,
    [none],
    table.cell(fill: yellow)[
      #table.cell(fill: red, align: left)[col1]
    ],
  )

  #repr(table.cell(fill: red, align: left)[col1].body)

]

#pagebreak()

== 自定义 table cell, map

#let q(a) = { a }
#let myb(a) = {
  table.cell(fill: blue)[#a]
}

#let myL = (
  [none],
  [col1],
  [],
  [],
  [row1],
  q[1],
  q[2],
  [],
  [row2],
  q["qq"],
  ["ww"],
  "rr",
  [row3],
  ["u1"],
  "u2",
  "u3",
)

#table(
  columns: 4,
  ..myL.map(myb),
)
