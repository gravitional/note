/*
#set table(
  stroke: none,
  gutter: 0.2em,
  fill: (x, y) =>
    if x == 0 or y == 0 { gray },
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
    it
  }
}

#let a = table.cell(
  fill: green.lighten(60%),
)[A]
#let b = table.cell(
  fill: aqua.lighten(60%),
)[B]

#table(
  columns: 4,
  [], [Exam 1], [Exam 2], [Exam 3],

  [John], [], a, [],
  [Mary], [], a, a,
  [Robert], b, a, b,
)
*/

#let ytab(n, arr_of_n) = {
  // assert(rows.len() >= n)
  // let inset = polygon.regular(
  //   fill: blue.lighten(80%),
  //   stroke: blue,
  //   size: 8pt,
  //   vertices: 4,
  // )
  let inset = [\u{25A2}]
  let empty = []
  let cell_on = (table.cell(fill: green.lighten(60%), inset),)
  let cell_off = (table.cell(fill: none, empty),)
  let b = for i in arr_of_n {
    cell_on * i
    cell_off * (n - i)
  }


  table(
    columns: n,
    stroke: none,
    fill: blue,
    ..b
  )
}

/* Example:
#ytab(5, (5, 4, 2, 2, 1))
*/


#let ydiag(n, ..arr_i2) = {
  // assert(rows.len() >= n)
  let empty = [] // [\u{25A2}]
  let arr_i2 = arr_i2.pos()
  let cell_on(e) = table.cell(fill: green.lighten(60%), [#e])
  let cell_off = (table.cell(fill: none, empty),)
  let b = for row in arr_i2 {
    let iLen = row.len()
    row.map(cell_on)
    cell_off * (n - iLen)
  }


  table(
    columns: n,
    stroke: none,
    fill: blue,
    ..b
  )
}

/* Example:
#ydiag(
  8,
  (6, 5, 4),
  (3, 2),
  (1,),
)
*/
