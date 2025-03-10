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

#let ytab(dim_n: none, dims, ..args) = {
  let x = ()
  let cell_on = (table.cell(fill: green.lighten(60%), "1"),)
  let cell_off = (table.cell(fill: none, " "),)
  let dim_arr = args.pos()
  for i in args.pos() {
    x += cell_on * i
    x += cell_off * (dim_n - i)
  }

  table(
    columns: dim_n,
    stroke: none,
    fill: blue,
    ..x
  )
}


#ytab(dim_n: 6, 5, 4, 2, 2, 1)

