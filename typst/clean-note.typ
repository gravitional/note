#let template(
  title: "",
  authors: (),
  date: datetime.today().display(),
  columns: 1,
  body
) = {
  set document(
    author: authors.map(a => a.name),
    title: title
  )
  set page(
    margin: (x: 1.25in, y: 1in),
    header: context {
      let (n,) = counter(page).get()
      emph(if calc.odd(n) {
        set align(right)
        if n > 1 { title }
        else {date}
      } else if calc.even(n) {
        let selector = selector(heading.where(level: 1)).before(here())
        let num = counter(selector)
        let heading = query(selector).last()
        num.display(heading.numbering)
        [ ]
        heading.body
      })
    },
    footer: context {
      let (n,) = counter(page).get()
      set align(if calc.even(n) { left } else { right })
      set text(9pt)
      counter(page).display(
        "1 / 1",
        both: true,
      )
    },
    paper: "a4",
    columns: columns
  )
  set par(
    first-line-indent: 1em,
    justify: true
  )

  // Numbering
  set heading(numbering: "1.")
  set math.equation(numbering: "(1)", number-align: end + bottom)
  show ref: it => {
    let eq = math.equation
    let el = it.element
    if el != none and el.func() == eq {
      // Override equation references.
      link(el.location(), numbering(
        el.numbering,
        ..counter(eq).at(el.location())
      ))
    } else {
      // Other references as usual.
      it
    }
  }

  // Front staff
  place(
    top + center,
    scope: "parent",
    float: true,
    {
      // Title
      pad(bottom: 1em, text(2em, weight:"bold", title))

      // Authors
      pad(bottom: 0.5em, authors.map(a => a.name).join([, ], last: [ and ]))
    }
  )

  // Body
  body
}