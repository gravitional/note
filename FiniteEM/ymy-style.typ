#import "@preview/frame-it:1.0.0": *

// define a function that:
// - takes content
// - applies styling to it
// - returns the styled content
//
#let _eq_cnter = counter("eq")
#let myeqn(body) = {
  set math.equation(numbering: none)
  body
}

#let myeqs(..args) = {
  for eq in args.pos() {
    _eq_cnter.step(level: 3)
    eq
  }
  _eq_cnter.step(level: 2)
}

#let apply-template(body) = [
  //====================== 子公式编号
  #set heading(numbering: "1.")
  #set math.equation(numbering: it => _eq_cnter.display("(1-1.a)"))
  #show heading.where(level: 1): it => it + _eq_cnter.step() + _eq_cnter.step(level: 2)
  #show math.equation.where(block: true): it => {
    it
    if it.numbering != none {
      if _eq_cnter.get().len() == 2 {
        _eq_cnter.step(level: 2)
      }
    }
  }

  //================ return count
  #body
]

// You have to define the kinds of frames you need
#let (mytheo, mylem, mydef, myimp, myexa, myrem, mycor) = make-frames(
  // This identifies the counter used for all theorems in this definition
  "counter-id",
  mytheo: ("定理",),
  // You can provide a color or leave it out and it will be generated
  mylem: ("引理", gray),
  // For each frame kind, you have to provide its supplement title to be displayed
  mydef: ("定义",),
  // You can add as many as you want
  myimp: ("重要", blue.lighten(25%)),
  myexa: ("例 ", orange.lighten(25%)),
  myrem: ("注", olive.lighten(25%)),
  mycor: ("推论", lime.lighten(25%)),
)


// #let phia = sym.phi.alt
#let to = sym.arrow
#let phi = sym.phi.alt
#let varphi = sym.phi
#let pd = sym.partial
