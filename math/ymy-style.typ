#import "@preview/frame-it:1.0.0": *

//================== theorem box
// You have to define the kinds of frames you need
#let (mytheo, mylem, mydef, mycor, myimp, myexa, myrem, mysupp) = make-frames(
  // This identifies the counter used for all theorems in this definition
  "counter-id",
  mytheo: ("定理", green.lighten(0%)),
  // You can provide a color or leave it out and it will be generated
  mylem: ("引理", gray),
  // For each frame kind, you have to provide its supplement title to be displayed
  mydef: ("定义", green.lighten(5%)),
  mycor: ("推论", olive.lighten(20%)),
  // You can add as many as you want
  myimp: ("重要", green.lighten(20%)),
  myexa: ("例 ", blue.lighten(25%)),
  myrem: ("注", olive.lighten(25%)),
  mysupp: ("补充", yellow.darken(20%)),
)

// #let phia = sym.phi.alt
#let to = sym.arrow
#let phi = sym.phi.alt
#let varphi = sym.phi
#let pd = sym.partial
#let arrD = sym.arrow.double
#let mybl(arg) = text(fill: blue)[#arg]
// dots
#let cdots = sym.dots.h.c
#let vdots = sym.dots.v
#let ddots = sym.dots.down
#let idots = sym.dots.up
#let qquad = math.wide

#let myeqbl(..args) = { args.pos().map(it => text(fill: blue)[#it]) }
