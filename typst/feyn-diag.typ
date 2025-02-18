#import "@preview/fletcher:0.5.4" as fletcher: diagram, node, edge, resolve
#import fletcher.shapes as shapes

// ---- Utilities --------------------------------------------------------------
//   Calc
/* #let map-deep-coord(f, arr) = {
  if type(arr) == array {
    if arr.any(it => type(it) != array) {
      f(..arr)
    } else {
      arr.map(subarr => map-deep-coord(f, subarr))
    }
  } else {
    arr
  }
} */

#let neg-v(a) = {
  a.map((x) => -x)
}

#let add-v(a, b) = {
  a.zip(b).map(((x, y)) => x + y)
}

#let sub-v(a, b) = {
  a.zip(b).map(((x, y)) => x - y)
}

#let scale-v(a, c) = {
  a.map((x) => c*x)
}

#let interp-v(a, b, t) = {
  a.zip(b).map(((x, y)) => (1-t)*x + t*y)
}

//   Wrappers
#let mom-label(e, label-side, label-sep, label-anchor) = {
  let (x, y) = sub-v(..e.final-vertices.rev())
  let angle = calc.atan2(x/1pt, y/1pt)
  box(
    diagram(edge(
      (20pt, 0pt), "-latex",
      label:e.label,
      label-angle:-angle,
      label-sep:label-sep,
      label-side:label-side,
      label-anchor:label-anchor
    )),
    inset: .2em,
    radius: .2em,
    fill: e.label-fill,
  )
}


// ---- Elements ---------------------------------------------------------------
#let stroke-thickness = 0.75pt

#let propagator(start, end, mom, ..exts) = {
  let opts = if mom == none {
    (:)
  } else {
    if type(mom) != dictionary { mom = (label:mom) }
    (
      label:mom.at("label", default:""),
      label-pos:mom.at("pos", default:0.5),
      label-angle:auto,
      label-side:mom.at("side", default:left),
      label-sep:mom.at("arrow-sep", default:3pt),
      label-wrapper: e =>
        mom-label(
          e,
          mom.at("side", default:left),
          mom.at("label-sep", default:0.7em),
          mom.at("label-anchor" , default:"center")
        )
    )
  }
  edge(
    start, end,
    stroke:stroke-thickness,
    mark-scale: (stroke-thickness/1pt)*100%,
    ..exts, ..opts
  )
}

#let fermion(start, end, mom:none, ..exts) = propagator(start, end, mom, "-|>-", ..exts)
#let antifermion(start, end, mom:none, ..exts) = propagator(start, end, mom, "-<|-", ..exts)
#let photon(start, end, mom:none, ..exts) = propagator(start, end, mom, "~", ..exts)
#let gluon(start, end, mom:none, ..exts) = propagator(start, end, mom, "coil", ..exts)
