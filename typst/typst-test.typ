#set page(width: 18cm, height: auto)
#set heading(numbering: "1.")

= Fibonacci sequence
The Fibonacci 数列 is defined through the
recurrence relation $F_n = F_(n-1) + F_(n-2)$. It can also be expressed in _closed form:_

$
  F_n = round(1 / sqrt(5) phi.alt^n), quad
  phi.alt = (1 + sqrt(5)) / 2
  quad epsilon "epsilon"
$

#let count = 8
#let nums = range(1, count + 1)
#let fib(n) = (if n <= 2 { 1 } else { fib(n - 1) + fib(n - 2) })

The first #count numbers of the sequence are:

#align(center, table(columns: count, ..nums.map(n => $F_#n$), ..nums.map(n => str(fib(n)))))

$ a+b=c $


#show <awa>: set text(fill: red)
#{
  [a]
  [<awa>]
}
#[b] <awa>

#[ 一段文本 ]

#repr([ 一段文本 ])

#type(none), #(type(none) == none), #type(type(none))

$1 < 2$的结果为：#(1 < 2)

可以使用以下方法从代码块获得字符串：\
#repr(`包含换行符和双引号的

"内容"`.text)


将对应参数应用于函数可以取得对应的结果：
#let f(x, y) = [两个值#(x)和#(y)偷偷混入了我们内容之中。]

#let x = "Hello world!!"
#let y = [一段文本]
#f(repr(x), y)

#let f(loc) = [当前页码为#loc.page()。]
#locate(f)

#let cat = (neko-mimi: 2)
#("neko-mimi" in cat);\
#("neko-kiki" in cat)

#let my_cnt = 0;

#(++my_cnt). 列表项1
#(++my_cnt). 列表项2

列表间插入一段描述。

#my_cnt. 列表项3 #(my_cnt+=1);
+ 列表项4 #(my_cnt+=1);
+ 列表项5 #(my_cnt+=1);

#repr($ s=q+p $)

#for i in range(3) { $ a^2 + b^2 =c^2 $ }

$ #rect(width: 1cm) $

#{
  let a = [from]
  let b = [*world*]
  [hello ]
  a + [ the ] + b
}

// Origin: https://typst.app/project/r0SkRmsZYIYNxjs6Q712aP
#import "png.typ": *
#let prelude = (0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A)
#let ihdr(w, h) = chunk("IHDR", be32(w) + be32(h) + (8, 2, 0, 0, 0))
#let idat(lines) = chunk(
  "IDAT",
  {
    let data = lines.map(line => (0x00,) + line).flatten()
    let len = le32(data.len()).slice(0, 2)
    (0x08, 0x1D, 0x01)
    len
    len.map(xor.with(0xFF))
    data
    be32(adler32(data))
  },
)
#align(
  center,
  box(
    width: 25%,
    image.decode(
      bytes({
        let (w, h) = (8, 8)
        prelude
        ihdr(w, h)
        idat(for y in range(h) {
          (
            for x in range(w) {
              (calc.floor(256 * x / w), 128, calc.floor(256 * y / h))
            },
          )
        })
        chunk("IEND", ())
      }),
    ),
  ),
)
