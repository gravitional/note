# latex 各种符号

## 商标, 版权符号 circled C, circled R

[how to get good looking copyright and registered symbols](https://tex.stackexchange.com/questions/1676/how-to-get-good-looking-copyright-and-registered-symbols)

Use the textcomp package, which offers a \textregistered symbol (both serif and sans-serif), different to standard LaTeX which uses \textcircled.

```latex
\documentclass{article}
\usepackage{textcomp}
\begin{document}
\textregistered\textcopyright
\sffamily\textregistered\textcopyright
\end{document}
```

Here are the original LaTeX definitions from latex.ltx:

```latex
\DeclareTextCommandDefault{\textcopyright}{\textcircled{c}}
\DeclareTextCommandDefault{\textregistered}{\textcircled{%
      \check@mathfonts\fontsize\sf@size\z@\math@fontsfalse\selectfont R}}
If designed symbols like those of textcomp wouldn't fit to your text font, you could use \textcircled similarly to create a symbol with the used font together with some correction if necessary, with \raisebox etc.
```

For ConTeXt, use the \registered{} and \trademark{} macros.

### Davide Des

I know it's a very old post but I found another solution which provides an output almost equal to

```latex
$^{\tiny{\textregistered}}$
```

but without using the math environment

```latex
Matlab\textsuperscript{\tiny\textregistered}
```

My minimal working example (MWE) is:

```latex
\documentclass{article}
\usepackage[utopia]{mathdesign}

\begin{document}
Before: Matlab\textregistered

After: Matlab\,\textsuperscript{\tiny\textregistered}
\end{document}
```

My preferred version leaves a bit more non-breaking space between Matlab and the registered symbol.

## 负间距, negative space

[Large negative spaces](https://tex.stackexchange.com/questions/67912/large-negative-spaces)

In text mode: `\kern-1em` equals "minus quad"

In math mode: `\mkern-18mu` equals "minus quad"

For "minus qquad" use `-2em` or `-36mu` respectively.

## Werner

For LaTeX, horizontal spacing is achieved using `\hspace{<len>}` where `<len>` is a length (either positive or negative); TeX's equivalent to \hskip. 
Since `\quad` [`\qquad`] is equivalent to a horizontal skip 
of `1em` [`2em`], use `\hspace{-1em}` [`\hspace{-2em}`] to obtain a negative space amount.

`\kern` inserts a space depending on the mode that TeX is in, 
and could therefore be either vertical or horizontal.

Text-based spacing can be inserted using any of the 
`\phantom`-related commands: `\phantom{<stuff>}`, or `\hphantom{<stuff>}`, for example.