# latex minipage parbox

[\parbox vs. minipage: Differences in applicability](https://tex.stackexchange.com/questions/6472/parbox-vs-minipage-differences-in-applicability)

The main reason I see to use minipage over \parbox is to allow verbatim
(\verb, verbatim, etc.) text inside the box (unless, of course, you also put the minipage inside a macro argument).

EDIT Here are other differences between minipage and \parbox
(from the comments to Yiannis' answer and from looking at the source code of both these macros in source2e).

A first difference, as already mentioned by lockstep in his question,
is in the footnote treatment: minipage handles them
by putting them at the bottom of the box while footnotes are lost in a \parbox
(to avoid this, you must resort to the \footnotemark/footnotetext trick):

![alt text](https://i.stack.imgur.com/dvscl.png)

```latex
\documentclass{article}
\begin{document}
\parbox[t]{3cm}{text\footnote{parbox footnote}}
\begin{minipage}[t]{3cm}text\footnote{minipage footnote}\end{minipage}
\end{document}
```

A second difference is in that minipage resets the \@listdepth counter,
meaning that, inside a minipage, you don't have to worry about the list nesting level when using them.
Here's an example which illustrates the point:

```latex
\documentclass{article}
\begin{document}
\begin{list}{}{}\item\begin{list}{}{}\item\begin{list}{}{}\item\begin{list}{}{}\item
    \begin{list}{}{}\item\begin{list}{}{}
  \item %\parbox{5cm}{\begin{list}{}{}\item \end{list}}% error
  \item %\begin{minipage}{5cm}\begin{list}{}{}\item \end{list}\end{minipage}% no error
\end{list}\end{list}\end{list}\end{list}\end{list}\end{list}
\end{document}
```

A third difference is that minipage sets the boolean `\@minipagefalse`
which in turn deactivates `\addvspace` if it's the first thing to occur inside a minipage.
This means that minipage will have better spacing and allow better alignment compared
to \parbox in some cases like the following (left is minipage, right is \parbox):

![alt text](https://i.stack.imgur.com/0DJky.png)

```latex
\documentclass{article}
\begin{document}
Pros: \begin{minipage}[t]{3cm}\begin{itemize}\item first \item second%
    \end{itemize}\end{minipage}
Cons: \parbox[t]{3cm}{\begin{itemize}\item first \item second\end{itemize}}
\end{document}
```
