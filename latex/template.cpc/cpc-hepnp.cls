%%
%% This is file `cpc-hepnp.cls',
%% 
%% Copyright (c) 2007 Chinese Physics C
%% http://cpc-hepnp.ihep.ac.cn
%% mailto:hepnp@ihep.ac.cn

%------------------------------------------------------------------------------------------------------

\NeedsTeXFormat{LaTeX2e}[1996/12/01]
\ProvidesClass{cpc-hepnp}
\def\class@name{cpc-hepnp}%

\NeedsTeXFormat{LaTeX2e}
   \@obsoletefile{article.cls}{article.sty}
   \LoadClass{article}

\RequirePackage{fancyhdr}
\RequirePackage[numbers,sort&compress]{natbib}

\DeclareMathSizes{10}{10}{6}{5}  

%－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－

\setlength{\textheight}{230mm}
\setlength{\textwidth}{178mm}
\setlength{\topmargin}{-0.3in}  
\leftmargin 0mm
\evensidemargin 0mm
\headheight 12mm               
\headsep 5mm                  
\footskip=30pt                
\oddsidemargin-10mm             
\evensidemargin-10mm            
\columnsep 6mm               

%--------------------------------------------------------------------------------------------------------------

\renewcommand{\baselinestretch}{1.0}  
\renewcommand{\arraystretch}{1.2}
\setcounter{page}{1}


\setlength{\parskip}{0pt}       
\setlength{\floatsep}{6pt}         
\setlength{\textfloatsep}{6pt}    
\setlength\footnotesep{8pt}        

\setlength{\abovecaptionskip}{4pt plus1pt minus1pt}
\setlength{\belowcaptionskip}{4pt plus1pt minus1pt} 
\setlength{\abovedisplayskip}{6pt plus1pt minus1pt}
\setlength{\belowdisplayskip}{6pt plus1pt minus1pt} 
\addtolength{\thinmuskip}{-1mu}         
\addtolength{\medmuskip}{-2mu}      
\addtolength{\thickmuskip}{-2mu}      


%---------------------------------------------------------------------------------------------------

\font\scten=euex10 at 10pt
\newcommand{\vint}{\mathop{{\vcenter{\hbox{\scten\char90}}}}}
\renewcommand{\int}{\vint}

\font\sctenr=euex10 at 9pt
\newcommand{\sint}{\mathop{{\vcenter{\hbox{\sctenr\char82}}}}} 

\font\timeroman=psyr at 10pt
\newcommand{\vpartial}{\mathop{{\hbox{\timeroman \char182}}}} 



%---------------------------------------------------------------------------------------------------

\def\journal#1#2#3#4#5{{#1}. {#2}, {#3}, {\bf #4}: {#5}}
\def\book#1#2#3#4#5#6{{#1}. {#2}. {#3}: {#4}, {#5}. {#6}}
\def\diss#1#2#3#4#5#6#7{{#1}. {#2}. {#3}. {#4}: {#5}, {#6}. {#7}}

\newcommand{\thanksmark}{\textsuperscript{\,\rm{*}}}   
\newcommand{\dsize}{\displaystyle}      
\newcommand{\kou}{\rule[-2.5ex]{0ex}{6ex}}
%\renewcommand{\,}{\hspace{0.125em plus 0.025em minus 0.025em}}
\newcommand{\oa}{\hbox{\fontfamily{ptm}\selectfont @}}                          %使用新字体◎
\newcommand{\ruleup}{\vspace*{-0.5cm}\noindent\rule{8.35cm}{0.2pt}\rule{0.6pt}{0.4cm}}        %复杂公式与文本
\newcommand{\ruledown}{\hfill\noindent{\lower.38cm\hbox{\rule{0.2pt}{0.4cm}}\rule{8.35cm}     %用折线分开
             {0.2pt}}\vspace*{-0.5cm}}
\newcommand{\wen}[1]{\raisebox{0.3mm}{\textsuperscript{[#1]}}} 
\newcommand{\bb}{\raisebox{0.4mm}[0pt]{:}}
\newcommand{\newsmall}{\fontsize{9pt}{1\baselineskip}\selectfont}

%----------------------------------\thanks, \footnote--------------------------------------

\makeatletter

\renewcommand{\thefootnote}{\fnsymbol{footnote}}  

\renewcommand{\thanks}[1]{\thanksmark
    \protected@xdef\@thanks{\@thanks
        \protect\footnotetext[0]{\hspace*{-6pt}$*$\;#1}}}

\newcounter{email}            
\setcounter{email}{1}
\newcommand{\email}[1]{%
    \protected@xdef\@thanks{\@thanks%
        \protect\footnotetext[0]{\hspace*{-8pt}\arabic{email})\,{E-mail:\,}#1}}%
        \stepcounter{email}}%
        
\newcommand{\emails}[1]{%
   \footnotetext[0]{\hspace*{-8pt}\arabic{email})\,{E-mail:\,}#1}%
    \stepcounter{email}}%


\renewcommand\footnoterule{%
  \kern 1\p@
  \hrule \@width37mm
  \kern 8\p@}

\renewcommand\@makefntext[1]{%
    \parindent 1em%
    \noindent
    \hb@xt@2em{\hss\@makefnmark}#1}

%---------------------------------------------------------------------------------------

\renewcommand\maketitle{\par%
  \begingroup
    \renewcommand\thefootnote{\@fnsymbol\c@footnote}%
    \def\@makefnmark{\rlap{\@textsuperscript{\normalfont\@thefnmark}}}%
    \long\def\@makefntext##1{\parindent 1em\noindent
            \hb@xt@2em{%
                \hss\@textsuperscript{\normalfont\@thefnmark}}##1}%
    \if@twocolumn
      \ifnum \col@number=\@ne
        \@maketitle
      \else
        \twocolumn[\@maketitle]%
      \fi
    \else
      \newpage
      \global\@topnum\z@   % Prevents figures from going at top of page.
      \@maketitle
    \fi
  \@thanks
  \endgroup
  \setcounter{footnote}{0}%
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@author\@empty
  \global\let\@date\@empty
  \global\let\@title\@empty
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax}

\renewcommand\@maketitle{%
  \begin{center}%
  \let \footnote \thanks
   \vspace*{-1mm}
    {\Large\bf \@title \par}%
    {\normalsize%
      \lineskip .5em%
      \vskip 4mm%
      \begin{tabular}[t]{c}%
        \normalsize\@author%
      \end{tabular}}%
  \end{center}}%
\makeatother

%---------------------------------------------------------------------------------------------

\newcommand{\address}[1]{%
  \vspace*{0mm}
  \begin{center}%
    \vskip -5mm%
    \begin{center}%
      {\footnotesize #1}%
    \end{center}%
  \end{center}%
}%

\newcommand{\ym}[1]{\normalsize #1}%

%-----------------------------------------------------------------------------------

\renewenvironment{abstract}%
  {\newsmall\vspace{-2mm}%
   \list{}{\rightmargin 2em%
           \leftmargin 2em}%
    \item{}{\bf Abstract:}\hspace*{0.5em}\relax}%
   {\endlist}


%-----------------------------------------------------------------------------------

\newenvironment{keyword}%
  {\small%
    \list{}{\rightmargin 2em%
           \leftmargin 2em}%
                \item{}{\bf Key~words:}\hspace*{0.5em}\relax }%
       {\endlist%
        }%

\newenvironment{pacs}%
  {\small%
    \list{}{\rightmargin 2em%
           \leftmargin 2em}%
                \item{}{\bf PACS:}\hspace*{0.5em}\relax }%
       {\endlist%
        \vskip 1.5mm}%


%---------------------------------------------------------------

\newcommand{\acknowledgments}[1]{%
   {\it #1}%
  }


\makeatletter

%--------------------\section, \subsection--------------------------------------------------------------

\renewcommand \thesection {\bf\@arabic\c@section}  

\renewcommand\section{\@startsection {section}{1}{\z@}%
                                    {4mm \@plus.2ex \@minus .2ex}%
                                   {3.5mm \@plus.2ex \@minus .2ex}%
                                   {\normalfont\large\bfseries}}
\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                   %  {-2.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\normalfont\bfseries}}

\renewcommand\subsubsection{\renewcommand \thesection {\@arabic\c@section}
                            \@startsection{subsubsection}{3}{\z@}%
                                   %  {-2.25ex\@plus -1ex \@minus -.2ex}%
                                     {0.5ex}%
                                     {0.5ex}%
                                     {\normalfont}}
                                     
                                     
%----------------------------------------------------------------------------------------------

\renewcommand{\@biblabel}[1]{#1}
\renewcommand\refname{{\normalsize\bf References}}
\renewenvironment{thebibliography}[1]
     {\noindent\refname%
      \@mkboth{\MakeUppercase\refname}{\MakeUppercase\refname}%
      \footnotesize
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      %\sloppy
      %\setlength{\itemsep}{-1.5mm}
      \setlength{\itemsep}{0mm}
      \setlength{\labelsep}{0.8em}
      \setlength{\parsep}{0mm}
      \setlength{\parskip}{0mm}
      \setlength{\topsep}{0mm}
      \setlength{\partopsep}{0mm}
      \clubpenalty4000
      \@clubpenalty \clubpenalty
      \widowpenalty4000%
      \sfcode`\.\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}

%---------------------------------------------------------------------------------

\newenvironment{mylabc}
                {%
                 \small
                 \let\\\@centercr
                 \list{}{\itemsep      \z@
                         \itemindent   -1em%
                         \listparindent0em
                         \leftmargin   3em
                         \rightmargin  2em}
                         \item\relax}
                {\endlist}


\newenvironment{zhu}                   %用于图表注
                {\footnotesize
                 \vspace{-1mm}
                 \let\\\@centercr
                 \list{}{\itemsep      \z@
                         \itemindent   0em%
                         \listparindent0em
                         \leftmargin   2mm
                         \rightmargin  0em}
                         \item\relax}
                {\endlist}
                
\makeatother




\fancypagestyle{myfoot}
{%
%\fancyhf{} 
\renewcommand{\headrule}{\vskip 2pt  \hrule height0.4pt width\headwidth}
}%
%\thispagestyle{myfoot}

 
\pagestyle{fancy} 
\fancyhead{} 
\renewcommand{\headrule}{\vskip 2pt  \hrule height0.4pt width\headwidth}


\makeatletter

\newcommand\figcaption{\def\@captype{figure}\caption}
\renewcommand\figurename{{\small Fig.}}
\newcommand\tabcaption{\def\@captype{table}\caption}

\long\def\@makecaption#1#2{%
\vskip\abovecaptionskip
\sbox\@tempboxa{\small #1.\quad #2}%
\ifdim \wd\@tempboxa >\hsize
  \begin{mylabc}
  \vspace{-2mm}
  {\protect #1.\quad #2}%
  \vskip 1mm%
  \end{mylabc}\par
\else
   \global \@minipagefalse
   \hb@xt@\hsize{\hfil\box\@tempboxa\hfil}%
\fi
\vskip\belowcaptionskip}

\makeatother



\makeatletter
\newcommand\capt[1]{%
\sbox\@tempboxa{\small #1}%
\ifdim \wd\@tempboxa >\hsize
\begin{mylabc}
\vspace*{-2mm}
\small #1%
\vskip 1mm%
\end{mylabc}
\else
\global \@minipagefalse
\vspace*{-2mm}
\hb@xt@\hsize{\hfil\box\@tempboxa\hfil}%
\vskip 1mm%
\fi}
\makeatother




%%
%% End of file `cpc-hepnp.cls'.
