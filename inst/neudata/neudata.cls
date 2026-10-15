% ---------------------------------------------------------------------------
% neudata.cls — Neudata Consulting Ltd document class
% Reports, proposals and technical notes in the Neudata house style.
% Built on article; compiles with pdfLaTeX (Overleaf default) or XeLaTeX.
%
% \documentclass[options]{neudata}
%
%   style1    (default) Matches the Neudata Quarto report: teal top strip,
%             navy cover band, navy headings with teal rules, striped tables.
%   style2    Matches the Neudata PowerPoint template: white cover, teal
%             headings, "Neudata | #ClearDataClearImpact" footer.
%
%   report | proposal | note   document type label on the cover (default: report)
%   draft                      adds a DRAFT watermark
%   nocover                    simple title block instead of a cover page
% ---------------------------------------------------------------------------
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{neudata}[2026/10/02 Neudata Consulting Ltd document class]

\newcommand{\neudata@doctype}{Report}
\newif\ifneudata@draft
\newif\ifneudata@cover   \neudata@covertrue
\newif\ifneudata@styleone \neudata@styleonetrue
\DeclareOption{style1}{\neudata@styleonetrue}
\DeclareOption{style2}{\neudata@styleonefalse}
\DeclareOption{report}{\renewcommand{\neudata@doctype}{Report}}
\DeclareOption{proposal}{\renewcommand{\neudata@doctype}{Proposal}}
\DeclareOption{note}{\renewcommand{\neudata@doctype}{Technical note}}
\DeclareOption{draft}{\neudata@drafttrue}
\DeclareOption{nocover}{\neudata@coverfalse}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax
\LoadClass[11pt, a4paper]{article}

% ----- Packages ---------------------------------------------------------------
\RequirePackage[T1]{fontenc}
\RequirePackage[utf8]{inputenc}
\RequirePackage{tgadventor}                % style 2 headings: Century Gothic look (qag)
\RequirePackage{tgheros}                   % body: Helvetica-like sans, Segoe UI / Arial look (qhv)
\renewcommand{\sfdefault}{qhv}
\renewcommand{\familydefault}{\sfdefault}
\RequirePackage{microtype}
\RequirePackage[a4paper, top=2.6cm, bottom=2.4cm, left=2.2cm, right=2.2cm,
                headheight=1.2cm, headsep=0.6cm, footskip=1.1cm]{geometry}
\RequirePackage[table]{xcolor}
\RequirePackage{graphicx}
\RequirePackage{tikz}
\RequirePackage{fancyhdr}
\RequirePackage{titlesec}
\RequirePackage{titletoc}
\RequirePackage{lastpage}
\RequirePackage{booktabs}
\RequirePackage{array}
\RequirePackage{enumitem}
\RequirePackage[most]{tcolorbox}
\RequirePackage{etoolbox}

% ----- Colours ----------------------------------------------------------------
\definecolor{NeudataDeepTeal}{HTML}{055F56}   % brand teal (logo outer arc)
\definecolor{NeudataPPTTeal}{HTML}{0D7377}    % PowerPoint title teal
\definecolor{NeudataBlue}{HTML}{0070C0}       % PowerPoint note blue
\definecolor{NeudataNavy}{HTML}{04242F}
\definecolor{NeudataLogoBlue}{HTML}{0B376C}
\definecolor{NeudataMist}{HTML}{EAF2F5}
\definecolor{NeudataGrey}{HTML}{8A99A3}
\definecolor{NeudataInk}{HTML}{1F2D33}
\definecolor{NeudataSeafoam}{HTML}{9FD3CB}

% Role colours: what each style uses for accents, headings and sub-headings
\ifneudata@styleone
  \colorlet{NeudataTeal}{NeudataDeepTeal}
  \colorlet{NeudataHeading}{NeudataNavy}
  \colorlet{NeudataSubheading}{NeudataLogoBlue}
  \colorlet{NeudataSubsubheading}{NeudataDeepTeal}
  \colorlet{NeudataLink}{NeudataLogoBlue}
\else
  \colorlet{NeudataTeal}{NeudataPPTTeal}
  \colorlet{NeudataHeading}{NeudataPPTTeal}
  \colorlet{NeudataSubheading}{NeudataBlue}
  \colorlet{NeudataSubsubheading}{NeudataPPTTeal}
  \colorlet{NeudataLink}{NeudataBlue}
\fi

\RequirePackage[font={small}, labelfont={bf, color=NeudataTeal}]{caption}
\RequirePackage[hidelinks]{hyperref}
\hypersetup{colorlinks=true, linkcolor=NeudataHeading, urlcolor=NeudataLink, citecolor=NeudataTeal}
\color{NeudataInk}

\ifneudata@styleone
  \newcommand{\neudata@headfont}{\fontfamily{qhv}\selectfont}
\else
  \newcommand{\neudata@headfont}{\fontfamily{qag}\selectfont}
\fi

% ----- Metadata ------------------------------------------------------------------
\newcommand{\neudata@subtitle}{}
\newcommand{\neudata@client}{}
\newcommand{\neudata@reference}{}
\newcommand{\neudata@confidentiality}{}
\newcommand{\neudata@version}{}
\newcommand{\neudata@affiliation}{}
\newcommand{\neudata@abstract}{}
\newcommand{\neudata@logo}{figures/neudata-logo.png}
\newcommand{\subtitle}[1]{\renewcommand{\neudata@subtitle}{#1}}
\newcommand{\client}[1]{\renewcommand{\neudata@client}{#1}}
\newcommand{\reference}[1]{\renewcommand{\neudata@reference}{#1}}
\newcommand{\confidentiality}[1]{\renewcommand{\neudata@confidentiality}{#1}}
\newcommand{\version}[1]{\renewcommand{\neudata@version}{#1}}
\newcommand{\affiliation}[1]{\renewcommand{\neudata@affiliation}{#1}}
\renewcommand{\abstract}[1]{\renewcommand{\neudata@abstract}{#1}}
\newcommand{\logo}[1]{\renewcommand{\neudata@logo}{#1}}
\newcommand{\neudatatoday}{\two@digits{\day}-\two@digits{\month}-\the\year}
\date{\neudatatoday}

% ----- Headings -----------------------------------------------------------------------
\setcounter{secnumdepth}{3}
\ifneudata@styleone
  % Quarto style: navy section titles over a teal rule, blue sub-sections, teal sub-sub-sections
  \titleformat{\section}
    {\neudata@headfont\Large\bfseries\color{NeudataHeading}}
    {\thesection}{0.5em}{}[{\color{NeudataTeal}\vspace{-0.4ex}\titlerule[1.2pt]}]
\else
  % PowerPoint style: teal section titles over a thin teal rule
  \titleformat{\section}
    {\neudata@headfont\Large\bfseries\color{NeudataHeading}}
    {\thesection}{0.6em}{}[{\color{NeudataTeal}\titlerule[0.6pt]}]
\fi
\titleformat{\subsection}
  {\neudata@headfont\large\bfseries\color{NeudataSubheading}}{\thesubsection}{0.5em}{}
\titleformat{\subsubsection}
  {\neudata@headfont\normalsize\bfseries\color{NeudataSubsubheading}}{\thesubsubsection}{0.5em}{}
\titlespacing*{\section}{0pt}{2.6ex plus .5ex minus .2ex}{1.4ex}
\titlespacing*{\subsection}{0pt}{1.8ex plus .4ex minus .2ex}{0.6ex}

% Table of contents: same heading look as sections
\renewcommand{\contentsname}{\ifneudata@styleone Table of contents\else Contents\fi}
\ifneudata@styleone
  \dottedcontents{section}[1.6em]{\addvspace{0.2em}}{1.6em}{0.6pc}
  \dottedcontents{subsection}[3.8em]{}{2.2em}{0.6pc}
\fi
\renewcommand{\tableofcontents}{%
  \section*{\contentsname}\@starttoc{toc}\par\vspace{1em}}

\setlist[itemize,1]{label={\color{NeudataTeal}\textbullet}, leftmargin=1.4em}
\setlist[itemize,2]{label={\color{NeudataSubheading}\textendash}}
\setlist[enumerate,1]{label={\color{NeudataTeal}\bfseries\arabic*.}}
\setlength{\parskip}{0.5em}
\setlength{\parindent}{0pt}

% ----- Header and footer -------------------------------------------------------
\pagestyle{fancy}
\fancyhf{}
\renewcommand{\headrule}{{\color{NeudataTeal}\hrule height 0.6pt}}
\fancyhead[R]{\includegraphics[height=0.85cm]{\neudata@logo}}
\ifneudata@styleone
  \fancyhead[L]{\footnotesize\color{NeudataNavy}\@title}
  \fancyfoot[L]{\scriptsize\color{NeudataGrey}Neudata Consulting Ltd \textperiodcentered\ \textit{Insight. Impact. Innovation.}}
  \fancyfoot[R]{\scriptsize\color{NeudataGrey}\thepage\ / \pageref*{LastPage}}
  \fancypagestyle{plain}{\fancyhf{}\renewcommand{\headrule}{}%
    \fancyfoot[R]{\scriptsize\color{NeudataGrey}\thepage\ / \pageref*{LastPage}}}
\else
  \fancyhead[L]{\small\color{NeudataTeal}\neudata@headfont\@title}
  \fancyfoot[C]{\scriptsize\color{NeudataTeal}Neudata \,|\, \#ClearDataClearImpact}
  \fancyfoot[R]{\scriptsize\color{NeudataGrey}\thepage\ / \pageref*{LastPage}}
  \fancyfoot[L]{\scriptsize\color{NeudataGrey}\neudata@confidentiality}
  \fancypagestyle{plain}{\fancyhf{}\renewcommand{\headrule}{}%
    \fancyfoot[C]{\scriptsize\color{NeudataTeal}Neudata \,|\, \#ClearDataClearImpact}}
\fi

% ----- Abstract / summary box ----------------------------------------------------------
\newtcolorbox{neudatasummary}[1][Abstract]{%
  enhanced, breakable, colback=NeudataMist, colframe=NeudataMist, boxrule=0pt, arc=3pt,
  left=10pt, right=10pt, top=8pt, bottom=8pt,
  fonttitle=\bfseries\color{NeudataNavy}, coltitle=NeudataNavy, colbacktitle=NeudataMist,
  title=#1, titlerule=0pt, toptitle=2pt, bottomtitle=0pt}

% ----- Cover pages ------------------------------------------------------------------
\newcommand{\neudata@authorline}{%
  \textbf{\@author}\ifx\neudata@affiliation\@empty\else\ --- \neudata@affiliation\fi}

% Style 1: teal strip, logo, navy band with title, details bottom-left
\newcommand{\neudata@coverone}{%
  \begin{titlepage}
    \begin{tikzpicture}[remember picture, overlay]
      \fill[NeudataDeepTeal] (current page.north west) rectangle ([yshift=-0.25cm]current page.north east);
      \node[anchor=north west, inner sep=0] at ([xshift=2.2cm, yshift=-1.9cm]current page.north west)
        {\includegraphics[width=4.2cm]{\neudata@logo}};
      \fill[NeudataNavy] ([yshift=-9.9cm]current page.north west) rectangle ([yshift=-16.2cm]current page.north east);
      \node[anchor=west, text width=\textwidth, align=left, inner sep=0, text=white]
        at ([xshift=2.2cm, yshift=-13.05cm]current page.north west) {%
          {\fontsize{26}{31}\selectfont\bfseries\@title\par}
          \ifx\neudata@subtitle\@empty\else\vspace{1.1em}{\Large\color{NeudataSeafoam}\neudata@subtitle\par}\fi
          \ifx\neudata@client\@empty\else\vspace{1.3em}{\normalsize Prepared for: \textbf{\neudata@client}\par}\fi};
      \node[anchor=south west, text width=\textwidth, align=left, inner sep=0, text=NeudataNavy, font=\small]
        at ([xshift=2.2cm, yshift=2.3cm]current page.south west) {%
          \neudata@authorline\par
          \@date\par
          \ifx\neudata@reference\@empty\else Reference: \neudata@reference\par\fi
          \ifx\neudata@version\@empty\else Version: \neudata@version\par\fi
          \ifx\neudata@confidentiality\@empty\else\vspace{0.9em}{\bfseries\color{NeudataDeepTeal}\MakeUppercase{\neudata@confidentiality}\par}\fi
          \vspace{1.3em}{\footnotesize\color{NeudataGrey}Neudata Consulting Ltd \textperiodcentered\ www.neu-data.com \textperiodcentered\ contact@neu-data.com\par}};
    \end{tikzpicture}
  \end{titlepage}}

% Style 2: white cover matching the PowerPoint title slide
\newcommand{\neudata@covertwo}{%
  \begin{titlepage}
    \begin{tikzpicture}[remember picture, overlay]
      \node[anchor=north west, inner sep=0] at ([xshift=2.3cm, yshift=-2.2cm]current page.north west)
        {\includegraphics[height=3.2cm]{\neudata@logo}};
      \node[anchor=north east, inner sep=0, text=NeudataGrey, font=\small\neudata@headfont]
        at ([xshift=-2.3cm, yshift=-2.6cm]current page.north east)
        {\MakeUppercase{\neudata@doctype}};
      \draw[NeudataTeal, line width=0.8pt]
        ([xshift=2.3cm, yshift=-7.2cm]current page.north west) -- ([xshift=-2.3cm, yshift=-7.2cm]current page.north east);
      \node[anchor=north west, text width=\textwidth, align=left, inner sep=0]
        at ([xshift=2.3cm, yshift=-8.2cm]current page.north west) {%
          {\neudata@headfont\fontsize{30}{36}\selectfont\bfseries\color{NeudataTeal}\@title\par}
          \ifx\neudata@subtitle\@empty\else\vspace{0.8em}{\neudata@headfont\Large\color{NeudataNavy}\neudata@subtitle\par}\fi
          \ifx\neudata@client\@empty\else\vspace{2.2em}{\large Prepared for: \textbf{\neudata@client}\par}\fi};
      \node[anchor=south west, text width=\textwidth, align=left, inner sep=0]
        at ([xshift=2.3cm, yshift=3.4cm]current page.south west) {%
          {\large \neudata@authorline\par}\vspace{0.4em}
          {\normalsize Date: \textbf{\@date}\par}
          \ifx\neudata@reference\@empty\else{\normalsize Reference: \neudata@reference\par}\fi
          \ifx\neudata@version\@empty\else{\normalsize Version: \neudata@version\par}\fi
          \ifx\neudata@confidentiality\@empty\else\vspace{0.8em}{\bfseries\color{NeudataBlue}\MakeUppercase{\neudata@confidentiality}\par}\fi};
      \node[anchor=south, inner sep=0, text=NeudataTeal, font=\small]
        at ([yshift=1.6cm]current page.south)
        {Neudata Consulting Ltd \,|\, \href{https://www.neu-data.com}{www.neu-data.com} \,|\, contact@neu-data.com};
      \node[anchor=south, inner sep=0, text=NeudataGrey, font=\footnotesize\itshape]
        at ([yshift=1.1cm]current page.south) {Insight. Impact. Innovation.};
    \end{tikzpicture}
  \end{titlepage}}

\renewcommand{\maketitle}{%
  \ifneudata@cover
    \ifneudata@styleone\neudata@coverone\else\neudata@covertwo\fi
    \setcounter{page}{1}%
  \else
    \begin{center}
      \includegraphics[height=2cm]{\neudata@logo}\par\vspace{1em}
      {\neudata@headfont\LARGE\bfseries\color{NeudataHeading}\@title\par}
      \ifx\neudata@subtitle\@empty\else\vspace{0.4em}{\large\color{NeudataNavy}\neudata@subtitle\par}\fi
      \vspace{0.8em}{\@author\ \,|\, \@date\par}
    \end{center}\vspace{1em}
  \fi
  \ifx\neudata@abstract\@empty\else
    \begin{neudatasummary}[\ifneudata@styleone Abstract\else Summary\fi]\neudata@abstract\end{neudatasummary}
    \vspace{1em}
  \fi}

% ----- Draft watermark ----------------------------------------------------------------
\ifneudata@draft
  \AddToHook{shipout/background}{%
    \put(0,0){\begin{tikzpicture}[remember picture, overlay]
      \node[rotate=45, text=NeudataGrey!25, font=\fontsize{90}{90}\selectfont\bfseries]
        at (current page.center) {DRAFT};
    \end{tikzpicture}}}
\fi

% ----- Boxes ---------------------------------------------------------------------------
% \begin{keyfindings} ... \end{keyfindings}
\newtcolorbox{keyfindings}[1][Key findings]{%
  enhanced, breakable, colback=NeudataMist, colframe=NeudataTeal,
  boxrule=0pt, leftrule=4pt, arc=0pt, outer arc=0pt,
  left=10pt, right=10pt, top=6pt, bottom=6pt,
  fonttitle=\bfseries, coltitle=\ifneudata@styleone NeudataNavy\else NeudataTeal\fi,
  colbacktitle=NeudataMist, title=#1, titlerule=0pt}
% \begin{neudatanote}[Title] ... \end{neudatanote}
\ifneudata@styleone
  \newtcolorbox{neudatanote}[1][Note]{%
    enhanced, breakable, colback=white, colframe=NeudataLogoBlue!45, boxrule=0.6pt, arc=2pt,
    fonttitle=\bfseries, coltitle=NeudataLogoBlue, colbacktitle=NeudataLogoBlue!12,
    title={\textcolor{NeudataLogoBlue}{\textsf{i}}\ #1}}
\else
  \newtcolorbox{neudatanote}[1][Note]{%
    enhanced, breakable, colback=white, colframe=NeudataBlue, boxrule=0.6pt, arc=2pt,
    fonttitle=\bfseries, coltitle=white, colbacktitle=NeudataBlue, title=#1}
\fi

% ----- Tables: navy header row (+ striped body in style 1) --------------------------------
% Header row:  \neudataheader \neudatath{Group} & \neudatath{N} & \neudatath{\%} \\
\newcommand{\neudataheader}{\rowcolor{NeudataNavy}}
\newcommand{\neudatath}[1]{\textcolor{white}{\textbf{#1}}}
\ifneudata@styleone
  \AtBeginEnvironment{tabular}{\rowcolors{2}{white}{NeudataMist}}
\fi

% ----- Closing copyright page ------------------------------------------------------------
\newcommand{\neudatacopyrightpage}{%
  \clearpage\thispagestyle{plain}
  \vspace*{\fill}
  \noindent\makebox[\textwidth][c]{\includegraphics[height=4.2cm]{\neudata@logo}}\par
  \vspace{1.5em}
  {\large\bfseries\color{\ifneudata@styleone NeudataNavy\else NeudataBlue\fi}Copyright \textcopyright\ Neudata, \the\year\par}\medskip
  {\ttfamily\small\frenchspacing
  All property rights and copyright are reserved.\par\smallskip
  This document contains proprietary data, information, and materials developed by Neudata for
  dedicated use only and may not be communicated, copied, reproduced, distributed, published, or
  cited, in whole or in part, without the prior written consent of Neudata. Where explicit written
  permission has been granted, appropriate attribution must be included, followed by ``by courtesy
  of Neudata''.\par\smallskip
  Any unauthorized use or infringement of these materials may give rise to legal action and claims
  for damages, without prejudice to any other rights of Neudata, including rights relating to
  patents, trademarks, or other forms of intellectual property protection.\par}
  \vspace*{\fill}}

\endinput
