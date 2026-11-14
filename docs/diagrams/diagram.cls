\NeedsTeXFormat{LaTeX2e}
\ProvidesPackage{diagram}[2025/3/29]
\LoadClass[margin=5mm]{standalone}


\RequirePackage{tikz}
\RequirePackage{pxfonts}
\RequirePackage{enumitem}

\usetikzlibrary{calc,fit}


% Default value for /my/box options
\def\my@boxoptions{}


\pgfkeys{
  /my/.cd,
  id/.store in=\my@id,
  name/.store in=\my@name,
  at/.store in=\my@position,
  box/.style={},
  tag/.style={},
  below at/.code 2 args={\pgfkeysalso{/tikz/anchor=north west,at={($ (#1.south west) + (0,-#2) $)}}},
  below/.code={\pgfkeysalso{/tikz/anchor=north west,at={($ (#1.south west) + (0,-0.5) $)}}},  
  last module/.estore in=\lastmodule,
  below last/.default={0.5},
  below last/.style={below at={\lastmodule}{#1}},
  submodule of/.code={\pgfkeysalso{/modules/#1/.append/.expanded=(\my@id)}},
  layer/.initial=main,
  layern/.style={layer=layer #1},
}

\newcommand{\module}[2][1]{
  \pgfkeys{/my/.cd,box/.style={},tag/.style={},#1}
  \pgfkeys{/my/layer/.get=\my@layer}
  \pgfonlayer{\my@layer}
    \node[module box,inner sep=1mm,/my/box] (\my@id--box) at \my@position {\parbox{4cm}{\tiny #2}};
    \node[module tag,anchor=south west,/my/tag] (\my@id--tag) at (\my@id--box.north west) { \texttt{\my@name} };
    \node[module group,fit=(\my@id--box) (\my@id--tag)] (\my@id) {};
  \endpgfonlayer
  \pgfkeys{/my/last module=\my@id}
}

\newcommand{\supermodule}[1]{
  \pgfkeys{/my/.cd,#1}
  \pgfkeys{/modules/\my@id/.get=\submodules}
  \node[module box,fit=\submodules,inner sep=2mm,/my/box] (\my@id--box) {};
  \node[module tag,anchor=south west,/my/tag] (\my@id--tag) at (\my@id--box.north west) { \texttt{\my@name} };
  \node[module group,fit=(\my@id--box) (\my@id--tag)] (\my@id) {};
  \pgfkeys{/my/last module=\my@id}
}


\newcommand{\addsubmodule}[2]{
   \pgfkeys{/modules/#1/.append=(#2)}
}


% Styling for itemize lists
\setlist{nosep,leftmargin=1em,label={$\bullet$}}

% Unused as of yet, but meant to counteract the fact that inner modules are dimmer than their enclosing modules
\pgfdeclarelayer{layer 1}
\pgfdeclarelayer{layer 2}
\pgfdeclarelayer{layer 3}
\pgfdeclarelayer{layer 4}
\pgfdeclarelayer{layer 5}
\pgfsetlayers{main,layer 1,layer 2,layer 3,layer 4,layer 1,layer 5}

\newcommand{\microsail}{$\mu$Sail}