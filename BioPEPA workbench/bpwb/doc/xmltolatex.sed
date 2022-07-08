s/<div/%%%<div/g
s/<\/div/%%%<\/div/g
s/<h1>/\\subsection{/g
s/<\/h1>/}/g
s/<h3>/\\subsubsection{/g
s/<\/h3>/}/g
s/<p>//g
s/<\/p>/%/g
s/<p\/>//g
s/<br\/>//g
s/<tt>/\\texttt{/g
s/<\/tt>/}/g
s/<code>/\\begin{verbatim}/g
s/<\/code>/\\end{verbatim}/g
s/ target="_blank"//g
s/<a href=/\\ahref{/g
s/<\/a>/}/g
s/">/}{/g
s/"//g
s/C:\\/C:\\textbackslash /g 
s/&nbsp;/~/g
s/    / /g
