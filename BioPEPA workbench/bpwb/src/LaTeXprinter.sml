(* 
 File: LaTeXprinter.sml

 The structure which prints the input BioPEPA model in LaTeX syntax.
*)

structure LaTeXprinter 
       :> LaTeXprinter 
= struct

  val commentDepth = ref 0
  val inTextMode = ref true

  datatype token = 
      Ident of BioPEPA.Identifier
    | Whitespace of string
    | Float of string
    | Symbol of char

  fun isIdChar c = Char.isAlphaNum c orelse Char.contains "'_:@-" c

  fun isPepaSym c = Char.contains "/{}<>().,=+#;_-*" c

  fun last s = if s = "" then #" " else List.hd(List.rev (explode s))

  fun analyse []  = []
    | analyse (a::x) =
        if Char.isSpace a then getwhitespace [a] x else 
	if isPepaSym a then Symbol a :: analyse x else 
	if Char.isDigit a then getnumber [a] x else 
        if Char.isAlpha a then getword [a] x else
           Symbol a :: analyse x
  and getwhitespace l [] = [space l]
    | getwhitespace l (a::x) = 
        if Char.isSpace a
	then getwhitespace (a::l) x
	else space l :: analyse (a::x)
  and getword l [] = [ident l]
    | getword l (a::x) = 
        if isIdChar a
	then getword (a::l) x
	else ident l :: analyse (a::x)
  and getnumber l [] = [float l]
    | getnumber l (a::x) = 
        if Char.isDigit a 
	   orelse a = #"." 
	   orelse a = #"E" (* scientific notation *)
	then getnumber (a::l) x
	else float l :: analyse (a::x)
  and getrate l [] = [float l]
    | getrate l (a::x) = 
        if a <> #"]" 
        then getrate (a::l) x
	else float (a::l) :: analyse (x)

  and space l = 
        Whitespace ((implode (rev l)))
  and ident l = 
        Ident ((implode (rev l)))
  and float l = 
        Float (implode (rev l))

  fun isComponent r = Char.isUpper (List.hd (explode r))
  fun isActivity r = Char.isAlpha (List.hd (explode r))
  fun isFloat r = Char.isDigit (List.hd (explode r))

  val style = StyleSheet.lookup

  fun newLineToNewLinePercent c = 
      if c = #"\n" then "%\n" else str c
  fun quoteNewLine s = String.translate newLineToNewLinePercent s

  fun pr [] = if !inTextMode then "\n" else "\\end{eqnarray*}\n"
    | pr ((Ident a)::t) = 
               if !commentDepth > 0 then 
                  a ^ pr t
               else if !inTextMode then
	                   (inTextMode := false ; 
	                   "\\begin{eqnarray*}\n" ^ style a ^ pr t)
                    else style a ^ pr t
    | pr ((Symbol #"=")::t) = 
               if !inTextMode then
	          "=" ^ pr t
               else "& = &" ^ pr t
    | pr ((Symbol #";")::t) = 
               if !inTextMode then
	          ";" ^ pr t
               else "\\\\" ^ pr t
    | pr ((Symbol (#"/"))::
                (Symbol (#"*"))::t) =
                (if !inTextMode = false
                then "\\end{eqnarray*}\n" else "") ^ 
                (commentDepth := !commentDepth + 1 ; 
                 inTextMode := true;
                 pr (dr t))
    | pr ((Symbol (#"*"))::
                (Symbol (#"/"))::t) =
                (commentDepth := !commentDepth - 1 ; 
                 pr t)
    | pr ((Symbol (#"<"))::
                (Symbol (#">"))::t) =
                if !inTextMode 
                then "\\ensuremath{\\parallel}" ^ pr t
                else "{\\parallel}" ^ pr t
    | pr ((Symbol (#"<"))::
                (Symbol (#"<"))::t) =
                if !inTextMode 
                then "\\ensuremath{\\downarrow}" ^ pr t
                else "{\\downarrow}" ^ pr t
    | pr ((Symbol (#">"))::
                (Symbol (#">"))::t) =
                if !inTextMode 
                then "\\ensuremath{\\uparrow}" ^ pr t
                else "{\\uparrow}" ^ pr t
    | pr ((Symbol (#"<"))::t) =
                if inActivitySet t
                then "\\sync{\\{" ^ prAct t
                else "<" ^ pr t
    | pr ((Symbol (#"("))::
                (Symbol (#"+"))::
		(Symbol #")")::t) =
                if !inTextMode 
                then "\\ensuremath{\\oplus}" ^ pr t
                else "\\oplus " ^ pr t
    | pr ((Symbol (#"("))::
                (Symbol (#"-"))::
		(Symbol #")")::t) =
                if !inTextMode 
                then "\\ensuremath{\\ominus}" ^ pr t
                else "\\ominus " ^ pr t
    | pr ((Symbol (#"("))::
                (Symbol (#"."))::
		(Symbol #")")::t) =
                if !inTextMode 
                then "\\ensuremath{\\odot}" ^ pr t
                else "\\odot " ^ pr t
    | pr ((Symbol #"*")::t) = 
               if !inTextMode then
	          "*" ^ pr t
               else "\\times " ^ pr t
    | pr ((Symbol #"(")::t) = 
               if !commentDepth > 0 then 
                  "(" ^ pr t
               else if !inTextMode then
	                   (inTextMode := false ; 
	                   "\\begin{eqnarray*}\n(" ^ pr t)
                    else "(" ^ pr t
    | pr ((Symbol c)::t) = 
                str c ^ pr t
    | pr ((Float f)::t) = 
                f ^ pr t
    | pr ((Whitespace a)::t) = 
               if !commentDepth > 0 then a ^ pr t else
               if !inTextMode
               then if a = "\n" then "%\n" ^ pr t else a ^ pr t
               else quoteNewLine a ^ pr t
  and dr ((Whitespace a)::t) = 
         if a = "\n" then (Whitespace "%\n") :: dr t else ((Whitespace a)::t)
    | dr t = t
  and prAct ((Symbol #">")::t) = "\\}}" ^ pr t
    | prAct ((Symbol #",")::t) = "," ^ prAct t
    | prAct ((Whitespace w)::t) = w ^ prAct t
    | prAct ((Ident i)::t) = style i ^ prAct t
    | prAct _ = Error.fatal_error "Not in activity set ..."
  and inActivitySet ((Symbol #">")::t) = true
    | inActivitySet ((Symbol #",")::t) = inActivitySet t
    | inActivitySet ((Whitespace w)::t) = inActivitySet t
    | inActivitySet ((Ident i)::t) = inActivitySet t
    | inActivitySet _ = false

 
(*
    fun compress (#"\n" :: #"\n" :: t) = compress (#"\n" :: t)
      | compress (c::t) = c::compress t
      | compress [] = []
*)
    fun printReport s = 
      let 
          val basename = JobName.showBasename()
          val texfilename = basename ^ ".tex"
          val rep = TextIO.openOut(texfilename)
              handle _ => Error.fatal_error("Could not open " ^ texfilename)

          fun pr s = (TextIO.output(rep, s); TextIO.flushOut rep)
              handle _ => 
                 Error.fatal_error("Could not write to " ^ texfilename)

          fun lpr s = pr("\\" ^ s ^ "\n")
          fun lookup s = Configuration.lookup ("biopepa.report." ^ s)
          val resultsformat = Configuration.lookup ("gnuplot.results.format")
          val scale = lookup ("image.scale")
          fun graphics [] = ()
            | graphics [s] = 
              lpr ("includegraphics[scale=" ^ 
                     scale ^ "]{" ^ resultsformat ^ "/" ^ s ^ "}")
            | graphics (h::t) = 
              (graphics [h]; lpr "hfill"; graphics t)
          fun newcommand name 0 command = 
              "newcommand{\\" ^ name ^ "}{\\" ^ command ^ "}"
            | newcommand name n command = 
              "newcommand{\\" ^ name ^ "}[" ^ Int.toString n ^ "]{\\" ^ command ^ "}"
       in 
	  lpr "nonstopmode";
	  lpr "documentclass{llncs}";
 	  lpr "usepackage{graphicx}";
 	  lpr "usepackage{verbatim}";
 	  lpr "usepackage{amssymb}";
          lpr (newcommand 
		   "Butterfly" 
		   0 
		   "mbox{\\large $\\rhd\\!\\!\\!\\lhd$}");
	  lpr (newcommand 
		   "sync" 
		   1 
		   "raisebox{-1.0ex}{$\\;\\stackrel{\\Butterfly}{\\scriptscriptstyle #1}\\,$}") ;
	  lpr ("title{" ^ (lookup "title") ^ "}");
	  lpr ("author{" ^ (lookup "author") ^ "}");
	  lpr ("institute{" ^ (lookup "institute") ^ "}");
 	  lpr "begin{document}";
 	  lpr "maketitle";
 	  lpr "section{Bio-PEPA model}";
 	  lpr ("input{" ^ basename ^ "_biopepa}");
          lpr "begin{figure}[htbp]";
          lpr "begin{center}";
          lpr ("includegraphics[width=0.6\\textwidth]{dot/" ^basename^ ".pdf}");
 	  lpr "caption{Reaction network}";
          lpr "end{center}";
          lpr "end{figure}";
          lpr "newpage";
 	  lpr "section{Graphs from Bio-PEPA model}";
	  graphics (Gnuplot.generated()); 
 	  lpr "appendix";
 	  lpr "newpage";
 	  lpr "section{Bio-PEPA input file}";
 	  lpr ("verbatiminput{" ^ basename ^ ".biopepa}");
 	  lpr "newpage";
 	  lpr "section{Dizzy equivalent input file}";
 	  lpr ("verbatiminput{dizzy/" ^ basename ^ "001.dizzy}");
 	  lpr "newpage";
 	  lpr "section{PRISM equivalent input file}";
 	  lpr ("verbatiminput{prism/" ^ basename ^ "001.sm}");
 	  lpr "end{document}";
	  TextIO.closeOut(rep)
              handle _ => 
                 Error.fatal_error("Could not close " ^ texfilename)
      end

    fun print s = 
      let 
	  val _ = printReport s 
          val os = TextIO.openOut(JobName.showBasename() ^ "_biopepa.tex") 
       in 
	  TextIO.output(os, pr(analyse (s)));
	  TextIO.closeOut(os)
      end

end;
