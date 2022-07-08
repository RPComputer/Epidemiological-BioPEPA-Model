(* 
 File: Lexer.sml

 Structure which performs lexical analysis of BioPEPA models.
*)

structure Lexer :> Lexer 
= struct
  datatype token = 
      Ident of BioPEPA.Identifier
    | Float of string
    | Symbol of char

  fun prettyprintToken (Ident i)  = "Identifier : " ^ i ^ "\n"
    | prettyprintToken (Float f)  = "     Float : " ^ f ^ "\n"
    | prettyprintToken (Symbol c) = "    Symbol : " ^ str(c) ^ "\n"

  fun isIdChar c = Char.isAlphaNum c orelse Char.contains "'_:@-" c

  fun isPepaSym c = Char.contains "/{}<>().,=+#;_-*&\\" c

  fun analyse []  = []
    | analyse (a::x) =
        if Char.isSpace a then analyse x else 
	if isPepaSym a then 
           if a = #"/" 
           then if length x > 0 andalso List.hd x = #"*"
                then skipcomment (List.tl x)
                else Symbol a :: analyse x
           else Symbol a :: analyse x else 
	if Char.isDigit a then getnumber [a] x else 
        if Char.isAlpha a then getword [a] x else
        if a = #"[" then getrate [a] x else
	   Error.lexical_error ("Unrecognised token "
				      ^ implode (a :: x))
  and skipcomment ((#"*")::(#"/")::t) = analyse t
    | skipcomment (h::t) = skipcomment t
    | skipcomment [] = Error.fatal_error("End of file occurred in comment.") 
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

  and ident l = 
        Ident ((implode (rev l)))
  and float l = 
        Float (implode (rev l))

end;
