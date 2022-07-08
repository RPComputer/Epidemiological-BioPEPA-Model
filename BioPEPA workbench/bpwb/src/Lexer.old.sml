(* 
 File: Lexer.sml

 Structure which performs lexical analysis of PEPA Net models.
*)

structure Lexer :> Lexer =
struct
  datatype token = 
      Ident of PepaNets.Identifier
    | Symbol of char

  fun isIdChar c = Char.isAlphaNum c orelse Char.contains "'_*-" c

  fun isPepaSym c = Char.contains "/{}<>().,=+#;_[]-" c

  fun analyse []  = []
    | analyse (a::x) =
        if Char.isSpace a then analyse x else 
	if isPepaSym a then Symbol a :: analyse x else 
        if Char.isAlpha a then getword [a] x else
	   Error.lexical_error ("Unrecognised token "
				      ^ implode (a :: x))
  and getword l [] = [ident l]
    | getword l (a::x) = 
        if isIdChar a
	then getword (a::l) x
	else ident l :: analyse (a::x)

  and ident l = 
        Ident (case implode (rev l) of 
	        "infty" => HashTable.literal "infty"
	       | s      => HashTable.hash s)

end;
