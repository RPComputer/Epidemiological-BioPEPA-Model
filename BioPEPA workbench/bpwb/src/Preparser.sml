(* 
 File: Preparser.sml

 The structure which contains the BioPEPA preparser 
 which checks for simple syntax errors.
*)

structure Preparser 
       :> Preparser 
= struct

  fun isComponent r = Char.isUpper (List.hd (explode r))
  fun isActivity r = Char.isAlpha (List.hd (explode r))
  fun isFloat r = Char.isDigit (List.hd (explode r))

  fun preparse [] = 
      if Stack.empty() then [] 
      else Error.fatal_error("Bracket not closed at end of file.")
    | preparse ((Lexer.Symbol (#"\\"))::t) =
                preparse t
    | preparse ((Lexer.Symbol (#"&"))::t) =
                preparse t
    | preparse (h::(Lexer.Symbol (#"&"))::t) =
                preparse (h::t)
    | preparse ((Lexer.Ident (a))::(Lexer.Ident (b))::_) =
	  if isComponent a andalso isComponent b
	     then Error.fatal_error ("BioPEPA parser:: Semicolon expected between species identifiers '"
			 ^ a ^ "' and '" ^ b ^ "'")
	     else if isActivity a andalso isFloat b
		  then Error.fatal_error ("BioPEPA parser:: Comma expected between reaction '"
			      ^ a ^ "' and stoichiometry '" ^ b ^ "'")
		  else if isActivity a andalso isActivity b
		       then Error.fatal_error ("BioPEPA parser:: Comma expected between '"
				   ^ a ^ "' and '" ^ b ^ "'")
		       else Error.fatal_error ("BioPEPA parser:: Separator expected between '"
				   ^ a ^ "' and '" ^ b ^ "'")
    | preparse ((Lexer.Symbol (#")"))::
		     (Lexer.Ident (b))::_) =
	       Error.fatal_error
		   ("BioPEPA parser:: Period expected between closing bracket and '"
			 ^ b ^ "'")
    | preparse ((Lexer.Ident (a))::
		     (Lexer.Symbol (#"("))::_) =
	       Error.fatal_error
		   ("BioPEPA parser:: Equals expected between '" ^
		      a ^ "' and opening bracket")
(*
    | preparse ((Lexer.Symbol (#")"))::
		     (Lexer.Symbol (#"("))::_) =
	       Error.fatal_error
		   ("BioPEPA parser:: Operator expected between closing and opening bracket")
*)
    | preparse ((Lexer.Symbol (#","))::
		     (Lexer.Symbol sym)::_) =
	       Error.fatal_error
		   ("BioPEPA parser:: Identifier expected between comma and '" ^ str sym ^ "'")
    | preparse ((Lexer.Symbol (#"+"))::(Lexer.Symbol (#"("))::t) =
		     (Lexer.Symbol (#"+"))::(preparse ((Lexer.Symbol (#"("))::t))
    | preparse ((Lexer.Symbol (#"("))::
                (Lexer.Symbol (#"+"))::
		(Lexer.Symbol #")")::t) =
                (Lexer.Symbol (#"("))::
                (Lexer.Symbol (#"+"))::
		(Lexer.Symbol #")")::preparse t
    | preparse ((Lexer.Symbol (#"("))::
                (Lexer.Symbol (#"-"))::
		(Lexer.Symbol #")")::t) =
                (Lexer.Symbol (#"("))::
                (Lexer.Symbol (#"-"))::
		(Lexer.Symbol #")")::preparse t
    | preparse ((Lexer.Symbol (#"("))::
                (Lexer.Symbol (#"."))::
		(Lexer.Symbol #")")::t) =
                (Lexer.Symbol (#"("))::
                (Lexer.Symbol (#"."))::
		(Lexer.Symbol #")")::preparse t
    | preparse ((Lexer.Symbol (#"+"))::
		     (Lexer.Symbol sym)::t) =
	       Error.fatal_error
		   ("BioPEPA parser:: Unexpected symbol found in choice.  Found '" ^
		     str sym ^ "' after '+'")
    | preparse ((Lexer.Symbol (#"/"))::
                (Lexer.Symbol (#"*"))::t) =
                skipcomment (map Lexer.Symbol (explode " */")) t
    | preparse ((Lexer.Symbol (#"("))::t) = 
                (Stack.push (#"(");
                 Lexer.Symbol (#"(")::preparse t)
    | preparse ((Lexer.Symbol (#")"))::t) = 
                (Stack.pop (#"(");
                 Lexer.Symbol (#")")::preparse t)
    | preparse (h::t) = h::(preparse t)
  and skipcomment _ ((Lexer.Symbol (#"*"))::
                (Lexer.Symbol (#"/"))::t) =
                preparse t
    | skipcomment l (h::t) = skipcomment (h::l) t
    | skipcomment l [] = 
                Error.fatal_error 
                  ("Preparser.  Comment not closed at the end of file.\n>>\t" ^
                   truncate 12 (List.rev l))
  and truncate 0 _ = "..."
    | truncate _ [] = ""
    | truncate n ((Lexer.Symbol h)::t) = str h ^ truncate (n - 1) t
    | truncate n ((Lexer.Ident i)::t) = i ^ " " ^ truncate (n - 1) t
    | truncate n ((Lexer.Float f)::t) = f ^ " " ^ truncate (n - 1) t

end;
