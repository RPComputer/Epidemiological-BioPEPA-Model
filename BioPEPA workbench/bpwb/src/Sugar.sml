(* 
 File: Sugar.sml

 Structure which contains the BioPEPA process
 which removes syntactic sugar from models.  I.e. removes 
 keywords such as species and kineticLawOf.
*)

structure Sugar 
       :> Sugar 
= struct

  (* Need to loosen rules on names to allow any case? *)
  fun isComponent r = Char.isUpper (List.hd (explode r))
  fun isActivity r = Char.isAlpha (List.hd (explode r))
  fun isFloat r = Char.isDigit (List.hd (explode r))

  fun desugar [] = []
    | desugar ((Lexer.Ident (a))::(Lexer.Ident (b))::(Lexer.Symbol (#":"))::t) =
	  if a = "species"
          then skipSpecies b t
          else if a = "kineticLawOf" 
               then (Lexer.Ident b) :: (Lexer.Symbol (#"=")) :: (Lexer.Float ("1")) ::t
               else Error.fatal_error ("BioPEPA syntax:: Could not resolve '"
			 ^ a ^ " " ^ b ^ " : '")
    | desugar (h::t) = h::desugar t
(*
    | desugar ((Lexer.Symbol (#")"))::
		     (Lexer.Ident (b))::_) =
	       Error.fatal_error
		   ("BioPEPA parser:: Period expected between closing bracket and '"
			 ^ b ^ "'")
    | desugar ((Lexer.Ident (a))::
		     (Lexer.Symbol (#"("))::_) =
	       Error.fatal_error
		   ("BioPEPA parser:: Equals expected between '" ^
		      a ^ "' and opening bracket")
(*
    | desugar ((Lexer.Symbol (#")"))::
		     (Lexer.Symbol (#"("))::_) =
	       Error.fatal_error
		   ("BioPEPA parser:: Operator expected between closing and opening bracket")
*)
    | desugar ((Lexer.Symbol (#","))::
		     (Lexer.Symbol sym)::_) =
	       Error.fatal_error
		   ("BioPEPA parser:: Identifier expected between comma and '" ^ str sym ^ "'")
    | desugar ((Lexer.Symbol (#"+"))::(Lexer.Symbol (#"("))::t) =
		     (Lexer.Symbol (#"+"))::(desugar ((Lexer.Symbol (#"("))::t))
    | desugar ((Lexer.Symbol (#"("))::
                (Lexer.Symbol (#"+"))::
		(Lexer.Symbol #")")::t) =
                (Lexer.Symbol (#"("))::
                (Lexer.Symbol (#"+"))::
		(Lexer.Symbol #")")::desugar t
    | desugar ((Lexer.Symbol (#"("))::
                (Lexer.Symbol (#"-"))::
		(Lexer.Symbol #")")::t) =
                (Lexer.Symbol (#"("))::
                (Lexer.Symbol (#"-"))::
		(Lexer.Symbol #")")::desugar t
    | desugar ((Lexer.Symbol (#"("))::
                (Lexer.Symbol (#"."))::
		(Lexer.Symbol #")")::t) =
                (Lexer.Symbol (#"("))::
                (Lexer.Symbol (#"."))::
		(Lexer.Symbol #")")::desugar t
    | desugar ((Lexer.Symbol (#"+"))::
		     (Lexer.Symbol sym)::t) =
	       Error.fatal_error
		   ("BioPEPA parser:: Unexpected symbol found in choice.  Found '" ^
		     str sym ^ "' after '+'")
    | desugar ((Lexer.Symbol (#"/"))::
                (Lexer.Symbol (#"*"))::t) =
                skipcomment (map Lexer.Symbol (explode " */")) t
    | desugar ((Lexer.Symbol (#"("))::t) = 
                (Stack.push (#"(");
                 Lexer.Symbol (#"(")::desugar t)
    | desugar ((Lexer.Symbol (#")"))::t) = 
                (Stack.pop (#"(");
                 Lexer.Symbol (#")")::desugar t)
    | desugar (h::t) = h::(desugar t)
  and skipcomment _ ((Lexer.Symbol (#"*"))::
                (Lexer.Symbol (#"/"))::t) =
                desugar t
    | skipcomment l (h::t) = skipcomment (h::l) t
    | skipcomment l [] = 
                Error.fatal_error 
                  ("Sugar.  Comment not closed at the end of file.\n>>\t" ^
                   truncate 12 (List.rev l))
  and truncate 0 _ = "..."
    | truncate _ [] = ""
    | truncate n ((Lexer.Symbol h)::t) = str h ^ truncate (n - 1) t
    | truncate n ((Lexer.Ident i)::t) = i ^ " " ^ truncate (n - 1) t
    | truncate n ((Lexer.Float f)::t) = f ^ " " ^ truncate (n - 1) t
*)
  and skipSpecies name [] = Error.fatal_error ("BioPEPA syntax: ill-formed definition of '" ^ name ^ "' missing terminator")
    | skipSpecies name ((Lexer.Symbol (#";"))::t) = desugar t
    | skipSpecies name (h::t) = h::skipSpecies name t
end;
