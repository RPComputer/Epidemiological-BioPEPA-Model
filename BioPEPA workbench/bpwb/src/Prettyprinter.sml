(* 
 File: Prettyprinter.sml

 The structure which prettyprints BioPEPA models
*)

structure Prettyprinter :> Prettyprinter =
struct

  open BioPEPA

  datatype printMode = compressed | uncompressed | verbose

  val mode = ref compressed

  fun setMode m = mode := m

  fun printId s = s
  val printRate = printId

  fun print (INCREASES ((alpha, rate), P)) 
    = "("^printId alpha^", "^printRate rate^") >> "^(print P)
    | print (DECREASES ((alpha, rate), P)) 
    = "("^printId alpha^", "^printRate rate^") << "^(print P)
    | print (CHOICE (P, Q)) 
      = (print P) 
	^ (if !mode = verbose then "\n    + " else " + ")
        ^ (print Q)
    | print (COOP (P, Q, L)) 
      = (if !mode = verbose then "(" else "")
	^ (print P) 
	^ (if !mode = verbose then " <" ^ (printList L) ^ "> " else "|")
	^ (print Q) 
        ^ (if !mode = verbose then ")" else "")
    | print (HIDING (P, [])) 
      = (print P)
    | print (HIDING (P, L)) 
      = (print P) ^ "/{" ^ (printList L) ^ "}"
    | print (VAR I) 
      = printId I
    | print (RATE I) 
      = printId I
    | print (CONSTS (I, P, L)) 
      = (printId I)
   	 ^ (if !mode = verbose andalso notRate P then " =\n      " else " = ")
         ^ (print P) ^ ";\n" ^ (print L)
  and printlist [] = ""
    | printlist [x] = print x
    | printlist (h::t) = print h ^ ", " ^ printlist t
 
  and notRate (VAR I) = not (isNumeric I)
    | notRate _ = true
  and isNumeric s = List.all isNumericChar (explode s)
  and isNumericChar c = Char.isDigit c orelse c = #"."
  and printPriorities nil  = ""
    | printPriorities [x]  = printList x
    | printPriorities (x::y::z)  = printList x ^ " > " ^ (printPriorities (y::z))
  and printList nil        = ""
    | printList [x]        = printId x
    | printList (x::y::z)  = printId x ^ ", " ^ (printList (y::z))
  and printMarking nil     = ""
    | printMarking [x]     = print x
    | printMarking (x::xs) = print x ^ (if !mode = compressed then "," else ",\n\t ") ^ 
                             printMarking xs

  fun pad s = s ^ "   "
  fun print' indent (INCREASES ((alpha, rate), P)) 
    = "("^printId alpha^", "^printRate rate^") >> "^(print' indent P)
    | print' indent (DECREASES ((alpha, rate), P)) 
    = "("^printId alpha^", "^printRate rate^") << "^(print' indent P)
    | print' indent (CHOICE (P, Q)) 
      = let val indent' = pad indent 
        in (print' indent' P) 
	   ^ ("\n     + ")
           ^ (print' indent' Q)
        end
    | print' indent (COOP (P, Q, L)) 
      = let val indent' = pad indent 
        in "("
  	   ^ (print' indent' P) 
	   ^ (if L = [] then "\n" ^ indent ^ "<>\n" ^ indent  else 
              ("\n" ^ indent ^ "<\n" ^ indent' ^ (printList' indent' L) ^ 
               "\n" ^ indent ^ ">\n" ^ indent))
	   ^ (print' indent' Q) 
           ^ ")"
        end
    | print' indent (HIDING (P, [])) 
      = (print' indent P)
    | print' indent (HIDING (P, L)) 
      = let val indent' = pad indent 
        in (print' indent P) ^ "/{" ^ (printList' indent' L) ^ "}"
        end
    | print' indent (VAR I) 
      = printId I
    | print' indent P = print P
  and printList' indent nil        = ""
    | printList' indent [x]        = printId x
    | printList' indent (x::y::z)  = printId x ^ ",\n" ^ indent ^ 
                                     (printList' indent (y::z))
 
  val print = fn P => if !Options.compile then (print' "" P) ^ "\n" else print P
end;
