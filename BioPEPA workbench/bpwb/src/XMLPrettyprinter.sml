(* 
 File: XMLPrettyprinter.sml

 The structure which prettyprints BioPEPA models
 as XML output
*)

structure XMLPrettyprinter :> XMLPrettyprinter =
struct

  open BioPEPA

  datatype printMode = verbose

  val mode = ref verbose

  fun setMode m = mode := m

  fun pad 0 s = s
    | pad n s = " " ^ (pad (n - 1) s)

  fun BioPEPAsyntax s Component = 
      let val _ = Prettyprinter.setMode Prettyprinter.verbose
      in
        s ^ "<BioPEPAsyntax>\n" ^
        s ^ "  " ^ "<![CDATA[\n" ^
        s ^ "    " ^ (Prettyprinter.print Component) ^ "\n" ^
        s ^ "  " ^ "]]>\n" ^
        s ^ "</BioPEPAsyntax>\n" 
      end

  fun printId s = s
  val printRate = printId

  fun print s (Component as (INCREASES ((alpha, rate), P)))
      = s ^ "<Component>\n" ^
                   BioPEPAsyntax (pad 2 s) Component ^
	s ^ "  " ^ "<Increases>\n" ^ 
	s ^ "    " ^ "<Activity>\n" ^ 
			(printId' (pad 6 s) alpha) ^ 
			(printRate' (pad 6 s) rate) ^
	s ^ "    " ^ "</Activity>\n" ^ 
	                (print (pad 4 s) P) ^
	s ^ "  " ^ "</Increases>\n" ^
	s ^ "</Component>\n"
    | print s (Component as (DECREASES ((alpha, rate), P)))
      = s ^ "<Component>\n" ^
                   BioPEPAsyntax (pad 2 s) Component ^
	s ^ "  " ^ "<Decreases>\n" ^ 
	s ^ "    " ^ "<Activity>\n" ^ 
			(printId' (pad 6 s) alpha) ^ 
			(printRate' (pad 6 s) rate) ^
	s ^ "    " ^ "</Activity>\n" ^ 
	                (print (pad 4 s) P) ^
	s ^ "  " ^ "</Decreases>\n" ^
	s ^ "</Component>\n"
    | print s (Component as (CHOICE (P, Q)))
      = s ^ "<Component>\n" ^ 
                   BioPEPAsyntax (pad 2 s) Component ^
        s ^ "  " ^ "<Choice>\n" ^ 
                      (print (pad 4 s) P) ^
                      (print (pad 4 s) Q) ^ 
        s ^ "  " ^ "</Choice>\n" ^ 
        s ^ "</Component>\n"
    | print s (Component as (COOP (P, Q, L)))
      = s ^ "<Component>\n" ^ 
                   BioPEPAsyntax (pad 2 s) Component ^
        s ^ "  " ^ "<Cooperation>\n" ^ 
                      (print (pad 4 s) P) ^
                      (print (pad 4 s) Q) ^ 
        s ^ "    " ^ "<CooperationSet>\n" ^ 
                      (printList (pad 6 s) L) ^ 
        s ^ "    " ^ "</CooperationSet>\n" ^ 
        s ^ "  " ^ "</Cooperation>\n" ^ 
        s ^ "</Component>\n"
    | print s (Component as (HIDING (P, [])))
      = (print s P)
    | print s (Component as (HIDING (P, L)))
      = s ^ "<Component>\n" ^ 
                   BioPEPAsyntax (pad 2 s) Component ^
        s ^ "  " ^ "<Hiding>\n" ^ 
                      (print (pad 4 s) P) ^
                      (printList (pad 4 s) L) ^ 
        s ^ "  " ^ "</Hiding>\n" ^ 
        s ^ "</Component>\n"
    | print s (VAR I) 
      = s ^ "<Component>\n" ^ 
        s ^ "  " ^ "<Identifier>" ^ (printId I) ^ "</Identifier>\n" ^
        s ^ "</Component>\n"
    | print s (RATE I) 
      = s ^ "<Component>\n" ^ 
        s ^ "  " ^ "<Rate>" ^ (printId I) ^ "</Rate>\n" ^
        s ^ "</Component>\n"
    | print s (CONSTS (I, P, L)) 
      = s ^ "<Constant>\n" ^
        s ^ "  " ^ "<ConstantIdentifier>" ^ (printId I) ^ "</ConstantIdentifier>\n" ^
                    (print (pad 2 s) P) ^ 
        s ^ "</Constant>\n" ^
             (print s L)
  and printlist s [] = ""
    | printlist s [x] = print s x
    | printlist s (h::t) = print s h ^ printlist s t
  and notRate (VAR I) = not (isNumeric I)
    | notRate _ = true
  and isNumeric s = List.all isNumericChar (explode s)
  and isNumericChar c = Char.isDigit c orelse c = #"."
  and printList s nil        = ""
    | printList s [x]        = printId' s  x
    | printList s (x::y::z)  = printId' s  x ^ ", " ^ (printList s (y::z))
  and printId' s i = s ^ "<Id>" ^ i ^ "</Id>\n"
  and printRate' s r = s ^ "<Stoichiometry>" ^ printRate r ^ "</Stoichiometry>\n"

  val print = fn P => print "  " P

end;


