(* 
 File: Parser.sml

 The structure which contains the BioPEPA parser
*)

structure Parser :> Parser =
struct

(* Parser tools.  Taken from Chris Reade's
   book ``Elements of Functional Programming''.
*)

(* Utility functions *) 

fun pair a b = (a,b)
fun fst(x,y) = x
fun snd(x,y) = y

fun consonto x a = a :: x
fun fold f [] b     = b
  | fold f (h::t) b = f (h, fold f t b)
fun link llist = fold (op @) llist []

datatype 'a possible = 
    Ok of 'a 
  | Fail

type 'a parser = Lexer.token list -> ('a * Lexer.token list) possible

infixr 4 <&>
infixr 3 <|>
infix  0 modify
 
fun (parser1 <|> parser2) s =
    let fun parser2_if_fail Fail = parser2 s
          | parser2_if_fail x    = x
    in
        parser2_if_fail (parser1 s)
    end

fun (parser modify f) s =
    let fun modresult Fail        = Fail
	  | modresult (Ok (x, y)) = Ok (f x, y)
    in
        modresult (parser s)
    end

fun (parser1 <&> parser2) s =
    let fun parser2_after Fail          = Fail
	  | parser2_after (Ok (x1, s1)) = (parser2 modify (pair x1)) s1
    in
        parser2_after (parser1 s)
    end
 
fun emptyseq s = Ok ([], s)

fun optional pr = (pr modify (consonto []))
                  <|> emptyseq

fun sequence pr =
    let fun seqpr s = ((pr <&> seqpr modify (op ::))
                       <|> emptyseq) s
    in
        seqpr
    end

fun seqwith (front, sep, back) pr =
    let val sep_pr = sep <&> pr               modify snd
        val items  = pr  <&> sequence sep_pr  modify (op ::)	
    in
	front <&> optional items <&> back modify (link o fst o snd)
    end

fun parserList []           = emptyseq
  | parserList (pr :: rest) = pr <&> (parserList rest) modify (op ::)

fun alternatives []           = (fn x => Fail)
  | alternatives (pr :: rest) = pr <|> alternatives rest;
    
 
(* Basic parsers *)

fun variable (Lexer.Ident x :: s)   = Ok (x, s)
  | variable other                  = Fail 

fun variableOrFloat (Lexer.Ident x :: s)   = Ok (x, s)
  | variableOrFloat (Lexer.Float x :: s)   = Ok (x, s)
  | variableOrFloat  other                 = Fail 

fun float (Lexer.Float x :: s)      = Ok (x, s)
  | float other                     = Fail
                                                                                
fun literal a (Lexer.Symbol x :: s) = if a = x then Ok (x, s) else Fail
  | literal a other                 = Fail;
    
fun sugar s = optional (literal s)

fun either x y z = (x <|> y <|> z)

(* A parser for PEPA.
*)


fun unparenth (bra, (e, ket)) = e;
    
fun parenseq s = seqwith (literal #"(", literal #",", literal #")") s;

fun bracedseq s = seqwith (literal #"{", literal #",", literal #"}") s;

fun angledseq s = seqwith (literal #"<", literal #",", literal #">") s;

val actlist : BioPEPA.Identifier list parser    = bracedseq variable;

val cooplist: BioPEPA.Identifier list parser    = angledseq variable;

(* val componentList: BioPEPA.Component list parser = parenseq agenta; *)

fun debug s = (); 
fun debugActive s = (TextIO.output(TextIO.stdOut, s ^ "\n"));

fun def  s   = (debug "starting def" ; 
               ((adef <&> optional (literal #";" <&> def) modify opt_consts)
               <|> agenta                                                   ) s)

and adef s   = (debug "  starting adef" ; 
               ((optional (literal #"#")  <&> variable  <&>
                literal #"="     <&> agenta                 modify mk_const)) s)

and varList s = (debug "starting varlist" ; 
               (((variable <&> literal #"," <&> varList)    modify mk_varList')
                <|> (variable                                 modify mk_varList)) s)

and agenta s = (debug "    starting agenta" ; 
               (agentb <&> optional (literal #"/" <&> actlist) 
                                                           modify mk_hiding) s)

and agentb s = (debug "      starting agentb" ; 
               (agentc <&> optional (literal #"+" <&> agentb) 
                                                             modify mk_plus) s)

and agentc s = (debug "        starting agentc" ; 
               (agent <&> optional (cooplist <&> agentc) 
                                                             modify mk_coop) s)

and agent s  = (debug "        starting agent" ; 
               (debug "          trying INCREASES" ; 
               ((sugar #"("    <&> variable    <&>
                 sugar #","    <&> optional variableOrFloat    <&>
                 sugar #")"    <&> literal #">" <&> literal #">" <&> 
                 optional agent                              modify mk_increases)
               <|> 
                (debug "          trying DECREASES" ; 
                 (sugar #"("    <&> variable    <&>
                  sugar #","    <&> optional variableOrFloat    <&>
                  sugar #")"    <&> literal #"<" <&> literal #"<" <&> 
                 optional agent                              modify mk_decreases))
               <|> 
                (debug "          trying MOD" ; 
                 (sugar #"("    <&> variable    <&>
                  sugar #","    <&> optional variableOrFloat    <&>
                  sugar #")"    <&> literal #"(" <&> 
                                       either (literal #"+") (literal #"-") (literal #".") <&>  
                                    literal #")" <&> 
                 optional agent                              modify mk_mod))
               <|> 
                (debug "          trying PARENS" ; 
                 (literal #"(" <&> agenta      <&> literal #")" 
                                                             modify unparenth))
               <|>
                (debug "          trying VAR" ; 
                 (variable                                   modify mk_agent))
               <|> 
                (debug "          trying FLOAT" ; 
                 (float                                      modify mk_rate_literal))) s))

and mk_agent s                               = BioPEPA.VAR s

and mk_rate_literal s                        = BioPEPA.RATE s

and mk_const (hash, (i, (eq, e)))            = (i, e)

and mk_hiding (P, [])                        = P
  | mk_hiding (P, [(slash, L)])              = BioPEPA.HIDING (P, L)
  | mk_hiding (P, _)                         = error ("hiding", P)

and mk_plus (P, [])                          = P
  | mk_plus (P, [(plus, Q)])                 = BioPEPA.CHOICE (P, Q)
  | mk_plus (P, _)                           = error ("plus", P)

and mk_coop (P, [])                          = P
  | mk_coop (P, [(L, Q)])                    = BioPEPA.COOP (P, Q, L)
  | mk_coop (P, _)                           = error ("cooperation", P)

and mk_patched_rate []                       = "1"
  | mk_patched_rate [rate]                   = rate
  | mk_patched_rate (h::t)                   = Error.fatal_error("parsing stoichiometry " ^ h)

and mk_patched_component []                  = BioPEPA.VAR ""
  | mk_patched_component [P]                 = P
  | mk_patched_component (h::t)              = Error.fatal_error("parsing component " ^ 
                                                       Prettyprinter.print h)

and mk_increases (bra, (alpha, (comma, (rate, (ket, (gt1, (gt2, P)))))))
                                             = (debug "increases" ; 
                                                BioPEPA.INCREASES ((alpha, mk_patched_rate rate), 
                                                   mk_patched_component P))

and mk_decreases (bra, (alpha, (comma, (rate, (ket, (lt1, (lt2, P)))))))
                                             = (debug "decreases" ; 
                                                BioPEPA.DECREASES ((alpha, mk_patched_rate rate), 
                                                   mk_patched_component P))

and mk_mod          (bra, (alpha, (comma, (rate, (ket, (_, (mod_, (_, P))))))))
                                             = (debug "mod" ; 
                                                BioPEPA.INCREASES ((alpha, "0"), 
                                                     mk_patched_component P))

and mk_varList (var)                         = [var]
and mk_varList' (var, (comma, varList))      = var:: varList

and opt_consts ((i, e1), [(oper, e2)])       = BioPEPA.CONSTS (i, e1, e2)
  | opt_consts ((i, P), _)                   = error ("constants", P)

and error (s, P)                             = (Prettyprinter.setMode Prettyprinter.uncompressed;
                                                Error.parse_error ("[" ^ s ^ "] at or near:\n\t" ^ 
                                                                   Prettyprinter.print P))
     
local
  fun lit (Lexer.Symbol #";") = ";\n\t"
    | lit (Lexer.Symbol #"=") = " = "
    | lit (Lexer.Symbol #"+") = " + "
    | lit (Lexer.Symbol #",") = ", "
    | lit (Lexer.Symbol #"<") = "<"
    | lit (Lexer.Symbol #">") = ">"
    | lit (Lexer.Symbol c) = str c
    | lit (Lexer.Float f)  = f
    | lit (Lexer.Ident s)  = s
in
  fun report Fail         = Error.parse_error "ill-formed BioPEPA model definition"
    | report (Ok (c, [])) = c
    | report (Ok (c, x))  = Error.parse_error (String.concat 
                                  ("Unparsed :-\n" ::  (map lit x)))
end

(* Only for debugging *)
fun show C = (Prettyprinter.setMode Prettyprinter.verbose;
              TextIO.output (TextIO.stdOut, Prettyprinter.print C ^ "\n"); C)

fun substitute I (BioPEPA.CHOICE(P,Q)) = BioPEPA.CHOICE(substitute I P, substitute I Q)
  | substitute I (BioPEPA.INCREASES (a, P)) = BioPEPA.INCREASES(a, substitute I P)
  | substitute I (BioPEPA.DECREASES (a, P)) = BioPEPA.DECREASES(a, substitute I P)
  | substitute I (BioPEPA.VAR "") = BioPEPA.VAR I
  | substitute I C = C
fun desugar (BioPEPA.CONSTS(I, def, C)) = BioPEPA.CONSTS(I, substitute I def, desugar C)
  | desugar C = C

val parse = desugar o report o def

end;
