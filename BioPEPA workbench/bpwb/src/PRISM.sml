(* 
 File: PRISM.sml

 PRISM compilation for the Bio-PEPA Workbench
*)

structure PRISM :> PRISM =
struct

  val parameters = ref [] : ((string * string) list) ref

  local
    fun lookup (x, []) = ";"
      | lookup (x, (k, v)::t) = 
        if x=k then " init " ^ v ^ ";" else lookup(x,t)
  in
    fun init x = lookup(x, !parameters)
  end


  local
    fun count [] = 0
      | count (#":" :: t) = 1 + count t
      | count (_ :: t) = count t
    fun countColons s = count (explode s)
    fun lookup (x, []) = ""
      | lookup (x, (k, v)::t) = 
        if x=k then 
           if v="0" then "0" else
           let val c = countColons x
            in if c = 0 then v else "(" ^ Int.toString (c+1) ^ " * " ^ v ^ ")"
           end
        else lookup(x,t)
  in
    fun initVal x = lookup(x, !parameters)
  end


  local
    fun lookup (x, []) = ";"
      | lookup (x, (k, v)::t) = 
        if x=k then " = " ^ v ^ ";" else lookup(x,t)
  in
    fun find x = lookup(x, !parameters)
  end

  val MAX = ref "MAX" ; 
  val longMAX = "MAXIMUM_MOLECULE_COUNT" ; 

  fun rep s = 
      if s = "if" then "((" else
      if s = "then" then ") ?" else
      if s = "else" then " : " else
      if s = "fi" then ")" else
      if s = "and" then "& " else
      if s = "or" then "| " else
      if s = "begin" then "(" else
      if s = "end" then ")" else
         s

  fun PRISMid i = 
    let val ir = rep i
        fun toCId #":" = "_colon_"
          | toCId #"-" = "_dash_"
          | toCId #"@" = "_at_"
          | toCId #"'" = "_prime_"
          | toCId c = str c
     in if ir = i then "_" ^ String.translate toCId i else ir
    end

  fun ^^ (s1, s2) = s1 ^ "\n" ^ s2
  infixr ^^

  fun header () = 
      "// PRISM model compiled from Bio-PEPA input file \"" ^ JobName.show() ^ "\" by" ^^
      "// " ^ Version.banner ^^
      "" ^^
      "ctmc" ^^
      ""

  fun footer () = 
      "// End PRISM model compiled from " ^ JobName.show() ^^ ""

  fun brackets #"[" = "("
    | brackets #"]" = ")"
    | brackets c = str c

  datatype token = 
      Ident of BioPEPA.Identifier
    | Whitespace of string
    | Float of string
    | Symbol of char

  fun isIdChar c = Char.isAlphaNum c orelse Char.contains "'_@-:" c

  fun analyse []  = []
    | analyse (a::x) =
	if Char.isSpace a then getwhitespace [a] x else 
	if Char.isDigit a then getnumber [a] x else 
        if Char.isAlpha a then getword [a] x else
	   Symbol a :: analyse x 
  and getword l [] = [ident l]
    | getword l (a::x) = 
        if isIdChar a
	then getword (a::l) x
	else ident l :: analyse (a::x)
  and getwhitespace l [] = [whitespace l]
    | getwhitespace l (a::x) = 
        if Char.isSpace a
	then getwhitespace (a::l) x
	else whitespace l :: analyse (a::x)
  and getnumber l [] = [float l]
    | getnumber l (a::x) = 
        if Char.isDigit a 
	   orelse a = #"." 
	   orelse a = #"E" (* scientific notation *)
	then getnumber (a::l) x
	else float l :: analyse (a::x)
  and ident l = 
        Ident ((implode (rev l)))
  and whitespace l = 
        Whitespace ((implode (rev l)))
  and float l = 
        Float (implode (rev l))

  fun repaintSymbol #"[" = "("
    | repaintSymbol #"]" = ")"
    | repaintSymbol c = str c
 
  fun repaintTokens ((Ident i)::(Symbol #"(")::t) = 
      "func(" :: i :: ", " :: repaintTokens t
    | repaintTokens ((Ident i)::t) = 
      PRISMid i :: repaintTokens t
    | repaintTokens ((Float f)::t)   = f :: repaintTokens t
    | repaintTokens ((Whitespace w)::t)  = w :: repaintTokens t
    | repaintTokens ((Symbol #":")::(Symbol #"=")::t)  = "=" :: repaintTokens t
    | repaintTokens ((Symbol #"=")::t)  = "==" :: repaintTokens t
    | repaintTokens ((Symbol c)::t) = repaintSymbol(c) :: repaintTokens t
    | repaintTokens [] = []

  fun repaint s = String.concat (repaintTokens (analyse (explode s)))

  fun rate r rExp = 
      "  [" ^ PRISMid r ^ "] (" ^ repaint rExp ^ " > 0) -> " ^ repaint rExp ^ 
                 " : true;"

  fun isSpecies s = Char.isUpper(List.hd(explode s))
  fun rateDefs (BioPEPA.CONSTS(r, BioPEPA.RATE rExp, C)) =
      (if isSpecies r then "" else rate r rExp) ^^ rateDefs C
    | rateDefs (BioPEPA.CONSTS(_, _, C)) = rateDefs C
    | rateDefs _ = ""

  fun rates C = 
      "module Rates" ^^
      "" ^^
         rateDefs C ^^
      "endmodule" ^^
      ""

  fun reactions S (BioPEPA.CHOICE(L,R)) = 
      reactions S L ^^ reactions S R
    | reactions S (BioPEPA.INCREASES((r,s), _)) = 
      "  [" ^ PRISMid r ^ 
          "] (" ^ PRISMid S ^ " + " ^ s ^ " <= " ^ !MAX ^
              ") -> 1 : (" ^ PRISMid S ^ "' = " ^ 
                              PRISMid S ^  " + " ^ 
                              s ^ 
                         ");"
    | reactions S (BioPEPA.DECREASES((r,s), _)) = 
      "  [" ^ PRISMid r ^ 
          "] (" ^ PRISMid S ^ " >= " ^ s ^ 
              ") -> 1 : (" ^ PRISMid S ^ "' = " ^ 
                             PRISMid S ^  " - " ^ s ^ 
                        ");"
    | reactions S _ = ""

  fun harvestReactions (BioPEPA.CONSTS(_, def, C)) = 
      harvestReactions def @ harvestReactions C
    | harvestReactions (BioPEPA.INCREASES((r, _),_)) = [r]
    | harvestReactions (BioPEPA.DECREASES((r, _),_)) = [r]
    | harvestReactions _ = []

  fun module (BioPEPA.CONSTS(S, defS, _)) = 
      "module " ^ PRISMid S ^^
      "" ^^
      "  " ^ PRISMid S ^ " : [0.." ^ !MAX ^ "]" ^ init(S) ^^
      "" ^^
      reactions S defS ^^ 
      "" ^^
      "endmodule" ^^ 
      "\n"
    | module _ = ""

  fun modules (M as BioPEPA.CONSTS(S, BioPEPA.CHOICE _, C)) = 
      module M ^ modules C
    | modules (M as BioPEPA.CONSTS(S, BioPEPA.INCREASES _, C)) = 
      module M ^ modules C
    | modules (M as BioPEPA.CONSTS(S, BioPEPA.DECREASES _, C)) = 
      module M ^ modules C
    | modules (M as BioPEPA.CONSTS(S, _, C)) = 
      modules C
    | modules _ = ""

  fun ireward s = 
      "// count rewards: \"number of occurrences of " ^ s ^ "\"" ^^
      "rewards \"" ^ PRISMid s ^ "\"" ^^
      "  [" ^ PRISMid s ^ "] true : 1;" ^^
      "endrewards" ^^ 
      "\n"

  fun reward (BioPEPA.CONSTS(S, defS, _)) = 
      "// rewards: \"number of " ^ S ^ " molecules present\"" ^^
      "rewards \"" ^ PRISMid S ^ "\"" ^^
      "  true : " ^ PRISMid S ^ ";" ^^
      "endrewards" ^^ 
      "" ^^
      "// rewards: \"square of number of " ^ S ^ " molecules present" ^
                    " (used to calculate standard derivation)\"" ^^
      "rewards \"" ^ PRISMid S ^ "_squared\"" ^^
      "  true : " ^ PRISMid S ^ " * " ^ PRISMid S ^ ";" ^^
      "endrewards" ^^ 
      "\n"
    | reward _ = ""

  fun rewards (M as BioPEPA.CONSTS(S, BioPEPA.CHOICE _, C)) = 
      reward M ^ rewards C
    | rewards (M as BioPEPA.CONSTS(S, BioPEPA.INCREASES _, C)) = 
      reward M ^ rewards C
    | rewards (M as BioPEPA.CONSTS(S, BioPEPA.DECREASES _, C)) = 
      reward M ^ rewards C
    | rewards (M as BioPEPA.CONSTS(S, BioPEPA.RATE r, C)) = 
      if isSpecies S 
      then rewards C
      else ireward S ^ rewards C
    | rewards _ = ""

  fun modelComponent (BioPEPA.CONSTS(S, BioPEPA.RATE defS, _)) = 
      "// Model component: " ^ S ^ " = " ^ defS ^^
      "rewards \"" ^ PRISMid S ^ "\"" ^^
      "  true : " ^ repaint defS ^ ";" ^^
      "endrewards" ^^ 
      "\n"
    | modelComponent _ = ""

  fun modelComponents (M as BioPEPA.CONSTS(S, BioPEPA.RATE _, C)) = 
      (if isSpecies S then modelComponent M else "") ^ modelComponents C
    | modelComponents (M as BioPEPA.CONSTS(S, _, C)) = 
      modelComponents C
    | modelComponents _ = ""

  fun checkForMax (BioPEPA.CONSTS(S, Exp, C)) = 
      if S = "MAX" then MAX := longMAX else (checkForMax Exp; checkForMax C)
    | checkForMax (BioPEPA.CHOICE(L, R)) = (checkForMax L; checkForMax R)
    | checkForMax (BioPEPA.INCREASES((r, s), BioPEPA.VAR S)) = 
      if S = "MAX" orelse r = "MAX" orelse s = "MAX" then MAX := longMAX else ()
    | checkForMax (BioPEPA.DECREASES((r, s), BioPEPA.VAR S)) = 
      if S = "MAX" orelse r = "MAX" orelse s = "MAX" then MAX := longMAX else ()
    | checkForMax (BioPEPA.VAR I) = 
      if I = "MAX" then MAX := longMAX else ()
    | checkForMax _ = ()

  fun getGuards r (BioPEPA.CONSTS(S, Exp, C)) = 
      getGuards r Exp @ getGuards r C
    | getGuards r (BioPEPA.CHOICE(L,R)) = 
      getGuards r L @ getGuards r R
    | getGuards r (BioPEPA.INCREASES((r2, s), BioPEPA.VAR S)) = 
      if r = r2 then [PRISMid S ^ " + " ^ s ^ " <= " ^ !MAX] else []
    | getGuards r (BioPEPA.DECREASES((r2, s), BioPEPA.VAR S)) = 
      if r = r2 then [PRISMid S ^ " >= " ^ s] else []
    | getGuards _ _ = []

  fun guard C r = 
  let fun conjoin [] = ""
        | conjoin [x] = x
        | conjoin (h::t) = h ^ " & " ^ conjoin t
   in "(" ^ conjoin (getGuards r C) ^ ")"
  end 

  fun debug C = 
  let 
      val reactions = Sort.quicksort (harvestReactions C)
      fun printList [] = ""
        | printList (h::t) = h ^^ printList t
      val reactionGuards = map (guard C) reactions
   in header() ^^
      printList reactions ^^
      printList reactionGuards ^^
      rates C ^^
      modules C ^^
      rewards C ^^
      footer()
  end

  fun harvestSpecies (BioPEPA.CONSTS(I, BioPEPA.RATE _, C)) = 
      harvestSpecies C
    | harvestSpecies (BioPEPA.CONSTS(I, _, C)) = 
      I :: harvestSpecies C
    | harvestSpecies _ = []

  fun comment1 [] = "\n"
    | comment1 [x] = PRISMid x ^ "\n"
    | comment1 (h::t) = PRISMid h ^ ", " ^ comment1 t
  fun comment s = "// Species: " ^ comment1 s

  fun sum [] = ""
    | sum [x] = initVal x
    | sum (h::t) = initVal h ^ " + " ^ sum t
  fun max species = "const int " ^ !MAX ^ " = " ^ sum species ^ ";\n"

  fun declarations species = 
  let
    fun member x [] = false
      | member x (h::t) = x=h orelse member x t 
    fun filter ([], s) = []
      | filter (((k, v)::t, s)) = 
          if member k s then filter(t, s) else k :: filter(t, s)
    fun dec d = "const double " ^ PRISMid d ^ find d
    fun decs [] = ""
      | decs (h::t) = dec h ^^ decs t
   in decs (filter(!parameters, species))
  end

  fun generate (filename, C, params) = 
  let 
      val f = TextIO.openOut(filename ^ ".sm")
      val _ = checkForMax C
      val species = harvestSpecies C
   in parameters := params;
      TextIO.output(f,
        header() ^^
        declarations species ^^
        rates C ^^
        comment species ^^
        max species ^^
        modules C ^^
        rewards C ^^
        modelComponents C ^^
        footer());
      TextIO.closeOut f
  end

end;
