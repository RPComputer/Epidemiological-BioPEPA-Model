(* 
 File: VFgen.sml

 VFgen file generation for the BioPEPA Workbench
*)

structure VFgen :> VFgen =
struct

  val parameters = ref [] : ((string * string) list) ref

  fun quote s = "\"" ^ s ^ "\""

  local
    fun lookup (x, []) = "0"
      | lookup (x, (k, v)::t) = 
        if x=k then v else lookup(x,t)
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
    fun lookup (x, []) =
        Error.fatal_error("VFgen generator could not find " ^ x)
      | lookup (x, (k, v)::t) = 
        if x=k then v else lookup(x,t)
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

  fun VFgenId i = 
    let val ir = rep i
        fun toCId #":" = "_colon_"
          | toCId #"-" = "_dash_"
          | toCId #"@" = "_at_"
          | toCId #"'" = "_prime_"
          | toCId c = str c
     in if ir = i then String.translate toCId i ^ "_" else ir
    end

  fun ^^ (s1, s2) = s1 ^ "\n" ^ s2
  infixr ^^

  fun header () = 
      "<!-- VFgen model compiled from Bio-PEPA input file \"" ^ JobName.show() ^ "\" by" ^^
      " " ^ Version.banner ^^
      "-->" ^^
      ""

  fun footer () = 
      "<!-- End VFgen model compiled from " ^ JobName.show() ^^ "-->"

  (* VFgen does not accept brackets in formulae *)
  fun brackets #"[" = ""
    | brackets #"]" = ""
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

  (* VFgen does not accept brackets in expression formulae *)
  fun repaintSymbol #"[" = ""
    | repaintSymbol #"]" = ""
    | repaintSymbol c = str c
 
  fun repaintTokens ((Ident i)::(Symbol #"(")::t) = 
      "func(" :: i :: ", " :: repaintTokens t
    | repaintTokens ((Ident i)::t) = 
      VFgenId i :: repaintTokens t
    | repaintTokens ((Float f)::t)   = f :: repaintTokens t
    | repaintTokens ((Whitespace w)::t)  = w :: repaintTokens t
    | repaintTokens ((Symbol #":")::(Symbol #"=")::t)  = "=" :: repaintTokens t
    | repaintTokens ((Symbol #"=")::t)  = "==" :: repaintTokens t
    | repaintTokens ((Symbol c)::t) = repaintSymbol(c) :: repaintTokens t
    | repaintTokens [] = []

  fun repaint s = String.concat (repaintTokens (analyse (explode s)))

  fun rate r rExp = 
      "<Expression Name=" ^ quote(VFgenId r) ^ "\n" ^
      "            Description=" ^ quote r ^ "\n" ^ 
      "            Latex=" ^ quote(StyleSheet.lookup r) ^ "\n" ^ 
      "            Formula=" ^ quote(repaint rExp) ^ "/>"

  fun isSpecies s = Char.isUpper(List.hd(explode s))
  fun rates (BioPEPA.CONSTS(r, BioPEPA.RATE rExp, C)) =
      (if isSpecies r then "" else rate r rExp) ^^ rates C
    | rates (BioPEPA.CONSTS(_, _, C)) = rates C
    | rates _ = ""

  datatype LabelledReactions = PLUS of string | MINUS of string
  fun reactions S (BioPEPA.CHOICE(L,R)) = 
          reactions S L @ reactions S R
    | reactions S (BioPEPA.INCREASES((r,s), _)) = 
          if s = "0" 
          then [] (* modifiers *)
          else [PLUS(VFgenId r)]
    | reactions S (BioPEPA.DECREASES((r,s), _)) = 
          [MINUS(VFgenId r)]
    | reactions S _ = []

  fun harvestReactions (BioPEPA.CONSTS(_, def, C)) = 
      harvestReactions def @ harvestReactions C
    | harvestReactions (BioPEPA.INCREASES((r, _),_)) = [r]
    | harvestReactions (BioPEPA.DECREASES((r, _),_)) = [r]
    | harvestReactions _ = []

(*  fun filterReactions [] = ""
    | filterReactions ((PLUS r1)::(MINUS r2)::t) = 
      if r1 = r2 then filterReactions t else 
         let val head = r1 ^ " - " ^ r2 
             val tail = filterReactions t 
          in case t of 
                             []  => head
              | ((PLUS _) :: _)  => head ^ " + " ^ tail
              | ((MINUS _) :: _) => head ^ " - " ^ tail
             end
         end
    | filterReactions ((PLUS r1)::(PLUS r2)::t) = 
         r1 ^ " + " ^ filterReactions ((PLUS r2)::t) 
    | filterReactions ((MINUS r1)::(PLUS r2)::t) = 
         r1 ^ " + " ^ filterReactions ((PLUS r2)::t) 
    | filterReactions ((MINUS r1)::(MINUS r2)::t) = 
         r1 ^ " + " ^ filterReactions ((PLUS r2)::t) 
*)
(*
    fun filterReactions [] = []
(*    WRONG CODE ...
      | filterReactions ((PLUS r1)::(MINUS r2)::t) = 
          if r1 = r2 then filterReactions t else 
             (PLUS r1)::(MINUS r2)::filterReactions t
*)
      | filterReactions ((PLUS (r, 0))::t) = filterReactions t
      | filterReactions ((PLUS r)::t) = (PLUS r)::filterReactions t
      | filterReactions ((MINUS r)::t) = (MINUS r)::filterReactions t
*)
    fun stringifyReactions [] = ""
      | stringifyReactions ((PLUS r)::t)  = r ^ signedStringify t
      | stringifyReactions ((MINUS r)::t) = " - " ^ r ^ signedStringify t
    and signedStringify [] = ""
      | signedStringify ((PLUS r)::t) = " + " ^ r ^ signedStringify t
      | signedStringify ((MINUS r)::t) = " - " ^ r ^ signedStringify t

  fun formula S defS = 
      let val s = stringifyReactions(reactions S defS)
       in if s = "" then "0" else s
      end
      
  fun stateVariable (BioPEPA.CONSTS(S, defS, _)) = 
      "<StateVariable Name=" ^ quote(VFgenId S) ^^
      "               Description=" ^ quote S ^^
      "               Latex=" ^ quote (StyleSheet.lookup S) ^^
      "               DefaultInitialCondition=" ^ quote(init(S)) ^^
      "               Formula=" ^ quote(formula S defS) ^ "/>" ^^ 
      "\n"
    | stateVariable _ = ""

  fun stateVariables (M as BioPEPA.CONSTS(S, BioPEPA.CHOICE _, C)) = 
      stateVariable M ^ stateVariables C
    | stateVariables (M as BioPEPA.CONSTS(S, BioPEPA.INCREASES _, C)) = 
      stateVariable M ^ stateVariables C
    | stateVariables (M as BioPEPA.CONSTS(S, BioPEPA.DECREASES _, C)) = 
      stateVariable M ^ stateVariables C
    | stateVariables (M as BioPEPA.CONSTS(S, _, C)) = 
      stateVariables C
    | stateVariables _ = ""

  fun ireward s = 
      "// count rewards: \"number of occurrences of " ^ s ^ "\"" ^^
      "rewards \"" ^ VFgenId s ^ "\"" ^^
      "  [" ^ VFgenId s ^ "] true : 1;" ^^
      "endrewards" ^^ 
      "\n"

  fun reward (BioPEPA.CONSTS(S, defS, _)) = 
      "// rewards: \"number of " ^ S ^ " molecules present\"" ^^
      "rewards \"" ^ VFgenId S ^ "\"" ^^
      "  true : " ^ VFgenId S ^ ";" ^^
      "endrewards" ^^ 
      "" ^^
      "// rewards: \"square of number of " ^ S ^ " molecules present" ^
                    " (used to calculate standard derivation)\"" ^^
      "rewards \"" ^ VFgenId S ^ "_squared\"" ^^
      "  true : " ^ VFgenId S ^ " * " ^ VFgenId S ^ ";" ^^
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
      "<Expression Name=" ^ quote(VFgenId S) ^^
      "            Formula=" ^ quote(repaint defS) ^ " />" ^^
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
      if r = r2 then [VFgenId S ^ " + " ^ s ^ " <= " ^ !MAX] else []
    | getGuards r (BioPEPA.DECREASES((r2, s), BioPEPA.VAR S)) = 
      if r = r2 then [VFgenId S ^ " >= " ^ s] else []
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
      stateVariables C ^^
      rewards C ^^
      footer()
  end

  fun harvestSpecies (BioPEPA.CONSTS(I, BioPEPA.RATE _, C)) = 
      harvestSpecies C
    | harvestSpecies (BioPEPA.CONSTS(I, _, C)) = 
      I :: harvestSpecies C
    | harvestSpecies _ = []

  fun comment1 [] = "\n"
    | comment1 [x] = VFgenId x ^ "\n"
    | comment1 (h::t) = VFgenId h ^ ", " ^ comment1 t
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
    fun dec d = "<Parameter Name=" ^ quote (VFgenId d) ^ "\n" ^
                "           Description=" ^ quote d ^ "\n" ^
                "           Latex=" ^ quote (StyleSheet.lookup d) ^ "\n" ^
                "           DefaultValue=" ^ quote(find d) ^ " />"
    fun decs [] = ""
      | decs (h::t) = dec h ^^ decs t
   in decs (filter(!parameters, species))
  end

  fun generate (filename, C, params) = 
  let 
      val file = filename ^ ".vf"
      val f = TextIO.openOut (file) handle _ => 
	  Error.fatal_error ("Cannot write to " ^ file)

      val species = harvestSpecies C
   in parameters := params;
      TextIO.output (f, "<?xml version=\"1.0\"?>\n");
      TextIO.output (f, header());
      TextIO.output (f, "<VectorField Name=" ^ quote filename ^ ">\n\n");
      (* Generate declarations of parameter constants *)
      TextIO.output (f, declarations species);
      TextIO.output (f, "\n");
      (* Generate expressions for kinetic functions *)
      TextIO.output (f, rates C);
      TextIO.output (f, "\n");
      (* Generate declarations of state variables *)
      TextIO.output (f, stateVariables C);
      (* Generate declarations of functions for model components *)
      TextIO.output (f, modelComponents C);
      TextIO.output (f, footer());

(*
      TextIO.output(f,
        header() ^^
        declarations species ^^
        rates C ^^
        comment species ^^
        max species ^^
        stateVariables C ^^
        rewards C ^^
        modelComponents C ^^
        footer());
*)
      TextIO.output (f, "\n</VectorField>\n");
      TextIO.closeOut f
  end
 
end;
