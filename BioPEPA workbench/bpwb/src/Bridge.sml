(* 
 File: Bridge.sml

 Makes a bridge between PEPA and Dizzy
*)

structure Bridge :> Bridge =
struct
local
  open PepaNets
in
  datatype Dizzyrate = RateConstant of Identifier
		     | Concentration of Identifier
		     | Theta of Identifier

		     (* 
		      I'm obviously not particularly happy about including this but
                      it is more of a place holder for the time being until we can
                      do some proper translation between pepa rates and dizzy rates.
                      *)
		     | ComplexDizzyRate of string
(*
		     | Rate_fun of string * Dizzyrate list
		     | Binary_rate_op of Dizzyrate * string * Dizzyrate
*)


  (*
    A function to convert the rates of Pepa into that of Dizzy.
    This is a placeholder implementation until I find the time to
    do it properly, this will involve actually extending the data
    type Dizzyrate above in the way that I have sort of hinted at
    in the commented out constructors.
   *)
  fun dizzy_rate_of_pepa_rate pepa_rate =
      let val s = printRate HashTable.unhash pepa_rate
      in ComplexDizzyRate s
      end

(*
  (* This version has been superseded *)
  fun printDizzyRate (RateConstant r) = 
      HashTable.unhash r 
    | printDizzyRate (Concentration c) = HashTable.unhash c
    | printDizzyRate (Theta i) = "theta( " ^ HashTable.unhash i ^ " )"
  fun printDizzyRates sep [] = " ]"
    | printDizzyRates sep (h::t) = sep ^ printDizzyRate h ^ printDizzyRates " * " t
  fun printDizzyRateList l = printDizzyRates "[ " l
*)


  fun printDizzyRate (RateConstant r)  = HashTable.unhash r 
    | printDizzyRate (Concentration c) = HashTable.unhash c
    | printDizzyRate (Theta i)         = HashTable.unhash i 
    | printDizzyRate (ComplexDizzyRate s) = s
  fun printDizzyMinOfRates [] = 
      Error.fatal_error ("Empty rate list has been found, searching for min")
    | printDizzyMinOfRates [x] = printDizzyRate x (* individual actions, or last in the list *)
    | printDizzyMinOfRates (h::t) = "min (" ^ printDizzyRate h ^ ", " ^ printDizzyMinOfRates t ^ ")"
  fun printDizzyRates [] = Error.fatal_error ("Empty rate list has been found")
    | printDizzyRates (h::t) = "[ " ^ printDizzyRate h ^ " * " ^ printDizzyMinOfRates t ^ " ]"
  fun endsWith c [] = false
    | endsWith c [c2] = c = c2
    | endsWith c (_::t) = endsWith c t
  fun isProbe x = endsWith #"_" (explode (printDizzyRate x))
  fun removeProbes [] = []
    | removeProbes (h::t) = if isProbe h then removeProbes t else h :: removeProbes t
  fun printDizzyRateList l = printDizzyRates (removeProbes l)

  (* Rate constants are always stored at the head of the list, and there is at most one of them *) 
  fun noRateConstant [] = true
    | noRateConstant ((RateConstant _)::_) = false
    | noRateConstant (_::t) = true

  fun lookup [] r = 
      Error.fatal_error ("Could not find a value for rate " ^ HashTable.unhash r)
    | lookup ((r1, v1) :: t) r = 
      if r = r1 
      then case Real.fromString v1 of
	       SOME v => v
	     | NONE => 
	       Error.fatal_error ("Non-numeric rate '" ^ v1  ^ "' bound to " ^
				  HashTable.unhash r)
      else lookup t r

  fun min E r r1 = 
      if HashTable.isInfty r1 then true
      else if HashTable.isInfty r then false
      else (lookup E r) < (lookup E r1)

  fun replaceIfLess E r ((RateConstant r1) :: rs) = 
      if min E r r1 
      then (RateConstant r) :: rs
      else (RateConstant r1) :: rs
    | replaceIfLess E r rs = 
      Error.fatal_error ("Non-well-formed rate list : " ^ printDizzyRateList rs)
      
  fun addReaction E (P, (a, Top), Q) [] = 
      [(a, [P], Q, [Theta P])]
    | addReaction E (P, (a, r), Q) [] = 
      [(a, [P], Q, [dizzy_rate_of_pepa_rate r, Concentration P])]

    | addReaction E (P, (a, Top), Q) ((a1, PRE, POST, rs)::t) = 
      if a = a1 then (a1, PRE @ [P], POST @ Q, rs @ [Theta P]) :: t
      else (a1, PRE, POST, rs):: (addReaction E (P, (a, Top), Q) t)

    | addReaction E (P, (a, Rate r), Q) ((a1, PRE, POST, rs)::t) = 
      if a = a1 then 
	  if noRateConstant rs then 
              (a1, PRE @ [P], POST @ Q, (RateConstant r) :: rs @ [Concentration P]) :: t
	  else 
	      (a1, PRE @ [P], POST @ Q, replaceIfLess E r rs @ [Concentration P]) :: t
      else (a1, PRE, POST, rs) :: addReaction E (P, (a, Rate r), Q) t


    | addReaction E (P, (a, rate), Q) ((a1, PRE, POST, rs) :: t) =
      if a = a1
	 (*
	  Since we matched against Rate r above we can assume that it is not 
	  a rate constant and just put it on the end of the list for now, that
          way we can ensure that if there is a rate constant at the front
	  then we don't replace it mistakenly with this non-constant rate.
	  *)
      then
	  let val dizzy_rate = dizzy_rate_of_pepa_rate rate
	  in
	      ( a1, PRE @ [P], POST @ Q, (rs @ [dizzy_rate, Concentration P]) ) :: t
	  end
      else (a1, PRE, POST, rs) :: addReaction E (P, (a, rate), Q) t

    (*
    | addReaction E (P, (a, r), Q) _ =
      Error.fatal_error ("Could not add reaction with complex rate " ^ Prettyprinter.printRate r)
     *)

  fun removeVar (VAR I) = I
    | removeVar P = 
      Error.fatal_error ("Component name expected: could not translate complex term " ^
			 Prettyprinter.print P)

  val isStop = PepaNets.isStop

  fun traverseDefinition rates reactions I (CHOICE (P, Q)) =
        let val reactions' = traverseDefinition rates reactions I P
         in traverseDefinition rates reactions' I Q
        end
    | traverseDefinition rates reactions I (PREFIX ((a, r), VAR I')) =
        addReaction rates (I, (a, r), [I']) reactions
    | traverseDefinition rates reactions I (PREFIX ((a, r), COOP (P, L))) =
        let 
	    val names = map removeVar P
	in
            addReaction rates (I, (a, r), names) reactions
	end
    | traverseDefinition rates reactions I (VAR P) =
      if isStop P then reactions else 
        Error.fatal_error ("Could not translate aliased name " ^
			   HashTable.unhash P)        
    | traverseDefinition rates reactions I P =
        Error.fatal_error ("Could not translate complex term " ^
			   Prettyprinter.print P)

  fun traverseDefinitions rates reactions [] = reactions
    | traverseDefinitions rates reactions ((P, defP)::t) =
      if isStop P then traverseDefinitions rates reactions t
      else 
         let val reactions' = traverseDefinition rates reactions P defP
          in traverseDefinitions rates reactions' t
         end

  fun harvestDefinitions (CONSTS (I, RATE r, L)) = harvestDefinitions L
    | harvestDefinitions (CONSTS (I, P, L)) = 
      (if isStop I then [] else [(I, P)]) @ harvestDefinitions L
    | harvestDefinitions _ = []

  fun harvestRates (CONSTS (I, RATE r, L)) = (I, r) :: harvestRates L
    | harvestRates (CONSTS (I, P, L)) = harvestRates L
    | harvestRates _ = []

  fun lowercase string = map Char.toLower (explode string)

  fun equalsIgnoreCase s1 s2 =
      lowercase (HashTable.unhash s1) = lowercase (HashTable.unhash s2)

  fun memberIgnoreCase x [] = false
    | memberIgnoreCase x (h::t) = equalsIgnoreCase x h orelse memberIgnoreCase x t

  fun matchIgnoreCase x (h::t) = if equalsIgnoreCase x h then h else matchIgnoreCase x t
    | matchIgnoreCase x [] = Error.internal_error ("Case matching failed on " ^ HashTable.unhash x)

  fun caseFilter including [] _ = []
    | caseFilter including ((h as (r, v))::t) componentNames = 
        let val filtered = caseFilter including t componentNames
	    val concentration = memberIgnoreCase r componentNames 
	in
            if including then 
		if concentration then 
		    (matchIgnoreCase r componentNames, v) :: filtered else filtered
            else 
		if concentration then filtered else h :: filtered
	end

  fun printRates [] = ""
    | printRates ((r,  v) :: t) = 
          HashTable.unhash r ^ " = " ^ v ^ ";\n" ^ 
	  printRates t

  fun printSum [] = ""
    | printSum [x] = HashTable.unhash x
    | printSum (h::t) = HashTable.unhash h ^ " + " ^ printSum t

  fun removeStop l = List.filter (not o isStop) l

  fun printReaction (a1, PRE, POST, rates) = 
      let
	  val sumInputs = printSum PRE
	  val sumOutputs = printSum (removeStop POST)
          val header = HashTable.unhash a1 ^ ",\n\t" ^ 
		       sumInputs ^ " ->\n\t" ^
		       sumOutputs ^ ", " 
	  val footer = (printDizzyRateList rates) ^ ";\n\n"
      in
	  if size header + size footer <= 72
	  then header ^ footer
	  else header ^ "\n\t\t" ^ footer
      end

  fun printReactions [] = ""
    | printReactions (h::t) = printReaction h ^ printReactions t

  fun now () = Date.toString(Date.fromTimeLocal(LocalTime.now())) 
(*  fun now () = DateTime.now() *)

  fun haveConcentration i1 [] = false
    | haveConcentration i1 ((i2, _) :: t) = i1 = i2 orelse haveConcentration i1 t

  fun checkConcentrations [] _ = ()
    | checkConcentrations ((I, _)::t) initialConcentrations = 
        (if haveConcentration I initialConcentrations then () 
         else Error.fatal_error ("No initial concentration specified for " ^
				 HashTable.unhash I);
	 checkConcentrations t initialConcentrations)

  fun bridgeToDizzy P = 
      let
	  val rates = harvestRates P
	  val definitions = harvestDefinitions P
	  val componentNames = map #1 definitions
          val reactionRates = caseFilter false rates componentNames
          val initialConcentrations = caseFilter true rates componentNames
          val _ = checkConcentrations definitions initialConcentrations
	  val reactions = traverseDefinitions rates [] definitions
          fun quote s = "\"" ^ s ^ "\""
	  val jobname = quote (JobName.show())
	  val basename = quote (JobName.showBasename())
          fun dotToUnderscore #"." = "_"
	    | dotToUnderscore c = str c
          fun replaceDots s = String.translate dotToUnderscore s
	  val dotlessBasename = replaceDots basename
      in "// Dizzy version of PEPA model " ^ basename ^ "\n" ^
         "// Compiled from source " ^ jobname ^ "\n" ^
         "// Generated by the PEPA Workbench " ^ Version.version ^ "\n" ^
         (if now() <> "..." 
          then "// Date and time translated: " ^ now() ^ "\n" 
          else "") ^ "\n" ^
         "#model " ^ dotlessBasename ^ ";\n" ^ 
         "\n// Rates of reactions \n" ^ 
         printRates reactionRates ^ 
	 "\n// Initial concentrations of reagents \n" ^ 
         printRates initialConcentrations ^ 
	 "\n// Reaction definitions \n" ^ 
	 printReactions reactions ^ 
	 "\n// End of model " ^ basename ^ "\n" 
      end

  fun bridgeToDizzyFromFile filename =
      let val file = Files.readFile filename
          val tokens = Lexer.analyse file
          val component = Parser.parse tokens
	  val _ = Semantic.analyse component
          val fileOutName = rename filename
          val fileOut = TextIO.openOut(fileOutName)
       in Reporting.report ("Bridging from PEPA model to Dizzy\n");
          Prettyprinter.setMode Prettyprinter.verbose;
          TextIO.output(fileOut, bridgeToDizzy component);
          TextIO.closeOut(fileOut);
          Reporting.report ("Writing PEPA model to " ^ fileOutName ^ "\n")
      end
  and rename filename = (removeSuffix o rev o explode) filename ^ ".dizzy"
  and removeSuffix (#"a" :: #"p" :: #"e" :: #"p" :: #"." :: front) = implode (rev front)
    | removeSuffix t = implode (rev t)
 
end;
end;
