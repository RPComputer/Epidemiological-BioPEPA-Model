(* 
 File: Semantic.sml

 The structure which performs semantic analysis of BioPEPA models.
 Errors include component definitions which are missing or duplicated.
 Warnings include components which are defined but never used.

*)


structure Semantic :> Semantic =
struct

  (* Species begin with a capital letter *)
  fun isSpecies s = Char.isUpper(List.hd (explode s))

  local
    (* Collects the set of all activity names *)
    fun act (BioPEPA.INCREASES ((a, _), C)) =  a :: act C
      | act (BioPEPA.DECREASES ((a, _), C)) =  a :: act C
      | act (BioPEPA.VAR _) = []
      | act (BioPEPA.RATE _) = []
      | act (BioPEPA.CHOICE (P, Q)) = act P @ act Q
      | act (BioPEPA.COOP (P, Q, _)) = act P @ act Q
      | act (BioPEPA.HIDING (P, _)) = act P
      | act (BioPEPA.CONSTS (_, P, C)) = act P @ act C

    (* Collects the set of all rates *)
    fun rates (BioPEPA.INCREASES ((a, r), C)) =  r :: rates C
      | rates (BioPEPA.DECREASES ((a, r), C)) =  r :: rates C
      | rates (BioPEPA.VAR _) = []
      | rates (BioPEPA.RATE _) = []
      | rates (BioPEPA.CHOICE (P, Q)) = rates P @ rates Q
      | rates (BioPEPA.COOP (P, Q, _)) = rates P @ rates Q
      | rates (BioPEPA.HIDING (P, _)) = rates P
      | rates (BioPEPA.CONSTS (_, P, C)) = rates P @ rates C

    (* Collects the set of all component identifiers *)
    fun used (BioPEPA.VAR I) = [I]
      | used (BioPEPA.RATE I) = []
      | used (BioPEPA.INCREASES (_, C)) = used C
      | used (BioPEPA.DECREASES (_, C)) = used C
      | used (BioPEPA.CHOICE (P, Q)) = used P @ used Q
      | used (BioPEPA.COOP (P, Q,_)) = used P @ used Q
      | used (BioPEPA.HIDING (P, _)) = used P
      | used (BioPEPA.CONSTS (_, P, C)) = used P @ used C

    fun defined (BioPEPA.CONSTS (I, BioPEPA.RATE _, C)) = defined C
      | defined (BioPEPA.CONSTS (I, _, C)) = I :: defined C
      | defined _ = []

    fun ratedefs (BioPEPA.CONSTS (I, BioPEPA.RATE r, C)) = 
        if isSpecies I 
        then ratedefs C
        else I :: ratedefs C
      | ratedefs (BioPEPA.CONSTS (I, _, C)) =  ratedefs C
      | ratedefs _ = []
 
  in
    val activities = Sort.quicksort o act
    val rates = Sort.quicksort o rates
    val used = Sort.quicksort o used
    val defined = Sort.quicksort o defined
    val ratedefs = Sort.quicksort o ratedefs
  end

  fun rmDup [] = []
    | rmDup [x] = ([x] : string list)
    | rmDup (x1 :: (t as (x2 :: _))) =
        let val tail = rmDup t
         in if x1 = x2 then tail else x1 :: tail
        end

  local 
    fun fstNotSnd ([], _) = []
      | fstNotSnd (fst, []) = (fst : string list)
      | fstNotSnd (h1::fst, h2::snd) =
	if h1 < h2 
	then h1 :: fstNotSnd (fst, h2::snd)
	else if h1 = h2 then fstNotSnd (fst, h2::snd)
	     else fstNotSnd (h1::fst, snd)
  in
    fun firstNotSecond (fst, snd) = fstNotSnd (rmDup fst, rmDup snd)
  end

  fun checkDup [] = ()
    | checkDup [_] = ()
    | checkDup (x1 :: (t as (x2 :: _))) =
      if x1 = x2 
      then 
         Error.fatal_error ("Species multiply defined: " ^ x1)
      else checkDup t

  fun reportNotUsed C = 
      Error.warning ("Species definition unused: " ^ C)

  fun reportRateNotUsed C = 
      Error.warning ("Rate definition unused: " ^ C)

  fun reportNoRateLawFound C = 
      Error.fatal_error ("No rate law found for: " ^ C)

  fun reportNotDefined C = 
      Error.fatal_error ("Species not defined: " ^ C)

  (* This version of member works on sorted string lists *)
  fun member (s : string) =
    let
      fun mem [] = false
        | mem (h::t) =
          not (h > s) andalso (h = s orelse mem t)          
    in mem end

  fun purifylist act class =
    let
      fun pL [] = []
        | pL (h :: t) = 
           case h of
             "tau" =>
               (Error.warning ("tau found in " ^ class ^ " set, ignoring");
                pL t)
           | id  => 
               if member id act
               then id :: pL t
               else (Error.warning ("unused activity name `" ^
                  id ^ "' found in " ^ class ^ " set, ignoring");
                pL t)

    in pL end

  fun purify act =
    let 
      fun p (BioPEPA.COOP (P, Q, L)) = 
          BioPEPA.COOP (p P, p Q, purifylist act "cooperation" L)
        | p (BioPEPA.HIDING (P, L)) =
          BioPEPA.HIDING (p P, purifylist act "hiding" L)
        | p (BioPEPA.CONSTS (I, P, C)) =
          BioPEPA.CONSTS (I, p P, p C)
        | p C = C
    in p end

  fun getSpecies (BioPEPA.CONSTS (I, BioPEPA.RATE _, C)) = getSpecies C
    | getSpecies (BioPEPA.CONSTS (I, P, C)) = (I, P) :: getSpecies C
    | getSpecies _ = []

  fun getReactions (BioPEPA.INCREASES ((r, _), BioPEPA.VAR I')) = [r]
    | getReactions (BioPEPA.DECREASES ((r, _), BioPEPA.VAR I')) = [r]
    | getReactions (BioPEPA.CHOICE (P, Q)) = getReactions P @ getReactions Q
    | getReactions (BioPEPA.CONSTS (I, P, C)) = getReactions P @ getReactions C
    | getReactions _ = []

  local
    fun checkFor (I, r1, []) = ()
      | checkFor (I, r1, r2::t) = 
        if r1 = r2 then 
	   Error.fatal_error ("Species " ^ I ^ " has multiple definitions for reaction channel " ^ r1)
        else checkFor(I, r1, t)
    fun check (I, []) = ()
      | check (I, h::t) = (checkFor (I, h, t) ; check (I, t))
  in  
(*    fun checkUnique (I, P) = check (I, getReactions P) *)
    fun checkUnique (I, P) = true
  end

  fun dropLeading r1 [] = []
    | dropLeading r1 (rs as (r2::t)) = 
      if r1 = r2 then dropLeading r1 t else rs
(*
  fun singletonError r = Error.fatal_error ("Reaction channel " ^ r ^ " only occurs once")

  fun checkMultiplicity [] = ()
    | checkMultiplicity ([r]) = singletonError r
    | checkMultiplicity (r1::r2::t) =
      if r1 = r2 then checkMultiplicity (dropLeading r1 t)
      else singletonError r1
  fun checkMultiplicities C = checkMultiplicity (Sort.quicksort (getReactions C))
*)

  fun checkCyclic (I, BioPEPA.INCREASES (_, BioPEPA.VAR I')) = 
        if I = I' then () else Error.fatal_error ("Species " ^ I ^ " increases to species " ^ I')
    | checkCyclic (I, BioPEPA.DECREASES (_, BioPEPA.VAR I')) = 
        if I = I' then () else Error.fatal_error ("Species " ^ I ^ " decreases to species " ^ I')
    | checkCyclic (I, BioPEPA.CHOICE (P, Q)) = 
        (checkCyclic (I, P); checkCyclic (I, Q))
    | checkCyclic (I, BioPEPA.COOP (P, Q, L)) = 
        Error.fatal_error ("Species " ^ I ^ " is defined by cooperation")
    | checkCyclic (I, BioPEPA.HIDING (P, _)) = 
        Error.fatal_error ("Species " ^ I ^ " is defined by hiding")
    | checkCyclic (I, P) = 
        Error.fatal_error ("Species " ^ I ^ " is not well-formed\n\t[Reason: " ^ 
			  Prettyprinter.print P ^ "]")

  fun checkCycles C = 
     let val s = getSpecies C 
     in  ((map checkCyclic s) ; (map checkUnique s))
     end

  fun harvestReactions (BioPEPA.CONSTS (I, BioPEPA.RATE _, C)) = 
      if isSpecies I 
      then harvestReactions C
      else I :: harvestReactions C
    | harvestReactions (BioPEPA.CONSTS (I, _, C)) = harvestReactions C
    | harvestReactions _ = []

  fun reactionsToString [] = "\n"
    | reactionsToString [r] = r ^ "\n"
    | reactionsToString (h::t) = h ^ ", " ^ reactionsToString t
  fun reportReactions r = TextIO.output(TextIO.stdOut, "Reaction channels: " ^ reactionsToString r)


  fun harvestSpecies (BioPEPA.CONSTS (I, BioPEPA.RATE _, C)) = harvestSpecies C
    | harvestSpecies (BioPEPA.CONSTS (I, _, C)) = I :: harvestSpecies C
    | harvestSpecies _ = []

  fun SpeciesToString [] = "\n"
    | SpeciesToString [r] = r ^ "\n"
    | SpeciesToString (h::t) = h ^ ", " ^ SpeciesToString t
  fun reportSpecies r = TextIO.output(TextIO.stdOut, "Species defined: " ^ SpeciesToString r)

  fun harvestModelComponents (BioPEPA.CONSTS (I, BioPEPA.RATE _, C)) = 
      if isSpecies I 
      then I :: harvestModelComponents C
      else harvestModelComponents C
    | harvestModelComponents (BioPEPA.CONSTS (I, _, C)) = 
      harvestModelComponents C
    | harvestModelComponents _ = []

  fun modelComponentsToString [] = "\n"
    | modelComponentsToString [r] = r ^ "\n"
    | modelComponentsToString (h::t) = h ^ ", " ^ modelComponentsToString t
  fun reportModelComponents r = TextIO.output(TextIO.stdOut, "Model components defined: " ^ modelComponentsToString r)

  fun modelComponentDefs (BioPEPA.CONSTS (I, BioPEPA.RATE r, C)) = 
      if isSpecies I 
      then (I, r) :: modelComponentDefs C
      else modelComponentDefs C
    | modelComponentDefs (BioPEPA.CONSTS (I, _, C)) = 
      modelComponentDefs C
    | modelComponentDefs _ = []

  fun analyse C = 
    let
      val usedActivities = rmDup (activities C)
      val usedNames = used C
      val definedNames = defined C
      val defNotUsed = firstNotSecond (definedNames, usedNames)
      val _ = map reportNotUsed defNotUsed
      val usedNotDefined = firstNotSecond (usedNames, definedNames)
      val _ = map reportNotDefined usedNotDefined
      val _ = checkDup definedNames
      val usedRates = rmDup (rates C)

      val rateDefs = ratedefs C
      val ratesNotUsed = firstNotSecond (rateDefs, usedActivities)
      val _ = map reportRateNotUsed ratesNotUsed

      val _ = map reportNoRateLawFound (firstNotSecond (usedActivities, rateDefs))

      val _ = checkCycles C

      val reactions = harvestReactions C
      val _ = reportReactions reactions

      val species = harvestSpecies C
      val _ = reportSpecies species

      val modelComponents = harvestModelComponents C
      val _ = reportModelComponents modelComponents

      val modeldefs = modelComponentDefs C
      val _ = Gnuplot.setModelDefs modeldefs

(*
      val _ = checkMultiplicities C
*)
    in
      purify usedActivities C
    end

end;
