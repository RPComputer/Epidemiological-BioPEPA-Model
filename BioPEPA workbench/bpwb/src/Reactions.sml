(*
   File: Reactions.sml

   Compile reactions in Dizzy format for BioPEPA models
*)

structure Reactions :> Reactions =
struct

  fun isSpecies s = Char.isUpper(List.hd (explode s))

  fun harvestReactions (BioPEPA.CONSTS (I, BioPEPA.RATE _, C)) =
      if isSpecies I 
      then harvestReactions C
      else I :: harvestReactions C
    | harvestReactions (BioPEPA.CONSTS (I, _, C)) = harvestReactions C
    | harvestReactions _ = []

  fun harvestKinetics (r1, BioPEPA.CONSTS (r2, BioPEPA.RATE k, C)) = 
      if r1 = r2 then k else harvestKinetics (r1, C)
    | harvestKinetics (r, _) = Error.fatal_error ("Could not find kinetics for " ^ r)

  fun harvestSpecies (BioPEPA.CONSTS (I, BioPEPA.RATE _, C)) = harvestSpecies C
    | harvestSpecies (BioPEPA.CONSTS (I, _, C)) = I :: harvestSpecies C
    | harvestSpecies _ = []

  fun checkStoichiometry s = 
      case (Int.fromString s) of 
        NONE => Error.fatal_error ("Could not interpret '" ^ s ^ " as a stoichiometric constant")
      | SOME i => i

  fun stoichiometry 0 i = []
    | stoichiometry n i = i :: stoichiometry (n - 1) i

  fun harvestReactants (r, BioPEPA.CONSTS (I, P, C)) = 
      harvestReactants (r, P) @ harvestReactants (r, C) 
    | harvestReactants (r, BioPEPA.DECREASES ((I, stoc), BioPEPA.VAR C)) = 
         if r = I then stoichiometry (checkStoichiometry stoc) C else []
    | harvestReactants (r, BioPEPA.INCREASES ((I, _), BioPEPA.VAR C)) = []
    | harvestReactants (r, BioPEPA.CHOICE (P, Q)) = 
         harvestReactants (r, P) @ harvestReactants (r, Q)
    | harvestReactants _ = []

  fun harvestProducts (r, BioPEPA.CONSTS (I, P, C)) = 
      harvestProducts (r, P) @ harvestProducts (r, C) 
    | harvestProducts (r, BioPEPA.INCREASES ((I, stoc), BioPEPA.VAR C)) = 
         if r = I then stoichiometry (checkStoichiometry stoc) C else []
    | harvestProducts (r, BioPEPA.DECREASES ((I, _), BioPEPA.VAR C)) = []
    | harvestProducts (r, BioPEPA.CHOICE (P, Q)) = 
         harvestProducts (r, P) @ harvestProducts (r, Q)
    | harvestProducts _ = []

  fun reactConcat [] = ""
    | reactConcat [r] = BioPEPA.repaintIdentifier r
    | reactConcat (r1::r2::t) = 
      BioPEPA.repaintIdentifier r1 ^ " + " ^ reactConcat (r2::t)

  fun repaintKinetics k = 
      let val k = String.tokens Char.isSpace k
          fun repaintId k = 
              if Char.isAlpha(hd (explode k))
              then BioPEPA.repaintIdentifier k
              else k
          fun concat [] = ""
            | concat (h::t) = h ^ " " ^ concat t
       in concat (map repaintId k)
      end

  fun channels C = 
    let 
       val reactions = harvestReactions C
       val reactants = (map reactConcat (map (fn r => harvestReactants (r, C)) reactions))
       val products = (map reactConcat (map (fn r => harvestProducts (r, C)) reactions))
       val kinetics = map (fn r => harvestKinetics (r, C)) reactions
    in (reactions, reactants, products, kinetics)
    end


  fun compile C = 
    let 
       fun quote s = "\"" ^ s ^ "\""
       fun printChannel (r1, R, P, k) = 
           quote r1 ^ ", " ^ R ^ " -> " ^ P ^ ", " ^ repaintKinetics k ^ ";\n"
       fun confirmChannel c = 
           if true then () else 
              TextIO.output(TextIO.stdOut, "Reaction: " ^ printChannel c)  
       fun printChannels ([], [], [], []) = []
         | printChannels (r::rs, R::Rs, P::Ps, k::ks) = 
              (confirmChannel (r, R, P, k) ; printChannel (r, R, P, k)) :: 
                 printChannels (rs, Rs, Ps, ks)
         | printChannels (_, _, _, _) = 
              Error.fatal_error ("BioPEPA Workbench internal error: printChannels")
    in printChannels (channels C)
    end

end;
