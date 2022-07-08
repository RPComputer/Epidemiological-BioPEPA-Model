(* 
 File: StochKit.sml

 Structure which performs lexical analysis of BioPEPA rate expressions
 and component definitions with the intention of mapping to C/C++ for
 compilation with the StochKit libraries.
*)

structure StochKit :> StochKit
= struct
  datatype token = 
      Ident of BioPEPA.Identifier
    | Whitespace of string
    | Float of string
    | Symbol of char

  fun prettyprintToken (Ident i)       = "Identifier : " ^ i ^ "\n"
    | prettyprintToken (Float f)       = "     Float : " ^ f ^ "\n"
    | prettyprintToken (Whitespace w)  = "Whitespace : " ^ w ^ "\n"
    | prettyprintToken (Symbol c)      = "    Symbol : " ^ str(c) ^ "\n"

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

  fun rep s = 
      if s = "if" then "((" else
      if s = "then" then ") ?" else
      if s = "else" then " : " else
      if s = "fi" then ")" else
      if s = "and" then "&& " else
      if s = "or" then "||" else
      if s = "begin" then "(" else
      if s = "end" then ")" else
      if s = "skip" then "0" else
      if s = "print" then "std::cout << " else
      if s = "time" then "t" else
         s

  fun repaintIdentifier i = 
    let val ir = rep i
        fun toCId #":" = "_colon_"
          | toCId #"-" = "_dash_"
          | toCId #"@" = "_at_"
          | toCId #"'" = "_prime_"
          | toCId c = str c
     in if ir = i then String.translate toCId i else ir
    end

  fun repaintSymbol #";" = ","
    | repaintSymbol #"[" = "("
    | repaintSymbol #"]" = ")"
    | repaintSymbol c = str c
 
  fun repaintTokens ((Ident i)::t)   = repaintIdentifier i :: repaintTokens t
    | repaintTokens ((Float f)::t)   = f :: repaintTokens t
    | repaintTokens ((Whitespace w)::t)  = w :: repaintTokens t
    | repaintTokens ((Symbol #":")::(Symbol #"=")::t)  = "=" :: repaintTokens t
    | repaintTokens ((Symbol #"=")::t)  = "==" :: repaintTokens t
    | repaintTokens ((Symbol c)::t) = repaintSymbol(c) :: repaintTokens t
    | repaintTokens [] = []

  fun repaintReaction s = String.concat (repaintTokens (analyse (explode s)))

  fun mkReactionIdentifiers n [] = 
      "#define ___REACTIONS " ^ Int.toString n ^ "\n" 
    | mkReactionIdentifiers n ((r,s)::t) = 
      "#define ___" ^ repaintIdentifier r ^ " " ^ Int.toString n ^ "\n" 
         ^ mkReactionIdentifiers (n + 1) t

  fun mkReactionDef r s = 
      "/*      "  ^ r ^ " = " ^ s ^ " */\n" ^
      "#define " ^ repaintIdentifier r ^ "   " ^ repaintReaction s ^ "\n"

  fun mkReactionDefs [] = ""
    | mkReactionDefs ((r,s)::t) = mkReactionDef r s ^ mkReactionDefs t

  fun getReactionDefs (BioPEPA.CONSTS (r, BioPEPA.RATE k, C)) = 
        (r, k) :: getReactionDefs C
    | getReactionDefs (BioPEPA.CONSTS (r, _, C)) = getReactionDefs C
    | getReactionDefs _ = []

  fun repaintReactionDefs C = 
      let val reactionDefs = getReactionDefs C 
       in "\n/* Constants for reaction identifiers */\n" ^
          mkReactionIdentifiers 0 reactionDefs ^ 
          (*
          "\n/* Constants for reaction definitions */\n" ^
          mkReactionDefs reactionDefs ^ *)
          "\n"
      end

  fun propensityEntries [] = ""
    | propensityEntries ((r,s)::t) = 
      let val r' = repaintIdentifier r
       in 
          "\n  /*      "  ^ r ^ " = " ^ s ^ " */\n" ^
          "  ___propensity(___" ^ r' ^ ") = " ^ repaintReaction s (*r'*) ^ ";\n" ^ 
          propensityEntries t
      end

  fun propensity C = 
      let val reactionDefs = getReactionDefs C 
       in "\nVector Propensity (const Vector& ___discreteSpeciesCount)\n" ^
          "{\n" ^
          "  Vector ___propensity(___REACTIONS);\n" ^
          propensityEntries(reactionDefs) ^ 
          "  return ___propensity;\n" ^
          "}\n" 
      end

  fun mkSpecies n s def = 
      let val _ = Species.set (s, n)
      in
        "#define ___" ^ repaintIdentifier s ^ "   " ^ Int.toString n ^ "\n" ^
        "#define    " ^ repaintIdentifier s ^ "   ___discreteSpeciesCount(" ^ Int.toString n ^ ")\n"
      end

  fun mkSpeciesList n [] = "#define ___SPECIES " ^ Int.toString n ^ "\n"
    | mkSpeciesList n ((s,def)::t) = 
      mkSpecies n s def ^ mkSpeciesList (n + 1) t

  fun getSpecies (BioPEPA.CONSTS (r, BioPEPA.RATE _, C)) = 
        getSpecies C
    | getSpecies (BioPEPA.CONSTS (r, def, C)) = 
        (r, def) :: getSpecies C
    | getSpecies _ = []

  fun repaintSpecies C =
      let val species = getSpecies C
       in "\n/* Constants for species identifiers */\n" ^
          mkSpeciesList 0 species
      end

  val header = 
      "// StochKit input generated by the Bio-PEPA Workbench " ^ Version.version ^ "\n\n" ^
      "#include \"ProblemDefinition.h\"\n" ^
      "#include <iostream>\n" ^
      "#include <stdlib.h>\n" ^
      "#include <math.h>\n\n" 

  fun reactionConstants C parameters = 
      let val species = map #1 (getSpecies C)
          fun mem x [] = false
            | mem x (k::t) =  x=k orelse mem x t
          fun removeSpecies [] = []
            | removeSpecies ((k,v)::t) = 
              if mem k species then removeSpecies t 
              else (k,v) :: removeSpecies t
          val reactionParameters = removeSpecies parameters
          fun compileReactionConstants [] = ""
            | compileReactionConstants [(x,v)] = 
              "    " ^ BioPEPA.repaintIdentifier x ^ " = " ^ v ^ ";\n"
            | compileReactionConstants ((x,v)::t) = 
              "    " ^ BioPEPA.repaintIdentifier x ^ " = " ^ v ^ ",\n"
                     ^ compileReactionConstants t
       in "\n/* Reaction constants */\n" ^
          "double\n" ^
          compileReactionConstants(reactionParameters) ^ 
          "\n" 
      end

  fun reactionConstantInitialisation C parameters = 
      let val species = map #1 (getSpecies C)
          fun mem x [] = false
            | mem x (k::t) =  x=k orelse mem x t
          fun removeSpecies [] = []
            | removeSpecies ((k,v)::t) = 
              if mem k species then removeSpecies t 
              else (k,v) :: removeSpecies t
          val reactionParameters = removeSpecies parameters
          fun compileReactionConstants [] = ""
            | compileReactionConstants ((x,v)::t) = 
              "  " ^ BioPEPA.repaintIdentifier x ^ " = " ^ v ^ ";\n"
                     ^ compileReactionConstants t
       in "\n  /* Reaction constant initialisation */\n" ^
          compileReactionConstants(reactionParameters) ^ 
          "\n" 
      end

  fun checkIntegerOrSymbolic h s = 
      let val i = Int.fromString s 
       in case i of 
            NONE => s
          | SOME x => 
              let val s' = Int.toString x 
               in if s' = s
                  then s 
                  else 
                     (Error.warning("Initial value of species " ^ h ^ " of " ^ s ^
                       " is not an integer.  Truncating to " ^ s' ^ ".\n");
                      s')
              end
      end

  fun initialise C parameters = 
      let val species = map #1 (getSpecies C)
          fun lookup x [] = "***not found***"
            | lookup x ((k,v)::t) = if x=k then v else lookup x t
          fun compileSpecies [] = ""
            | compileSpecies (h::t) = 
              let val i = lookup h parameters 
               in 
                 "  ___initialSpeciesCount(___" ^ BioPEPA.repaintIdentifier h ^ 
                 ") = " ^ checkIntegerOrSymbolic h i ^ ";\n" ^
                 compileSpecies t
              end
       in "\nVector Initialize ()\n" ^
          "{\n" ^
          reactionConstantInitialisation C parameters ^ 
          "  Vector ___initialSpeciesCount(___SPECIES, 0.0);\n" ^
          compileSpecies(species) ^ 
          "  return ___initialSpeciesCount;\n" ^
          "}\n" 
      end

(*
   nu(0,0) = 1; nu(0,1) = -1; 
*)
  fun sInc s = 
      if Char.isDigit(List.hd (explode s)) 
      then "+" ^ s
      else s

  fun sDec s = 
      if Char.isDigit(List.hd (explode s)) 
      then "-" ^ s
      else "(-1 * " ^ s ^ ")"

  fun sumStoichiometry r1 [] = ""
    | sumStoichiometry r1 (BioPEPA.INCREASES((r2, s), BioPEPA.VAR S)::t) =
      let val s2 =  sumStoichiometry r1 t in
          if r1=r2 
          then if s2 = "" then sInc s else sInc s ^ " + " ^ s2
          else s2
      end
    | sumStoichiometry r1 (BioPEPA.DECREASES((r2, s), BioPEPA.VAR S)::t) =
      let val s2 =  sumStoichiometry r1 t in
          if r1=r2 
          then if s2 = "" then sDec s else sDec s ^ " + " ^ s2
          else s2
      end
    | sumStoichiometry r1 _ = 
          Error.internal_error("sumStoichiometry function given a list which was not flattened")

  fun toStoichiometry P = 
      let 
          fun getChoices (BioPEPA.CHOICE(P,Q)) = getChoices P @ getChoices Q
            | getChoices P = [P]
          val choices = getChoices P
          fun getSpecies (BioPEPA.INCREASES (_, BioPEPA.VAR S)) = S
            | getSpecies (BioPEPA.DECREASES (_, BioPEPA.VAR S)) = S
            | getSpecies (BioPEPA.VAR S) = S
            | getSpecies P = Error.internal_error("getSpecies was applied to " ^ 
						 Prettyprinter.print P)
          val species = getSpecies (List.hd choices)
          fun getReactions (BioPEPA.INCREASES ((r, _), _)) = [r]
            | getReactions (BioPEPA.DECREASES ((r, _), _)) = [r]
            | getReactions (BioPEPA.CHOICE(P,Q)) = getReactions P @ getReactions Q
            | getReactions P = Error.internal_error("getReactions was applied to " ^ 
						 Prettyprinter.print P)
          fun rem x [] = []
            | rem x (h::t) = if x=h then rem x t else h :: rem x t
          fun removeDup [] = []
            | removeDup (h::t) = h :: rem h (removeDup t)
          val reactions = removeDup(getReactions P)
          fun stoich r = 
               "  ___stoichiometry(___" ^ BioPEPA.repaintIdentifier species ^ ", ___" ^
                               BioPEPA.repaintIdentifier r ^ ") = " ^ 
                               sumStoichiometry r choices ^ ";\n"     
       in
	  String.concat(map stoich reactions)
      end
(*
  fun toStoichiometry (BioPEPA.INCREASES ((r, s), BioPEPA.VAR S)) = 
      "  ___stoichiometry(___" ^ BioPEPA.repaintIdentifier S ^ ", ___" ^
                               BioPEPA.repaintIdentifier r ^ ") = " ^ 
                               sInc s ^ ";\n"     
    | toStoichiometry (BioPEPA.DECREASES ((r, s), BioPEPA.VAR S)) = 
      "  ___stoichiometry(___" ^ BioPEPA.repaintIdentifier S ^ ", ___" ^
                               BioPEPA.repaintIdentifier r ^ ") = " ^ 
                               sDec s ^ ";\n"  
    | toStoichiometry (BioPEPA.CHOICE(P, Q)) = 
      toStoichiometry P ^ 
      toStoichiometry Q
    | toStoichiometry _ = "***unknown***"    
*)
  fun speciesStoichiometry1 (s, def) =
      "\n  /* "  ^ s ^ " = " ^ Prettyprinter.print def ^ " */\n" ^
      toStoichiometry def

  fun speciesStoichiometry [] = ""
    | speciesStoichiometry (h::t) = 
	speciesStoichiometry1 h ^ speciesStoichiometry t

  fun stoichiometry C = 
      let val species = getSpecies C
       in "\nMatrix Stoichiometry ()\n" ^
          "{\n" ^
          "  Matrix ___stoichiometry(___SPECIES, ___REACTIONS, 0.0);\n" ^
          speciesStoichiometry species ^
          "\n  return ___stoichiometry;\n" ^
          "}\n" 
      end

  val kineticFunctionsFile = 
      "\n#include \"KineticFunctions.cpp\"\n"

  fun compile C = 
      let
        val headerFile = TextIO.openOut ("biopepa.h") 
        val iterations = Configuration.lookup("biopepa.independent.replications") 
        val iterationsParsed = Int.fromString(iterations) 
        val testIterations = 
               if iterationsParsed = NONE 
               then
                  Error.fatal_error("Could not interpret biopepa.independent.replications of " ^ iterations)
               else ()
      in
        TextIO.output(headerFile, 
           "// Output by the Bio-PEPA Workbench\n");
        TextIO.output(headerFile, 
           "#define TimeFinal " ^ 
               Configuration.lookup("biopepa.simulation.stoptime") ^
                 "\n");
        TextIO.output(headerFile, 
           "#define Report " ^ 
               Configuration.lookup("biopepa.report.simulations.every") ^
                 "\n");
        TextIO.output(headerFile, 
           "#define Iterations " ^ iterations ^ "\n");
        TextIO.output(headerFile, 
           "#define ProgressInterval " ^ 
               Configuration.lookup("stochkit.opt.progress.interval") ^ 
                 "\n");
        TextIO.closeOut headerFile;

        CSVIterator.initialise();
        let val parameters = ref(CSVIterator.getParameters())
          in 
             if (!parameters = []) then
		 TextIO.output(TextIO.stdOut, "No parameter data found...\n")
             else
             while (!parameters <> []) do 
             let 
                val modelName = CSVIterator.getFileName()
                val stochkit = TextIO.openOut (modelName ^ ".cpp") 
             in
	        TextIO.output(stochkit, header);
	        TextIO.output(stochkit, repaintReactionDefs C);
	        TextIO.output(stochkit, repaintSpecies C);
	        TextIO.output(stochkit, reactionConstants C (!parameters));
	        TextIO.output(stochkit, stoichiometry C);
	        TextIO.output(stochkit, initialise C (!parameters));
		TextIO.output(stochkit, kineticFunctionsFile);
	        TextIO.output(stochkit, propensity C);
                parameters := CSVIterator.getParameters();
		TextIO.closeOut stochkit
             end
        end;
        CSVIterator.finalise()
      end
  
end;
