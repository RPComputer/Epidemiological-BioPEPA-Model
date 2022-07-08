(* 
 File: CSL.sml

 CSL compilation for the Bio-PEPA Workbench
*)

structure CSL :> CSL =
struct

  val PRISMid = PRISM.PRISMid

  fun ^^ (s1, s2) = s1 ^ "\n" ^ s2
  infixr ^^

  fun header () = 
      "// CSL formulae generated for PRISM model compiled from Bio-PEPA input file \"" ^ JobName.show() ^ "\" by" ^^
      "// " ^ Version.banner ^^
      "" ^^
      "// Constants:" ^^
      "const double T; // time instant" ^^
      "const int i; // number of molecules" ^^
      "const int rew; // reward variable" ^^
      ""

  fun footer () = 
      "// End CSL formulae model compiled from " ^ JobName.show() ^^ ""

  fun harvestSpecies (BioPEPA.CONSTS(I, BioPEPA.RATE _, C)) = 
      harvestSpecies C
    | harvestSpecies (BioPEPA.CONSTS(I, _, C)) = 
      I :: harvestSpecies C
    | harvestSpecies _ = []

  fun isSpecies s = Char.isUpper(List.hd(explode s))
  fun harvestModelComponents (BioPEPA.CONSTS(I, BioPEPA.RATE _, C)) = 
      let val result = harvestModelComponents C
       in if isSpecies I then I :: result else result
      end
    | harvestModelComponents (BioPEPA.CONSTS(I, _, C)) = 
      harvestModelComponents C
    | harvestModelComponents _ = []

  fun harvestReactions (BioPEPA.CONSTS(I, BioPEPA.RATE _, C)) = 
      let val result = harvestReactions C
       in if isSpecies I then result else I :: result
      end
    | harvestReactions (BioPEPA.CONSTS(I, _, C)) = 
      harvestReactions C
    | harvestReactions _ = []

  fun formulae species = 
  let
    fun quote s = "\"" ^ s ^ "\""

    fun dec d = 
        "// Probability of there being i molecules of species " ^ d ^ " at time T?" ^^
        "P=? [ true U[T,T] " ^ PRISMid d ^ "=i ]" ^^
        "" ^^
        "// What is the probability of reaching the maximum number of molecules of species " ^ d ^ " at time T?" ^^
        "P=? [ true U[T,T] " ^ quote (PRISMid d ^ "_at_maximum") ^ " ]" ^^
        "" ^^
        "// What is the probability of having reached the maximum number of molecules of species " ^ d ^ " at time T or before?" ^^
        "P=? [ true U<=T " ^ quote (PRISMid d ^ "_at_maximum") ^ " ]" ^^
        "" ^^
        "// Expected number of " ^ d ^ " molecules at time T?" ^^
        "R{\"" ^ PRISMid d ^ "\"}=? [ I=T ]" ^^
        "" ^^
        "// Expected long-run number of " ^ d ^ " molecules?" ^^
        "R{\"" ^ PRISMid d ^ "\"}=? [ S ]" ^^
        "" ^^
        "// Stability of species " ^ d ^ " in the steady-state?" ^^
        "S=? [(" ^ PRISMid d ^ " >= (i - 1)) & (" ^ PRISMid d ^ " <= (i + 1)) ]" ^^
        ""
    fun decs [] = ""
      | decs (h::t) = dec h ^^ decs t
   in decs (species)
  end

  fun mcformulae species = 
  let
    fun dec d = 
        "// Expected value of model component " ^ d ^ " at time T?" ^^
        "R{\"" ^ d ^ "\"}=? [ I=T ]" ^^
        "" ^^
        "// Expected long-run value of model component " ^ d ^ "?" ^^
        "R{\"" ^ d ^ "\"}=? [ S ]" ^^
        ""
    fun decs [] = dec "rew"
      | decs (h::t) = dec (PRISMid h) ^^ decs t
   in decs (species)
  end

  fun reactionFormulae species = 
  let
    fun dec d = 
        "// Expected number of occurrences of reaction " ^ d ^ " at time T?" ^^
        "R{\"" ^ PRISMid d ^ "\"}=? [ C<=T ]" ^^
        ""
    fun decs [] = ""
      | decs (h::t) = dec h ^^ decs t
   in decs (species)
  end

  fun labels species = 
  let
    fun quote s = "\"" ^ s ^ "\""
    fun lab d = 
        "// Is species " ^ d ^ " depleted?" ^^
        "label " ^ quote (PRISMid d ^ "_depleted") ^ " = " ^ PRISMid d ^ " = 0;" ^^
        "" ^^
        "// Is species " ^ d ^ " at its maximum value?" ^^
        "label " ^ quote (PRISMid d ^ "_at_maximum") ^ " = " ^ PRISMid d ^ " = MAX;" ^^
        ""
    fun labs [] = ""
      | labs (h::t) = lab h ^^ labs t
   in labs (species)
  end

  fun generate C = 
  let 
      val f = TextIO.openOut(JobName.show() ^ ".csl")
      val species = harvestSpecies C
      val modelComponents = harvestModelComponents C
      val reactions = harvestReactions C
   in 
      TextIO.output(f,
        header() ^^
        formulae species ^^
        mcformulae modelComponents ^^
        reactionFormulae reactions ^^
        labels species ^^
        footer());
      TextIO.closeOut f
  end

end;
