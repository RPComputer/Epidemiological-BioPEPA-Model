(* 
 File: Dot.sml

 Dot file generation for the BioPEPA Workbench
*)

structure Dot :> Dot =
struct
 
  (* Species begin with a capital letter *)
  fun isSpecies s = Char.isUpper(List.hd (explode s))


  fun repaintIdentifier i = 
    let 
        fun toCId #":" = "_colon_"
          | toCId #"-" = "_dash_"
          | toCId #"@" = "_at_"
          | toCId #"'" = "_prime_"
          | toCId #" " = ""
          | toCId c = str c
     in String.translate toCId i 
    end

  fun printSpeciesNode I = 
      repaintIdentifier I ^ " [style=bold,label=\"" ^ I ^ "\"];"
      
  fun printReactionNode I = 
      repaintIdentifier I ^ " [shape=box,fontsize=10,height=.2,label=\"" ^ I ^ "\"];"
      
  fun printReactantList (f, x, []) = ()
    | printReactantList (f, x, r::rs) = 
      (TextIO.output(f, 
        repaintIdentifier r ^ " -> " ^ repaintIdentifier x ^ ";\n");
       printReactantList (f, x, rs))

  fun printProductList (f, x, []) = ()
    | printProductList (f, x, p::ps) = 
      (TextIO.output(f, 
        repaintIdentifier x ^ " -> " ^ repaintIdentifier p ^ ";\n");
       printProductList (f, x, ps))

  fun printChannels (f, [], _, _) = ()
    | printChannels (f, h::t, r::rs, p::ps) = 
      (printReactantList (f, h, r);
       printProductList (f, h, p);
       TextIO.output (f, "\n");
       printChannels (f, t, rs, ps))
    | printChannels _ = 
       Error.fatal_error ("Failed when printing channels")

  fun compileChannels (f, C) =
      let 
          val (reactions, reactants, products, kinetics)
                = Reactions.channels C
          fun isPlus c = (c = #"+")
          fun split s = String.tokens isPlus s
      in printChannels (f, reactions, map split reactants, 
                                      map split products)
      end

  fun compileReactions (f, BioPEPA.CONSTS(I, D, M)) =
      if isSpecies I 
      then (TextIO.output(f, printSpeciesNode I ^ "\n");
            compileReactions (f, M))
      else (TextIO.output(f, printReactionNode I ^ "\n");
            compileReactions (f, M))
    | compileReactions (f, _) = ()

  fun compile m =
    let 
      val name = JobName.showBasename()
      val file = name ^ ".dot"
      val f = TextIO.openOut (file) handle _ => 
	  Error.fatal_error ("Cannot write to " ^ file)

    in
      TextIO.output (f, "/* This file was automatically generated\n");
      TextIO.output (f, "   by the Bio-PEPA Workbench.            */\n");
      TextIO.output (f, "digraph " ^ name ^ " {\n\n");
      compileChannels (f, m);
      compileReactions (f, m);
      TextIO.output (f, "\n}\n");
      TextIO.closeOut f
    end

end;
