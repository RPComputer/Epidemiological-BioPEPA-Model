(*
  File: Arcs.sml
 
  Functions for processing arcs in a PEPA Net.
*)
structure Arcs :> Arcs =
struct

  val arcs = ref [] : (PepaNets.Identifier * 
                         (PepaNets.Identifier * PepaNets.Rate) * 
                       PepaNets.Identifier) list ref

  (* Compute the set of arcs *)
  fun collectArcs (PepaNets.CONSTS (_, PepaNets.ARC a, P)) = a :: collectArcs P
    | collectArcs (PepaNets.CONSTS (_, _, P)) = collectArcs P
    | collectArcs _ = []
                                                                                
  fun lookup (P, []) = Error.fatal_error ("Could not find definition of " ^
        HashTable.unhash P)
    | lookup (P, (P1, (a, r), P2) :: t) = 
        if P = P1 
        then ((a,r), P2) :: lookup (P, t)
        else lookup (P, t)

  (* Compute the set of arcs from a given place *)
  fun arcsFrom P = lookup (P, !arcs)

  (* Store the set of arcs collected from the model *)                                                 
  fun compute M = arcs := collectArcs M

  (* For debugging: print the set of arcs *)
  fun show () = TextIO.output (TextIO.stdOut, String.concat (map pr (!arcs)))
  and pr (P1, (a,r), P2) = HashTable.unhash P1 ^ " -(" ^ 
      HashTable.unhash a ^ ", " ^ Prettyprinter.printRate r ^ ")-> " ^
      HashTable.unhash P2 ^ "\n" 

end;
