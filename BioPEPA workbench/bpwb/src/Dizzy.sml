(* File: Dizzy.sml

   Format parameters for Dizzy
*)

structure Dizzy :> Dizzy =
struct

   fun formatHeader s = 
     "// Dizzy model generated by the BioPEPA Workbench\n\n" ^
     "#model \"" ^ s ^ "\";\n" 

   fun formatParameters [] = ""
     | formatParameters ((p, v)::t) = BioPEPA.repaintIdentifier p ^ " = " ^ v ^ ";\n" ^ formatParameters t

   (* FIX ME *)
   fun formatReactions [] = ""
     | formatReactions (r::t) = r ^ formatReactions t

end;
