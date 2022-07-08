(* 
 File: Dot.sig

 Dot file generation for the Bio-PEPA Workbench
*)

signature Dot =
sig

  (* Compile to Dot format for visualisation *)
  val compile : BioPEPA.Component -> unit

end;
