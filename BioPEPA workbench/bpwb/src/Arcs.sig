(*
  File: Arcs.sig

  Functions for processing arcs in a PEPA Net.
*)
signature Arcs = 
sig
  val compute : PepaNets.Component -> unit 

  val arcsFrom : PepaNets.Identifier -> 
      ((PepaNets.Identifier * PepaNets.Rate) * PepaNets.Identifier) list

  val show : unit -> unit
end;
