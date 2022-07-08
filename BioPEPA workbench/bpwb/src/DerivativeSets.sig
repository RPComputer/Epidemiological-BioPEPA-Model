(*
  File: DerivativeSets.sig

  Operations on the derivative sets of BioPEPA Components
*)
signature DerivativeSets = 
sig
  val compute : BioPEPA.Component -> unit 
  val ds : BioPEPA.Component -> BioPEPA.Component list
  val in_ds : (BioPEPA.Component * BioPEPA.Identifier) -> bool
  val show : unit -> unit
end;
