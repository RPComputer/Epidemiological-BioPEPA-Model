(* 
  File: BPReporter.sig

  This is the BioPEPA Workbench Report Generator.
*)
signature BPReporter =
sig
  val main : unit -> unit
  val Main : string list -> unit
  val command : string -> unit
end;
