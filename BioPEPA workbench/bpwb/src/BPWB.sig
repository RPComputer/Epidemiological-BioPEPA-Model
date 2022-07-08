(* 
  File: BPWB.sig

  This is the BioPEPA Workbench
*)
signature BPWB =
sig
  val main : unit -> unit
  val Main : string list -> unit
  val command : string -> unit
end;
