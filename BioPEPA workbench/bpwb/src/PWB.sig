(* 
  File: PWB.sig

  This is the PEPA Workbench
*)
signature PWB =
sig
  val main : unit -> unit
  val Main : string list -> unit
  val command : string -> unit
end;
