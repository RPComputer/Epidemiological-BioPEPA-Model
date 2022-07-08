(* 
 File: Species.sig

 Species handling for the PEPA Workbench
*)

signature Species =
sig

  (* Set a species to a particular index *)
  val set : (string * int) -> unit

  (* Look up the index of a species *)
  val lookup : string -> int

end;
