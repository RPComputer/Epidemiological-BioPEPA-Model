(*
   File: Debugging.sig

   Printing messages for the user.
*)
signature Debugging =
sig

  (* Be silent: report nothing.  Default: false *)
  val silent : bool ref
  val setSilent : unit -> unit
  val unsetSilent : unit -> unit

  (* Report messages to the user *)
  val report : string -> unit
  val shout : string -> unit

end;
