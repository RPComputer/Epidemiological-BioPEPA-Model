(*
   File: Reporting.sig

   Printing messages for the user.
*)
signature Reporting =
sig

  (* Be silent: report nothing.  Default: false *)
  val silent : bool ref
  val setSilent : unit -> unit
  val unsetSilent : unit -> unit

  (* Initialise log file *)
  val initialise : string -> unit

  (* Flush buffered log messages *)
  val flush : unit -> unit

  (* Finalise log file *)
  val finalise : string -> unit

  (* Report messages to the user *)
  val report : string -> unit
  val shout : string -> unit

end;
