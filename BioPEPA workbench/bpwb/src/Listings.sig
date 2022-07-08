(*
   File: Listings.sig

   Printing a compiler listings file for the PEPA nets compiler.
*)
signature Listings =
sig

  (* Be silent: report nothing.  Default: true *)
  val silent : bool ref
  val setSilent : unit -> unit
  val unsetSilent : unit -> unit

  (* Initialise lis file *)
  val initialise : string -> unit

  (* Flush buffered lis messages *)
  val flush : unit -> unit

  (* Finalise lis file *)
  val finalise : string -> unit

  (* Report messages to the user *)
  val report : string -> unit
  val shout : string -> unit

end;
