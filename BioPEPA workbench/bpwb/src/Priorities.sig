(*
   File: Priorities.sig

   Priorities for PEPA net firings
*)

signature Priorities =
sig
  
   (* Priorities are enabled: default false *)
   val enabled : bool ref

   (* Enable priorities *)
   val setEnabled : unit -> unit

   (* Set the priorities of firings *)
   val setPriorities : PepaNets.Identifier list list -> unit

   (* Get the priority of a firing, either NONE or SOME int *)
   val getPriority : PepaNets.Identifier -> int option

end;
