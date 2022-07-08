(*
  File: Statespace.sig

  Signature for the state and transition monitoring operations.
*)

signature Statespace =
sig

  (* open output stream *)
  val initialize : unit -> unit
 
  (* close ouput stream *)
  val finalize : unit -> unit
 
  (* add one transition *)
  val addTransition : BioPEPA.Transition -> unit

  (* Counting events (either firings or transitions) *)
  val countFiring : unit -> unit
  val countTransition : unit -> unit

  (* Finalisation routine, print values of counters *)
  val printCounters : unit -> unit

  (* initial marking *)
  val initialMarking : BioPEPA.Component ref

end;
