(* 
 File: Table.sig

 Signature for the table which stores states of a BioPEPA model. 
*)

signature Table =
sig
  (* Initialise the table *)
  val initialize : unit -> unit

  (* Finalize the table *)
  val finalize : unit -> unit

  (* Display the entire table *)
  val show : unit -> (int * string) list

  (* Return the number of entries in the table *)
  val show_size : unit -> int

  (* Code for referring to states *)
  type stateCode

  (* Report the addition of a state *)
  datatype stateResult = 
    NotYetSeen of stateCode
  | AlreadySeen of stateCode

  (* Add a state if it is not there already *)
  val addState : BioPEPA.State -> stateResult

  (* Mark a state as having been seen *)
  val markSeen : stateCode -> unit

  (* Add a state if it is not there already *)
  val addifmissing : string -> unit  

  (* Get the numeric code for a state *)
  val getCode : BioPEPA.State -> int

  (* Get the numeric code for a state *)
  val getcode : string -> (int * bool)
end;
