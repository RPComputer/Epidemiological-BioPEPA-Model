(* 
 File: Options.sig

 User-controllable options for the PEPA Workbench.
*)

signature Options =
sig
  (* The recognised solvers *)
  datatype solver = XMLoutput | Maple | Matlab | Mathematica

  (* The user-selected solver (default: Maple) *)
  val solver : solver ref
  val setSolver : solver -> unit

  (* The `no hashing' flag (default: false) *)
  val nohashing : bool ref
  val setNohashing : unit -> unit

  (* The `branching' flag (default: false) *)
  val branching : bool ref
  val setBranching : unit -> unit

  (* The `state space only' flag (default: false) *)
  val stateSpaceOnly : bool ref
  val setStateSpaceOnly : unit -> unit

  (* The `aggregating' flag (default: false) *)
  val aggregating : bool ref
  val setAggregating : unit -> unit

  (* The `compile PEPA net to PEPA' flag (default: false) *)
  val compile : bool ref
  val setCompile : unit -> unit

  (* The `bridge PEPA model to Dizzy' flag (default: false) *)
  val bridge : bool ref
  val setBridge : unit -> unit

  (* The `generate a listing when compile PEPA net to PEPA' flag (default: false) *)
  val listing : bool ref
  val setListing : unit -> unit

  (* The `view intermediate' flag (default: false) *)
  val viewintermediate : bool ref
  val setViewIntermediate : unit -> unit

  (* Preferred filename extension for the selected solver *)
  val ext : unit -> string
end;
