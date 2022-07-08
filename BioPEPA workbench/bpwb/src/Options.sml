(* 
 File: Options.sml

 User-controllable options for the PEPA Workbench.
*)

structure Options :> Options =
struct
  (* The recognised solvers *)
  datatype solver = XMLoutput | Maple | Matlab | Mathematica

  (* The user-selected solver (default: Maple) *)
  val solver = ref Maple
  fun setSolver s = solver := s

  (* The `no hashing' flag (default: false) *)
  val nohashing = ref false
  fun setNohashing () = nohashing := true

  (* The `branching' flag (default: false) *)
  val branching = ref false
  fun setBranching () = branching := true

  (* The `state space only' flag (default: false) *)
  val stateSpaceOnly = ref false
  fun setStateSpaceOnly () = stateSpaceOnly := true

  (* The `aggregating' flag (default: false) *)
  val aggregating = ref false
  fun setAggregating () = aggregating := true

  (* The `compile PEPA nets to PEPA' flag (default: false) *)
  val compile = ref false
  fun setCompile () = compile := true

  (* The `bridge PEPA model to Dizzy' flag (default: false) *)
  val bridge = ref false
  fun setBridge () = (bridge := true;  nohashing := true)

  (* The `generate a listing when compile PEPA net to PEPA' flag (default: false) *)
  val listing = ref false
  fun setListing () = listing := true

  (* The `view intermediate' flag (default: false) *)
  val viewintermediate = ref false
  fun setViewIntermediate () = viewintermediate := true

  (* Preferred filename extension for the selected solver *)
  fun ext () =
    case !solver of
	XMLoutput => ".xml"
      | Matlab => ".m"
      | Maple  => ".maple"
      | Mathematica  => ".m"

end;
