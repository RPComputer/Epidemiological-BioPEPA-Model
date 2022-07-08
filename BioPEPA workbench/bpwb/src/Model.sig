(* 
 File: Model.sig

 Model component handling for the PEPA Workbench
*)

signature Model =
sig

  (* Converts a model component definition into a GNUplot
     string *)
  val toGnuplotString : string -> string

end;
