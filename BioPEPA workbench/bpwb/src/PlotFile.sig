(* 
 File: PlotFile.sig

 The file which stores the names of the parameters to be plotted, if
 less than all are required.
*)

signature PlotFile =
sig

    val initialise : unit -> unit
    val finalise : unit -> unit
    val getParameters : unit -> string list

end;
