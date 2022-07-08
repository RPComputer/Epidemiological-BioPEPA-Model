(* 
   File: JobName.sig

   Record the name of the current job.
*)

signature JobName =
sig
   val set : string -> unit
   val show : unit -> string
   val showBasename : unit -> string
   val showExt : unit -> string
   val stripExt : string -> string
end;
