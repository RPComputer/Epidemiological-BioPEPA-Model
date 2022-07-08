(*
   File: Configuration.sig

   Lookup up Bio-PEPA Workbench configuration settings.
*)
signature Configuration =
sig
   val initialise : unit -> unit
   val lookup : string -> string
   val set : (string * string) -> unit
end;
