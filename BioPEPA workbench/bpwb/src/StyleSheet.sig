(*
   File: StyleSheet.sig

   Lookup up Bio-PEPA Workbench LaTeX stylesheet settings.
*)
signature StyleSheet =
sig
   val readStyleSheet : string -> unit
   val initialise : unit -> unit
   val lookup : string -> string
end;
