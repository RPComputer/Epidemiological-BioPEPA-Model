(*
   File: Gnuplot.sig

   Format parameters for Gnuplot
*)

signature Gnuplot = 
sig
   val setModelDefs : (string * string) list -> unit
   val formatHeader : string -> string
   val formatParameters : string -> (string * string) list -> string
   val formatParametersFiltered : string -> (string * string) list -> string list -> string
   val generate : (string * (string * string) list) -> unit
   val generated : unit -> string list
end;
