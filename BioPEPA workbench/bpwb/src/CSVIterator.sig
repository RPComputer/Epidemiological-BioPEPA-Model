(* 
 File: CSVIterator.sig

 Signature for the CSV table which stores parameters of a BioPEPA model. 
*)

signature CSVIterator =
sig
    val initialise : unit -> unit
    val getFileName : unit -> string
    val getParameters : unit -> (string * string) list
    val finalise : unit -> unit
end;
