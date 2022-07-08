(*
   File: Dizzy.sig

   Format parameters for Dizzy
*)

signature Dizzy = 
sig
   val formatHeader : string -> string
   val formatParameters : (string * string) list -> string
   val formatReactions : string list -> string
end;
