(*
   File: Reactions.sig

   Compile reactions in Dizzy format for BioPEPA models
*)

signature Reactions =
sig

  val channels : BioPEPA.Component -> 
                 string list * string list * string list * string list
  val compile : BioPEPA.Component -> string list

end;
