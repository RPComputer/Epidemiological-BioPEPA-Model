(* 
   Stack of bracket characters scanned when parsing.
*)
signature Stack = 
sig

   val push : char -> unit
   val empty : unit -> bool
   val top : unit -> char
   val pop : char -> unit

end;
