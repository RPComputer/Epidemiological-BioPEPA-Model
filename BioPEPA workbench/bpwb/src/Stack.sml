(* 
   Stack of bracket characters scanned when parsing.
*)
structure Stack :> Stack = 
struct

   val stack = ref [] : char list ref

   fun push c = stack := c :: !stack

   fun empty() = !stack = []

   fun top() = 
       if empty() then #"?"
       else List.hd(!stack)

   fun pop c = 
      if empty() then 
          Error.fatal_error("Too many closing brackets.\n>>\t" ^
            "Could not find a matching \"" ^ str(c) ^ "\"")
       else if top() = c then stack := List.tl(!stack) else
          Error.fatal_error("Brackets are mis-matched.\n>>\t" ^
            "Bracket \"" ^ str(c) ^ "\" is not closed with a match.")

end;
