(*
   File: Debugging.sml

   Printing messages for the user.
*)
structure Debugging :> Debugging =
struct

  val silent = ref false
  fun setSilent () = silent := true
  fun unsetSilent () = silent := false

  fun report s = 
      if !silent then () 
      else
	  (TextIO.output(TextIO.stdOut, "DEBUGGING: " ^ s); 
           TextIO.flushOut TextIO.stdOut)

  fun banner s = 
      let 
	  val angles = ">>>>>>>>>>>>>>>>>>>>>>>>>>" 
	  fun toUpper s = String.implode (map Char.toUpper (String.explode s))
      in "\n" ^ angles ^ " " ^ toUpper(s) ^ " " ^ angles ^ "\n"
      end


  fun shout s = 
      if !silent then () 
      else
	  (TextIO.output(TextIO.stdOut, banner s); 
           TextIO.flushOut TextIO.stdOut)

end;
