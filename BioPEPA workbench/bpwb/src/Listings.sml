(*
   File: Listings.sml

   Printing a compiler listings file for the PEPA nets compiler.
*)
structure Listings :> Listings =
struct

  val silent = ref true
  fun setSilent () = silent := true
  fun unsetSilent () = silent := false

  val lisFile = ref NONE : TextIO.outstream option ref
  val buffer = ref [] : string list ref

  fun print (os, s) = (TextIO.output(os, s); TextIO.flushOut os)

  fun log s = case !lisFile of 
                   NONE => buffer := !buffer @ [s]
                 | SOME os => print(os, s)

  fun console s = print (TextIO.stdOut, s)

  fun report s = 
      (if !silent then () else console s ; log s)

  fun banner s = 
      let 
	  val angles = ">>>>>>>>>>>>>>>>>>>>>>>>>>" 
	  fun toUpper s = String.implode (map Char.toUpper (String.explode s))
      in "\n" ^ angles ^ " " ^ toUpper(s) ^ " " ^ angles ^ "\n"
      end

  fun now () = Date.toString(Date.fromTimeLocal(LocalTime.now()))

  fun initialise s = (lisFile := SOME (TextIO.openOut(JobName.show() ^ ".lis")); 
		      log ("[Lis file opened: " ^ now () ^ "]\n");
		      report s)

  fun flush ()     = (List.app log (!buffer))

  fun finalise s = case !lisFile of 
                     NONE => ()
                   | SOME os => (report s; 
				 log ("[Lis file closed: " ^ now () ^ "]\n");
                                 TextIO.closeOut(os))


  fun shout s = 
      report (banner s)

end;
