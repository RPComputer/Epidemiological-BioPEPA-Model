(*
   File: Reporting.sml

   Printing messages for the user.
*)
structure Reporting :> Reporting =
struct

  val silent = ref false
  fun setSilent () = silent := true
  fun unsetSilent () = silent := false

  val logFile = ref NONE : TextIO.outstream option ref
  val buffer = ref [] : string list ref
  
  fun openLog()  = 
      case !logFile of 
          NONE => logFile := SOME (TextIO.openOut(JobName.show() ^ ".log"))
        | SOME os => ()

  fun print (os, s) = 
      (TextIO.output(os, s); TextIO.flushOut os)

  (* Attempt to make printing safe against closed files *)
  fun printSafe (os, s) = 
      (TextIO.output(os, s); TextIO.flushOut os)
      handle IO.Io {cause,function,name} =>
	     (openLog(); TextIO.output(os, s); TextIO.flushOut os)


  fun log s = case !logFile of 
                   NONE => buffer := !buffer @ [s]
                 | SOME os => printSafe(os, s)

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

  fun initialise s = (report s)

  fun flush ()     = (openLog();
		      log ("[Log file opened: " ^ now () ^ "]\n");
		      List.app log (!buffer))

  fun finalise s = case !logFile of 
                     NONE => ()
                   | SOME os => (report s; 
				 log ("[Log file closed: " ^ now () ^ "]\n");
                                 TextIO.closeOut(os);
				 logFile := NONE)


  fun shout s = 
      report (banner s)

end;
