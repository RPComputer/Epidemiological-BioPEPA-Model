(* 
  File: pwb.sml

  This is wrapper used to compile the MLJ version of the Workbench.
  The filename is used as the name of the command so mljpwb.sml would
  not be as good.

  Usage:  In Persimmon MLJ, issue the command 
                make MLApp.pwb pwb
          to compile the Workbench.
*)

structure pwb  =
struct 

  fun main args = 
     (CommandLine.setArguments args;
      PWB.main (); OS.Process.success)
         handle Error.Fatal_error => 
                 (TextIO.output (TextIO.stdOut, 
                   "Exiting PEPA Workbench abnormally.\n");
                  TextIO.flushOut TextIO.stdOut;
                  OS.Process.failure)
              | uncaughtExn => 
                  (TextIO.output (TextIO.stdOut, 
                      "Sorry, uncaught exception: " 
                      ^ exnMessage uncaughtExn ^ "\n");
                   TextIO.flushOut TextIO.stdOut;
                   OS.Process.failure);

end;
