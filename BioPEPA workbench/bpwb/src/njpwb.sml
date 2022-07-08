(* 
  File: njpwb.sml

  Usage:  In Standard ML of New Jersey with CM loaded, issue the command
                CM.make();
          to recompile the Workbench and then
                use "njpwb.sml";
          to export it.
*)

CM.make();

(* Workaround for Windows/Linux portability bug *)
fun main () = 
    let
        val unixargs = CommandLine.arguments()
        val os = SMLofNJ.SysInfo.getOSKind()
    in
       if os =  SMLofNJ.SysInfo.UNIX then
            PWB.Main (unixargs)
       else
            PWB.Main (List.tl(unixargs))
    end


fun NJmain _ = (main (); OS.Process.success) 
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

SMLofNJ.exportFn ("pwb.image", NJmain);
