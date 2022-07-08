(*
   File: Help.sml

   Display help messages on command-line usage of the Workbench
*)

structure Help :> Help =
struct

  fun print s  = (TextIO.output(TextIO.stdOut, s);
                  TextIO.output(TextIO.stdOut, "\n"))
  fun flush () = (TextIO.flushOut(TextIO.stdOut))

  fun help (Exit: exn) = 
    (print "Usage:";
     print "  pwb [options] filename.pepa";
     print "where options include ";
     print "  -help               Print this message and exit";
     print "  -version            Print version number information and exit";
     print "  -silent             Run silently, produce no console messages";
     print "  -nohashing          Do not write a hash table file";
     print "  -statespaceonly     Do not write a transition matrix file";
     print "  -aggregate          Try to aggregate the model to reduce state space ";
     print "  -aggregating        Provided as a synonym for -aggregate ";
     print "  -viewintermediate   View intermediate transformations when aggregating ";
     print "  -branching          Print branching information for each state ";
     print "  -xmloutput          Write output in XML format";
     print "  -maple              Write output in Maple(TM) format";
     print "  -matlab             Write output in Matlab(TM) format";
     print "  -mathematica        Write output in Mathematica(TM) format";
     print "  -priorities         Allow use of priorities on firings";
     print "  -compile            Compile a PEPA net to an equivalent PEPA model";
     print "  -bridge             Bridge between the PEPA Workbench and Dizzy simulator";
     print "  -listing            Generate a compiler listing when compiling to PEPA";
     flush ();
     raise Exit)
end;
