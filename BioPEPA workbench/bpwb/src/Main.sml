structure Main :> Main =
struct
  fun main args = 
     (TextIO.output (TextIO.stdOut, "Starting ... \n");
      CommandLine.setArguments args;
      map pr (CommandLine.arguments ());
      map pr args;
      TextIO.output (TextIO.stdOut, "... finished! \n");
      TextIO.flushOut TextIO.stdOut)
  and pr s = TextIO.output (TextIO.stdOut, s ^ "\n")

end;
