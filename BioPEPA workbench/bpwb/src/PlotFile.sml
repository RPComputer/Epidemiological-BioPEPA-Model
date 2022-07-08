(* 
 File: PlotFile.sml

 The file which stores the names of the parameters to be plotted, if
 less than all are required.
*)

structure PlotFile :> PlotFile =
struct

    val f = ref TextIO.stdIn
    val initialised = ref false

    val parameters : string list ref = ref []


    fun isComma c = c = #","
    fun splitCommas s = String.tokens isComma s

    fun initialise () =
       (f := TextIO.openIn ( JobName.show()^".plot");
        parameters := Sort.quicksort(splitCommas(FileUtils.inputLine (!f))))
             handle _ => ();

    fun finalise () = TextIO.closeIn (!f)

    fun getParameters() = 
        if (!initialised) then !parameters
        else (initialise(); initialised := true ; !parameters)

end;
