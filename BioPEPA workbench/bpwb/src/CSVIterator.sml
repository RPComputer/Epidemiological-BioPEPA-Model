(* 
 File: CSVIterator.sml

 The CSV table which stores parameters of a BioPEPA model. 
*)

structure CSVIterator :> CSVIterator =
struct

    val f = ref TextIO.stdIn

    val counter = ref 0
    val parameters = ref [ "uninitialised" ]

    fun zip (x::xs, y::ys) = (x,y) :: zip(xs,ys)
      | zip _ = []

    fun intToPaddedString n = 
        if n > 99 then Int.toString n
        else if n < 10 then "00" ^ Int.toString n
             else "0" ^ Int.toString n

    fun getFileName () = 
        (counter := !counter + 1; 
         JobName.show() ^ intToPaddedString(!counter))

    fun isComma c = c = #","
    fun deQuote #"\"" = "" 
      | deQuote #"(" = "" 
      | deQuote #")" = "" 
      | deQuote #" " = "" 
      | deQuote c = str c
    fun noQuotes s = String.translate deQuote s
    fun splitCommas s = map noQuotes (String.tokens isComma s)

    fun getParameters () =
        zip(!parameters, splitCommas(FileUtils.inputLine (!f)))

    fun initialise () =
    let 

      val c = ref (SOME #" ")
      val r = ref [] : char list ref 

      fun prepend (ref NONE) = ()
        | prepend (ref (SOME c)) = r := c :: !r

      fun printParams [] = ""
        | printParams (h::t) = h ^ " " ^ printParams t
    in
      counter := 0;
      f := 
        (TextIO.openIn ( JobName.show()^".csv") handle _ =>
        (TextIO.openIn ( JobName.show()^".parameters") handle _ =>
          Error.fatal_error ("Cannot open Bio-PEPA input file \"" ^ JobName.show() ^ ".{csv,parameters}\"")));
      parameters := splitCommas(FileUtils.inputLine (!f));
      (* TextIO.output(TextIO.stdOut, "Parameters: " ^ printParams (!parameters)); *)
      if (!parameters = []) then 
          Error.fatal_error ("Bio-PEPA input file \"" ^ JobName.show() ^ ".csv\" contains no data")
      else ()
    end

    fun finalise () = TextIO.closeIn (!f)

end;
