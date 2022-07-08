(* 
 File: Files.sml

 File handling for the BioPEPA Workbench
*)

structure Files :> Files =
struct

  val job = ref ""

  fun jobName () = JobName.stripExt (!job)

  val reverse = implode o rev o explode
  fun endsWith s1 s2 = String.isPrefix (reverse s2) (reverse s1)

  fun jobExt () = 
    if (endsWith (!job) ".biopepa") then ".biopepa"
	 else " [unknown type] "

  fun checkForOptions commentString = 
    let 
      fun isColon #":" = true | isColon _ = false
      val tokens = String.tokens isColon commentString
      val hd = List.hd
      val tl = List.tl
      fun stripSpace #" " = "" | stripSpace c = str c
      fun stripSpaces s = String.translate stripSpace s
    in
      if length tokens = 2 then
	  Configuration.set(stripSpaces (hd tokens), 
            hd (tl tokens))
      else ()
    end

  val inRate = ref false;

  fun readFile s =
    let 
      fun hasext s = endsWith s ".biopepa"
      val f = TextIO.openIn s handle _ => 
	TextIO.openIn (s^".biopepa") handle _ =>
	  if hasext s
	  then Error.fatal_error ("Cannot open \"" ^ s ^ "\"")
	  else Error.fatal_error ("Cannot open \"" ^ s ^ "\" or \"" ^ 
					   s ^ ".biopepa\"")

      val c = ref (SOME #" ")
      val r = ref [] : char list ref 

      fun prepend (ref NONE) = ()
        | prepend (ref (SOME c)) = r := c :: !r
    in
      job := s;
      while (c := TextIO.input1 f; !c <> NONE) do
        (* Added comment syntaxes *)
        if (!c = SOME #"/" andalso TextIO.lookahead f = SOME #"/") 
	then checkForOptions (FileUtils.inputLine f)
	else (* Not in comment, check for rate *)
             if (!c = SOME #"[") orelse (!c = SOME #":")
             then (inRate := true; prepend c) 
             else if (!inRate)
                  then if (!c = SOME #"]") orelse (!c = SOME #";")
                       then (inRate := false; prepend c) 
                       else prepend c
                  else (* Not in comment or rate, check for options *)
                       if (!c = SOME #"%") 
                       then checkForOptions (FileUtils.inputLine f)
                       else prepend c;
      TextIO.closeIn f;
      rev (!r)
    end

end;
