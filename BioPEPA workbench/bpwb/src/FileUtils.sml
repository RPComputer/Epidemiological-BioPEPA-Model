(*
   File: FileUtils.sml

   Implement post-2005 revision of the Basis inputLine

*)
structure FileUtils :> FileUtils =
struct

    fun inputLineChars is = 
       let val c = TextIO.input1 is
        in case c of
             NONE => [] 
           | SOME c' =>
                if c' = #"\n" orelse c' = #"\r"
                then []
                else c' :: inputLineChars is
       end

    fun inputLine is = 
       let val cs = inputLineChars is 
        in implode cs
       end

end;

