(*
   File: StyleSheet.sml
   
   Lookup Bio-PEPA Workbench LaTeX stylesheet settings.
*)

structure StyleSheet :> StyleSheet =
struct

   val lookupTable = ref [] : (string * string) list ref;

   fun dropLeadingSpaces v = 
       let fun drop (#" " :: t) = drop t
             | drop x = x 
        in implode(drop (explode v))
       end
   fun set (k,v) = lookupTable := (k, dropLeadingSpaces v) :: !lookupTable

   fun readStyleSheet s = 
       let 
           val f = TextIO.openIn(s ^ ".style")
           val line = ref (FileUtils.inputLine f)
        in 
           while (!line <> "") do 
              let 
                val thisList = String.tokens Char.isSpace (!line)
              in 
                if length(thisList) >= 2 
                then 
                   let 
                     val hd = List.hd
                     val tl = List.tl
                     fun concat [] = ""
                       | concat [x] = x
                       | concat (h::t) = h ^ ":" ^ concat t
                     val (key, value) = (hd thisList, concat (tl thisList))
                   in 
 	             set(key, value)
                   end
                else ();
                line := FileUtils.inputLine f
              end;
           TextIO.closeIn f
       end handle Empty => Error.warning("Bio-PEPA stylesheet corrupt")
                | _ => ()

   fun initialise () = readStyleSheet "biopepa"

   fun hbox s = "\\hbox{" ^ s ^ "}"
   fun mathit s = "\\mathit{" ^ s ^ "}"
   fun textit s = "\\textit{" ^ s ^ "}"

   local
      fun mem x [] = false
        | mem x (h::t) = x = h orelse mem x t
   in
      fun hasColon x = mem #":" (explode x)
   end

   fun find x [] = 
       if size x = 1 then x else 
       if hasColon x then hbox(textit x) else mathit x
     | find x ((k,v)::t) = if x=k then v else find x t;

   fun lookup s = find s (!lookupTable)

end;
