(* 
   File: JobName.sml

   Record the name of the current job.
*)

structure JobName :> JobName =
struct
   val jobname = ref ""
   val jobnameExt = ref ""
   val basename = ref "notset"

   fun show () = !jobname

   (* Include both directions of slashes for Windows and Linux *)
   fun hasSlash [] = false
     | hasSlash (h::t) = h = #"/" orelse h = #"\\" orelse hasSlash t

   (* The following predicate tests if the filename contains a 
      period after the last slash *)
   fun hasPeriod [] = false
     | hasPeriod (h::t) = 
       if (h = #".")
	  then if hasSlash t
		  then hasPeriod t
	       else true
       else hasPeriod t

   fun dropPeriod [] = []
     | dropPeriod (#"." :: t) = t
     | dropPeriod (h::t) = dropPeriod t

   fun dropWhitespace [] = []
     | dropWhitespace (#" " :: t) = t
     | dropWhitespace (#"\n" :: t) = t
     | dropWhitespace (#"\r" :: t) = t
     | dropWhitespace (h::t) = dropWhitespace t

   fun takePeriod [] = []
     | takePeriod (#"." :: t) = [#"."]
     | takePeriod (h::t) = h :: takePeriod t

   fun stripExt string = 
       let val s = rev (explode string) 
        in if hasPeriod (rev s)
           then (jobnameExt := implode (rev (takePeriod s));
		 implode (rev (dropPeriod s)))
           else implode (rev (dropWhitespace s))
       end

   fun showExt () = !jobnameExt

   (* Include both directions of slashes for Windows and Linux *)
   fun keepToSlash [] = []
     | keepToSlash (#"/" :: t) = []
     | keepToSlash (#"\\" :: t) = []
     | keepToSlash (h::t) = h :: keepToSlash t

   fun lastName string = 
       let val s = rev (explode string) 
        in if hasSlash s
           then implode (rev (keepToSlash s))
           else string 
       end

   fun showBasename () = !basename

   fun set s = (jobname := s; basename := lastName s)

end;
