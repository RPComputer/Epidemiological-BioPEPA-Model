(*
   File: Configuration.sml
   
   Lookup Bio-PEPA Workbench configuration settings.
*)

structure Configuration :> Configuration =
struct

   val defaults = [
       ("biopepa.simulation.stoptime", "100"),
       ("biopepa.independent.replications", "100"),
       ("biopepa.report.simulations.every", "10"),
       ("biopepa.report.title", "Bio-PEPA Model Report"),
       ("biopepa.report.author", "Bio-PEPA Workbench"),
       ("biopepa.report.institute", "\\today"),
       ("stochkit.opt.progress_interval", "1"),
       ("gnuplot.terminal", "postscript eps colour dash 20"),
       ("gnuplot.linestyle", "with linespoints"),
       ("gnuplot.xlabel", "Time"),
       ("gnuplot.ylabel", "Number"),
       ("gnuplot.key.position", "top right"),
       ("gnuplot.linewidth", ""),
       ("gnuplot.points", "") (* every point *)
   ];

   val lookupTable = ref defaults;

   fun isColon #":" = true | isColon _ = false

   fun dropLeadingSpaces v = 
       let fun drop (#" " :: t) = drop t
             | drop x = x 
        in implode(drop (explode v))
       end
   fun set (k,v) = lookupTable := (k, dropLeadingSpaces v) :: !lookupTable

   fun initialise () = 
       let 
           val f = TextIO.openIn("biopepa.cfg")
           val line = ref (FileUtils.inputLine f)
        in 
           while (!line <> "") do 
              let 
                val thisList = String.tokens isColon (!line)
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
       end handle Empty => Error.warning("Bio-PEPA configuration file corrupt")
                | _ => ()

   fun find x [] = ""
     | find x ((k,v)::t) = if x=k then v else find x t;

   fun lookup s = find s (!lookupTable)

end;
