(* File: Highslide.sml

   Format parameters for Highslide
*)

structure Highslide :> Highslide =
struct

  val rep = ref TextIO.stdOut

  fun pr s = (TextIO.output(!rep, s ^ "\n"))
         handle _ => 
            let val basename = JobName.showBasename()
                val htmlfilename = basename ^ ".html"
            in
                Error.fatal_error("Could not write to " ^ htmlfilename)
            end

  fun initialise () = 
    let val basename = JobName.showBasename()
        val htmlfilename = basename ^ ".html"
     in rep := TextIO.openOut(htmlfilename)
            handle _ => Error.fatal_error("Could not open " ^ htmlfilename);
        pr "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"xhtml11.dtd\">";
        pr ("<!-- Generated by " ^ Version.banner ^ " -->");
        pr "<html>";
        pr "<head>";
        pr "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />";
        pr "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/highslide.css\" />";
        pr ("<title>Bio-PEPA model: " ^ basename ^ "</title>");
        pr "";
        pr "<!-- ";
        pr "    1 ) Reference to the file containing the javascript. ";
        pr "    This file must be located on your server. ";
        pr "-->";
        pr "";
        pr "<script type=\"text/javascript\" src=\"highslide/highslide.js\"></script>";
        pr "";
        pr "<!-- ";
        pr "    2) Optionally override the settings defined at the top";
        pr "    of the highslide.js file. The parameter hs.graphicsDir is important!";
        pr "-->";
        pr "";
        pr "<script type=\"text/javascript\">";    
        pr "    hs.graphicsDir = 'highslide/graphics/';";
        pr "";    
        pr "    // Identify a caption for all images. This can also be";
        pr "    // set inline for each image.";
        pr "    hs.captionId = 'the-caption';";
        pr "";    
        pr "    hs.outlineType = 'rounded-white';";
        pr "</script>";
        pr "";
        pr "</head>";
        pr "";
        pr "<body style=\"background-color: silver\">";
        pr "<div>";
        pr "<!-- ";
        pr "    4) This is how you mark up the thumbnail image ";
        pr "       with an anchor tag around it.  The anchor's href ";
        pr "       attribute defines the URL of the full-size image.";
        pr "-->";
        pr ""
    end

  fun writeOne s = 
  let
  in 
        pr ("<a href=\"png/" ^ s ^ ".png\" class=\"highslide\" onclick=\"return hs.expand(this)\">");
        pr ("        <img src=\"thumbnails/" ^ s ^ ".png\" alt=\"" ^ s ^ "\""); 
        pr ("                title=\"" ^ s ^ ": click to enlarge\" height=\"75\" width=\"100\" /></a>");
        pr ""
  end

  fun writeAll [] = ()
    | writeAll (h::t) = (writeOne h ; writeAll t)

  fun finalise () = 
    let 
    in 
        pr "";
        pr "<!-- ";
        pr "    5 (optional). This is how you mark up the caption.";
        pr "-->";
        pr "<div class='highslide-caption' id='the-caption'>";
        pr "    <a href=\"#\" onclick=\"return hs.previous(this)\" class=\"control\" style=\"float:left; display: block\">";
        pr "            Previous";
        pr "            <br/>";
        pr "            <small style=\"font-weight: normal; text-transform: none\">left arrow key</small>";
        pr "    </a>";
        pr "        <a href=\"#\" onclick=\"return hs.next(this)\" class=\"control\""; 
        pr "                        style=\"float:left; display: block; text-align: right; margin-left: 50px\">";
        pr "                Next";
        pr "                <br/>";
        pr "                <small style=\"font-weight: normal; text-transform: none\">right arrow key</small>";
        pr "        </a>";
        pr "    <a href=\"#\" onclick=\"return hs.close(this)\" class=\"control\">Close</a>";
        pr "    <a href=\"#\" onclick=\"return false\" class=\"highslide-move control\">Move</a>";
        pr "    <div style=\"clear:both\"></div>";
        pr "</div>";
        pr "";
        pr "</div>";
        pr "</body>";
        pr "</html>";
       TextIO.closeOut(!rep)
    end

  fun write s = 
      let val basename = JobName.showBasename()
      in initialise() ; writeAll (basename :: s) ; finalise()
      end

end;
