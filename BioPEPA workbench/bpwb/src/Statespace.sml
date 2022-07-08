(*
  File: Statespace.sml

  Structure for the state and transition monitoring operations.
*)

structure Statespace :> Statespace =
struct

  (* Events counter *)
  val events = ref 0;

  (* Transitions counter *)
  val transitions = ref 0;

  (* Firings counter *)
  val firings = ref 0;

  (* Transition file *)
  val transStream = ref TextIO.stdOut

  (* Write XML headers *)
  fun XMLinitialise () = 
      let val timestamp = Date.toString (Date.fromTimeLocal(LocalTime.now())) 
      in
	  TextIO.output(!transStream, "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n");
	  TextIO.output(!transStream, "<!-- Generated: " ^ timestamp ^ " -->\n");
	  TextIO.output(!transStream, "<!-- Generator: " ^ Version.banner ^ " -->\n");
	  TextIO.output(!transStream, "<!-- Input was: " ^ 
                                      Files.jobName() ^ 
                                      JobName.showExt() ^ " -->\n");
	  TextIO.output(!transStream, "<BioPEPA_Workbench_Matrix_Description>\n")
      end

  (* Initialization routine *)
  fun initialize () = 
     (transitions := 0; 
      firings := 0; 
      events := 0; 
      transStream := TextIO.openOut (Files.jobName() ^ Options.ext());
      if (!Options.solver = Options.XMLoutput) 
      then XMLinitialise()
      else ())

  (* Finalization routine *)
  fun finalize () = 
      (if (!Options.solver = Options.XMLoutput) 
       then TextIO.output(!transStream, "</BioPEPA_Workbench_Matrix_Description>\n")
       else (); 
       TextIO.closeOut (!transStream))
      
  fun formatTransition (n, (alpha,rate), m) =
    let
      val ns = Int.toString n 
      val ms = Int.toString m

      val (LPAR, RPAR, EQ, SEMI, COMBEGIN, COMEND) =
	  case !Options.solver of
	      Options.XMLoutput => ("", "", "", "", "", "")
	    | Options.Matlab => ("(", ")", "=", ";", " % ", "\n")
	    | Options.Maple  => ("[", "]", ":=", ":", " # ", "\n")
	    | Options.Mathematica  => ("[[", "]]", "=", "", " (* ", " *)\n")

      val stamp = str(String.sub (Files.jobName(), 0))
		  
      val Qentry   = "Q"^LPAR^ns^","^ms^RPAR
      val Qassign  = Qentry ^ EQ ^ Qentry^"+"^rate^SEMI^"\n"
      val Qtarget  = "Q"^LPAR^ns^","^ns^RPAR
      val Qcorrect = Qtarget ^ EQ ^ Qtarget^"-"^rate^SEMI
      val Comment  = COMBEGIN^stamp^ns^"--"^ alpha ^","^rate^"->"^stamp^ms^COMEND

      val XMLcomment = "  <!-- "^stamp^ns^"-("^ alpha ^","^rate^")->"^stamp^ms^" -->\n"
      fun XMLtransition ns ms activity rate = 
	  " <MatrixTransition start=\""^ns^
          "\"  activity = \""^activity^
          "\"  rate = \""^rate^
          "\"  finish =\""^ms^"\"/>\n"
      fun XMLassign ns ms rate = 
	  " <MatrixEntry row=\""^ns^
          "\"  column = \""^ms^"\" value =\""^rate^"\"/>\n"
                             
    in
      if (!Options.solver = Options.XMLoutput) 
      then XMLcomment ^ 
           XMLtransition ns ms alpha rate 
           (* ^  XMLassign ns ms rate ^ 
                 XMLassign ns ns ("-"^rate) *)
      else Qassign ^ Qcorrect ^ Comment
    end

  fun addTransition (P, NONE, Q) = 
      (Prettyprinter.setMode Prettyprinter.uncompressed;
       Error.warning ("phantom component " ^ Prettyprinter.print P))
    | addTransition (P, SOME ((alpha, rate), n), Q) = 
      (events := !events + 1;
       if !Options.stateSpaceOnly then () else 
          let val p = Table.getCode P
              val a = alpha
              val r = Prettyprinter.printRate rate
              val q = Table.getCode Q
              val t = formatTransition (p, (a, r), q)
          in 
	      TextIO.output (!transStream, t);
	      TextIO.flushOut(!transStream);
              if r = "infty"
              then Error.fatal_error ("Unsynchronised passive activity \"" ^ a ^ "\"")
              else ()
          end
      )
  fun countFiring () = firings := !firings + 1
  fun countTransition () = transitions := !transitions + 1
  fun transitionsspace 1 = "only one transition\n"
    | transitionsspace n = Int.toString n ^ " transitions\n"
  fun firingsspace 1 = "only one firing\n"
    | firingsspace n = Int.toString n ^ " firings\n"
  fun printCounters () = 
      (Reporting.report ("The model has " ^ transitionsspace (!transitions));
       Reporting.report ("The model has " ^ firingsspace (!firings)))
 
   val initialMarking = ref (BioPEPA.VAR " -- no initial state -- ")


end;
