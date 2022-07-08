(* 
  File: PWB.sml

  This is the PEPA Workbench for PEPA Nets.
*)

structure PWB :> PWB  =
struct 

  fun tableentries 1 = "only one entry\n"
    | tableentries n = Int.toString n ^ " entries\n"
  fun statespace 1 = "only one state\n"
    | statespace n = Int.toString n ^ " states\n"

  fun startsWithHyphen (#"-" :: _) = true
    | startsWithHyphen _ = false

  fun isNotFlag s = not (startsWithHyphen (explode s))

  exception AbruptExit

  fun checkForFlags "-nohashing" = Options.setNohashing ()

    | checkForFlags "-help" = Help.help AbruptExit
    | checkForFlags "-version" = printVersion AbruptExit

    | checkForFlags "-silent" = (Reporting.setSilent ();
                                 Listings.setSilent())

    | checkForFlags "-aggregate" = Options.setAggregating ()
    | checkForFlags "-aggregating" = Options.setAggregating ()
    | checkForFlags "-viewintermediate" = (Options.setViewIntermediate ();
					   Options.setAggregating ();
					   Reporting.unsetSilent();
                                           Listings.unsetSilent())

    | checkForFlags "-branching" = Options.setBranching ()

    | checkForFlags "-compile" = Options.setCompile ()
    | checkForFlags "-bridge" = Options.setBridge ()
    | checkForFlags "-listing" = Options.setListing ()

    | checkForFlags "-statespaceonly" = Options.setStateSpaceOnly ()

    | checkForFlags "-xmloutput" = (Options.setNohashing ();
                                    Options.setSolver Options.XMLoutput)
    | checkForFlags "-maple" = Options.setSolver Options.Maple
    | checkForFlags "-matlab" = Options.setSolver Options.Matlab
    | checkForFlags "-mathematica" = Options.setSolver Options.Mathematica

    | checkForFlags "-priorities" = Priorities.setEnabled()

    | checkForFlags s = if isNotFlag s then () else 
        (Error.warning ("unrecognised flag ignored: " ^ s);
	 Help.help AbruptExit)
  and printVersion (Exit: exn) = 
        (TextIO.output (TextIO.stdOut, (Version.banner ^ "\n")); 
         TextIO.flushOut (TextIO.stdOut);
	 raise Exit)

  val nonFlagArgs = List.filter isNotFlag
 
  fun getJobName args =
    case nonFlagArgs args of
      [] => (TextIO.output (TextIO.stdOut, "Filename: ");
             TextIO.flushOut TextIO.stdOut;
             (FileUtils.inputLine TextIO.stdIn))
    | [x] => x
    | xs  => concatWithSpaces xs
  and concatWithSpaces [] = ""
    | concatWithSpaces [x] = x
    | concatWithSpaces (h::t) = h ^ " " ^ concatWithSpaces t

  fun marking (P as PepaNets.MARKING _) = P
    | marking (P as PepaNets.CONSTS (_, _, P')) = marking P'
    | marking _ = Error.fatal_error "No marking in model"

  fun run jobname = 
    let
      val theFile = Files.readFile jobname
      fun viewIntermediate (title, model) =
	  if !Options.viewintermediate 
          then (Reporting.shout ("Begin " ^ title ^ " ");
		Prettyprinter.setMode Prettyprinter.verbose;
		Reporting.report (Prettyprinter.print (!model));
		Reporting.shout ("End of " ^ title))
          else ()
    in
      Reporting.report ("Processing input from " ^ jobname ^"\n");
      Reporting.report ("Compiling the model\n");
      let 
        val M = ref (Semantic.analyse 
                       (Parser.parse 
                          ((* Preparser.preparse  *) (fn s => s)
                             (Lexer.analyse theFile))))
      in
	JobName.set(JobName.stripExt jobname);
	viewIntermediate("parsed model", M);
	if !Options.aggregating then M := Folding.fold (!M) else ();
	viewIntermediate("folded model", M);

	Statespace.initialMarking := marking (!M);

        DerivativeSets.compute (!M);

        Arcs.compute (!M);

        Reporting.report ("Generating the derivation graph\n");
        Graph.compile (!M)
      end;
      Reporting.report ("The model has " ^ statespace (Table.show_size()));
      Statespace.printCounters()
    end

  fun writeHashFile jobname =
    if !Options.nohashing then () else
      let 
        val hashfile = jobname ^ ".hash"
        val h = TextIO.openOut hashfile
      in 
        Reporting.report ("Writing the hash table file to " ^ hashfile ^ "\n");
        TextIO.output (h, HashTable.show_hash_table());
        TextIO.closeOut h
      end

  (* The arguments to the PEPA Workbench can include UNIX-style flags 
     -statespaceonly, -nohashing, -maple, -mathematica, -matlab, -compile
     and so forth.  They can include a filename. 
   *)
  fun Main args =
    let
    in
      (* Print "PEPA Workbench Version ... [compiled  ...]" *)
      map checkForFlags args;
      Reporting.initialise (Version.banner ^ "\n");


      if (!Options.nohashing) 
         then Reporting.report "[ Identifiers will not be hashed ]\n" else ();
      if (!Options.aggregating) 
         then Reporting.report "[ Setting model aggregation on ]\n" else ();
      if (!Priorities.enabled)  
         then Reporting.report "[ Use of priorities is enabled ]\n" else ();

      let 
	  val jobname = getJobName args
      in  
	  JobName.set (JobName.stripExt jobname);
	  Reporting.flush();
	  if (!Options.compile) then 
               (Listings.initialise (Version.banner ^ "\n");
                Compiler.compileFromFile jobname;
                Listings.finalise("Exiting PEPA nets compiler.\n"))
	  else if (!Options.bridge) then 
               (Listings.initialise (Version.banner ^ "\n");
                Bridge.bridgeToDizzyFromFile (JobName.stripExt jobname);
                Listings.finalise("Exiting PEPA Workbench bridge to Dizzy.\n"))
          else (run jobname;
	        writeHashFile (JobName.stripExt jobname));
	  Reporting.finalise ("Exiting PEPA Workbench.\n")
      end
    end
    handle 
        Error.Fatal_error => 
             Reporting.finalise ("Exiting PEPA Workbench abnormally.\n")
      | IO.Io {cause,function,name} =>
	     Error.fatal_error ("PWB: attempted " ^ function ^ " on [" ^ name ^ "]") 
      | AbruptExit => ()
  (* The main function requests a list of arguments from CommandLine.  *)
  and main () = Main (CommandLine.arguments ()) 
  and command c = Main (String.tokens Char.isSpace c)

end;
