(* 
  File: BPReporter.sml

  This is the BioPEPA Workbench Report Generator.
*)

structure BPReporter :> BPReporter  =
struct 

  fun tableentries 1 = "only one entry\n"
    | tableentries n = Int.toString n ^ " entries\n"
  fun statespace 1 = "only one state\n"
    | statespace n = Int.toString n ^ " states\n"

  fun startsWithHyphen (#"-" :: _) = true
    | startsWithHyphen _ = false

  fun isNotFlag s = not (startsWithHyphen (explode s))

  exception AbruptExit

  (* TODO: remove or rewrite this function *)
  fun checkForFlags "-nohashing" = Options.setNohashing ()

    | checkForFlags "-help" = Help.help AbruptExit
    | checkForFlags "-version" = printVersion AbruptExit

    | checkForFlags "-aggregate" = Options.setAggregating ()
    | checkForFlags "-aggregating" = Options.setAggregating ()
    | checkForFlags "-viewintermediate" = (Options.setViewIntermediate ();
					   Options.setAggregating ();
					   Reporting.unsetSilent())

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

  fun marking (P as BioPEPA.CONSTS (_, _, P')) = marking P'
    | marking P = P

  fun run jobname = 
    let
      val theFile = Files.readFile jobname
      val M = ref (Semantic.analyse 
                       (Parser.parse 
                          (Preparser.preparse
                             (Lexer.analyse theFile))))
    in
      Reporting.report ("Processing input from " ^ jobname ^"\n");
      StochKit.compile (!M);
      CSVIterator.initialise();
      PlotFile.initialise();
      let val parameters = ref(CSVIterator.getParameters())
      in while (!parameters <> []) do 
             let 
                 val modelName = CSVIterator.getFileName()
             in 
                 Gnuplot.generate(modelName, !parameters);
		 parameters := CSVIterator.getParameters()
             end
      end;
      Highslide.write(Gnuplot.generated());
      StyleSheet.readStyleSheet(JobName.showBasename());
      LaTeXprinter.print theFile;
      PlotFile.finalise();
      CSVIterator.finalise()
    end

  fun Main args =
    let
    in
      map checkForFlags args;
      Reporting.initialise ("Report generation in progress.\n");
      Configuration.initialise();
      StyleSheet.initialise();

      let 
	  val jobname = getJobName args
      in  
	  JobName.set (JobName.stripExt jobname);
	  Reporting.flush();
          run jobname;
          Reporting.finalise ("Report generation complete.\n")
      end
    end
    handle 
        Error.Fatal_error => 
             Reporting.finalise ("Exiting BioPEPA Workbench abnormally.\n")
      | IO.Io {cause,function,name} =>
	     Error.fatal_error ("BioPEPA Workbench: attempted " ^ function ^ " on [" ^ name ^ "]") 
      | AbruptExit => ()
  (* The main function requests a list of arguments from CommandLine.  *)
  and main () = Main (CommandLine.arguments ()) 
  and command c = Main (String.tokens Char.isSpace c)

end;
