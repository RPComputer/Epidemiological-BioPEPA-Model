(* 
  File: BPWB.sml

  This is the BioPEPA Workbench.
*)

structure BPWB :> BPWB  =
struct 


  fun startsWithHyphen (#"-" :: _) = true
    | startsWithHyphen _ = false

  fun isNotFlag s = not (startsWithHyphen (explode s))


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
                          (Preparser.preparse
                             (Lexer.analyse theFile))))
      in
	JobName.set(JobName.stripExt jobname);
	viewIntermediate("parsed model", M);

	Reporting.report ("Starting Dot file compilation.\n");
	Dot.compile (!M);
	Reporting.report ("Finished Dot file compilation.\n");

	Reporting.report ("Starting StochKit compilation.\n");
	StochKit.compile (!M);
	Reporting.report ("Finished StochKit compilation.\n");

        let val reactions = Reactions.compile (!M)
        in
          CSVIterator.initialise();
          PlotFile.initialise();
          let val parameters = ref(CSVIterator.getParameters())
            in while (!parameters <> []) do 
               let 
                  val modelName = CSVIterator.getFileName()
                  val dizzy = TextIO.openOut (modelName ^ ".dizzy") 
               in 
		  TextIO.output(dizzy, Dizzy.formatHeader(modelName)^"\n");
		  TextIO.output(dizzy, Dizzy.formatParameters(!parameters)^"\n");
		  TextIO.output(dizzy, Dizzy.formatReactions(reactions));
		  Gnuplot.generate(modelName, !parameters);
		  PRISM.generate(modelName, !M, !parameters);
		  CSL.generate(!M);

                  (* For LaTeX embedded in VFgen output *)
		  StyleSheet.readStyleSheet(JobName.showBasename());
		  VFgen.generate(modelName, !M, !parameters);
(*
                  let val plotparameters = PlotFile.getParameters() 
                  in 
	            if plotparameters = [] 
   		    then let val gnuplot = TextIO.openOut (modelName ^ ".dizzy.gnu") 
                         in 
		            TextIO.output(gnuplot, Gnuplot.formatHeader(modelName)^"\n");
		            TextIO.output(gnuplot, 
                                              Gnuplot.formatParameters(modelName)(!parameters)^"\n");
		            TextIO.closeOut gnuplot
                         end 
                    else let val plotfile = TextIO.openOut (modelName ^ ".dizzy.plot.gnu") 
                         in
                            TextIO.output(plotfile, Gnuplot.formatHeader(modelName^"plot")^"\n");
	                    TextIO.output(plotfile,
                                              Gnuplot.formatParametersFiltered(modelName)
                                                  (!parameters)(plotparameters)^"\n");
                             TextIO.closeOut plotfile
                         end
                  end;
*)
                  parameters := CSVIterator.getParameters();
		  TextIO.closeOut dizzy
               end
          end;
          CSVIterator.finalise();
          PlotFile.finalise()
        end
      end
    end

  (* The arguments to the BioPEPA Workbench can include UNIX-style flags 
     -statespaceonly, -nohashing, -maple, -mathematica, -matlab, -compile
     and so forth.  They can include a filename. 
   *)
  fun Main args =
    let
    in
      (* Print "BioPEPA Workbench Version ... [compiled  ...]" *)

      Reporting.initialise (Version.banner ^ "\n");
      Configuration.initialise();
      StyleSheet.initialise();

      let 
	  val jobname = getJobName args
      in  
	  JobName.set (JobName.stripExt jobname);
	  Reporting.flush();
          run jobname;
	  Reporting.finalise ("Compilation complete.\n")
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
