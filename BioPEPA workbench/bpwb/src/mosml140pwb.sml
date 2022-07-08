(* 
  File: mosml140pwb.sml

  Used to compile the PEPA Workbench with Moscow ML 1.40 (June 1996)
  on Solaris machines.
*)

val _ = (CommandLine.setArguments (tl (Mosml.argv())); 
	 PWB.main ());
